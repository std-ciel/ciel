#include "codegen/riscv32_regalloc.hpp"
#include "codegen/riscv32_backend.hpp"
#include <algorithm>
#include <ranges>
#include <stdexcept>

namespace ciel {
namespace codegen {
namespace riscv32 {

LinearScanAllocator::LinearScanAllocator(MachineFunction &mfn) : mfn_(mfn)
{
    const auto int_allocatable = get_allocatable_regs();
    free_int_regs_.assign(int_allocatable.begin(), int_allocatable.end());

    const auto fp_allocatable = get_fp_allocatable_regs();
    free_fp_regs_.assign(fp_allocatable.begin(), fp_allocatable.end());
}

void LinearScanAllocator::run()
{
    const auto &instructions = mfn_.get_instructions();
    if (instructions.empty())
        return;

    // Convert MachineBasicBlocks to internal BasicBlock representation
    // (includes liveness sets which are computed, not stored in
    // MachineFunction)
    const auto &machine_bbs = mfn_.get_basic_blocks();
    basic_blocks_.reserve(machine_bbs.size());
    for (const auto &mbb : machine_bbs) {
        auto &bb = basic_blocks_.emplace_back();
        bb.start_pos = static_cast<uint32_t>(mbb.start_instr_idx);
        bb.end_pos = static_cast<uint32_t>(mbb.end_instr_idx);
        bb.successors.assign(mbb.successors.begin(), mbb.successors.end());
        bb.predecessors.assign(mbb.predecessors.begin(),
                               mbb.predecessors.end());
    }

    compute_liveness();
    build_live_intervals();
    allocate_registers();
    handle_call_sites();
    rewrite_instructions();
}

void LinearScanAllocator::compute_liveness()
{
    const auto &instructions = mfn_.get_instructions();

    // Compute gen/kill sets: gen = upward-exposed uses, kill = definitions
    for (auto &block : basic_blocks_) {
        for (uint32_t pos = block.start_pos; pos <= block.end_pos; ++pos) {
            const auto &instr = instructions[pos];

            for (const auto vreg : instr.get_uses()) {
                if (vreg != INVALID_VREG && !block.kill.contains(vreg)) {
                    block.gen.insert(vreg);
                }
            }

            for (const auto vreg : instr.get_defs()) {
                if (vreg != INVALID_VREG) {
                    block.kill.insert(vreg);
                }
            }
        }
    }

    // Fixed-point iteration: propagate liveness backwards through CFG
    // live_in[B] = gen[B] ∪ (live_out[B] - kill[B])
    // live_out[B] = ∪_{S ∈ succ[B]} live_in[S]
    for (bool changed = true; changed;) {
        changed = false;

        for (auto &block : basic_blocks_ | std::views::reverse) {
            std::unordered_set<VirtReg> new_live_out;
            for (const auto succ_idx : block.successors) {
                const auto &succ = basic_blocks_[succ_idx];
                new_live_out.insert(succ.live_in.begin(), succ.live_in.end());
            }

            auto new_live_in = new_live_out;
            for (const auto vreg : block.kill) {
                new_live_in.erase(vreg);
            }
            new_live_in.insert(block.gen.begin(), block.gen.end());

            if (new_live_in != block.live_in ||
                new_live_out != block.live_out) {
                block.live_in = std::move(new_live_in);
                block.live_out = std::move(new_live_out);
                changed = true;
            }
        }
    }
}

void LinearScanAllocator::build_live_intervals()
{
    // Very conservative approach, may over-approximate live ranges
    const auto &instructions = mfn_.get_instructions();
    std::unordered_map<VirtReg, std::pair<size_t, size_t>> ranges;

    // Compute live ranges from liveness info and explicit uses/defs
    for (const auto &block : basic_blocks_) {
        for (const auto vreg : block.live_in) {
            auto &[start, end] = ranges[vreg];
            if (start == 0 && end == 0)
                start = block.start_pos;
            end = std::max(end, block.end_pos);
        }

        for (size_t pos = block.start_pos; pos <= block.end_pos; ++pos) {
            const auto &instr = instructions[pos];

            for (const auto vreg : instr.get_uses()) {
                if (vreg == INVALID_VREG)
                    continue;
                auto &[start, end] = ranges[vreg];
                if (start == 0 && end == 0)
                    start = pos;
                end = std::max(end, pos);
            }

            for (const auto vreg : instr.get_defs()) {
                if (vreg == INVALID_VREG)
                    continue;
                auto &[start, end] = ranges[vreg];
                if (start == 0 && end == 0)
                    start = pos;
                end = std::max(end, pos);
            }
        }

        for (const auto vreg : block.live_out) {
            auto &[start, end] = ranges[vreg];
            if (start == 0 && end == 0)
                start = block.start_pos;
            end = std::max(end, block.end_pos);
        }
    }

    // Create intervals and track use/def positions for each vreg
    intervals_.reserve(ranges.size());
    for (const auto &[vreg, range] : ranges) {
        const auto [start, end] = range;
        auto &interval = intervals_.emplace_back(vreg,
                                                 determine_reg_class(vreg),
                                                 start,
                                                 end);

        for (uint32_t pos = start; pos <= end; ++pos) {
            const auto &instr = instructions[pos];
            for (const auto v : instr.get_uses()) {
                if (v == vreg)
                    interval.use_positions.insert(pos);
            }
            for (const auto v : instr.get_defs()) {
                if (v == vreg)
                    interval.def_positions.insert(pos);
            }
        }
    }

    // Sort by start position for linear scan
    std::ranges::sort(intervals_, {}, &LiveInterval::start);

    // Build index map for fast vreg → interval lookup
    for (size_t i = 0; i < intervals_.size(); ++i) {
        vreg_to_interval_idx_[intervals_[i].vreg] = i;
    }
}

void LinearScanAllocator::allocate_registers()
{
    for (size_t idx = 0; idx < intervals_.size(); ++idx) {
        auto &interval = intervals_[idx];

        if (interval.reg_class == RegClass::INTEGER) {
            expire_old_intervals(interval, active_int_, free_int_regs_);

            auto reg = try_allocate_reg(interval, free_int_regs_);

            if (reg.has_value()) {
                interval.assigned_reg = reg.value();
                location_map_[interval.vreg] =
                    LocationInfo::in_register(reg.value());

                if (is_callee_saved(reg.value())) {
                    mfn_.get_frame().mark_callee_reg_used(reg.value());
                }

                active_int_.push_back(idx);
            } else {
                spill_interval(interval);
            }
        } else {
            expire_old_intervals(interval, active_fp_, free_fp_regs_);

            auto reg = try_allocate_reg(interval, free_fp_regs_);

            if (reg.has_value()) {
                interval.assigned_reg = reg.value();
                location_map_[interval.vreg] =
                    LocationInfo::in_register(reg.value());

                if (is_callee_saved(reg.value())) {
                    mfn_.get_frame().mark_callee_reg_used(reg.value());
                }

                active_fp_.push_back(idx);
            } else {
                spill_interval(interval);
            }
        }
    }
}

void LinearScanAllocator::expire_old_intervals(const LiveInterval &current,
                                               std::vector<size_t> &active,
                                               std::vector<PhysReg> &free_pool)
{
    active.erase(std::remove_if(active.begin(),
                                active.end(),
                                [&](size_t idx) {
                                    const auto &interval = intervals_[idx];
                                    if (interval.end < current.start) {
                                        free_pool.push_back(
                                            interval.assigned_reg);
                                        return true;
                                    }
                                    return false;
                                }),
                 active.end());
}

std::optional<PhysReg>
LinearScanAllocator::try_allocate_reg(const LiveInterval &interval,
                                      std::vector<PhysReg> &free_pool)
{
    if (free_pool.empty()) {
        return std::nullopt;
    }

    PhysReg reg = free_pool.front();
    free_pool.erase(free_pool.begin());
    return reg;
}

void LinearScanAllocator::spill_interval(LiveInterval &interval)
{
    interval.spilled = true;
    interval.spill_slot =
        mfn_.get_frame().allocate_spill_slot(interval.vreg, 4);
    location_map_[interval.vreg] = LocationInfo::on_stack(interval.spill_slot);
}

void LinearScanAllocator::handle_call_sites()
{
    const auto &instructions = mfn_.get_instructions();

    // Spill caller-saved registers live across function calls
    // (callee-saved regs are preserved by callee, no spill needed)
    for (uint32_t pos = 0; pos < instructions.size(); ++pos) {
        if (!instructions[pos].is_call())
            continue;

        for (auto &interval : intervals_) {
            if (interval.is_live_at(pos) && !interval.spilled &&
                is_caller_saved(interval.assigned_reg)) {
                interval.spilled = true;
                interval.spill_slot =
                    mfn_.get_frame().allocate_spill_slot(interval.vreg, 4);
                location_map_[interval.vreg] =
                    LocationInfo::on_stack(interval.spill_slot);
            }
        }
    }
}

void LinearScanAllocator::rewrite_instructions()
{
    auto &instructions = mfn_.get_instructions();
    std::vector<MachineInstr> new_instructions;
    new_instructions.reserve(instructions.size() * 2);

    constexpr auto int_temps = get_int_spill_temps();
    constexpr auto fp_temps = get_fp_spill_temps();

    for (auto &instr : instructions) {
        const auto original_uses = instr.get_uses();
        const auto original_defs = instr.get_defs();
        std::unordered_map<VirtReg, PhysReg> spilled_to_temp;

        size_t next_int_temp = 0;
        size_t next_fp_temp = 0;

        // Insert loads for spilled uses before instruction
        // Cycle through reserved temps (T0-T2, FT0-FT2) to handle multiple
        // spills
        for (const auto vreg : original_uses) {
            if (vreg == INVALID_VREG)
                continue;

            if (const auto it = vreg_to_interval_idx_.find(vreg);
                it != vreg_to_interval_idx_.end()) {
                const auto &interval = intervals_[it->second];

                if (interval.spilled && !spilled_to_temp.contains(vreg)) {
                    const PhysReg temp =
                        (interval.reg_class == RegClass::INTEGER)
                            ? int_temps[next_int_temp++ % int_temps.size()]
                            : fp_temps[next_fp_temp++ % fp_temps.size()];

                    const auto opcode =
                        (interval.reg_class == RegClass::INTEGER)
                            ? MachineOpcode::LW
                            : MachineOpcode::FLW;

                    new_instructions.emplace_back(opcode)
                        .add_operand(RegOperand(temp))
                        .add_operand(
                            MemOperand(PhysReg::S0_FP, interval.spill_slot));

                    spilled_to_temp[vreg] = temp;
                }
            }
        }

        // Replace vregs with allocated physical registers
        for (const auto &interval : intervals_) {
            if (!interval.spilled &&
                interval.assigned_reg != PhysReg::INVALID) {
                instr.replace_vreg(interval.vreg, interval.assigned_reg);
            }
        }

        // Replace spilled vregs with their temp registers
        for (const auto &[vreg, temp] : spilled_to_temp) {
            instr.replace_vreg(vreg, temp);
        }

        new_instructions.push_back(std::move(instr));

        // Insert stores for spilled defs after instruction
        for (const auto vreg : original_defs) {
            if (vreg == INVALID_VREG)
                continue;

            if (const auto it = vreg_to_interval_idx_.find(vreg);
                it != vreg_to_interval_idx_.end()) {
                const auto &interval = intervals_[it->second];

                if (interval.spilled) {
                    const PhysReg temp =
                        spilled_to_temp.contains(vreg)
                            ? spilled_to_temp[vreg]
                            : (interval.reg_class == RegClass::INTEGER
                                   ? int_temps[0]
                                   : fp_temps[0]);

                    const auto opcode =
                        (interval.reg_class == RegClass::INTEGER)
                            ? MachineOpcode::SW
                            : MachineOpcode::FSW;

                    new_instructions.emplace_back(opcode)
                        .add_operand(RegOperand(temp))
                        .add_operand(
                            MemOperand(PhysReg::S0_FP, interval.spill_slot));
                }
            }
        }
    }

    instructions = std::move(new_instructions);
}

RegClass LinearScanAllocator::determine_reg_class(VirtReg vreg) const
{
    const auto &instructions = mfn_.get_instructions();

    // Infer register class from instruction opcodes
    // Default to INTEGER; float ops explicitly identified
    for (const auto &instr : instructions) {
        for (const auto v : instr.get_uses()) {
            if (v != vreg)
                continue;

            switch (instr.get_opcode()) {
            case MachineOpcode::FADD_S:
            case MachineOpcode::FSUB_S:
            case MachineOpcode::FMUL_S:
            case MachineOpcode::FDIV_S:
            case MachineOpcode::FSQRT_S:
            case MachineOpcode::FMIN_S:
            case MachineOpcode::FMAX_S:
            case MachineOpcode::FEQ_S:
            case MachineOpcode::FLT_S:
            case MachineOpcode::FLE_S:
            case MachineOpcode::FMV_S:
            case MachineOpcode::FSGNJ_S:
            case MachineOpcode::FSGNJN_S:
            case MachineOpcode::FSGNJX_S:
                return RegClass::FLOAT;

            case MachineOpcode::FSW:
                // FSW rs2, offset(rs1): rs2 is float data, rs1 is integer
                // address
                if (!instr.get_uses().empty() && instr.get_uses()[0] == vreg) {
                    return RegClass::FLOAT;
                }
                break;
            default:
                break;
            }
        }

        for (const auto v : instr.get_defs()) {
            if (v != vreg)
                continue;

            switch (instr.get_opcode()) {
            case MachineOpcode::FADD_S:
            case MachineOpcode::FSUB_S:
            case MachineOpcode::FMUL_S:
            case MachineOpcode::FDIV_S:
            case MachineOpcode::FSQRT_S:
            case MachineOpcode::FMIN_S:
            case MachineOpcode::FMAX_S:
            case MachineOpcode::FLW:
            case MachineOpcode::FMV_S:
            case MachineOpcode::FSGNJ_S:
            case MachineOpcode::FSGNJN_S:
            case MachineOpcode::FSGNJX_S:
            case MachineOpcode::FCVT_S_W:
            case MachineOpcode::FCVT_S_WU:
                return RegClass::FLOAT;
            default:
                break;
            }
        }
    }

    return RegClass::INTEGER;
}

std::optional<PhysReg> LinearScanAllocator::get_allocation(VirtReg vreg) const
{
    const auto it = location_map_.find(vreg);
    if (it != location_map_.end() && it->second.is_register()) {
        return it->second.get_register();
    }
    return std::nullopt;
}

bool LinearScanAllocator::is_spilled(VirtReg vreg) const
{
    const auto it = vreg_to_interval_idx_.find(vreg);
    return it != vreg_to_interval_idx_.end() && intervals_[it->second].spilled;
}

int32_t LinearScanAllocator::get_spill_slot(VirtReg vreg) const
{
    const auto it = vreg_to_interval_idx_.find(vreg);
    if (it != vreg_to_interval_idx_.end()) {
        const auto &interval = intervals_[it->second];
        if (interval.spilled) {
            return interval.spill_slot;
        }
    }
    throw std::runtime_error("Virtual register not spilled");
}

LocationInfo LinearScanAllocator::get_location(VirtReg vreg) const
{
    const auto it = location_map_.find(vreg);
    if (it != location_map_.end()) {
        return it->second;
    }
    throw std::runtime_error("Virtual register has no location");
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
