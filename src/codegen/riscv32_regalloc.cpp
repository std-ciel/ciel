#include "codegen/riscv32_regalloc.hpp"
#include "codegen/riscv32_backend.hpp"
#include <algorithm>
#include <iostream>
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
    // split_intervals_with_holes();  // Disabled: first def to last use is
    // sufficient
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

void LinearScanAllocator::split_intervals_with_holes()
{
    std::vector<LiveInterval> new_intervals;

    for (auto &interval : intervals_) {
        // Find holes: gaps where vreg is not used or defined
        std::vector<size_t> all_positions;
        for (auto pos : interval.use_positions)
            all_positions.push_back(pos);
        for (auto pos : interval.def_positions)
            all_positions.push_back(pos);
        std::ranges::sort(all_positions);
        all_positions.erase(
            std::unique(all_positions.begin(), all_positions.end()),
            all_positions.end());

        if (all_positions.empty())
            continue;

        // Detect holes: gaps > 1 instruction between consecutive uses/defs
        std::vector<std::pair<size_t, size_t>> segments;
        size_t segment_start = all_positions[0];
        size_t segment_end = all_positions[0];

        for (size_t i = 1; i < all_positions.size(); ++i) {
            if (all_positions[i] <= segment_end + 1) {
                // Consecutive or adjacent - extend current segment
                segment_end = all_positions[i];
            } else {
                // Gap found - close current segment and start new one
                segments.emplace_back(segment_start, segment_end);
                segment_start = all_positions[i];
                segment_end = all_positions[i];
            }
        }
        segments.emplace_back(segment_start, segment_end);

        if (segments.size() > 1) {
            // Split into multiple intervals
            std::cerr << "Splitting vreg" << interval.vreg << " into "
                      << segments.size() << " segments\n";
            for (const auto &[seg_start, seg_end] : segments) {
                auto &new_int = new_intervals.emplace_back(interval.vreg,
                                                           interval.reg_class,
                                                           seg_start,
                                                           seg_end);

                // Copy relevant use/def positions
                for (auto pos : interval.use_positions) {
                    if (pos >= seg_start && pos <= seg_end) {
                        new_int.use_positions.insert(pos);
                    }
                }
                for (auto pos : interval.def_positions) {
                    if (pos >= seg_start && pos <= seg_end) {
                        new_int.def_positions.insert(pos);
                    }
                }
            }
        }
    }

    // Replace split intervals
    if (!new_intervals.empty()) {
        // Remove intervals that were split
        std::unordered_set<VirtReg> split_vregs;
        for (const auto &interval : new_intervals) {
            split_vregs.insert(interval.vreg);
        }

        intervals_.erase(
            std::remove_if(intervals_.begin(),
                           intervals_.end(),
                           [&](const LiveInterval &iv) {
                               return split_vregs.contains(iv.vreg) &&
                                      std::ranges::any_of(
                                          new_intervals,
                                          [&](const LiveInterval &new_iv) {
                                              return new_iv.vreg == iv.vreg &&
                                                     (new_iv.start !=
                                                          iv.start ||
                                                      new_iv.end != iv.end);
                                          });
                           }),
            intervals_.end());

        // Add new split intervals
        intervals_.insert(intervals_.end(),
                          new_intervals.begin(),
                          new_intervals.end());
    }
}

void LinearScanAllocator::build_live_intervals()
{
    const auto &instructions = mfn_.get_instructions();

    // Use optional to avoid sentinel ambiguity
    std::unordered_map<VirtReg, std::optional<std::pair<size_t, size_t>>>
        ranges; // First pass: compute precise live ranges from uses/defs
    for (size_t pos = 0; pos < instructions.size(); ++pos) {
        const auto &instr = instructions[pos];

        for (const auto vreg : instr.get_uses()) {
            if (vreg == INVALID_VREG)
                continue;
            auto &range_opt = ranges[vreg];
            if (!range_opt.has_value()) {
                range_opt = std::make_pair(pos, pos);
            } else {
                auto &[start, end] = *range_opt;
                start = std::min(start, pos);
                end = std::max(end, pos);
            }
        }

        for (const auto vreg : instr.get_defs()) {
            if (vreg == INVALID_VREG)
                continue;
            auto &range_opt = ranges[vreg];
            if (!range_opt.has_value()) {
                range_opt = std::make_pair(pos, pos);
            } else {
                auto &[start, end] = *range_opt;
                start = std::min(start, pos);
                end = std::max(end, pos);
            }
        }
    }

    // Second pass: extend ranges for live-in/live-out to block boundaries
    // ONLY if the vreg is actually live at those points
    for (const auto &block : basic_blocks_) {
        // For live_in: extend START to block start (vreg must be live from
        // entry)
        for (const auto vreg : block.live_in) {
            auto &range_opt = ranges[vreg];
            if (range_opt.has_value()) {
                auto &[start, end] = *range_opt;
                start = std::min(start, static_cast<size_t>(block.start_pos));
            }
        }

        // For live_out: extend END to block end (vreg must be live until exit)
        for (const auto vreg : block.live_out) {
            auto &range_opt = ranges[vreg];
            if (range_opt.has_value()) {
                auto &[start, end] = *range_opt;
                end = std::max(end, static_cast<size_t>(block.end_pos));
            }
        }
    }

    // Create intervals and track use/def positions for each vreg
    intervals_.reserve(ranges.size());
    for (const auto &[vreg, range_opt] : ranges) {
        if (!range_opt.has_value())
            continue;

        const auto &[start, end] = *range_opt;
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

    // Detect and split intervals with holes (dead zones)
    // split_intervals_with_holes();  // Disabled: first def to last use is
    // sufficient

    // Sort by start position for linear scan
    std::ranges::sort(intervals_, {}, &LiveInterval::start);

    // Build index map for fast vreg → interval lookup
    for (size_t i = 0; i < intervals_.size(); ++i) {
        vreg_to_interval_idx_[intervals_[i].vreg] = i;
    }

    if (debug_mode_) {
        std::cerr << "\n=== LIVE INTERVALS ===\n";
        for (const auto &interval : intervals_) {
            std::cerr << "vreg" << interval.vreg << ": [" << interval.start
                      << ", " << interval.end << "]";
            std::cerr << " uses={";
            bool first = true;
            for (auto pos : interval.use_positions) {
                if (!first)
                    std::cerr << ",";
                std::cerr << pos;
                first = false;
            }
            std::cerr << "} defs={";
            first = true;
            for (auto pos : interval.def_positions) {
                if (!first)
                    std::cerr << ",";
                std::cerr << pos;
                first = false;
            }
            std::cerr << "}\n";
        }
        std::cerr << "Total instructions: " << instructions.size() << "\n";
        std::cerr << "======================\n\n";
    }
}

void LinearScanAllocator::allocate_registers()
{
    if (debug_mode_) {
        std::cerr << "\n=== REGISTER ALLOCATION ===\n";
    }

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

                if (debug_mode_) {
                    std::cerr << "vreg" << interval.vreg << " -> "
                              << get_reg_name(reg.value()) << "\n";
                }
            } else {
                spill_interval(interval);
                if (debug_mode_) {
                    std::cerr << "vreg" << interval.vreg
                              << " -> SPILL (slot=" << interval.spill_slot
                              << ")\n";
                }
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

                if (debug_mode_) {
                    std::cerr << "vreg" << interval.vreg << " -> "
                              << get_reg_name(reg.value()) << "\n";
                }
            } else {
                spill_interval(interval);
                if (debug_mode_) {
                    std::cerr << "vreg" << interval.vreg
                              << " -> SPILL (slot=" << interval.spill_slot
                              << ")\n";
                }
            }
        }
    }

    if (debug_mode_) {
        std::cerr << "===========================\n\n";
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
                            ? MachineOpcode::LD
                            : MachineOpcode::FLD;

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
                            ? MachineOpcode::SD
                            : MachineOpcode::FSD;

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
            case MachineOpcode::FADD_D:
            case MachineOpcode::FSUB_D:
            case MachineOpcode::FMUL_D:
            case MachineOpcode::FDIV_D:
            case MachineOpcode::FSQRT_D:
            case MachineOpcode::FMIN_D:
            case MachineOpcode::FMAX_D:
            case MachineOpcode::FEQ_D:
            case MachineOpcode::FLT_D:
            case MachineOpcode::FLE_D:
            case MachineOpcode::FMV_D:
            case MachineOpcode::FSGNJ_D:
            case MachineOpcode::FSGNJN_D:
            case MachineOpcode::FSGNJX_D:
                return RegClass::FLOAT;

            case MachineOpcode::FSD:
                // FSD rs2, offset(rs1): rs2 is float data, rs1 is integer
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
            case MachineOpcode::FADD_D:
            case MachineOpcode::FSUB_D:
            case MachineOpcode::FMUL_D:
            case MachineOpcode::FDIV_D:
            case MachineOpcode::FSQRT_D:
            case MachineOpcode::FMIN_D:
            case MachineOpcode::FMAX_D:
            case MachineOpcode::FLD:
            case MachineOpcode::FMV_D:
            case MachineOpcode::FSGNJ_D:
            case MachineOpcode::FSGNJN_D:
            case MachineOpcode::FSGNJX_D:
            case MachineOpcode::FCVT_D_W:
            case MachineOpcode::FCVT_D_WU:
            case MachineOpcode::FCVT_D_L:
            case MachineOpcode::FCVT_D_LU:
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
