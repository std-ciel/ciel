#include "codegen/riscv32_regalloc.hpp"
#include <algorithm>
#include <stdexcept>

namespace ciel {
namespace codegen {
namespace riscv32 {

// ============================================================================
// Stack-Only Allocator Implementation (Baseline)
// ============================================================================

StackOnlyAllocator::StackOnlyAllocator(std::vector<MachineInstr> &instrs,
                                       FrameLayout &frame)
    : instructions_(instrs), frame_(frame)
{
}

void StackOnlyAllocator::run()
{
    // For stack-only allocation, we simply ensure every virtual register
    // has a stack slot. The backend will load/store around each use.
    // This is slow but correct and serves as a baseline.

    for (const auto &instr : instructions_) {
        // Allocate slots for all defined virtual registers
        for (auto vreg : instr.get_defs()) {
            if (vreg != INVALID_VREG &&
                allocations_.find(vreg) == allocations_.end()) {
                // Allocate 4-byte slot (default for RV32)
                int32_t offset = frame_.allocate_spill_slot(vreg, 4);
                allocations_.emplace(vreg, StackSlot(offset, 4));
            }
        }

        // Allocate slots for all used virtual registers
        for (auto vreg : instr.get_uses()) {
            if (vreg != INVALID_VREG &&
                allocations_.find(vreg) == allocations_.end()) {
                int32_t offset = frame_.allocate_spill_slot(vreg, 4);
                allocations_.emplace(vreg, StackSlot(offset, 4));
            }
        }
    }
}

// ============================================================================
// Linear-Scan Allocator Implementation
// ============================================================================

LinearScanAllocator::LinearScanAllocator(std::vector<MachineInstr> &instrs,
                                         FrameLayout &frame)
    : instructions_(instrs), frame_(frame)
{
    // Initialize free register pool
    auto allocatable = get_allocatable_regs();
    free_regs_.assign(allocatable.begin(), allocatable.end());
}

void LinearScanAllocator::run()
{
    compute_live_intervals();
    allocate_registers();
    insert_spill_code();
}

void LinearScanAllocator::compute_live_intervals()
{
    // Build def/use positions for each virtual register
    std::unordered_map<VirtReg, std::pair<uint32_t, uint32_t>> vreg_positions;

    for (uint32_t pos = 0; pos < instructions_.size(); ++pos) {
        const auto &instr = instructions_[pos];

        // Record uses
        for (auto vreg : instr.get_uses()) {
            if (vreg == INVALID_VREG)
                continue;

            auto &positions = vreg_positions[vreg];
            if (positions.first == 0) {
                positions.first = pos;
            }
            positions.second = pos;
        }

        // Record defs
        for (auto vreg : instr.get_defs()) {
            if (vreg == INVALID_VREG)
                continue;

            auto &positions = vreg_positions[vreg];
            if (positions.first == 0) {
                positions.first = pos;
            }
            positions.second = pos;
        }
    }

    // Create live intervals
    for (const auto &[vreg, positions] : vreg_positions) {
        intervals_.emplace_back(vreg, positions.first, positions.second);
        vreg_to_interval_[vreg] = &intervals_.back();
    }

    // Sort intervals by start position (required for linear scan)
    std::sort(intervals_.begin(),
              intervals_.end(),
              [](const LiveInterval &a, const LiveInterval &b) {
                  return a.start < b.start;
              });
}

void LinearScanAllocator::allocate_registers()
{
    for (auto &interval : intervals_) {
        // Expire old intervals that are no longer live
        expire_old_intervals(interval);

        // Try to allocate a physical register
        auto reg = try_allocate_reg(interval);

        if (reg.has_value()) {
            interval.assigned_reg = reg.value();
            allocation_[interval.vreg] = reg.value();

            // Mark callee-saved register as used if needed
            if (is_callee_saved(reg.value())) {
                frame_.mark_callee_reg_used(reg.value());
            }

            // Add to active list
            active_.push_back(&interval);
        } else {
            // No free register, spill to stack
            spill_interval(interval);
        }
    }
}

void LinearScanAllocator::expire_old_intervals(const LiveInterval &interval)
{
    // Remove intervals from active list that end before current interval starts
    active_.erase(std::remove_if(active_.begin(),
                                 active_.end(),
                                 [&](LiveInterval *active_interval) {
                                     if (active_interval->end <
                                         interval.start) {
                                         // Interval expired, free its register
                                         free_regs_.push_back(
                                             active_interval->assigned_reg);
                                         return true;
                                     }
                                     return false;
                                 }),
                  active_.end());
}

std::optional<PhysReg>
LinearScanAllocator::try_allocate_reg(const LiveInterval &interval)
{
    if (free_regs_.empty()) {
        return std::nullopt;
    }

    // Allocate the first available register
    PhysReg reg = free_regs_.back();
    free_regs_.pop_back();

    return reg;
}

void LinearScanAllocator::spill_interval(LiveInterval &interval)
{
    interval.spilled = true;

    // Allocate a spill slot in the frame
    interval.spill_slot = frame_.allocate_spill_slot(interval.vreg, 4);
}

void LinearScanAllocator::insert_spill_code()
{
    // For simplicity in this baseline implementation, we'll handle spills
    // during emission by loading before use and storing after def.
    // A more sophisticated implementation would insert explicit spill/reload
    // instructions here.
}

std::optional<PhysReg> LinearScanAllocator::get_allocation(VirtReg vreg) const
{
    auto it = allocation_.find(vreg);
    if (it != allocation_.end()) {
        return it->second;
    }
    return std::nullopt;
}

bool LinearScanAllocator::is_spilled(VirtReg vreg) const
{
    auto it = vreg_to_interval_.find(vreg);
    if (it != vreg_to_interval_.end()) {
        return it->second->spilled;
    }
    return false;
}

int32_t LinearScanAllocator::get_spill_slot(VirtReg vreg) const
{
    auto it = vreg_to_interval_.find(vreg);
    if (it != vreg_to_interval_.end() && it->second->spilled) {
        return it->second->spill_slot;
    }
    throw std::runtime_error("Virtual register not spilled");
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
