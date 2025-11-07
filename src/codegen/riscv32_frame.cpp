#include "codegen/riscv32_frame.hpp"
#include <algorithm>
#include <stdexcept>

namespace ciel {
namespace codegen {
namespace riscv32 {

int32_t FrameLayout::allocate_slot(uint32_t size, uint32_t alignment)
{
    // Align current locals_size to requested alignment
    uint32_t align_mask = alignment - 1;
    locals_size_ = (locals_size_ + align_mask) & ~align_mask;

    // Allocate from top of locals area (growing downward)
    locals_size_ += size;

    return -(RESERVED_SPACE + locals_size_);
}

int32_t FrameLayout::allocate_spill_slot(VirtReg vreg, uint32_t size)
{
    auto it = spill_slots_.find(vreg);
    if (it != spill_slots_.end()) {
        return it->second.offset;
    }

    int32_t offset = allocate_slot(size, 8);
    spill_slots_.emplace(vreg, StackSlot(offset, size));

    return offset;
}

StackSlot FrameLayout::get_spill_slot(VirtReg vreg) const
{
    auto it = spill_slots_.find(vreg);
    if (it == spill_slots_.end()) {
        throw std::runtime_error("Spill slot not found for vreg");
    }
    return it->second;
}

void FrameLayout::mark_callee_reg_used(PhysReg reg)
{
    if (!is_callee_saved(reg)) {
        return;
    }

    // Check if already marked
    auto it =
        std::find(used_callee_regs_.begin(), used_callee_regs_.end(), reg);
    if (it == used_callee_regs_.end()) {
        used_callee_regs_.push_back(reg);
    }
}

void FrameLayout::update_max_call_args(uint32_t num_args)
{
    max_call_args_ = std::max(max_call_args_, num_args);
}

void FrameLayout::finalize()
{
    // Layout (from high to low address):
    // [caller's frame]
    // [saved RA]          offset = -8 (64-bit)
    // [saved FP/s0]       offset = -16 (64-bit)
    // [saved callee regs] offset = -24, -32, ... (8 bytes each)
    // [locals/spills]     offset = -(16 + 8*n_callee + locals_size)
    // [outgoing args]     offset = -(16 + 8*n_callee + locals_size + arg_area)
    // [alignment padding]

    int32_t current_offset = 0;

    // Reserve space for RA (8 bytes in RV64)
    current_offset -= 8;
    ra_offset_ = current_offset;

    // Reserve space for FP/s0 (8 bytes in RV64)
    current_offset -= 8;
    fp_offset_ = current_offset;

    // Reserve space for callee-saved registers (8 bytes each in RV64)
    for (auto reg : used_callee_regs_) {
        current_offset -= 8;
        callee_save_offsets_[reg] = current_offset;
    }

    // RV64: RESERVED_SPACE = 16 (RA+FP) + 96 (max callee-saves)
    int32_t most_negative_offset = -(RESERVED_SPACE + locals_size_);

    if (current_offset > most_negative_offset) {
        current_offset = most_negative_offset;
    }

    // Add outgoing argument area (for args beyond a0-a7)
    // Each argument is 8 bytes in RV64
    if (max_call_args_ > 8) {
        uint32_t stack_args = max_call_args_ - 8;
        current_offset -= static_cast<int32_t>(stack_args * 8);
    }

    // Align to 16 bytes (ABI requirement)
    int32_t total_size = -current_offset;
    frame_size_ = (total_size + 15) & ~15;
}

int32_t FrameLayout::get_callee_save_offset(PhysReg reg) const
{
    auto it = callee_save_offsets_.find(reg);
    if (it == callee_save_offsets_.end()) {
        throw std::runtime_error("Callee register not saved");
    }
    return it->second;
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
