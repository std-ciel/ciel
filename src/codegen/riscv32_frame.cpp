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

    // Return negative offset from frame pointer
    return -static_cast<int32_t>(locals_size_);
}

int32_t FrameLayout::allocate_temp(const std::string &temp_name, uint32_t size)
{
    // Check if already allocated
    auto it = temp_slots_.find(temp_name);
    if (it != temp_slots_.end()) {
        return it->second.offset;
    }

    // Allocate new slot
    int32_t offset = allocate_slot(size, std::min(size, 4u));
    temp_slots_.emplace(temp_name, StackSlot(offset, size));

    return offset;
}

StackSlot FrameLayout::get_temp_slot(const std::string &temp_name) const
{
    auto it = temp_slots_.find(temp_name);
    if (it == temp_slots_.end()) {
        throw std::runtime_error("Temporary not found: " + temp_name);
    }
    return it->second;
}

int32_t FrameLayout::allocate_spill_slot(VirtReg vreg, uint32_t size)
{
    auto it = spill_slots_.find(vreg);
    if (it != spill_slots_.end()) {
        return it->second.offset;
    }

    int32_t offset = allocate_slot(size, 4);
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
    // [saved RA]          offset = -4
    // [saved FP/s0]       offset = -8
    // [saved callee regs] offset = -12, -16, ...
    // [locals/spills]     offset = -(12 + 4*n_callee + locals_size)
    // [outgoing args]     offset = -(12 + 4*n_callee + locals_size + arg_area)
    // [alignment padding]

    int32_t current_offset = 0;

    // Reserve space for RA (4 bytes)
    current_offset -= 4;
    ra_offset_ = current_offset;

    // Reserve space for FP/s0 (4 bytes)
    current_offset -= 4;
    fp_offset_ = current_offset;

    // Reserve space for callee-saved registers
    for (auto reg : used_callee_regs_) {
        current_offset -= 4;
        callee_save_offsets_[reg] = current_offset;
    }

    // Add locals/spills area
    current_offset -= locals_size_;

    // Add outgoing argument area (for args beyond a0-a7)
    if (max_call_args_ > 8) {
        uint32_t stack_args = max_call_args_ - 8;
        current_offset -= static_cast<int32_t>(stack_args * 4);
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
