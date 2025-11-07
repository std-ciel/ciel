#ifndef CIEL_CODEGEN_RISCV32_FRAME_HPP
#define CIEL_CODEGEN_RISCV32_FRAME_HPP

#include "riscv32_register.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace ciel {
namespace codegen {
namespace riscv32 {

/// Stack slot for a virtual register or temporary
struct StackSlot {
    int32_t offset; // Offset from frame pointer (negative = below FP)
    uint32_t size;  // Size in bytes (1, 2, 4)

    StackSlot(int32_t off, uint32_t sz) : offset(off), size(sz) {}
};

/// Frame layout manager for a function
/// Manages stack allocation following RV32 ABI:
/// - 16-byte stack alignment at call sites
/// - Frame pointer (s0) points to saved s0 location
/// - Layout (high to low address):
///     [caller's frame]
///     [return address]        <- saved by caller
///     [saved s0/fp]          <- fp points here
///     [saved callee regs]
///     [local variables/spills]
///     [outgoing args > 8]
///     [padding for alignment] <- sp points here
// RV64: RA and FP are 8 bytes each

constexpr int32_t FIXED_OVERHEAD = 16;
// RV64: Each callee-saved register is 8 bytes, max 12 regs (s1-s11 + FP
// already counted)
constexpr int32_t MAX_CALLEE_SAVES = 96;
constexpr int32_t RESERVED_SPACE = FIXED_OVERHEAD + MAX_CALLEE_SAVES;

class FrameLayout {
  public:
    explicit FrameLayout(const std::string &fn_name)
        : function_name_(fn_name), frame_size_(0), locals_size_(0),
          max_call_args_(0)
    {
    }

    /// Allocate a stack slot for a virtual register or temporary
    /// Returns the offset from frame pointer (negative)
    int32_t allocate_slot(uint32_t size, uint32_t alignment = 4);

    /// Allocate spill slot for a virtual register
    int32_t allocate_spill_slot(VirtReg vreg, uint32_t size);

    /// Get spill slot for a virtual register
    StackSlot get_spill_slot(VirtReg vreg) const;

    /// Mark that a callee-saved register is used (needs save/restore)
    void mark_callee_reg_used(PhysReg reg);

    /// Update maximum number of arguments in any call
    void update_max_call_args(uint32_t num_args);

    /// Finalize frame layout (compute total size, alignment)
    void finalize();

    /// Get total frame size (always 16-byte aligned)
    int32_t get_frame_size() const
    {
        return frame_size_;
    }

    /// Get offset to save/restore callee-saved register
    int32_t get_callee_save_offset(PhysReg reg) const;

    /// Get list of callee-saved registers used in this function
    const std::vector<PhysReg> &get_used_callee_regs() const
    {
        return used_callee_regs_;
    }

    /// Get offset for return address save slot
    int32_t get_ra_offset() const
    {
        return ra_offset_;
    }

    /// Get offset for saved frame pointer
    int32_t get_fp_offset() const
    {
        return fp_offset_;
    }

  private:
    std::string function_name_;
    int32_t frame_size_;     // Total frame size (16-byte aligned)
    int32_t locals_size_;    // Current size of locals area
    int32_t ra_offset_;      // Offset to saved RA
    int32_t fp_offset_;      // Offset to saved FP (s0)
    uint32_t max_call_args_; // Max args in any call (for outgoing area)

    // Mapping from virtual registers to spill slots
    std::unordered_map<VirtReg, StackSlot> spill_slots_;

    // Callee-saved registers used
    std::vector<PhysReg> used_callee_regs_;
    std::unordered_map<PhysReg, int32_t> callee_save_offsets_;
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_FRAME_HPP
