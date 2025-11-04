#ifndef CIEL_CODEGEN_RISCV32_REGALLOC_HPP
#define CIEL_CODEGEN_RISCV32_REGALLOC_HPP

#include "riscv32_frame.hpp"
#include "riscv32_instruction.hpp"
#include "riscv32_register.hpp"
#include <optional>
#include <unordered_map>
#include <vector>

namespace ciel {
namespace codegen {
namespace riscv32 {

/// Live interval for a virtual register
struct LiveInterval {
    VirtReg vreg;
    uint32_t start;       // First use position
    uint32_t end;         // Last use position
    PhysReg assigned_reg; // Assigned physical register (or INVALID)
    int32_t spill_slot;   // Spill slot offset (if spilled)
    bool spilled;

    LiveInterval(VirtReg vr, uint32_t s, uint32_t e)
        : vreg(vr), start(s), end(e), assigned_reg(PhysReg::INVALID),
          spill_slot(0), spilled(false)
    {
    }

    bool overlaps(const LiveInterval &other) const
    {
        return !(end < other.start || other.end < start);
    }
};

/// Linear-scan register allocator
/// Simple and fast allocation strategy suitable for baseline codegen
class LinearScanAllocator {
  public:
    LinearScanAllocator(std::vector<MachineInstr> &instrs, FrameLayout &frame);

    /// Run the allocation algorithm
    void run();

    /// Get the allocation result for a virtual register
    std::optional<PhysReg> get_allocation(VirtReg vreg) const;

    /// Check if a virtual register was spilled
    bool is_spilled(VirtReg vreg) const;

    /// Get spill slot for a spilled virtual register
    int32_t get_spill_slot(VirtReg vreg) const;

  private:
    /// Compute live intervals for all virtual registers
    void compute_live_intervals();

    /// Allocate registers using linear scan
    void allocate_registers();

    /// Spill a virtual register to memory
    void spill_interval(LiveInterval &interval);

    /// Expire old intervals that are no longer live
    void expire_old_intervals(const LiveInterval &interval);

    /// Try to allocate a physical register for an interval
    std::optional<PhysReg> try_allocate_reg(const LiveInterval &interval);

    /// Insert spill code for a virtual register
    void insert_spill_code();

    std::vector<MachineInstr> &instructions_;
    FrameLayout &frame_;

    std::vector<LiveInterval> intervals_;
    std::unordered_map<VirtReg, LiveInterval *> vreg_to_interval_;

    // Active intervals (currently allocated to registers)
    std::vector<LiveInterval *> active_;

    // Register allocation result
    std::unordered_map<VirtReg, PhysReg> allocation_;

    // Free physical registers
    std::vector<PhysReg> free_regs_;
};

/// Simple naive allocator (all values in stack slots)
/// Used as a baseline or when register allocation is disabled
class StackOnlyAllocator {
  public:
    StackOnlyAllocator(std::vector<MachineInstr> &instrs, FrameLayout &frame);

    /// Allocate stack slots for all virtual registers
    void run();

  private:
    std::vector<MachineInstr> &instructions_;
    FrameLayout &frame_;
    std::unordered_map<VirtReg, StackSlot> allocations_;
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_REGALLOC_HPP
