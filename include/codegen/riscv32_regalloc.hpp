#ifndef CIEL_CODEGEN_RISCV32_REGALLOC_HPP
#define CIEL_CODEGEN_RISCV32_REGALLOC_HPP

#include "codegen/riscv32_register.hpp"
#include <cstdint>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace ciel {
namespace codegen {
namespace riscv32 {

enum class RegClass { INTEGER, FLOAT };

using Offset = int32_t;

struct LocationInfo {
    std::variant<PhysReg, Offset> value;

    [[nodiscard]] static constexpr LocationInfo
    in_register(PhysReg reg) noexcept
    {
        return LocationInfo{.value = reg};
    }

    [[nodiscard]] static constexpr LocationInfo on_stack(Offset offset) noexcept
    {
        return LocationInfo{.value = offset};
    }

    [[nodiscard]] constexpr bool is_register() const noexcept
    {
        return std::holds_alternative<PhysReg>(value);
    }

    [[nodiscard]] constexpr bool is_stack() const noexcept
    {
        return std::holds_alternative<Offset>(value);
    }

    [[nodiscard]] constexpr PhysReg get_register() const
    {
        return std::get<PhysReg>(value);
    }

    [[nodiscard]] constexpr Offset get_stack_offset() const
    {
        return std::get<Offset>(value);
    }
};

struct BasicBlock {
    size_t start_pos{};
    size_t end_pos{};
    std::vector<size_t> successors;
    std::vector<size_t> predecessors;
    std::unordered_set<VirtReg> live_in;
    std::unordered_set<VirtReg> live_out;
    std::unordered_set<VirtReg> gen;
    std::unordered_set<VirtReg> kill;
};

struct LiveInterval {
    VirtReg vreg;
    RegClass reg_class;
    size_t start;
    size_t end;
    PhysReg assigned_reg{PhysReg::INVALID};
    int32_t spill_slot{0};
    bool spilled{false};
    std::set<size_t> use_positions;
    std::set<size_t> def_positions;

    LiveInterval(VirtReg vr, RegClass rc, uint32_t s, uint32_t e)
        : vreg(vr), reg_class(rc), start(s), end(e)
    {
    }

    [[nodiscard]] constexpr bool
    overlaps(const LiveInterval &other) const noexcept
    {
        return !(end < other.start || other.end < start);
    }

    [[nodiscard]] constexpr bool is_live_at(uint32_t pos) const noexcept
    {
        return pos >= start && pos <= end;
    }
};

class MachineFunction;

class LinearScanAllocator {
  public:
    explicit LinearScanAllocator(MachineFunction &mfn);

    void run();
    [[nodiscard]] std::optional<PhysReg> get_allocation(VirtReg vreg) const;
    [[nodiscard]] bool is_spilled(VirtReg vreg) const;
    [[nodiscard]] int32_t get_spill_slot(VirtReg vreg) const;
    [[nodiscard]] LocationInfo get_location(VirtReg vreg) const;

  private:
    void compute_liveness();
    void build_live_intervals();
    void split_intervals_with_holes();
    void allocate_registers();
    void expire_old_intervals(const LiveInterval &current,
                              std::vector<size_t> &active,
                              std::vector<PhysReg> &free_pool);
    [[nodiscard]] std::optional<PhysReg>
    try_allocate_reg(const LiveInterval &interval,
                     std::vector<PhysReg> &free_pool);
    void spill_interval(LiveInterval &interval);
    void handle_call_sites();
    void rewrite_instructions();
    [[nodiscard]] RegClass determine_reg_class(VirtReg vreg) const;

    MachineFunction &mfn_;
    std::vector<BasicBlock> basic_blocks_;
    std::vector<LiveInterval> intervals_;
    std::unordered_map<VirtReg, size_t> vreg_to_interval_idx_;
    std::vector<size_t> active_int_;
    std::vector<size_t> active_fp_;
    std::unordered_map<VirtReg, LocationInfo> location_map_;
    std::vector<PhysReg> free_int_regs_;
    std::vector<PhysReg> free_fp_regs_;
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_REGALLOC_HPP
