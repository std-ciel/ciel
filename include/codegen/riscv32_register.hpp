#ifndef CIEL_CODEGEN_RISCV32_REGISTER_HPP
#define CIEL_CODEGEN_RISCV32_REGISTER_HPP

#include <array>
#include <cstdint>
#include <string_view>

namespace ciel {
namespace codegen {
namespace riscv32 {

/// Physical register enumeration for RV32I
enum class PhysReg : uint8_t {
    // Zero register (hardwired to 0)
    ZERO = 0,

    // Return address
    RA = 1,

    // Stack pointer
    SP = 2,

    // Global pointer
    GP = 3,

    // Thread pointer
    TP = 4,

    // Temporaries (caller-saved)
    T0 = 5,
    T1 = 6,
    T2 = 7,

    // Frame pointer / Saved register (callee-saved)
    S0_FP = 8,

    // Saved register (callee-saved)
    S1 = 9,

    // Function arguments / return values (caller-saved)
    A0 = 10,
    A1 = 11,
    A2 = 12,
    A3 = 13,
    A4 = 14,
    A5 = 15,
    A6 = 16,
    A7 = 17,

    // Saved registers (callee-saved)
    S2 = 18,
    S3 = 19,
    S4 = 20,
    S5 = 21,
    S6 = 22,
    S7 = 23,
    S8 = 24,
    S9 = 25,
    S10 = 26,
    S11 = 27,

    // Temporaries (caller-saved)
    T3 = 28,
    T4 = 29,
    T5 = 30,
    T6 = 31,

    // Floating-point registers (F extension, RV32F)
    // Temporary FP registers (caller-saved)
    FT0 = 32,
    FT1 = 33,
    FT2 = 34,
    FT3 = 35,
    FT4 = 36,
    FT5 = 37,
    FT6 = 38,
    FT7 = 39,
    FT8 = 40,
    FT9 = 41,
    FT10 = 42,
    FT11 = 43,

    // Saved FP registers (callee-saved)
    FS0 = 44,
    FS1 = 45,
    FS2 = 46,
    FS3 = 47,
    FS4 = 48,
    FS5 = 49,
    FS6 = 50,
    FS7 = 51,
    FS8 = 52,
    FS9 = 53,
    FS10 = 54,
    FS11 = 55,

    // FP argument/return value registers (caller-saved)
    FA0 = 56,
    FA1 = 57,
    FA2 = 58,
    FA3 = 59,
    FA4 = 60,
    FA5 = 61,
    FA6 = 62,
    FA7 = 63,

    // Pseudo-register for invalid/unallocated
    INVALID = 255
};

/// Virtual register identifier (unlimited supply during instruction selection)
using VirtReg = uint32_t;
constexpr VirtReg INVALID_VREG = 0;

constexpr std::string_view get_reg_name(PhysReg reg) noexcept
{
    constexpr std::array<std::string_view, 64> names = {
        // Integer registers (x0-x31)
        "zero",
        "ra",
        "sp",
        "gp",
        "tp",
        "t0",
        "t1",
        "t2",
        "s0",
        "s1",
        "a0",
        "a1",
        "a2",
        "a3",
        "a4",
        "a5",
        "a6",
        "a7",
        "s2",
        "s3",
        "s4",
        "s5",
        "s6",
        "s7",
        "s8",
        "s9",
        "s10",
        "s11",
        "t3",
        "t4",
        "t5",
        "t6",
        // Floating-point registers (f0-f31)
        "ft0",
        "ft1",
        "ft2",
        "ft3",
        "ft4",
        "ft5",
        "ft6",
        "ft7",
        "ft8",
        "ft9",
        "ft10",
        "ft11",
        "fs0",
        "fs1",
        "fs2",
        "fs3",
        "fs4",
        "fs5",
        "fs6",
        "fs7",
        "fs8",
        "fs9",
        "fs10",
        "fs11",
        "fa0",
        "fa1",
        "fa2",
        "fa3",
        "fa4",
        "fa5",
        "fa6",
        "fa7"};

    const auto idx = static_cast<uint8_t>(reg);
    if (idx < 64) {
        return names[idx];
    }
    return "INVALID";
}

constexpr bool is_caller_saved(PhysReg reg) noexcept
{
    return reg == PhysReg::RA || (reg >= PhysReg::T0 && reg <= PhysReg::T2) ||
           (reg >= PhysReg::A0 && reg <= PhysReg::A7) ||
           (reg >= PhysReg::T3 && reg <= PhysReg::T6) ||
           (reg >= PhysReg::FT0 && reg <= PhysReg::FT11) ||
           (reg >= PhysReg::FA0 && reg <= PhysReg::FA7);
}

constexpr bool is_callee_saved(PhysReg reg) noexcept
{
    return (reg >= PhysReg::S0_FP && reg <= PhysReg::S1) ||
           (reg >= PhysReg::S2 && reg <= PhysReg::S11) ||
           (reg >= PhysReg::FS0 && reg <= PhysReg::FS11);
}

constexpr bool is_allocatable(PhysReg reg) noexcept
{
    // Exclude: zero, sp, gp, tp, ra (special purpose)
    // Exclude: s0/fp (reserved as frame pointer)
    return reg != PhysReg::ZERO && reg != PhysReg::SP && reg != PhysReg::GP &&
           reg != PhysReg::TP && reg != PhysReg::RA && reg != PhysReg::S0_FP &&
           reg <= PhysReg::T6; // Only integer registers
}

constexpr bool is_fp_allocatable(PhysReg reg) noexcept
{
    return reg >= PhysReg::FT0 && reg <= PhysReg::FA7;
}

/// Get all allocatable integer registers in allocation order
/// Priority: temporaries first (no save/restore), then saved regs
inline const std::array<PhysReg, 26> &get_allocatable_regs() noexcept
{
    static const std::array<PhysReg, 26> regs = {
        {// Temporaries (no save/restore cost) - 7 regs
         PhysReg::T0,
         PhysReg::T1,
         PhysReg::T2,
         PhysReg::T3,
         PhysReg::T4,
         PhysReg::T5,
         PhysReg::T6,

         // Arguments (used for args but available for allocation) - 8 regs
         PhysReg::A0,
         PhysReg::A1,
         PhysReg::A2,
         PhysReg::A3,
         PhysReg::A4,
         PhysReg::A5,
         PhysReg::A6,
         PhysReg::A7,

         // Saved registers (require prologue/epilogue save/restore) - 11 regs
         PhysReg::S1,
         PhysReg::S2,
         PhysReg::S3,
         PhysReg::S4,
         PhysReg::S5,
         PhysReg::S6,
         PhysReg::S7,
         PhysReg::S8,
         PhysReg::S9,
         PhysReg::S10,
         PhysReg::S11}};
    return regs;
}

/// Get all allocatable floating-point registers in allocation order
/// Priority: temporaries first (no save/restore), then saved regs
inline const std::array<PhysReg, 32> &get_fp_allocatable_regs() noexcept
{
    static const std::array<PhysReg, 32> regs = {
        {// FP Temporaries (no save/restore cost) - 12 regs
         PhysReg::FT0,
         PhysReg::FT1,
         PhysReg::FT2,
         PhysReg::FT3,
         PhysReg::FT4,
         PhysReg::FT5,
         PhysReg::FT6,
         PhysReg::FT7,
         PhysReg::FT8,
         PhysReg::FT9,
         PhysReg::FT10,
         PhysReg::FT11,

         // FP Arguments (used for args but available for allocation) - 8 regs
         PhysReg::FA0,
         PhysReg::FA1,
         PhysReg::FA2,
         PhysReg::FA3,
         PhysReg::FA4,
         PhysReg::FA5,
         PhysReg::FA6,
         PhysReg::FA7,

         // FP Saved registers (require prologue/epilogue save/restore) - 12
         // regs
         PhysReg::FS0,
         PhysReg::FS1,
         PhysReg::FS2,
         PhysReg::FS3,
         PhysReg::FS4,
         PhysReg::FS5,
         PhysReg::FS6,
         PhysReg::FS7,
         PhysReg::FS8,
         PhysReg::FS9,
         PhysReg::FS10,
         PhysReg::FS11}};
    return regs;
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_REGISTER_HPP
