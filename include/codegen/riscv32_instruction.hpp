#ifndef CIEL_CODEGEN_RISCV32_INSTRUCTION_HPP
#define CIEL_CODEGEN_RISCV32_INSTRUCTION_HPP

#include "riscv32_register.hpp"
#include <cstdint>
#include <iosfwd>
#include <string>
#include <variant>
#include <vector>

namespace ciel {
namespace codegen {
namespace riscv32 {

/// Machine instruction opcodes for RV32IM
enum class MachineOpcode {
    // Pseudo-instructions
    NOP,
    LI,   // Load immediate
    LA,   // Load address
    MV,   // Move (addi rd, rs, 0)
    J,    // Jump (unconditional)
    JR,   // Jump register
    RET,  // Return (jalr x0, ra, 0)
    CALL, // Function call

    // Arithmetic
    ADD,
    SUB,
    MUL,
    DIV,
    DIVU,
    REM,
    REMU,
    ADDI,

    // Logical
    AND,
    OR,
    XOR,
    ANDI,
    ORI,
    XORI,

    // Shifts
    SLL,
    SRL,
    SRA,
    SLLI,
    SRLI,
    SRAI,

    // Comparisons
    SLT,
    SLTU,
    SLTI,
    SLTIU,

    // Set/compare pseudos
    SEQZ,
    SNEZ,

    // Branches
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,

    // Loads
    LB,
    LH,
    LW,
    LBU,
    LHU,

    // Stores
    SB,
    SH,
    SW,

    // Floating-point loads/stores (F extension)
    FLW, // Load word from memory to FP register
    FSW, // Store word from FP register to memory

    // Floating-point arithmetic (single-precision)
    FADD_S,  // FP add
    FSUB_S,  // FP subtract
    FMUL_S,  // FP multiply
    FDIV_S,  // FP divide
    FSQRT_S, // FP square root
    FMIN_S,  // FP minimum
    FMAX_S,  // FP maximum

    // Floating-point comparisons
    FEQ_S, // FP equal
    FLT_S, // FP less than
    FLE_S, // FP less than or equal

    // Floating-point conversions
    FCVT_W_S,  // Convert float to signed int
    FCVT_WU_S, // Convert float to unsigned int
    FCVT_S_W,  // Convert signed int to float
    FCVT_S_WU, // Convert unsigned int to float

    // Floating-point move/sign injection
    FMV_X_W, // Move float to integer register (bitcast)
    FMV_W_X, // Move integer to float register (bitcast)
    FMV_S, // Move float register to float register (pseudo: fsgnj.s rd, rs, rs)
    FSGNJ_S,  // Sign injection (copy)
    FSGNJN_S, // Sign injection with negation
    FSGNJX_S, // Sign injection with XOR

    // Label (pseudo for code generation)
    LABEL,

    // Directive (pseudo for assembly directives)
    DIRECTIVE
};

struct ImmOperand {
    int32_t value;
    constexpr explicit ImmOperand(int32_t val) noexcept : value(val) {}
};

struct RegOperand {
    PhysReg reg;
    constexpr explicit RegOperand(PhysReg r) noexcept : reg(r) {}
};

struct VRegOperand {
    VirtReg vreg;
    constexpr explicit VRegOperand(VirtReg vr) noexcept : vreg(vr) {}
};

struct LabelOperand {
    std::string label;
    explicit LabelOperand(std::string lbl) : label(std::move(lbl)) {}
};

struct MemOperand {
    PhysReg base;
    int32_t offset;
    constexpr MemOperand(PhysReg b, int32_t off) noexcept : base(b), offset(off)
    {
    }
};

using MachineOperand =
    std::variant<ImmOperand, RegOperand, VRegOperand, LabelOperand, MemOperand>;

/// Machine instruction representation
class MachineInstr {
  public:
    explicit constexpr MachineInstr(MachineOpcode opc) noexcept : opcode_(opc)
    {
    }

    [[nodiscard]] constexpr MachineOpcode get_opcode() const noexcept
    {
        return opcode_;
    }
    [[nodiscard]] constexpr bool is_call() const noexcept
    {
        return opcode_ == MachineOpcode::CALL;
    }

    [[nodiscard]] const auto &defs() const noexcept
    {
        return defs_;
    }
    [[nodiscard]] const auto &uses() const noexcept
    {
        return uses_;
    }
    [[nodiscard]] const auto &operands() const noexcept
    {
        return operands_;
    }
    [[nodiscard]] const std::string &label() const noexcept
    {
        return label_;
    }

    // Legacy accessors for compatibility
    [[nodiscard]] const std::vector<VirtReg> &get_defs() const noexcept
    {
        return defs_;
    }
    [[nodiscard]] const std::vector<VirtReg> &get_uses() const noexcept
    {
        return uses_;
    }
    [[nodiscard]] const std::vector<MachineOperand> &
    get_operands() const noexcept
    {
        return operands_;
    }
    [[nodiscard]] const std::string &get_label() const noexcept
    {
        return label_;
    }

    // Builder pattern
    MachineInstr &add_def(VirtReg vreg)
    {
        defs_.push_back(vreg);
        return *this;
    }
    MachineInstr &add_use(VirtReg vreg)
    {
        uses_.push_back(vreg);
        return *this;
    }
    MachineInstr &add_operand(MachineOperand op)
    {
        operands_.push_back(std::move(op));
        return *this;
    }
    MachineInstr &set_label(std::string lbl)
    {
        label_ = std::move(lbl);
        return *this;
    }

    void replace_vreg(VirtReg vreg, PhysReg preg);
    [[nodiscard]] bool defines(VirtReg vreg) const;
    [[nodiscard]] bool uses(VirtReg vreg) const;
    void emit(std::ostream &os) const;

  private:
    MachineOpcode opcode_;
    std::vector<VirtReg> defs_; // Defined virtual registers
    std::vector<VirtReg> uses_; // Used virtual registers
    std::vector<MachineOperand> operands_;
    std::string label_; // For LABEL pseudo-op
};

/// Helper functions to create common instructions
inline MachineInstr make_add(VirtReg dst, VirtReg src1, VirtReg src2)
{
    return MachineInstr(MachineOpcode::ADD)
        .add_def(dst)
        .add_use(src1)
        .add_use(src2);
}

inline MachineInstr make_addi(VirtReg dst, VirtReg src, int32_t imm)
{
    return MachineInstr(MachineOpcode::ADDI)
        .add_def(dst)
        .add_use(src)
        .add_operand(ImmOperand(imm));
}

inline MachineInstr make_li(VirtReg dst, int32_t imm)
{
    return MachineInstr(MachineOpcode::LI)
        .add_def(dst)
        .add_operand(VRegOperand(dst))
        .add_operand(ImmOperand(imm));
}

inline MachineInstr make_la(VirtReg dst, const std::string &label)
{
    return MachineInstr(MachineOpcode::LA)
        .add_def(dst)
        .add_operand(VRegOperand(dst))
        .add_operand(LabelOperand(label));
}

inline MachineInstr make_mv(VirtReg dst, VirtReg src)
{
    return MachineInstr(MachineOpcode::MV)
        .add_def(dst)
        .add_use(src)
        .add_operand(VRegOperand(dst))
        .add_operand(VRegOperand(src));
}

inline MachineInstr make_lw(VirtReg dst, PhysReg base, int32_t offset)
{
    return MachineInstr(MachineOpcode::LW)
        .add_def(dst)
        .add_operand(VRegOperand(dst))
        .add_operand(MemOperand(base, offset));
}

inline MachineInstr make_sw(VirtReg src, PhysReg base, int32_t offset)
{
    return MachineInstr(MachineOpcode::SW)
        .add_use(src)
        .add_operand(VRegOperand(src))
        .add_operand(MemOperand(base, offset));
}

inline MachineInstr make_label(const std::string &label)
{
    return MachineInstr(MachineOpcode::LABEL).set_label(label);
}

inline MachineInstr make_call(const std::string &target)
{
    return MachineInstr(MachineOpcode::CALL).add_operand(LabelOperand(target));
}

inline MachineInstr make_ret()
{
    return MachineInstr(MachineOpcode::RET);
}

inline MachineInstr make_flw(VirtReg dst, PhysReg base, int32_t offset)
{
    return MachineInstr(MachineOpcode::FLW)
        .add_def(dst)
        .add_operand(MemOperand(base, offset));
}

inline MachineInstr make_fsw(VirtReg src, PhysReg base, int32_t offset)
{
    return MachineInstr(MachineOpcode::FSW)
        .add_use(src)
        .add_operand(MemOperand(base, offset));
}

inline MachineInstr make_fadd_s(VirtReg dst, VirtReg src1, VirtReg src2)
{
    return MachineInstr(MachineOpcode::FADD_S)
        .add_def(dst)
        .add_use(src1)
        .add_use(src2);
}

inline MachineInstr make_fsub_s(VirtReg dst, VirtReg src1, VirtReg src2)
{
    return MachineInstr(MachineOpcode::FSUB_S)
        .add_def(dst)
        .add_use(src1)
        .add_use(src2);
}

inline MachineInstr make_fmul_s(VirtReg dst, VirtReg src1, VirtReg src2)
{
    return MachineInstr(MachineOpcode::FMUL_S)
        .add_def(dst)
        .add_use(src1)
        .add_use(src2);
}

inline MachineInstr make_fdiv_s(VirtReg dst, VirtReg src1, VirtReg src2)
{
    return MachineInstr(MachineOpcode::FDIV_S)
        .add_def(dst)
        .add_use(src1)
        .add_use(src2);
}

struct MachineBasicBlock {
    size_t start_instr_idx{};
    size_t end_instr_idx{};
    std::string label;
    std::vector<size_t> successors;
    std::vector<size_t> predecessors;

    MachineBasicBlock() = default;
    MachineBasicBlock(size_t start, size_t end, std::string lbl = "")
        : start_instr_idx(start), end_instr_idx(end), label(std::move(lbl))
    {
    }
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_INSTRUCTION_HPP
