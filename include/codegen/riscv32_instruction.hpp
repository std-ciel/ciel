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

/// Operand types for machine instructions
struct ImmOperand {
    int32_t value;
    explicit ImmOperand(int32_t val) : value(val) {}
};

struct RegOperand {
    PhysReg reg;
    explicit RegOperand(PhysReg r) : reg(r) {}
};

struct VRegOperand {
    VirtReg vreg;
    explicit VRegOperand(VirtReg vr) : vreg(vr) {}
};

struct LabelOperand {
    std::string label;
    explicit LabelOperand(std::string lbl) : label(std::move(lbl)) {}
};

struct MemOperand {
    PhysReg base;
    int32_t offset;
    MemOperand(PhysReg b, int32_t off) : base(b), offset(off) {}
};

using MachineOperand =
    std::variant<ImmOperand, RegOperand, VRegOperand, LabelOperand, MemOperand>;

/// Machine instruction representation
class MachineInstr {
  public:
    explicit MachineInstr(MachineOpcode opc) : opcode_(opc) {}

    MachineOpcode get_opcode() const
    {
        return opcode_;
    }

    // Builder-style interface
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

    const std::vector<VirtReg> &get_defs() const
    {
        return defs_;
    }
    const std::vector<VirtReg> &get_uses() const
    {
        return uses_;
    }
    const std::vector<MachineOperand> &get_operands() const
    {
        return operands_;
    }
    const std::string &get_label() const
    {
        return label_;
    }

    /// Replace virtual register with physical register
    void replace_vreg(VirtReg vreg, PhysReg preg);

    /// Check if this instruction defines/uses the given virtual register
    bool defines(VirtReg vreg) const;
    bool uses(VirtReg vreg) const;

    /// Check if this is a call instruction (clobbers caller-saved regs)
    bool is_call() const
    {
        return opcode_ == MachineOpcode::CALL;
    }

    /// Emit assembly string for this instruction
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

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_INSTRUCTION_HPP
