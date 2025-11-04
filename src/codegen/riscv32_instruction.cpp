#include "codegen/riscv32_instruction.hpp"
#include <algorithm>
#include <ostream>

namespace ciel {
namespace codegen {
namespace riscv32 {

void MachineInstr::replace_vreg(VirtReg vreg, PhysReg preg)
{
    // Replace in defs
    for (auto &def : defs_) {
        if (def == vreg) {
            def = 0; // Mark as physical (convention: vreg 0 = invalid)
        }
    }

    // Replace in uses
    for (auto &use : uses_) {
        if (use == vreg) {
            use = 0;
        }
    }

    // Replace in operands
    for (auto &operand : operands_) {
        if (std::holds_alternative<VRegOperand>(operand)) {
            auto &vreg_op = std::get<VRegOperand>(operand);
            if (vreg_op.vreg == vreg) {
                operand = RegOperand(preg);
            }
        }
    }
}

bool MachineInstr::defines(VirtReg vreg) const
{
    return std::find(defs_.begin(), defs_.end(), vreg) != defs_.end();
}

bool MachineInstr::uses(VirtReg vreg) const
{
    return std::find(uses_.begin(), uses_.end(), vreg) != uses_.end();
}

namespace {

const char *opcode_to_string(MachineOpcode opc)
{
    switch (opc) {
    case MachineOpcode::NOP:
        return "nop";
    case MachineOpcode::LI:
        return "li";
    case MachineOpcode::LA:
        return "la";
    case MachineOpcode::MV:
        return "mv";
    case MachineOpcode::J:
        return "j";
    case MachineOpcode::JR:
        return "jr";
    case MachineOpcode::RET:
        return "ret";
    case MachineOpcode::CALL:
        return "call";
    case MachineOpcode::ADD:
        return "add";
    case MachineOpcode::SUB:
        return "sub";
    case MachineOpcode::MUL:
        return "mul";
    case MachineOpcode::DIV:
        return "div";
    case MachineOpcode::DIVU:
        return "divu";
    case MachineOpcode::REM:
        return "rem";
    case MachineOpcode::REMU:
        return "remu";
    case MachineOpcode::ADDI:
        return "addi";
    case MachineOpcode::AND:
        return "and";
    case MachineOpcode::OR:
        return "or";
    case MachineOpcode::XOR:
        return "xor";
    case MachineOpcode::ANDI:
        return "andi";
    case MachineOpcode::ORI:
        return "ori";
    case MachineOpcode::XORI:
        return "xori";
    case MachineOpcode::SLL:
        return "sll";
    case MachineOpcode::SRL:
        return "srl";
    case MachineOpcode::SRA:
        return "sra";
    case MachineOpcode::SLLI:
        return "slli";
    case MachineOpcode::SRLI:
        return "srli";
    case MachineOpcode::SRAI:
        return "srai";
    case MachineOpcode::SLT:
        return "slt";
    case MachineOpcode::SLTU:
        return "sltu";
    case MachineOpcode::SLTI:
        return "slti";
    case MachineOpcode::SLTIU:
        return "sltiu";
    case MachineOpcode::SEQZ:
        return "seqz";
    case MachineOpcode::SNEZ:
        return "snez";
    case MachineOpcode::BEQ:
        return "beq";
    case MachineOpcode::BNE:
        return "bne";
    case MachineOpcode::BLT:
        return "blt";
    case MachineOpcode::BGE:
        return "bge";
    case MachineOpcode::BLTU:
        return "bltu";
    case MachineOpcode::BGEU:
        return "bgeu";
    case MachineOpcode::LB:
        return "lb";
    case MachineOpcode::LH:
        return "lh";
    case MachineOpcode::LW:
        return "lw";
    case MachineOpcode::LBU:
        return "lbu";
    case MachineOpcode::LHU:
        return "lhu";
    case MachineOpcode::SB:
        return "sb";
    case MachineOpcode::SH:
        return "sh";
    case MachineOpcode::SW:
        return "sw";
    // Floating-point instructions
    case MachineOpcode::FLW:
        return "flw";
    case MachineOpcode::FSW:
        return "fsw";
    case MachineOpcode::FADD_S:
        return "fadd.s";
    case MachineOpcode::FSUB_S:
        return "fsub.s";
    case MachineOpcode::FMUL_S:
        return "fmul.s";
    case MachineOpcode::FDIV_S:
        return "fdiv.s";
    case MachineOpcode::FSQRT_S:
        return "fsqrt.s";
    case MachineOpcode::FMIN_S:
        return "fmin.s";
    case MachineOpcode::FMAX_S:
        return "fmax.s";
    case MachineOpcode::FEQ_S:
        return "feq.s";
    case MachineOpcode::FLT_S:
        return "flt.s";
    case MachineOpcode::FLE_S:
        return "fle.s";
    case MachineOpcode::FCVT_W_S:
        return "fcvt.w.s";
    case MachineOpcode::FCVT_WU_S:
        return "fcvt.wu.s";
    case MachineOpcode::FCVT_S_W:
        return "fcvt.s.w";
    case MachineOpcode::FCVT_S_WU:
        return "fcvt.s.wu";
    case MachineOpcode::FMV_X_W:
        return "fmv.x.w";
    case MachineOpcode::FMV_W_X:
        return "fmv.w.x";
    case MachineOpcode::FMV_S:
        return "fmv.s";
    case MachineOpcode::FSGNJ_S:
        return "fsgnj.s";
    case MachineOpcode::FSGNJN_S:
        return "fsgnjn.s";
    case MachineOpcode::FSGNJX_S:
        return "fsgnjx.s";
    default:
        return "UNKNOWN";
    }
}

bool is_float_opcode(MachineOpcode opc)
{
    return opc == MachineOpcode::FLW || opc == MachineOpcode::FSW ||
           opc == MachineOpcode::FADD_S || opc == MachineOpcode::FSUB_S ||
           opc == MachineOpcode::FMUL_S || opc == MachineOpcode::FDIV_S ||
           opc == MachineOpcode::FSQRT_S || opc == MachineOpcode::FMIN_S ||
           opc == MachineOpcode::FMAX_S || opc == MachineOpcode::FEQ_S ||
           opc == MachineOpcode::FLT_S || opc == MachineOpcode::FLE_S ||
           opc == MachineOpcode::FCVT_W_S || opc == MachineOpcode::FCVT_WU_S ||
           opc == MachineOpcode::FCVT_S_W || opc == MachineOpcode::FCVT_S_WU ||
           opc == MachineOpcode::FMV_X_W || opc == MachineOpcode::FMV_W_X ||
           opc == MachineOpcode::FMV_S || opc == MachineOpcode::FSGNJ_S ||
           opc == MachineOpcode::FSGNJN_S || opc == MachineOpcode::FSGNJX_S;
}

void emit_operand(std::ostream &os, const MachineOperand &op, bool use_fp_regs)
{
    if (std::holds_alternative<ImmOperand>(op)) {
        os << std::get<ImmOperand>(op).value;
    } else if (std::holds_alternative<RegOperand>(op)) {
        os << get_reg_name(std::get<RegOperand>(op).reg);
    } else if (std::holds_alternative<VRegOperand>(op)) {
        os << "v" << std::get<VRegOperand>(op).vreg;
    } else if (std::holds_alternative<LabelOperand>(op)) {
        os << std::get<LabelOperand>(op).label;
    } else if (std::holds_alternative<MemOperand>(op)) {
        const auto &mem = std::get<MemOperand>(op);
        os << mem.offset << "(" << get_reg_name(mem.base) << ")";
    }
}

} // anonymous namespace

void MachineInstr::emit(std::ostream &os) const
{
    if (opcode_ == MachineOpcode::LABEL) {
        os << label_ << ":\n";
        return;
    }

    if (opcode_ == MachineOpcode::DIRECTIVE) {
        os << "    " << label_ << "\n";
        return;
    }

    os << "    " << opcode_to_string(opcode_);

    if (opcode_ == MachineOpcode::FLW || opcode_ == MachineOpcode::FSW ||
        opcode_ == MachineOpcode::LW || opcode_ == MachineOpcode::LH ||
        opcode_ == MachineOpcode::LB || opcode_ == MachineOpcode::LHU ||
        opcode_ == MachineOpcode::LBU || opcode_ == MachineOpcode::SW ||
        opcode_ == MachineOpcode::SH || opcode_ == MachineOpcode::SB) {

        if (operands_.size() >= 3) {
            os << " ";
            bool dest_is_fp = (opcode_ == MachineOpcode::FLW);
            emit_operand(os, operands_[0], dest_is_fp);
            os << ", ";
            emit_operand(os, operands_[2], false);
            os << "(";
            emit_operand(os, operands_[1], false);
            os << ")";
        }
    } else {
        bool dest_is_int_for_fcmp = (opcode_ == MachineOpcode::FLT_S ||
                                     opcode_ == MachineOpcode::FLE_S ||
                                     opcode_ == MachineOpcode::FEQ_S);

        bool use_fp = is_float_opcode(opcode_);
        bool first = true;
        for (size_t i = 0; i < operands_.size(); ++i) {
            if (!first) {
                os << ",";
            } else {
                os << " ";
            }
            bool this_operand_fp = use_fp && !(dest_is_int_for_fcmp && i == 0);
            emit_operand(os, operands_[i], this_operand_fp);
            first = false;
        }
    }

    os << "\n";
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
