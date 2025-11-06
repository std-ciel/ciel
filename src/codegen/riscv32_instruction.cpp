#include "codegen/riscv32_instruction.hpp"
#include "codegen/riscv32_register.hpp"
#include <algorithm>
#include <ostream>
#include <variant>

namespace ciel {
namespace codegen {
namespace riscv32 {

void MachineInstr::replace_vreg(VirtReg vreg, PhysReg preg)
{
    // Replace in defs
    for (auto &def : defs_) {
        if (def == vreg) {
            def = INVALID_VREG;
        }
    }

    // Replace in uses
    for (auto &use : uses_) {
        if (use == vreg) {
            use = INVALID_VREG;
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
    case MachineOpcode::LD:
        return "ld";
    case MachineOpcode::LBU:
        return "lbu";
    case MachineOpcode::LHU:
        return "lhu";
    case MachineOpcode::LWU:
        return "lwu";
    case MachineOpcode::SB:
        return "sb";
    case MachineOpcode::SH:
        return "sh";
    case MachineOpcode::SW:
        return "sw";
    case MachineOpcode::SD:
        return "sd";
    // Floating-point instructions (double-precision)
    case MachineOpcode::FLD:
        return "fld";
    case MachineOpcode::FSD:
        return "fsd";
    case MachineOpcode::FADD_D:
        return "fadd.d";
    case MachineOpcode::FSUB_D:
        return "fsub.d";
    case MachineOpcode::FMUL_D:
        return "fmul.d";
    case MachineOpcode::FDIV_D:
        return "fdiv.d";
    case MachineOpcode::FSQRT_D:
        return "fsqrt.d";
    case MachineOpcode::FMIN_D:
        return "fmin.d";
    case MachineOpcode::FMAX_D:
        return "fmax.d";
    case MachineOpcode::FEQ_D:
        return "feq.d";
    case MachineOpcode::FLT_D:
        return "flt.d";
    case MachineOpcode::FLE_D:
        return "fle.d";
    case MachineOpcode::FCVT_W_D:
        return "fcvt.w.d";
    case MachineOpcode::FCVT_WU_D:
        return "fcvt.wu.d";
    case MachineOpcode::FCVT_L_D:
        return "fcvt.l.d";
    case MachineOpcode::FCVT_LU_D:
        return "fcvt.lu.d";
    case MachineOpcode::FCVT_D_W:
        return "fcvt.d.w";
    case MachineOpcode::FCVT_D_WU:
        return "fcvt.d.wu";
    case MachineOpcode::FCVT_D_L:
        return "fcvt.d.l";
    case MachineOpcode::FCVT_D_LU:
        return "fcvt.d.lu";
    case MachineOpcode::FMV_X_D:
        return "fmv.x.d";
    case MachineOpcode::FMV_D_X:
        return "fmv.d.x";
    case MachineOpcode::FMV_D:
        return "fmv.d";
    case MachineOpcode::FSGNJ_D:
        return "fsgnj.d";
    case MachineOpcode::FSGNJN_D:
        return "fsgnjn.d";
    case MachineOpcode::FSGNJX_D:
        return "fsgnjx.d";
    default:
        return "UNKNOWN";
    }
}

bool is_float_opcode(MachineOpcode opc)
{
    return opc == MachineOpcode::FLD || opc == MachineOpcode::FSD ||
           opc == MachineOpcode::FADD_D || opc == MachineOpcode::FSUB_D ||
           opc == MachineOpcode::FMUL_D || opc == MachineOpcode::FDIV_D ||
           opc == MachineOpcode::FSQRT_D || opc == MachineOpcode::FMIN_D ||
           opc == MachineOpcode::FMAX_D || opc == MachineOpcode::FEQ_D ||
           opc == MachineOpcode::FLT_D || opc == MachineOpcode::FLE_D ||
           opc == MachineOpcode::FCVT_W_D || opc == MachineOpcode::FCVT_WU_D ||
           opc == MachineOpcode::FCVT_L_D || opc == MachineOpcode::FCVT_LU_D ||
           opc == MachineOpcode::FCVT_D_W || opc == MachineOpcode::FCVT_D_WU ||
           opc == MachineOpcode::FCVT_D_L || opc == MachineOpcode::FCVT_D_LU ||
           opc == MachineOpcode::FMV_X_D || opc == MachineOpcode::FMV_D_X ||
           opc == MachineOpcode::FMV_D || opc == MachineOpcode::FSGNJ_D ||
           opc == MachineOpcode::FSGNJN_D || opc == MachineOpcode::FSGNJX_D;
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

    if (opcode_ == MachineOpcode::FLD || opcode_ == MachineOpcode::FSD ||
        opcode_ == MachineOpcode::LD || opcode_ == MachineOpcode::LW ||
        opcode_ == MachineOpcode::LH || opcode_ == MachineOpcode::LB ||
        opcode_ == MachineOpcode::LHU || opcode_ == MachineOpcode::LBU ||
        opcode_ == MachineOpcode::LWU || opcode_ == MachineOpcode::SD ||
        opcode_ == MachineOpcode::SW || opcode_ == MachineOpcode::SH ||
        opcode_ == MachineOpcode::SB) {
        if (operands_.size() == 2 &&
            std::holds_alternative<MemOperand>(operands_[1])) {
            os << " ";
            bool dest_is_fp = (opcode_ == MachineOpcode::FLD ||
                               opcode_ == MachineOpcode::FSD);
            emit_operand(os, operands_[0], dest_is_fp);
            os << ", ";
            emit_operand(os, operands_[1], false);
        } else if (operands_.size() >= 3) {
            os << " ";
            bool dest_is_fp = (opcode_ == MachineOpcode::FLD);
            emit_operand(os, operands_[0], dest_is_fp);
            os << ", ";
            emit_operand(os, operands_[2], false);
            os << "(";
            emit_operand(os, operands_[1], false);
            os << ")";
        }
    } else {
        bool dest_is_int_for_fcmp = (opcode_ == MachineOpcode::FLT_D ||
                                     opcode_ == MachineOpcode::FLE_D ||
                                     opcode_ == MachineOpcode::FEQ_D);

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
