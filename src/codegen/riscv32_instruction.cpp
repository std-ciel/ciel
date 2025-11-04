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
    default:
        return "UNKNOWN";
    }
}

void emit_operand(std::ostream &os, const MachineOperand &op)
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

    bool first = true;
    for (const auto &operand : operands_) {
        if (!first) {
            os << ",";
        } else {
            os << " ";
        }
        emit_operand(os, operand);
        first = false;
    }

    os << "\n";
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
