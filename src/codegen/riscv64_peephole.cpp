#include "codegen/riscv64_peephole.hpp"
#include "codegen/riscv64_instruction.hpp"
#include <variant>

namespace ciel {
namespace codegen {
namespace riscv64 {

namespace {

bool is_phys_reg_caller_saved(PhysReg reg)
{
    switch (reg) {
    case PhysReg::RA:
    case PhysReg::T0:
    case PhysReg::T1:
    case PhysReg::T2:
    case PhysReg::A0:
    case PhysReg::A1:
    case PhysReg::A2:
    case PhysReg::A3:
    case PhysReg::A4:
    case PhysReg::A5:
    case PhysReg::A6:
    case PhysReg::A7:
    case PhysReg::T3:
    case PhysReg::T4:
    case PhysReg::T5:
    case PhysReg::T6:
        return true;
    default:
        // TODO: Float registers
        return false;
    }
}

bool defines(const MachineInstr &instr, PhysReg reg)
{
    auto op = instr.get_opcode();

    if (op == MachineOpcode::CALL) {
        return is_phys_reg_caller_saved(reg);
    }

    // Stores and Branches do not define registers
    switch (op) {
    case MachineOpcode::SD:
    case MachineOpcode::SW:
    case MachineOpcode::SH:
    case MachineOpcode::SB:
    case MachineOpcode::FSD:
    case MachineOpcode::BEQ:
    case MachineOpcode::BNE:
    case MachineOpcode::BLT:
    case MachineOpcode::BGE:
    case MachineOpcode::BLTU:
    case MachineOpcode::BGEU:
    case MachineOpcode::J:
    case MachineOpcode::JR:
    case MachineOpcode::RET:
        return false;
    default:
        break;
    }

    // Default: check operand 0
    if (instr.operands().empty())
        return false;
    const auto &op0 = instr.operands()[0];
    if (std::holds_alternative<RegOperand>(op0)) {
        return std::get<RegOperand>(op0).reg == reg;
    }
    return false;
}

} // namespace

PeepholeOptimizer::PeepholeOptimizer(MachineFunction &mfn) : mfn_(mfn) {}

void PeepholeOptimizer::run()
{
    bool changed = true;
    while (changed) {
        changed = false;
        changed |= optimize_redundant_moves();
        changed |= optimize_redundant_loads();
        changed |= optimize_jumps();
        changed |= optimize_arithmetic();
    }
}

bool PeepholeOptimizer::optimize_redundant_moves()
{
    bool changed = false;
    auto &instrs = mfn_.instructions();
    for (auto it = instrs.begin(); it != instrs.end();) {
        if (it->get_opcode() == MachineOpcode::MV) {
            const auto &ops = it->operands();
            if (ops.size() == 2) {
                if (std::holds_alternative<RegOperand>(ops[0]) &&
                    std::holds_alternative<RegOperand>(ops[1])) {
                    auto dst = std::get<RegOperand>(ops[0]).reg;
                    auto src = std::get<RegOperand>(ops[1]).reg;
                    if (dst == src) {
                        it = instrs.erase(it);
                        changed = true;
                        continue;
                    }
                }
            }
        }
        ++it;
    }
    return changed;
}

bool PeepholeOptimizer::optimize_redundant_loads()
{
    bool changed = false;
    auto &instrs = mfn_.instructions();
    if (instrs.empty())
        return false;

    for (size_t i = 0; i < instrs.size(); ++i) {
        auto &store_instr = instrs[i];

        // Check if it is a store
        bool is_store = (store_instr.get_opcode() == MachineOpcode::SD ||
                         store_instr.get_opcode() == MachineOpcode::SW);
        if (!is_store)
            continue;

        if (store_instr.operands().size() != 2)
            continue;
        if (!std::holds_alternative<RegOperand>(store_instr.operands()[0]) ||
            !std::holds_alternative<MemOperand>(store_instr.operands()[1]))
            continue;

        auto src_reg = std::get<RegOperand>(store_instr.operands()[0]).reg;
        auto store_mem = std::get<MemOperand>(store_instr.operands()[1]);

        // Look ahead
        for (size_t j = i + 1; j < instrs.size() && j < i + 10; ++j) {
            auto &load_instr = instrs[j];

            // Stop if src_reg is modified
            if (defines(load_instr, src_reg))
                break;

            // Stop if label (control flow join)
            if (load_instr.get_opcode() == MachineOpcode::LABEL)
                break;

            // Stop if unconditional jump or return (control flow break)
            if (load_instr.get_opcode() == MachineOpcode::J ||
                load_instr.get_opcode() == MachineOpcode::RET ||
                load_instr.get_opcode() == MachineOpcode::JR)
                break;

            // Check if it is a load
            bool is_load = (load_instr.get_opcode() == MachineOpcode::LD ||
                            load_instr.get_opcode() == MachineOpcode::LW);

            if (is_load) {
                if (load_instr.operands().size() == 2 &&
                    std::holds_alternative<RegOperand>(
                        load_instr.operands()[0]) &&
                    std::holds_alternative<MemOperand>(
                        load_instr.operands()[1])) {

                    auto dst_reg =
                        std::get<RegOperand>(load_instr.operands()[0]).reg;
                    auto load_mem =
                        std::get<MemOperand>(load_instr.operands()[1]);

                    if (store_mem.base == load_mem.base &&
                        store_mem.offset == load_mem.offset) {
                        bool match_size =
                            (store_instr.get_opcode() == MachineOpcode::SD &&
                             load_instr.get_opcode() == MachineOpcode::LD) ||
                            (store_instr.get_opcode() == MachineOpcode::SW &&
                             load_instr.get_opcode() == MachineOpcode::LW);

                        if (match_size) {
                            if (dst_reg == src_reg) {
                                instrs.erase(instrs.begin() + j);
                                changed = true;
                                j--;
                            } else {
                                MachineInstr mv(MachineOpcode::MV);
                                mv.add_operand(RegOperand(dst_reg));
                                mv.add_operand(RegOperand(src_reg));
                                instrs[j] = mv;
                                changed = true;
                            }
                            continue;
                        }
                    }
                }
            }

            // Stop if memory is modified (aliasing check)
            auto op = load_instr.get_opcode();
            if (op == MachineOpcode::SD || op == MachineOpcode::SW ||
                op == MachineOpcode::SH || op == MachineOpcode::SB ||
                op == MachineOpcode::FSD) {
                if (load_instr.operands().size() >= 2 &&
                    std::holds_alternative<MemOperand>(
                        load_instr.operands().back())) {
                    auto other_mem =
                        std::get<MemOperand>(load_instr.operands().back());
                    // Conservative alias check
                    if (other_mem.base == store_mem.base) {
                        if (other_mem.offset == store_mem.offset)
                            break; // Same location
                        // Different offset from same base -> No alias (assuming
                        // no overlap)
                    } else {
                        // Different base -> Potential alias
                        break;
                    }
                } else {
                    break;
                }
            } else if (op == MachineOpcode::CALL) {
                break; // Calls can modify memory
            }
        }
    }
    return changed;
}

bool PeepholeOptimizer::optimize_jumps()
{
    bool changed = false;
    auto &instrs = mfn_.instructions();
    if (instrs.empty())
        return false;

    for (size_t i = 0; i < instrs.size() - 1;) {
        if (instrs[i].get_opcode() == MachineOpcode::J) {
            if (instrs[i].operands().size() == 1 &&
                std::holds_alternative<LabelOperand>(instrs[i].operands()[0])) {

                std::string target =
                    std::get<LabelOperand>(instrs[i].operands()[0]).label;

                if (instrs[i + 1].get_opcode() == MachineOpcode::LABEL) {
                    if (instrs[i + 1].label() == target) {
                        instrs.erase(instrs.begin() + i);
                        changed = true;
                        continue;
                    }
                }
            }
        }
        ++i;
    }
    return changed;
}

bool PeepholeOptimizer::optimize_arithmetic()
{
    bool changed = false;
    auto &instrs = mfn_.instructions();
    for (auto &instr : instrs) {
        // ADDI rd, rs, 0 -> MV rd, rs
        if (instr.get_opcode() == MachineOpcode::ADDI) {
            const auto &ops = instr.operands();
            if (ops.size() == 3) {
                if (std::holds_alternative<RegOperand>(ops[0]) &&
                    std::holds_alternative<RegOperand>(ops[1]) &&
                    std::holds_alternative<ImmOperand>(ops[2])) {

                    auto imm = std::get<ImmOperand>(ops[2]).value;
                    if (imm == 0) {
                        auto dst = std::get<RegOperand>(ops[0]).reg;
                        auto src = std::get<RegOperand>(ops[1]).reg;

                        // Replace with MV
                        MachineInstr mv(MachineOpcode::MV);
                        mv.add_operand(RegOperand(dst));
                        mv.add_operand(RegOperand(src));
                        instr = mv;
                        changed = true;
                    }
                }
            }
        }
    }
    return changed;
}

} // namespace riscv64
} // namespace codegen
} // namespace ciel
