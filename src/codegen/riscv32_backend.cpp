#include "codegen/riscv32_backend.hpp"
#include "codegen/riscv32_regalloc.hpp"
#include <algorithm>
#include <cstring>
#include <ostream>

namespace ciel {
namespace codegen {
namespace riscv32 {

MachineFunction::MachineFunction(const TACFunction &tac_fn, TypeFactory &types)
    : name_(tac_fn.mangled_name), frame_(tac_fn.mangled_name), next_vreg_(1),
      types_(types)
{
}

void MachineFunction::build_cfg()
{
    if (instructions_.empty())
        return;

    basic_blocks_.clear();
    std::unordered_map<std::string, size_t> label_to_block;

    auto is_terminator = [](MachineOpcode op) {
        return op == MachineOpcode::J || op == MachineOpcode::JR ||
               op == MachineOpcode::BEQ || op == MachineOpcode::BNE ||
               op == MachineOpcode::BLT || op == MachineOpcode::BGE ||
               op == MachineOpcode::BLTU || op == MachineOpcode::BGEU ||
               op == MachineOpcode::RET;
    };

    auto get_branch_target =
        [](const MachineInstr &instr) -> std::optional<std::string> {
        const auto &operands = instr.get_operands();
        for (const auto &op : operands) {
            if (std::holds_alternative<LabelOperand>(op)) {
                return std::get<LabelOperand>(op).label;
            }
        }
        return std::nullopt;
    };

    // Phase 1: Identify block boundaries
    // Blocks start at: (1) function entry, (2) labels, (3) instruction after
    // terminator
    // Blocks end at: (1) terminator, (2) instruction before label,
    // (3) last instruction
    std::vector<bool> is_block_start(instructions_.size(), false);
    is_block_start[0] = true;

    for (size_t pos = 0; pos < instructions_.size(); ++pos) {
        const auto &instr = instructions_[pos];

        if (instr.get_opcode() == MachineOpcode::LABEL) {
            is_block_start[pos] = true;
        }

        if (is_terminator(instr.get_opcode()) &&
            pos + 1 < instructions_.size()) {
            is_block_start[pos + 1] = true;
        }
    }

    // Phase 2: Create basic blocks
    size_t block_start = 0;
    for (size_t pos = 0; pos < instructions_.size(); ++pos) {
        // Start new block when we hit a boundary (and not at the first
        // instruction of current block)
        if (is_block_start[pos] && pos > block_start) {
            basic_blocks_.emplace_back(block_start, pos - 1);
            block_start = pos;
        }

        if (instructions_[pos].get_opcode() == MachineOpcode::LABEL) {
            label_to_block[instructions_[pos].get_label()] =
                basic_blocks_.size();
        }
    }

    if (block_start < instructions_.size()) {
        basic_blocks_.emplace_back(block_start, instructions_.size() - 1);
    }

    // Phase 3: Build CFG edges (successors/predecessors)
    for (size_t bb_idx = 0; bb_idx < basic_blocks_.size(); ++bb_idx) {
        auto &block = basic_blocks_[bb_idx];
        const auto &last_instr = instructions_[block.end_instr_idx];
        const auto opcode = last_instr.get_opcode();

        if (is_terminator(opcode)) {
            if (auto target = get_branch_target(last_instr)) {
                if (auto it = label_to_block.find(*target);
                    it != label_to_block.end()) {
                    block.successors.push_back(it->second);
                    basic_blocks_[it->second].predecessors.push_back(bb_idx);
                }
            }

            const bool is_conditional =
                (opcode == MachineOpcode::BEQ || opcode == MachineOpcode::BNE ||
                 opcode == MachineOpcode::BLT || opcode == MachineOpcode::BGE ||
                 opcode == MachineOpcode::BLTU ||
                 opcode == MachineOpcode::BGEU);

            if (is_conditional && bb_idx + 1 < basic_blocks_.size()) {
                block.successors.push_back(bb_idx + 1);
                basic_blocks_[bb_idx + 1].predecessors.push_back(bb_idx);
            }
        } else {
            if (bb_idx + 1 < basic_blocks_.size()) {
                block.successors.push_back(bb_idx + 1);
                basic_blocks_[bb_idx + 1].predecessors.push_back(bb_idx);
            }
        }
    }
}

InstructionSelector::InstructionSelector(MachineFunction &mfn,
                                         TypeFactory &types,
                                         RiscV32Backend &backend)
    : mfn_(mfn), types_(types), backend_(backend)
{
}

void InstructionSelector::select(const TACFunction &tac_fn)
{
    for (size_t i = 0; i < tac_fn.parameters.size() && i < 8; ++i) {
        const auto &param = tac_fn.parameters[i];
        if (param.kind == TACOperand::Kind::SYMBOL) {
            SymbolPtr sym = std::get<SymbolPtr>(param.value);
            PhysReg arg_reg =
                static_cast<PhysReg>(static_cast<uint8_t>(PhysReg::A0) + i);
            param_to_reg_[sym->get_name()] = arg_reg;
        }
    }

    for (const auto &bb : tac_fn.basic_blocks) {
        if (!bb->label.empty()) {
            mfn_.add_instruction(make_label(bb->label));
        }

        for (size_t i = 0; i < bb->instructions.size(); ++i) {
            const auto &tac_instr = bb->instructions[i];
            try {
                select_instruction(*tac_instr);
            } catch (const std::exception &e) {
                throw std::runtime_error(
                    std::string("Error selecting instruction #") +
                    std::to_string(i) + " in BB: " + e.what());
            }
        }
    }
}

void InstructionSelector::select_instruction(const TACInstruction &instr)
{
    switch (instr.opcode) {
    case TACOpcode::RETURN:
        select_return(instr);
        break;

    case TACOpcode::ASSIGN:
        select_assign(instr);
        break;

    case TACOpcode::ADDR_OF:
        select_addr_of(instr);
        break;

    case TACOpcode::DEREF:
        select_deref(instr);
        break;

    case TACOpcode::NEG:
    case TACOpcode::NOT:
    case TACOpcode::LNOT:
        select_unary_op(instr);
        break;

    case TACOpcode::ADD:
    case TACOpcode::SUB:
    case TACOpcode::MUL:
    case TACOpcode::DIV:
    case TACOpcode::MOD:
    case TACOpcode::AND:
    case TACOpcode::OR:
    case TACOpcode::XOR:
    case TACOpcode::SHL:
    case TACOpcode::SHR:
        select_binary_op(instr);
        break;

    case TACOpcode::EQ:
    case TACOpcode::NE:
    case TACOpcode::LT:
    case TACOpcode::LE:
    case TACOpcode::GT:
    case TACOpcode::GE:
        select_comparison(instr);
        break;

    case TACOpcode::GOTO:
        select_goto(instr);
        break;

    case TACOpcode::IF_TRUE:
    case TACOpcode::IF_FALSE:
        select_if_branch(instr);
        break;

    case TACOpcode::PARAM:
        select_param(instr);
        break;

    case TACOpcode::CALL:
        select_call(instr);
        break;

    case TACOpcode::LABEL:
        // Emit the label from the instruction
        if (instr.operand1.is_valid() &&
            instr.operand1.kind == TACOperand::Kind::LABEL) {
            mfn_.add_instruction(make_label(instr.operand1.to_string()));
        }
        break;

    case TACOpcode::JUMP_TABLE:
        select_jump_table(instr);
        break;

    case TACOpcode::LOAD:
        select_load(instr);
        break;

    case TACOpcode::STORE:
        select_store(instr);
        break;

    default:
        break;
    }
}

void InstructionSelector::select_return(const TACInstruction &instr)
{
    if (instr.operand1.is_valid()) {
        VirtReg val = load_operand(instr.operand1);
        bool is_float = false;
        if (instr.operand1.type) {
            is_float = is_float_type(instr.operand1.type);
        } else {
            is_float = float_vregs_.contains(val);
        }
        if (is_float) {
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_D)
                                     .add_use(val)
                                     .add_operand(RegOperand(PhysReg::FA0))
                                     .add_operand(VRegOperand(val)));
        } else {
            mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                     .add_use(val)
                                     .add_operand(RegOperand(PhysReg::A0))
                                     .add_operand(VRegOperand(val)));
        }
    }
    mfn_.add_instruction(make_ret());
}

void InstructionSelector::select_assign(const TACInstruction &instr)
{
    VirtReg src = load_operand(instr.operand1);
    store_result(src, instr.result);
}
void InstructionSelector::select_addr_of(const TACInstruction &instr)
{
    if (instr.operand1.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr sym = std::get<SymbolPtr>(instr.operand1.value);
        std::string sym_name = sym->get_name();

        if (sym->get_storage_class() == StorageClass::AUTO) {

            int32_t offset = get_local_variable_offset(sym);
            VirtReg addr_vreg = mfn_.get_next_vreg();

            mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                     .add_def(addr_vreg)
                                     .add_operand(VRegOperand(addr_vreg))
                                     .add_operand(RegOperand(PhysReg::S0_FP))
                                     .add_operand(ImmOperand(offset)));

            store_result(addr_vreg, instr.result);
        } else {
            VirtReg addr_vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(make_la(addr_vreg, sym_name));
            store_result(addr_vreg, instr.result);
        }
    } else {
        VirtReg addr = load_operand(instr.operand1);
        store_result(addr, instr.result);
    }
}

void InstructionSelector::select_deref(const TACInstruction &instr)
{
    VirtReg addr = load_operand(instr.operand1);
    VirtReg result = mfn_.get_next_vreg();

    bool is_float = false;
    if (instr.result.type) {
        is_float = is_float_type(instr.result.type);
    }

    if (is_float) {
        float_vregs_.insert(result);
        auto load = MachineInstr(MachineOpcode::FLD);
        load.add_def(result);
        load.add_use(addr);
        load.add_operand(VRegOperand(result));
        load.add_operand(VRegOperand(addr));
        load.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(load));
    } else {
        auto load = MachineInstr(MachineOpcode::LD);
        load.add_def(result);
        load.add_use(addr);
        load.add_operand(VRegOperand(result));
        load.add_operand(VRegOperand(addr));
        load.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(load));
    }

    store_result(result, instr.result);
}

void InstructionSelector::select_unary_op(const TACInstruction &instr)
{
    VirtReg src = load_operand(instr.operand1);
    VirtReg dst = mfn_.get_next_vreg();

    switch (instr.opcode) {
    case TACOpcode::NEG:
        mfn_.add_instruction(MachineInstr(MachineOpcode::SUB)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(RegOperand(PhysReg::ZERO))
                                 .add_operand(VRegOperand(src)));
        break;
    case TACOpcode::NOT:
        mfn_.add_instruction(MachineInstr(MachineOpcode::XORI)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(src))
                                 .add_operand(ImmOperand(-1)));
        break;
    case TACOpcode::LNOT:
        mfn_.add_instruction(MachineInstr(MachineOpcode::SEQZ)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(src)));
        break;
    default:
        return;
    }

    store_result(dst, instr.result);
}

void InstructionSelector::select_binary_op(const TACInstruction &instr)
{
    VirtReg op1 = load_operand(instr.operand1);
    VirtReg op2 = load_operand(instr.operand2);
    VirtReg dst = mfn_.get_next_vreg();

    MachineOpcode opc;
    bool is_float = is_float_type(instr.result.type);
    bool is_signed = is_signed_type(instr.result.type);

    if (is_float) {
        float_vregs_.insert(dst);
        switch (instr.opcode) {
        case TACOpcode::ADD:
            opc = MachineOpcode::FADD_D;
            break;
        case TACOpcode::SUB:
            opc = MachineOpcode::FSUB_D;
            break;
        case TACOpcode::MUL:
            opc = MachineOpcode::FMUL_D;
            break;
        case TACOpcode::DIV:
            opc = MachineOpcode::FDIV_D;
            break;
        case TACOpcode::MOD:
            return;
        default:
            return;
        }
    } else {
        switch (instr.opcode) {
        case TACOpcode::ADD:
            opc = MachineOpcode::ADD;
            break;
        case TACOpcode::SUB:
            opc = MachineOpcode::SUB;
            break;
        case TACOpcode::MUL:
            opc = MachineOpcode::MUL;
            break;
        case TACOpcode::DIV:
            opc = is_signed ? MachineOpcode::DIV : MachineOpcode::DIVU;
            break;
        case TACOpcode::MOD:
            opc = is_signed ? MachineOpcode::REM : MachineOpcode::REMU;
            break;
        case TACOpcode::AND:
            opc = MachineOpcode::AND;
            break;
        case TACOpcode::OR:
            opc = MachineOpcode::OR;
            break;
        case TACOpcode::XOR:
            opc = MachineOpcode::XOR;
            break;
        case TACOpcode::SHL:
            opc = MachineOpcode::SLL;
            break;
        case TACOpcode::SHR:
            opc = is_signed ? MachineOpcode::SRA : MachineOpcode::SRL;
            break;
        default:
            return;
        }
    }

    mfn_.add_instruction(MachineInstr(opc)
                             .add_def(dst)
                             .add_use(op1)
                             .add_use(op2)
                             .add_operand(VRegOperand(dst))
                             .add_operand(VRegOperand(op1))
                             .add_operand(VRegOperand(op2)));

    store_result(dst, instr.result);
}

void InstructionSelector::select_comparison(const TACInstruction &instr)
{
    VirtReg op1 = load_operand(instr.operand1);
    VirtReg op2 = load_operand(instr.operand2);
    VirtReg dst = mfn_.get_next_vreg();

    bool is_float = is_float_type(instr.operand1.type);
    bool is_signed = is_signed_type(instr.operand1.type);

    if (is_float) {
        switch (instr.opcode) {
        case TACOpcode::EQ:
            mfn_.add_instruction(MachineInstr(MachineOpcode::FEQ_D)
                                     .add_def(dst)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            break;
        case TACOpcode::NE: {
            VirtReg tmp = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::FEQ_D)
                                     .add_def(tmp)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            mfn_.add_instruction(MachineInstr(MachineOpcode::XORI)
                                     .add_def(dst)
                                     .add_use(tmp)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(ImmOperand(1)));
            break;
        }
        case TACOpcode::LT:
            mfn_.add_instruction(MachineInstr(MachineOpcode::FLT_D)
                                     .add_def(dst)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            break;
        case TACOpcode::LE:
            mfn_.add_instruction(MachineInstr(MachineOpcode::FLE_D)
                                     .add_def(dst)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            break;
        case TACOpcode::GT:
            mfn_.add_instruction(MachineInstr(MachineOpcode::FLT_D)
                                     .add_def(dst)
                                     .add_use(op2)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op2))
                                     .add_operand(VRegOperand(op1)));
            break;
        case TACOpcode::GE:
            mfn_.add_instruction(MachineInstr(MachineOpcode::FLE_D)
                                     .add_def(dst)
                                     .add_use(op2)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op2))
                                     .add_operand(VRegOperand(op1)));
            break;
        default:
            return;
        }
    } else {
        switch (instr.opcode) {
        case TACOpcode::EQ: {
            VirtReg tmp = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::SUB)
                                     .add_def(tmp)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            mfn_.add_instruction(MachineInstr(MachineOpcode::SEQZ)
                                     .add_def(dst)
                                     .add_use(tmp)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(tmp)));
            break;
        }
        case TACOpcode::NE: {
            VirtReg tmp = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::SUB)
                                     .add_def(tmp)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            mfn_.add_instruction(MachineInstr(MachineOpcode::SNEZ)
                                     .add_def(dst)
                                     .add_use(tmp)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(tmp)));
            break;
        }
        case TACOpcode::LT: {
            auto opc = is_signed ? MachineOpcode::SLT : MachineOpcode::SLTU;
            mfn_.add_instruction(MachineInstr(opc)
                                     .add_def(dst)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            break;
        }
        case TACOpcode::GT: {
            auto opc = is_signed ? MachineOpcode::SLT : MachineOpcode::SLTU;
            mfn_.add_instruction(MachineInstr(opc)
                                     .add_def(dst)
                                     .add_use(op2)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(op2))
                                     .add_operand(VRegOperand(op1)));
            break;
        }
        case TACOpcode::LE: {
            auto opc = is_signed ? MachineOpcode::SLT : MachineOpcode::SLTU;
            VirtReg tmp = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(opc)
                                     .add_def(tmp)
                                     .add_use(op2)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(VRegOperand(op2))
                                     .add_operand(VRegOperand(op1)));
            mfn_.add_instruction(MachineInstr(MachineOpcode::XORI)
                                     .add_def(dst)
                                     .add_use(tmp)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(ImmOperand(1)));
            break;
        }
        case TACOpcode::GE: {
            auto opc = is_signed ? MachineOpcode::SLT : MachineOpcode::SLTU;
            VirtReg tmp = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(opc)
                                     .add_def(tmp)
                                     .add_use(op1)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(VRegOperand(op1))
                                     .add_operand(VRegOperand(op2)));
            mfn_.add_instruction(MachineInstr(MachineOpcode::XORI)
                                     .add_def(dst)
                                     .add_use(tmp)
                                     .add_operand(VRegOperand(dst))
                                     .add_operand(VRegOperand(tmp))
                                     .add_operand(ImmOperand(1)));
            break;
        }
        default:
            return;
        }
    }

    store_result(dst, instr.result);
}

void InstructionSelector::select_goto(const TACInstruction &instr)
{
    if (instr.operand1.kind == TACOperand::Kind::LABEL) {
        std::string target = std::get<std::string>(instr.operand1.value);
        mfn_.add_instruction(
            MachineInstr(MachineOpcode::J).add_operand(LabelOperand(target)));
    }
}

void InstructionSelector::select_if_branch(const TACInstruction &instr)
{
    // TAC IF_FALSE/IF_TRUE structure: (opcode,
    // operand1=condition, operand2=label)
    VirtReg cond = load_operand(instr.operand1);

    if (instr.operand2.kind == TACOperand::Kind::LABEL) {
        std::string target = std::get<std::string>(instr.operand2.value);

        // IF_TRUE: branch if cond != 0 (BNE with zero)
        // IF_FALSE: branch if cond == 0 (BEQ with zero)
        MachineOpcode branch_opc = (instr.opcode == TACOpcode::IF_TRUE)
                                       ? MachineOpcode::BNE
                                       : MachineOpcode::BEQ;

        mfn_.add_instruction(MachineInstr(branch_opc)
                                 .add_use(cond)
                                 .add_operand(VRegOperand(cond))
                                 .add_operand(RegOperand(PhysReg::ZERO))
                                 .add_operand(LabelOperand(target)));
    }
}

void InstructionSelector::select_param(const TACInstruction &instr)
{
    VirtReg param = load_operand(instr.operand1);
    pending_params_.push_back(param);
}

void InstructionSelector::select_call(const TACInstruction &instr)
{
    for (size_t i = 0, f_idx = 0, i_idx = 0;
         i < pending_params_.size() && i < 8;
         ++i) {
        if (float_vregs_.contains(pending_params_[i])) {
            mfn_.add_instruction(
                MachineInstr(MachineOpcode::FMV_D)
                    .add_use(pending_params_[i])
                    .add_operand(RegOperand(static_cast<PhysReg>(
                        static_cast<uint8_t>(PhysReg::FA0) + f_idx++)))
                    .add_operand(VRegOperand(pending_params_[i])));
        } else {
            mfn_.add_instruction(
                MachineInstr(MachineOpcode::MV)
                    .add_use(pending_params_[i])
                    .add_operand(RegOperand(static_cast<PhysReg>(
                        static_cast<uint8_t>(PhysReg::A0) + i_idx++)))
                    .add_operand(VRegOperand(pending_params_[i])));
        }
    }

    if (pending_params_.size() > 8) {
        mfn_.get_frame().update_max_call_args(
            static_cast<uint32_t>(pending_params_.size()));
    }

    TypePtr ret_type = instr.result.type;
    if (instr.operand1.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr func_sym = std::get<SymbolPtr>(instr.operand1.value);
        mfn_.add_instruction(make_call(func_sym->get_name()));
    }
    // else if (instr.operand1.kind == TACOperand::Kind::LABEL) {
    //     std::string func_name = std::get<std::string>(instr.operand1.value);
    //     mfn_.add_instruction(make_call(func_name));
    // }

    if (instr.result.is_valid()) {
        VirtReg result = mfn_.get_next_vreg();
        // TODO: handle this better for complex return types
        if (ret_type && is_float_type(ret_type)) {
            float_vregs_.insert(result);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_D)
                                     .add_def(result)
                                     .add_operand(VRegOperand(result))
                                     .add_operand(RegOperand(PhysReg::FA0)));
        } else {
            mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                     .add_def(result)
                                     .add_operand(VRegOperand(result))
                                     .add_operand(RegOperand(PhysReg::A0)));
        }
        store_result(result, instr.result);
    }

    pending_params_.clear();
}

void InstructionSelector::select_jump_table(const TACInstruction &instr)
{
    VirtReg index = load_operand(instr.result);

    // Simplified jump table: just emit a sequence of compares and branches
    // A full implementation would use a proper jump table in rodata
    for (size_t i = 0; i < instr.jump_table_labels.size(); ++i) {
        VirtReg tmp = mfn_.get_next_vreg();
        mfn_.add_instruction(make_li(tmp, static_cast<int32_t>(i)));

        mfn_.add_instruction(
            MachineInstr(MachineOpcode::BEQ)
                .add_use(index)
                .add_use(tmp)
                .add_operand(VRegOperand(index))
                .add_operand(VRegOperand(tmp))
                .add_operand(LabelOperand(instr.jump_table_labels[i])));
    }
}

void InstructionSelector::select_load(const TACInstruction &instr)
{
    // LOAD: result = *address
    VirtReg addr = load_operand(instr.operand1);
    VirtReg result = mfn_.get_next_vreg();

    bool is_float = false;
    if (instr.result.type) {
        is_float = is_float_type(instr.result.type);
    }

    if (is_float) {
        float_vregs_.insert(result);
        auto load = MachineInstr(MachineOpcode::FLD);
        load.add_def(result);
        load.add_use(addr);
        load.add_operand(VRegOperand(result));
        load.add_operand(VRegOperand(addr));
        load.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(load));
    } else {
        // All non-float types use LD (64-bit load)
        auto load = MachineInstr(MachineOpcode::LD);
        load.add_def(result);
        load.add_use(addr);
        load.add_operand(VRegOperand(result));
        load.add_operand(VRegOperand(addr));
        load.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(load));
    }

    store_result(result, instr.result);
}

void InstructionSelector::select_store(const TACInstruction &instr)
{
    // STORE: *address = value
    VirtReg addr = load_operand(instr.result);
    VirtReg value = load_operand(instr.operand1);

    bool is_float = false;
    if (instr.operand1.type) {
        is_float = is_float_type(instr.operand1.type);
    } else {
        is_float = float_vregs_.contains(value);
    }

    if (is_float) {
        auto store = MachineInstr(MachineOpcode::FSD);
        store.add_use(value);
        store.add_use(addr);
        store.add_operand(VRegOperand(value));
        store.add_operand(VRegOperand(addr));
        store.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(store));
    } else {

        auto store = MachineInstr(MachineOpcode::SD);
        store.add_use(value);
        store.add_use(addr);
        store.add_operand(VRegOperand(value));
        store.add_operand(VRegOperand(addr));
        store.add_operand(ImmOperand(0));
        mfn_.add_instruction(std::move(store));
    }
}

VirtReg InstructionSelector::load_operand(const TACOperand &operand)
{
    switch (operand.kind) {
    case TACOperand::Kind::CONSTANT: {
        VirtReg vreg = mfn_.get_next_vreg();
        if (std::holds_alternative<double>(operand.value)) {
            double fval = std::get<double>(operand.value);

            // Mark as float vreg
            float_vregs_.insert(vreg);

            // Add constant to pool and get label
            std::string label = backend_.add_float_constant(fval);

            // TODO: Proper approach is lui + fld with %hi/%lo relocations
            // For now, use la + fld pattern (will be fixed in register
            // allocation)
            VirtReg addr_vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(make_la(addr_vreg, label));

            auto fld_instr = MachineInstr(MachineOpcode::FLD);
            fld_instr.add_def(vreg)
                .add_use(addr_vreg)
                .add_operand(VRegOperand(vreg))
                .add_operand(VRegOperand(addr_vreg))
                .add_operand(ImmOperand(0));
            mfn_.add_instruction(std::move(fld_instr));

            return vreg;
        } else {
            int64_t val = std::get<int64_t>(operand.value);
            mfn_.add_instruction(make_li(vreg, static_cast<int32_t>(val)));
            return vreg;
        }
    }
    case TACOperand::Kind::LABEL: {
        VirtReg vreg = mfn_.get_next_vreg();
        std::string label = std::get<std::string>(operand.value);
        mfn_.add_instruction(make_la(vreg, label));
        return vreg;
    }
    case TACOperand::Kind::SYMBOL: {
        SymbolPtr sym = std::get<SymbolPtr>(operand.value);
        std::string sym_name = sym->get_name();
        // Create unique key: scope_id + name to handle shadowed variables
        std::string unique_key =
            std::to_string(sym->get_scope_id()) + "_" + sym_name;

        // Check if this symbol is a function parameter
        auto param_it = param_to_reg_.find(sym_name);
        if (param_it != param_to_reg_.end()) {
            // It's a parameter - copy from its argument register
            VirtReg vreg = mfn_.get_next_vreg();
            PhysReg param_reg = param_it->second;
            if (is_float_type(sym->get_type().type)) {
                float_vregs_.insert(vreg);
                mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_D)
                                         .add_def(vreg)
                                         .add_operand(VRegOperand(vreg))
                                         .add_operand(RegOperand(param_reg)));
            } else {
                mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                         .add_def(vreg)
                                         .add_operand(VRegOperand(vreg))
                                         .add_operand(RegOperand(param_reg)));
            }
            return vreg;
        } else if (sym->get_storage_class() == StorageClass::AUTO) {
            auto qual_type = sym->get_type();

            if (qual_type.type && qual_type.type->kind == TypeKind::ARRAY) {

                int32_t offset = get_local_variable_offset(sym);
                VirtReg vreg = mfn_.get_next_vreg();

                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::ADDI)
                        .add_def(vreg)
                        .add_operand(VRegOperand(vreg))
                        .add_operand(RegOperand(PhysReg::S0_FP))
                        .add_operand(ImmOperand(offset)));
                return vreg;
            } else {

                int32_t offset = get_local_variable_offset(sym);
                VirtReg vreg = mfn_.get_next_vreg();

                if (qual_type.type && is_float_type(qual_type.type)) {
                    float_vregs_.insert(vreg);
                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::FLD)
                            .add_def(vreg)
                            .add_operand(VRegOperand(vreg))
                            .add_operand(MemOperand(PhysReg::S0_FP, offset)));
                } else {
                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::LD)
                            .add_def(vreg)
                            .add_operand(VRegOperand(vreg))
                            .add_operand(MemOperand(PhysReg::S0_FP, offset)));
                }
                return vreg;
            }
        } else {
            // It's a global symbol (STATIC storage class) - load its address
            VirtReg vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(make_la(vreg, sym_name));
            return vreg;
        }
    }
    case TACOperand::Kind::TEMPORARY: {
        std::string temp_name = std::get<std::string>(operand.value);
        VirtReg vreg = get_or_create_vreg_for_temp(temp_name);
        if (operand.type && is_float_type(operand.type)) {
            float_vregs_.insert(vreg);
        }
        return vreg;
    }
    default:
        return INVALID_VREG;
    }
}

void InstructionSelector::store_result(VirtReg vreg, const TACOperand &dest)
{
    if (dest.kind == TACOperand::Kind::TEMPORARY) {
        std::string temp_name = std::get<std::string>(dest.value);

        auto it = temp_to_vreg_.find(temp_name);
        if (it != temp_to_vreg_.end()) {
            // Temporary already has a vreg (redefinition) - emit move
            VirtReg dest_vreg = it->second;
            if (dest_vreg != vreg) {
                bool is_float = float_vregs_.contains(vreg);
                if (is_float) {
                    float_vregs_.insert(dest_vreg);
                    auto fmv = MachineInstr(MachineOpcode::FMV_D)
                                   .add_def(dest_vreg)
                                   .add_use(vreg)
                                   .add_operand(VRegOperand(dest_vreg))
                                   .add_operand(VRegOperand(vreg));
                    mfn_.add_instruction(std::move(fmv));
                } else {
                    mfn_.add_instruction(make_mv(dest_vreg, vreg));
                }
            }
        } else {

            temp_to_vreg_[temp_name] = vreg;
        }
    } else if (dest.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr sym = std::get<SymbolPtr>(dest.value);
        if (sym->get_storage_class() == StorageClass::AUTO) {

            int32_t offset = get_local_variable_offset(sym);
            bool is_float = float_vregs_.contains(vreg);

            if (is_float) {
                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::FSD)
                        .add_use(vreg)
                        .add_operand(VRegOperand(vreg))
                        .add_operand(MemOperand(PhysReg::S0_FP, offset)));
            } else {

                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::SD)
                        .add_use(vreg)
                        .add_operand(VRegOperand(vreg))
                        .add_operand(MemOperand(PhysReg::S0_FP, offset)));
            }
        }
    }
}

uint32_t InstructionSelector::get_type_size(TypePtr type) const
{
    if (!type || !type->has_layout())
        return 4;
    return type->layout.size;
}

bool InstructionSelector::is_signed_type(TypePtr type) const
{
    if (!type)
        return true;

    // Check if it's a builtin integer type
    if (type->kind == TypeKind::BUILTIN) {
        auto builtin = std::static_pointer_cast<BuiltinType>(type);
        switch (builtin->builtin_kind) {
        case BuiltinTypeKind::INT:
        case BuiltinTypeKind::CHAR:
            return true;
        case BuiltinTypeKind::UNSIGNED:
            return false;
        default:
            return true;
        }
    }
    return true;
}

bool InstructionSelector::is_float_type(TypePtr type) const
{
    if (!type)
        return false;

    if (type->kind == TypeKind::BUILTIN) {
        auto builtin = std::static_pointer_cast<BuiltinType>(type);
        return builtin->builtin_kind == BuiltinTypeKind::FLOAT;
    }
    return false;
}

VirtReg
InstructionSelector::get_or_create_vreg_for_temp(const std::string &temp_name)
{
    auto it = temp_to_vreg_.find(temp_name);
    if (it != temp_to_vreg_.end()) {
        return it->second;
    }

    // Temp not yet defined - this should only happen for loads before stores
    // (e.g., loading a parameter or pre-existing symbol)
    VirtReg vreg = mfn_.get_next_vreg();
    temp_to_vreg_[temp_name] = vreg;
    mfn_.get_frame().allocate_temp(temp_name, 4);

    return vreg;
}

int32_t InstructionSelector::allocate_local_variable(SymbolPtr sym)
{

    std::string unique_key =
        std::to_string(sym->get_scope_id()) + "_" + sym->get_name();

    auto it = local_var_offsets_.find(unique_key);
    if (it != local_var_offsets_.end()) {
        return it->second;
    }

    uint32_t size = get_type_size(sym->get_type().type);
    uint32_t alignment = std::min(size, 4u);

    int32_t offset = mfn_.get_frame().allocate_slot(size, alignment);
    local_var_offsets_[unique_key] = offset;

    sym->set_stack_offset(offset);

    return offset;
}

int32_t InstructionSelector::get_local_variable_offset(SymbolPtr sym)
{

    std::string unique_key =
        std::to_string(sym->get_scope_id()) + "_" + sym->get_name();

    auto it = local_var_offsets_.find(unique_key);
    if (it != local_var_offsets_.end()) {
        return it->second;
    }

    return allocate_local_variable(sym);
}

RiscV32Backend::RiscV32Backend(const TACProgram &program,
                               SymbolTable &symtab,
                               TypeFactory &types)
    : program_(program), symtab_(symtab), types_(types),
      next_float_constant_id_(0)
{
    lower_functions();
}

std::string RiscV32Backend::add_float_constant(double value)
{

    uint64_t bits;
    std::memcpy(&bits, &value, sizeof(double));

    auto it = float_constants_.find(bits);
    if (it != float_constants_.end()) {
        return it->second;
    }

    std::string label = ".LC" + std::to_string(next_float_constant_id_++);
    float_constants_[bits] = label;
    return label;
}

void RiscV32Backend::lower_functions()
{
    for (const auto &tac_fn : program_.functions) {
        try {
            auto mfn = std::make_unique<MachineFunction>(*tac_fn, types_);

            InstructionSelector selector(*mfn, types_, *this);
            selector.select(*tac_fn);

            mfn->build_cfg();

            LinearScanAllocator allocator(*mfn);
            allocator.run();

            mfn->get_frame().finalize();

            machine_functions_.push_back(std::move(mfn));
        } catch (const std::exception &e) {
            throw std::runtime_error(std::string("Error lowering function '") +
                                     tac_fn->mangled_name + "': " + e.what());
        }
    }
}

void RiscV32Backend::emit(std::ostream &os)
{
    emit_preamble(os);
    emit_rodata(os);
    emit_globals(os);
    emit_text(os);
}

void RiscV32Backend::emit_preamble(std::ostream &os) const
{
    os << "    .option nopic\n";
    os << "    .attribute arch, \"rv64imfd\"\n";
}

void RiscV32Backend::emit_rodata(std::ostream &os) const
{
    bool has_data =
        !program_.string_literals.empty() || !float_constants_.empty();
    if (!has_data) {
        return;
    }

    os << "\n    .section .rodata\n";

    for (const auto &str_lit : program_.string_literals) {
        os << "    .balign 4\n";
        os << str_lit.label << ":\n";
        os << "    .asciz \"";

        // Escape string content
        for (char c : str_lit.value) {
            switch (c) {
            case '\n':
                os << "\\n";
                break;
            case '\t':
                os << "\\t";
                break;
            case '\r':
                os << "\\r";
                break;
            case '\"':
                os << "\\\"";
                break;
            case '\\':
                os << "\\\\";
                break;
            default:
                if (c >= 32 && c < 127) {
                    os << c;
                } else {
                    os << "\\x" << std::hex << (int)(unsigned char)c
                       << std::dec;
                }
                break;
            }
        }
        os << "\"\n";
    }

    std::vector<std::pair<uint64_t, std::string>> sorted_floats(
        float_constants_.begin(),
        float_constants_.end());
    std::sort(sorted_floats.begin(),
              sorted_floats.end(),
              [](const auto &a, const auto &b) { return a.second < b.second; });

    for (const auto &[bits, label] : sorted_floats) {
        os << "    .balign 8\n";
        os << label << ":\n";
        os << "    .dword   0x" << std::hex << bits << std::dec << "\n";
    }
}

void RiscV32Backend::emit_globals(std::ostream &os) const
{
    if (program_.global_variables.empty()) {
        return;
    }

    os << "\n    .section .bss\n";
    for (const auto &global : program_.global_variables) {
        auto type = global->get_type().type;
        size_t size = type->has_layout() ? type->layout.size : 4;
        size_t align = type->has_layout() ? type->layout.alignment : 4;

        os << "    .balign " << align << "\n";
        os << "    .globl " << global->get_name() << "\n";
        os << global->get_name() << ":\n";
        os << "    .zero " << size << "\n";
    }
}

void RiscV32Backend::emit_text(std::ostream &os)
{
    os << "\n    .section .text\n";

    for (auto &mfn : machine_functions_) {
        emit_function(os, *mfn);
    }
}

void RiscV32Backend::emit_function(std::ostream &os, MachineFunction &mfn)
{
    try {
        // For the main function, use "main" as the label name instead of the
        // mangled name This is required for linking with the C runtime
        std::string label_name = mfn.get_name();
        if (label_name == "_Z4mainv") {
            label_name = "main";
        }

        os << "\n    .globl " << label_name << "\n";
        os << "    .type " << label_name << ", @function\n";
        os << label_name << ":\n";

        emit_prologue(os, mfn);

        // Emit instructions
        for (size_t i = 0; i < mfn.get_instructions().size(); ++i) {
            const auto &instr = mfn.get_instructions()[i];
            emit_instruction(os, instr, mfn);
        }

        // Note: epilogue is emitted as part of RET instruction handling
    } catch (const std::exception &e) {
        throw std::runtime_error(std::string("Error in function ") +
                                 mfn.get_name() + ": " + e.what());
    }
}

void RiscV32Backend::emit_prologue(std::ostream &os,
                                   const MachineFunction &mfn) const
{
    const auto &frame = mfn.get_frame();
    int32_t frame_size = frame.get_frame_size();

    if (frame_size == 0) {
        return;
    }

    // Allocate stack frame
    os << "    addi sp, sp, -" << frame_size << "\n";

    // Save return address (8 bytes in RV64)
    os << "    sd ra, " << (frame_size + frame.get_ra_offset()) << "(sp)\n";

    // Save frame pointer (8 bytes in RV64)
    os << "    sd s0, " << (frame_size + frame.get_fp_offset()) << "(sp)\n";

    // Set up frame pointer
    os << "    addi s0, sp, " << frame_size << "\n";

    // Save callee-saved registers (8 bytes each in RV64)
    for (auto reg : frame.get_used_callee_regs()) {
        int32_t offset = frame.get_callee_save_offset(reg);
        if (reg >= PhysReg::FT0 && reg <= PhysReg::FA7) {
            os << "    fsd " << get_reg_name(reg) << ", "
               << (frame_size + offset) << "(sp)\n";
        } else {
            os << "    sd " << get_reg_name(reg) << ", "
               << (frame_size + offset) << "(sp)\n";
        }
    }
}

void RiscV32Backend::emit_epilogue(std::ostream &os,
                                   const MachineFunction &mfn) const noexcept
{
    const auto &frame = mfn.get_frame();
    int32_t frame_size = frame.get_frame_size();

    if (frame_size == 0) {
        os << "    ret\n";
        return;
    }

    // Restore callee-saved registers
    for (auto reg : frame.get_used_callee_regs()) {
        int32_t offset = frame.get_callee_save_offset(reg);
        if (reg >= PhysReg::FT0 && reg <= PhysReg::FA7) {
            os << "    fld " << get_reg_name(reg) << ", "
               << (frame_size + offset) << "(sp)\n";
        } else {
            os << "    ld " << get_reg_name(reg) << ", "
               << (frame_size + offset) << "(sp)\n";
        }
    }

    // Restore frame pointer (8 bytes in RV64)
    os << "    ld s0, " << (frame_size + frame.get_fp_offset()) << "(sp)\n";

    // Restore return address (8 bytes in RV64)
    os << "    ld ra, " << (frame_size + frame.get_ra_offset()) << "(sp)\n";

    // Deallocate stack frame
    os << "    addi sp, sp, " << frame_size << "\n";

    // Return
    os << "    ret\n";
}

void RiscV32Backend::emit_instruction(std::ostream &os,
                                      const MachineInstr &instr,
                                      const MachineFunction &mfn) const
{
    // Special handling for RET - emit epilogue
    if (instr.get_opcode() == MachineOpcode::RET) {
        emit_epilogue(os, mfn);
        return;
    }

    // For all other instructions, use the instruction's emit method
    instr.emit(os);
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
