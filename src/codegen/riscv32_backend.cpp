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

void InstructionSelector::store_param_from_register(PhysReg source_reg,
                                                    int32_t stack_offset,
                                                    MachineOpcode store_op)
{
    if (is_valid_imm12(stack_offset)) {
        mfn_.add_instruction(
            MachineInstr(store_op)
                .add_operand(RegOperand(source_reg))
                .add_operand(MemOperand(PhysReg::S0_FP, stack_offset)));
    } else {
        VirtReg addr = add_offset_to_phys_reg(PhysReg::S0_FP, stack_offset);
        mfn_.add_instruction(MachineInstr(store_op)
                                 .add_use(addr)
                                 .add_operand(RegOperand(source_reg))
                                 .add_operand(VRegOperand(addr))
                                 .add_operand(ImmOperand(0)));
    }
}

void InstructionSelector::store_param_from_stack(int32_t source_stack_offset,
                                                 int32_t dest_stack_offset,
                                                 MachineOpcode load_op,
                                                 MachineOpcode store_op)
{
    const auto tmp = mfn_.get_next_vreg();

    emit_load_from_fp(tmp, source_stack_offset, load_op);
    emit_store_to_fp(tmp, dest_stack_offset, store_op);
}

void InstructionSelector::handle_implicit_this_param(const TACOperand &param,
                                                     ParamAllocator &allocator)
{
    const auto temp_name = std::get<std::string>(param.value);
    const auto offset =
        mfn_.get_frame().allocate_slot(POINTER_SIZE, POINTER_SIZE);
    temp_to_offset_[temp_name] = offset;

    if (allocator.int_idx < MAX_INT_REGS) {
        store_param_from_register(allocator.next_int_reg(),
                                  offset,
                                  MachineOpcode::SD);
    } else {
        store_param_from_stack(allocator.next_stack_offset(),
                               offset,
                               MachineOpcode::LD,
                               MachineOpcode::SD);
    }
}

void InstructionSelector::handle_aggregate_param(SymbolPtr sym,
                                                 int32_t offset,
                                                 ParamAllocator &allocator)
{
    aggregate_params_.insert(sym->get_name());

    if (allocator.int_idx < MAX_INT_REGS) {
        store_param_from_register(allocator.next_int_reg(),
                                  offset,
                                  MachineOpcode::SD);
    } else {
        store_param_from_stack(allocator.next_stack_offset(),
                               offset,
                               MachineOpcode::LD,
                               MachineOpcode::SD);
    }
}

void InstructionSelector::handle_regular_param(const TACOperand &param,
                                               SymbolPtr sym,
                                               int32_t offset,
                                               ParamAllocator &allocator)
{
    const auto is_float = param.type && is_float_type(param.type);
    const auto tmp = mfn_.get_next_vreg();

    if (is_float) {
        float_vregs_.insert(tmp);
    }

    const auto in_int_reg = !is_float && allocator.int_idx < MAX_INT_REGS;
    const auto in_float_reg = is_float && allocator.float_idx < MAX_FLOAT_REGS;

    if (in_int_reg) {
        const auto store_op = get_store_opcode(param.type, false);
        store_param_from_register(allocator.next_int_reg(), offset, store_op);
    } else if (in_float_reg) {
        store_param_from_register(allocator.next_float_reg(),
                                  offset,
                                  MachineOpcode::FSD);
    } else {
        const auto stack_offset = allocator.next_stack_offset();
        const auto load_op = get_load_opcode(param.type, is_float);
        const auto store_op = get_store_opcode(param.type, is_float);

        // Load from incoming stack parameters and store to local variable slot
        emit_load_from_fp(tmp, stack_offset, load_op);
        emit_store_to_fp(tmp, offset, store_op);
    }
}

void InstructionSelector::select(const TACFunction &tac_fn)
{
    param_to_reg_.clear();
    aggregate_params_.clear();
    sret_ptr_vreg_ = INVALID_VREG;

    ParamAllocator allocator;

    const auto has_implicit_this = [&] {
        if (tac_fn.parameters.empty())
            return false;
        const auto &first_param = tac_fn.parameters[0];
        if (first_param.kind != TACOperand::Kind::TEMPORARY ||
            !first_param.is_valid()) {
            return false;
        }
        return std::get<std::string>(first_param.value) == "this";
    }();

    if (is_aggregate_type(tac_fn.return_type)) {
        sret_ptr_vreg_ = mfn_.get_next_vreg();
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_def(sret_ptr_vreg_)
                                 .add_operand(VRegOperand(sret_ptr_vreg_))
                                 .add_operand(RegOperand(PhysReg::A0)));
        allocator.int_idx = 1;
    }

    for (size_t param_idx = 0; param_idx < tac_fn.parameters.size();
         ++param_idx) {
        const auto &param = tac_fn.parameters[param_idx];

        if (param_idx == 0 && has_implicit_this) {
            handle_implicit_this_param(param, allocator);
            continue;
        }

        if (param.kind != TACOperand::Kind::SYMBOL) {
            continue;
        }

        const auto sym = std::get<SymbolPtr>(param.value);
        const auto offset = get_local_variable_offset(sym);
        const auto is_aggregate = param.type && is_aggregate_type(param.type);

        if (is_aggregate) {
            handle_aggregate_param(sym, offset, allocator);
        } else {
            handle_regular_param(param, sym, offset, allocator);
        }
    }

    for (const auto &bb : tac_fn.basic_blocks) {
        if (!bb->label.empty()) {
            mfn_.add_instruction(make_label(bb->label));
        }

        for (size_t i = 0; i < bb->instructions.size(); ++i) {
            try {
                select_instruction(*bb->instructions[i]);
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

    case TACOpcode::CAST:
        select_cast(instr);
        break;

    case TACOpcode::LOAD_MEMBER:
        select_load_member(instr);
        break;

    case TACOpcode::STORE_MEMBER:
        select_store_member(instr);
        break;

    default:
        break;
    }
}

void InstructionSelector::copy_aggregate_words(VirtReg dst_base,
                                               VirtReg src_base,
                                               uint32_t size_bytes)
{
    const uint32_t num_words = (size_bytes + WORD_SIZE - 1) / WORD_SIZE;

    for (uint32_t word = 0; word < num_words; ++word) {
        const auto offset = static_cast<int64_t>(word * WORD_SIZE);

        const auto src_addr = add_offset_to_reg(src_base, offset);
        const auto dst_addr = add_offset_to_reg(dst_base, offset);

        const auto temp = mfn_.get_next_vreg();
        mfn_.add_instruction(MachineInstr(MachineOpcode::LD)
                                 .add_def(temp)
                                 .add_use(src_addr)
                                 .add_operand(VRegOperand(temp))
                                 .add_operand(VRegOperand(src_addr))
                                 .add_operand(ImmOperand(0)));

        mfn_.add_instruction(MachineInstr(MachineOpcode::SD)
                                 .add_use(temp)
                                 .add_use(dst_addr)
                                 .add_operand(VRegOperand(temp))
                                 .add_operand(VRegOperand(dst_addr))
                                 .add_operand(ImmOperand(0)));
    }
}

void InstructionSelector::select_return(const TACInstruction &instr)
{
    if (instr.operand1.is_valid()) {
        if (instr.operand1.type && is_aggregate_type(instr.operand1.type)) {
            if (sret_ptr_vreg_ == INVALID_VREG) {
                throw std::runtime_error(
                    "Returning aggregate but no sret pointer available");
            }

            const auto src_addr = load_operand(instr.operand1);
            const auto size = get_type_size(instr.operand1.type);

            copy_aggregate_words(sret_ptr_vreg_, src_addr, size);

            mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                     .add_use(sret_ptr_vreg_)
                                     .add_operand(RegOperand(PhysReg::A0))
                                     .add_operand(VRegOperand(sret_ptr_vreg_)));
        } else {
            const auto val = load_operand(instr.operand1);
            const bool is_float = instr.operand1.type
                                      ? is_float_type(instr.operand1.type)
                                      : float_vregs_.contains(val);

            const auto return_reg = is_float ? PhysReg::FA0 : PhysReg::A0;
            const auto move_op =
                is_float ? MachineOpcode::FMV_D : MachineOpcode::MV;

            mfn_.add_instruction(MachineInstr(move_op)
                                     .add_use(val)
                                     .add_operand(RegOperand(return_reg))
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
            VirtReg addr_vreg = add_offset_to_phys_reg(PhysReg::S0_FP, offset);

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

    // Dereferencing a pointer-to-array, pointer-to-struct, or pointer-to-class
    // yields the address
    if (instr.result.type && (instr.result.type->kind == TypeKind::ARRAY ||
                              instr.result.type->kind == TypeKind::RECORD ||
                              instr.result.type->kind == TypeKind::CLASS)) {
        // Just move the address to the result (no actual dereferencing needed
        // for aggregate types)
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                 .add_def(result)
                                 .add_use(addr)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(VRegOperand(addr))
                                 .add_operand(ImmOperand(0)));
        store_result(result, instr.result);
        return;
    }

    bool is_float = false;
    if (instr.result.type) {
        is_float = is_float_type(instr.result.type);
    }

    MachineOpcode load_op = get_load_opcode(instr.result.type, is_float);
    if (is_float) {
        float_vregs_.insert(result);
    }

    auto load = MachineInstr(load_op);
    load.add_def(result);
    load.add_use(addr);
    load.add_operand(VRegOperand(result));
    load.add_operand(VRegOperand(addr));
    load.add_operand(ImmOperand(0));
    mfn_.add_instruction(std::move(load));

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

        bool op1_is_float = float_vregs_.contains(op1);
        bool op2_is_float = float_vregs_.contains(op2);

        if (!op1_is_float) {
            VirtReg converted = mfn_.get_next_vreg();
            float_vregs_.insert(converted);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_D_L)
                                     .add_def(converted)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(converted))
                                     .add_operand(VRegOperand(op1)));
            op1 = converted;
        }

        if (!op2_is_float) {
            VirtReg converted = mfn_.get_next_vreg();
            float_vregs_.insert(converted);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_D_L)
                                     .add_def(converted)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(converted))
                                     .add_operand(VRegOperand(op2)));
            op2 = converted;
        }

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
        bool op1_is_float = float_vregs_.contains(op1);
        bool op2_is_float = float_vregs_.contains(op2);

        if (!op1_is_float) {
            VirtReg converted = mfn_.get_next_vreg();
            float_vregs_.insert(converted);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_D_L)
                                     .add_def(converted)
                                     .add_use(op1)
                                     .add_operand(VRegOperand(converted))
                                     .add_operand(VRegOperand(op1)));
            op1 = converted;
        }

        if (!op2_is_float) {
            VirtReg converted = mfn_.get_next_vreg();
            float_vregs_.insert(converted);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_D_L)
                                     .add_def(converted)
                                     .add_use(op2)
                                     .add_operand(VRegOperand(converted))
                                     .add_operand(VRegOperand(op2)));
            op2 = converted;
        }

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
    // For constants (int or float), don't emit load instructions here.
    // select_call will emit them directly to arg registers for optimization.
    VirtReg param;
    if (instr.operand1.kind == TACOperand::Kind::CONSTANT) {
        param = mfn_.get_next_vreg();
    } else {
        param = load_operand(instr.operand1);
    }
    pending_params_.push_back({param, instr.operand1});
}

void InstructionSelector::select_call(const TACInstruction &instr)
{
    std::vector<QualifiedType> expected_param_types;
    bool is_variadic = false;
    TypePtr return_type = nullptr;

    if (instr.operand1.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr func_sym = std::get<SymbolPtr>(instr.operand1.value);
        auto func_qual_type = func_sym->get_type();

        if (func_qual_type.type &&
            func_qual_type.type->kind == TypeKind::FUNCTION) {
            auto func_type =
                std::static_pointer_cast<FunctionType>(func_qual_type.type);
            expected_param_types = func_type->param_types;
            is_variadic = func_type->is_variadic;
            return_type = func_type->return_type.type;
        }
    }

    VirtReg sret_slot_ptr = INVALID_VREG;
    bool returns_aggregate = return_type && is_aggregate_type(return_type);
    if (returns_aggregate) {
        uint32_t ret_size = get_type_size(return_type);
        int32_t sret_offset = mfn_.get_frame().allocate_slot(ret_size, 8);

        sret_slot_ptr = add_offset_to_phys_reg(PhysReg::S0_FP, sret_offset);
    }

    bool has_implicit_this =
        (pending_params_.size() == expected_param_types.size() + 1);

    size_t int_reg_idx = 0;
    size_t float_reg_idx = 0;
    std::vector<std::tuple<VirtReg, bool, TACOperand>> stack_params;

    if (returns_aggregate) {
        int_reg_idx = 1;
    }

    std::vector<MachineInstr> param_moves;

    for (size_t i = 0; i < pending_params_.size(); ++i) {
        VirtReg param_vreg = pending_params_[i].first;
        const TACOperand &original_operand = pending_params_[i].second;

        bool is_aggregate_param =
            original_operand.type && is_aggregate_type(original_operand.type);

        if (is_aggregate_param) {
            uint32_t agg_size = get_type_size(original_operand.type);

            int32_t agg_copy_offset =
                mfn_.get_frame().allocate_slot(agg_size, 8);

            VirtReg src_addr = param_vreg;

            VirtReg dst_addr =
                add_offset_to_phys_reg(PhysReg::S0_FP, agg_copy_offset);

            copy_aggregate_words(dst_addr, src_addr, agg_size);

            param_vreg = dst_addr;

            if (int_reg_idx < 8) {
                PhysReg target_reg = static_cast<PhysReg>(
                    static_cast<uint8_t>(PhysReg::A0) + int_reg_idx++);
                param_moves.push_back(
                    MachineInstr(MachineOpcode::MV)
                        .add_use(param_vreg)
                        .add_operand(RegOperand(target_reg))
                        .add_operand(VRegOperand(param_vreg)));
            } else {
                stack_params.push_back({param_vreg, false, original_operand});
            }
            continue;
        }

        bool is_float = float_vregs_.contains(param_vreg);
        if (!is_float && original_operand.kind == TACOperand::Kind::CONSTANT &&
            std::holds_alternative<double>(original_operand.value)) {
            is_float = true;
        }

        bool is_float_expected = false;
        bool is_vararg = (i >= expected_param_types.size()) && is_variadic;

        if (has_implicit_this && i == 0) {
            is_float_expected = false;
        } else if (has_implicit_this) {
            size_t expected_index = i - 1;
            if (expected_index < expected_param_types.size()) {
                is_float_expected =
                    expected_param_types[expected_index].type &&
                    is_float_type(expected_param_types[expected_index].type);
            } else {
                is_float_expected = false;
            }
        } else if (i < expected_param_types.size()) {
            is_float_expected = expected_param_types[i].type &&
                                is_float_type(expected_param_types[i].type);
        } else {
            is_float_expected = false;
        }

        VirtReg converted_vreg = param_vreg;

        // For variadic arguments, floats must be passed in integer registers
        if (is_vararg && is_float) {
            // Convert float to integer register using FMV.X.D
            converted_vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_X_D)
                                     .add_def(converted_vreg)
                                     .add_use(param_vreg)
                                     .add_operand(VRegOperand(converted_vreg))
                                     .add_operand(VRegOperand(param_vreg)));
            is_float_expected = false;
        } else if (is_float && !is_float_expected) {
            converted_vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_X_D)
                                     .add_def(converted_vreg)
                                     .add_use(param_vreg)
                                     .add_operand(VRegOperand(converted_vreg))
                                     .add_operand(VRegOperand(param_vreg)));
        } else if (!is_float && is_float_expected) {
            converted_vreg = mfn_.get_next_vreg();
            float_vregs_.insert(converted_vreg);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_D_X)
                                     .add_def(converted_vreg)
                                     .add_use(param_vreg)
                                     .add_operand(VRegOperand(converted_vreg))
                                     .add_operand(VRegOperand(param_vreg)));
        }

        bool placed_in_register = false;

        if (is_float_expected && float_reg_idx < 8) {
            PhysReg target_reg = static_cast<PhysReg>(
                static_cast<uint8_t>(PhysReg::FA0) + float_reg_idx++);

            // Optimization: if parameter is a float constant, load directly
            // from constant pool to argument register
            if (original_operand.kind == TACOperand::Kind::CONSTANT &&
                std::holds_alternative<double>(original_operand.value)) {
                double const_val = std::get<double>(original_operand.value);
                std::string label = backend_.add_float_constant(const_val);

                VirtReg addr_vreg = mfn_.get_next_vreg();
                param_moves.push_back(make_la(addr_vreg, label));

                param_moves.push_back(MachineInstr(MachineOpcode::FLD)
                                          .add_use(addr_vreg)
                                          .add_operand(RegOperand(target_reg))
                                          .add_operand(VRegOperand(addr_vreg))
                                          .add_operand(ImmOperand(0)));
            } else {
                param_moves.push_back(
                    MachineInstr(MachineOpcode::FMV_D)
                        .add_use(converted_vreg)
                        .add_operand(RegOperand(target_reg))
                        .add_operand(VRegOperand(converted_vreg)));
            }
            placed_in_register = true;
        } else if (!is_float_expected && int_reg_idx < 8) {
            PhysReg target_reg = static_cast<PhysReg>(
                static_cast<uint8_t>(PhysReg::A0) + int_reg_idx++);

            // Optimization: if parameter is an integer constant, emit li
            // directly
            if (original_operand.kind == TACOperand::Kind::CONSTANT &&
                std::holds_alternative<int64_t>(original_operand.value)) {
                int64_t const_val = std::get<int64_t>(original_operand.value);
                param_moves.push_back(MachineInstr(MachineOpcode::LI)
                                          .add_operand(RegOperand(target_reg))
                                          .add_operand(ImmOperand(const_val)));
            } else {
                param_moves.push_back(
                    MachineInstr(MachineOpcode::MV)
                        .add_use(converted_vreg)
                        .add_operand(RegOperand(target_reg))
                        .add_operand(VRegOperand(converted_vreg)));
            }
            placed_in_register = true;
        }

        if (!placed_in_register) {
            stack_params.push_back(
                {converted_vreg, is_float_expected, original_operand});
        }
    }

    // Store stack parameters FIRST (before register moves)
    // This is critical: stack param vregs might be allocated to a0-a7,
    // and we're about to overwrite those registers with register params
    for (size_t i = 0; i < stack_params.size(); ++i) {
        VirtReg param_vreg = std::get<0>(stack_params[i]);
        bool is_float = std::get<1>(stack_params[i]);
        const TACOperand &original_operand = std::get<2>(stack_params[i]);

        // Stack slot offset: SP + (i * 8)
        // We're storing relative to SP before the call
        int32_t stack_offset = i * 8;

        if (original_operand.kind == TACOperand::Kind::CONSTANT) {
            if (std::holds_alternative<int64_t>(original_operand.value)) {
                // Integer constant: LI then SD
                int64_t const_val = std::get<int64_t>(original_operand.value);
                VirtReg temp_vreg = mfn_.get_next_vreg();
                mfn_.add_instruction(make_li(temp_vreg, const_val));
                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::SD)
                        .add_use(temp_vreg)
                        .add_operand(VRegOperand(temp_vreg))
                        .add_operand(MemOperand(PhysReg::SP, stack_offset)));
            } else if (std::holds_alternative<double>(original_operand.value)) {
                // Float constant: LA + FLD from pool, then FSD to stack
                double const_val = std::get<double>(original_operand.value);
                std::string label = backend_.add_float_constant(const_val);

                VirtReg addr_vreg = mfn_.get_next_vreg();
                mfn_.add_instruction(make_la(addr_vreg, label));

                VirtReg temp_vreg = mfn_.get_next_vreg();
                float_vregs_.insert(temp_vreg);
                mfn_.add_instruction(MachineInstr(MachineOpcode::FLD)
                                         .add_def(temp_vreg)
                                         .add_use(addr_vreg)
                                         .add_operand(VRegOperand(temp_vreg))
                                         .add_operand(VRegOperand(addr_vreg))
                                         .add_operand(ImmOperand(0)));

                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::FSD)
                        .add_use(temp_vreg)
                        .add_operand(VRegOperand(temp_vreg))
                        .add_operand(MemOperand(PhysReg::SP, stack_offset)));
            }
        } else if (is_float) {
            mfn_.add_instruction(
                MachineInstr(MachineOpcode::FSD)
                    .add_use(param_vreg)
                    .add_operand(VRegOperand(param_vreg))
                    .add_operand(MemOperand(PhysReg::SP, stack_offset)));
        } else {
            mfn_.add_instruction(
                MachineInstr(MachineOpcode::SD)
                    .add_use(param_vreg)
                    .add_operand(VRegOperand(param_vreg))
                    .add_operand(MemOperand(PhysReg::SP, stack_offset)));
        }
    }

    // Emit parameter moves (to argument registers)
    // These can safely overwrite a0-a7/fa0-fa7 now that stack params are stored
    // Solution: Use the stack to break cycles, NOT temp registers (t0-t2 are
    // reserved for spill handling in the register allocator).
    //
    // For moves to argument registers (a0-a7 or fa0-fa7), we use stack slots to
    // save values that would be overwritten, then restore them after other
    // moves complete.

    // First pass: separate moves by type and destination
    std::vector<MachineInstr> int_arg_moves;   // MV to a0-a7
    std::vector<MachineInstr> float_arg_moves; // FMV_D to fa0-fa7
    std::vector<MachineInstr> other_moves; // LI or moves to non-arg registers

    for (const auto &move : param_moves) {
        bool is_to_arg_reg = false;

        if (move.operands().size() >= 1 &&
            std::holds_alternative<RegOperand>(move.operands()[0])) {
            PhysReg dest_reg = std::get<RegOperand>(move.operands()[0]).reg;

            // Check if moving to integer argument register
            if (move.get_opcode() == MachineOpcode::MV &&
                dest_reg >= PhysReg::A0 && dest_reg <= PhysReg::A7) {
                int_arg_moves.push_back(move);
                is_to_arg_reg = true;
            }
            // Check if moving to float argument register
            else if (move.get_opcode() == MachineOpcode::FMV_D &&
                     dest_reg >= PhysReg::FA0 && dest_reg <= PhysReg::FA7) {
                float_arg_moves.push_back(move);
                is_to_arg_reg = true;
            }
        }

        if (!is_to_arg_reg) {
            other_moves.push_back(move);
        }
    }

    // PHASE 1: Save all sources to stack (for parallel move resolution)

    bool int_needs_stack_save = false;
    if (!int_arg_moves.empty()) {
        for (const auto &move : int_arg_moves) {
            if (move.operands().size() >= 2 &&
                std::holds_alternative<VRegOperand>(move.operands()[1])) {
                int_needs_stack_save = true;
                break;
            }
        }

        if (int_needs_stack_save) {
            // Save all integer source vregs to stack
            for (size_t i = 0; i < int_arg_moves.size(); ++i) {
                const auto &move = int_arg_moves[i];
                if (move.operands().size() >= 2 &&
                    std::holds_alternative<VRegOperand>(move.operands()[1])) {
                    VirtReg src_vreg =
                        std::get<VRegOperand>(move.operands()[1]).vreg;
                    int32_t stack_offset = -8 * static_cast<int32_t>(i + 1);

                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::SD)
                            .add_use(src_vreg)
                            .add_operand(VRegOperand(src_vreg))
                            .add_operand(
                                MemOperand(PhysReg::SP, stack_offset)));
                }
            }
        }
    }

    bool float_needs_stack_save = false;
    if (!float_arg_moves.empty()) {
        for (const auto &move : float_arg_moves) {
            if (move.operands().size() >= 2 &&
                std::holds_alternative<VRegOperand>(move.operands()[1])) {
                float_needs_stack_save = true;
                break;
            }
        }

        if (float_needs_stack_save) {
            // Save all float source vregs to stack (using negative offsets
            // beyond int saves)
            size_t base_offset = int_arg_moves.size();
            for (size_t i = 0; i < float_arg_moves.size(); ++i) {
                const auto &move = float_arg_moves[i];
                if (move.operands().size() >= 2 &&
                    std::holds_alternative<VRegOperand>(move.operands()[1])) {
                    VirtReg src_vreg =
                        std::get<VRegOperand>(move.operands()[1]).vreg;
                    int32_t stack_offset =
                        -8 * static_cast<int32_t>(base_offset + i + 1);

                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::FSD)
                            .add_use(src_vreg)
                            .add_operand(VRegOperand(src_vreg))
                            .add_operand(
                                MemOperand(PhysReg::SP, stack_offset)));
                }
            }
        }
    }

    uint32_t total_spills = 0;
    if (int_needs_stack_save) {
        total_spills += static_cast<uint32_t>(int_arg_moves.size());
    }
    if (float_needs_stack_save) {
        total_spills += static_cast<uint32_t>(float_arg_moves.size());
    }
    if (total_spills > 0) {
        mfn_.get_frame().update_max_parallel_move_spills(total_spills);
    }

    // PHASE 2: Emit other_moves (safe now - all sources saved)
    for (const auto &move : other_moves) {
        mfn_.add_instruction(move);
    }

    // PHASE 3: Load destinations from stack (complete parallel move)
    if (int_needs_stack_save) {
        for (size_t i = 0; i < int_arg_moves.size(); ++i) {
            const auto &move = int_arg_moves[i];
            if (move.operands().size() >= 2 &&
                std::holds_alternative<RegOperand>(move.operands()[0]) &&
                std::holds_alternative<VRegOperand>(move.operands()[1])) {
                PhysReg dest_reg = std::get<RegOperand>(move.operands()[0]).reg;
                int32_t stack_offset = -8 * static_cast<int32_t>(i + 1);

                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::LD)
                        .add_operand(RegOperand(dest_reg))
                        .add_operand(MemOperand(PhysReg::SP, stack_offset)));
            }
        }
    } else if (!int_arg_moves.empty()) {
        // No stack save needed, emit moves directly
        for (const auto &move : int_arg_moves) {
            mfn_.add_instruction(move);
        }
    }

    if (float_needs_stack_save) {
        // Load from stack to destination float argument registers
        size_t base_offset = int_arg_moves.size();
        for (size_t i = 0; i < float_arg_moves.size(); ++i) {
            const auto &move = float_arg_moves[i];
            if (move.operands().size() >= 2 &&
                std::holds_alternative<RegOperand>(move.operands()[0]) &&
                std::holds_alternative<VRegOperand>(move.operands()[1])) {
                PhysReg dest_reg = std::get<RegOperand>(move.operands()[0]).reg;
                int32_t stack_offset =
                    -8 * static_cast<int32_t>(base_offset + i + 1);

                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::FLD)
                        .add_operand(RegOperand(dest_reg))
                        .add_operand(MemOperand(PhysReg::SP, stack_offset)));
            }
        }
    } else if (!float_arg_moves.empty()) {
        // No stack save needed, emit moves directly
        for (const auto &move : float_arg_moves) {
            mfn_.add_instruction(move);
        }
    } // Update max call args for frame layout
    if (pending_params_.size() > 8) {
        mfn_.get_frame().update_max_call_args(
            static_cast<uint32_t>(pending_params_.size()));
    }

    if (returns_aggregate && sret_slot_ptr != INVALID_VREG) {
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_use(sret_slot_ptr)
                                 .add_operand(RegOperand(PhysReg::A0))
                                 .add_operand(VRegOperand(sret_slot_ptr)));
    }

    if (instr.operand1.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr func_sym = std::get<SymbolPtr>(instr.operand1.value);
        mfn_.add_instruction(make_call(func_sym->get_name()));
    }

    if (instr.result.is_valid()) {
        TypePtr ret_type = instr.result.type;

        if (ret_type && is_aggregate_type(ret_type)) {
            store_result(sret_slot_ptr, instr.result);
        } else if (ret_type && is_float_type(ret_type)) {
            VirtReg result = mfn_.get_next_vreg();
            float_vregs_.insert(result);
            mfn_.add_instruction(MachineInstr(MachineOpcode::FMV_D)
                                     .add_def(result)
                                     .add_operand(VRegOperand(result))
                                     .add_operand(RegOperand(PhysReg::FA0)));
            store_result(result, instr.result);
        } else {
            VirtReg result = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                     .add_def(result)
                                     .add_operand(VRegOperand(result))
                                     .add_operand(RegOperand(PhysReg::A0)));
            store_result(result, instr.result);
        }
    }

    pending_params_.clear();
}

void InstructionSelector::select_jump_table(const TACInstruction &instr)
{
    VirtReg index = load_operand(instr.operand1);

    std::string table_label = backend_.add_jump_table(instr.jump_table_labels);

    VirtReg table_addr = mfn_.get_next_vreg();
    mfn_.add_instruction(MachineInstr(MachineOpcode::LA)
                             .add_def(table_addr)
                             .add_operand(VRegOperand(table_addr))
                             .add_operand(LabelOperand(table_label)));

    // Scale index by 8 (size of address pointer on rv64)
    VirtReg scale = mfn_.get_next_vreg();
    mfn_.add_instruction(make_li(scale, 8));

    VirtReg scaled_index = mfn_.get_next_vreg();
    mfn_.add_instruction(MachineInstr(MachineOpcode::MUL)
                             .add_def(scaled_index)
                             .add_use(index)
                             .add_use(scale)
                             .add_operand(VRegOperand(scaled_index))
                             .add_operand(VRegOperand(index))
                             .add_operand(VRegOperand(scale)));

    VirtReg entry_addr = mfn_.get_next_vreg();
    mfn_.add_instruction(MachineInstr(MachineOpcode::ADD)
                             .add_def(entry_addr)
                             .add_use(table_addr)
                             .add_use(scaled_index)
                             .add_operand(VRegOperand(entry_addr))
                             .add_operand(VRegOperand(table_addr))
                             .add_operand(VRegOperand(scaled_index)));

    VirtReg target_addr = mfn_.get_next_vreg();
    mfn_.add_instruction(MachineInstr(MachineOpcode::LD)
                             .add_def(target_addr)
                             .add_use(entry_addr)
                             .add_operand(VRegOperand(target_addr))
                             .add_operand(VRegOperand(entry_addr))
                             .add_operand(ImmOperand(0)));

    mfn_.add_instruction(MachineInstr(MachineOpcode::JR)
                             .add_use(target_addr)
                             .add_operand(VRegOperand(target_addr)));
}

void InstructionSelector::select_load(const TACInstruction &instr)
{
    VirtReg addr = load_operand(instr.operand1);
    VirtReg result = mfn_.get_next_vreg();

    bool is_float = false;
    if (instr.result.type) {
        is_float = is_float_type(instr.result.type);
    }

    MachineOpcode load_op = get_load_opcode(instr.result.type, is_float);
    if (is_float) {
        float_vregs_.insert(result);
    }

    auto load = MachineInstr(load_op);
    load.add_def(result);
    load.add_use(addr);
    load.add_operand(VRegOperand(result));
    load.add_operand(VRegOperand(addr));
    load.add_operand(ImmOperand(0));
    mfn_.add_instruction(std::move(load));

    store_result(result, instr.result);
}

void InstructionSelector::select_store(const TACInstruction &instr)
{
    VirtReg addr = load_operand(instr.result);
    VirtReg value = load_operand(instr.operand1);

    bool is_float = false;
    if (instr.operand1.type) {
        is_float = is_float_type(instr.operand1.type);
    } else {
        is_float = float_vregs_.contains(value);
    }

    MachineOpcode store_op = get_store_opcode(instr.operand1.type, is_float);
    auto store = MachineInstr(store_op);
    store.add_use(value);
    store.add_use(addr);
    store.add_operand(VRegOperand(value));
    store.add_operand(VRegOperand(addr));
    store.add_operand(ImmOperand(0));
    mfn_.add_instruction(std::move(store));
}

void InstructionSelector::select_cast(const TACInstruction &instr)
{
    VirtReg src = load_operand(instr.operand1);
    VirtReg dst = mfn_.get_next_vreg();

    bool src_is_float = float_vregs_.contains(src);
    bool dst_is_float = instr.result.type && is_float_type(instr.result.type);

    if (src_is_float && !dst_is_float) {
        mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_L_D)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(src)));
    } else if (!src_is_float && dst_is_float) {
        float_vregs_.insert(dst);
        mfn_.add_instruction(MachineInstr(MachineOpcode::FCVT_D_L)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(src)));
    } else {
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_def(dst)
                                 .add_use(src)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(src)));
    }

    store_result(dst, instr.result);
}

VirtReg InstructionSelector::compute_member_address(const TACOperand &operand,
                                                    size_t offset)
{
    VirtReg member_addr = mfn_.get_next_vreg();

    if (operand.kind == TACOperand::Kind::SYMBOL) {
        const SymbolPtr sym = std::get<SymbolPtr>(operand.value);

        if (sym->get_storage_class() == StorageClass::AUTO) {
            const int32_t local_offset = get_local_variable_offset(sym);

            // Check if this is an aggregate parameter or a pointer variable
            bool is_aggregate_param =
                aggregate_params_.contains(sym->get_name());
            bool is_pointer_var = false;
            auto qual_type = sym->get_type();
            if (qual_type.type && qual_type.type->kind == TypeKind::POINTER) {
                is_pointer_var = true;
            }

            if (is_aggregate_param || is_pointer_var) {
                const VirtReg ptr = mfn_.get_next_vreg();
                mfn_.add_instruction(
                    MachineInstr(MachineOpcode::LD)
                        .add_def(ptr)
                        .add_operand(VRegOperand(ptr))
                        .add_operand(MemOperand(PhysReg::S0_FP, local_offset)));

                if (offset == 0)
                    return ptr;

                // Add the member offset to the loaded pointer
                member_addr = add_offset_to_reg(ptr, offset);
            } else {
                // For non-pointer local variables, compute address directly
                member_addr = add_offset_to_phys_reg(PhysReg::S0_FP,
                                                     local_offset + offset);
            }
        } else {
            const VirtReg global_addr = mfn_.get_next_vreg();
            mfn_.add_instruction(make_la(global_addr, sym->get_name()));
            member_addr = add_offset_to_reg(global_addr, offset);
        }
    } else {
        const std::string temp_str = std::get<std::string>(operand.value);

        // For member access, use base_type; otherwise use type
        TypePtr temp_type =
            operand.base_type ? operand.base_type : operand.type;

        // Check if this temporary has an aggregate type
        if (temp_type && is_aggregate_type(temp_type)) {
            // Check if already allocated on stack
            if (auto offset_it = temp_to_offset_.find(temp_str);
                offset_it != temp_to_offset_.end()) {
                // Already allocated on stack, compute address
                int32_t temp_offset = offset_it->second;
                member_addr = add_offset_to_phys_reg(PhysReg::S0_FP,
                                                     temp_offset + offset);
            } else if (auto vreg_it = temp_to_vreg_.find(temp_str);
                       vreg_it != temp_to_vreg_.end()) {
                // Temporary holds an address (e.g., from LOAD_MEMBER)
                // Use it as the base address
                VirtReg base_addr = vreg_it->second;
                member_addr = add_offset_to_reg(base_addr, offset);
            } else {
                // First time seeing this aggregate temporary - allocate stack
                // space
                uint32_t size = get_type_size(temp_type);
                uint32_t alignment = std::min(size, 4u);
                int32_t temp_offset =
                    mfn_.get_frame().allocate_slot(size, alignment);
                temp_to_offset_[temp_str] = temp_offset;

                // Compute address
                member_addr = add_offset_to_phys_reg(PhysReg::S0_FP,
                                                     temp_offset + offset);
            }
        } else {
            // Scalar temporary - use virtual register
            const VirtReg temp_addr = get_or_create_vreg_for_temp(temp_str);
            member_addr = add_offset_to_reg(temp_addr, offset);
        }
    }

    return member_addr;
}

void InstructionSelector::select_load_member(const TACInstruction &instr)
{
    if (instr.operand1.kind != TACOperand::Kind::TEMPORARY &&
        instr.operand1.kind != TACOperand::Kind::SYMBOL) {
        return;
    }

    const VirtReg member_addr =
        compute_member_address(instr.operand1, instr.operand1.member_offset);

    if (instr.result.type && is_aggregate_type(instr.result.type)) {
        store_result(member_addr, instr.result);
        return;
    }

    const VirtReg result = mfn_.get_next_vreg();
    const bool is_float = instr.result.type && is_float_type(instr.result.type);
    MachineOpcode load_op = get_load_opcode(instr.result.type, is_float);

    if (is_float) {
        float_vregs_.insert(result);
    }

    mfn_.add_instruction(MachineInstr(load_op)
                             .add_def(result)
                             .add_use(member_addr)
                             .add_operand(VRegOperand(result))
                             .add_operand(VRegOperand(member_addr))
                             .add_operand(ImmOperand(0)));

    store_result(result, instr.result);
}

void InstructionSelector::select_store_member(const TACInstruction &instr)
{
    if (instr.result.kind != TACOperand::Kind::TEMPORARY &&
        instr.result.kind != TACOperand::Kind::SYMBOL) {
        return;
    }

    const VirtReg value = load_operand(instr.operand1);
    const VirtReg member_addr =
        compute_member_address(instr.result, instr.result.member_offset);

    const bool is_float = [&] {
        if (float_vregs_.contains(value))
            return true;
        if (instr.operand1.type && is_float_type(instr.operand1.type)) {
            float_vregs_.insert(value);
            return true;
        }
        return false;
    }();

    if (instr.operand1.type && is_aggregate_type(instr.operand1.type)) {
        const uint32_t size = get_type_size(instr.operand1.type);
        copy_aggregate_words(member_addr, value, size);
    } else {
        MachineOpcode store_op =
            get_store_opcode(instr.operand1.type, is_float);
        mfn_.add_instruction(MachineInstr(store_op)
                                 .add_use(value)
                                 .add_use(member_addr)
                                 .add_operand(VRegOperand(value))
                                 .add_operand(VRegOperand(member_addr))
                                 .add_operand(ImmOperand(0)));
    }
}

VirtReg InstructionSelector::load_operand(const TACOperand &operand)
{
    switch (operand.kind) {
    case TACOperand::Kind::CONSTANT: {
        VirtReg vreg = mfn_.get_next_vreg();
        if (std::holds_alternative<double>(operand.value)) {
            double fval = std::get<double>(operand.value);

            float_vregs_.insert(vreg);

            std::string label = backend_.add_float_constant(fval);

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
            mfn_.add_instruction(make_li(vreg, val));
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
        std::string unique_key =
            std::to_string(sym->get_scope_id()) + "_" + sym_name;

        if (sym->get_storage_class() == StorageClass::AUTO) {
            // All local variables and parameters are on the stack
            auto qual_type = sym->get_type();

            if (qual_type.type && is_aggregate_type(qual_type.type)) {
                if (aggregate_params_.contains(sym_name)) {
                    int32_t offset = get_local_variable_offset(sym);
                    VirtReg vreg = mfn_.get_next_vreg();

                    emit_load_from_fp(vreg, offset, MachineOpcode::LD);
                    return vreg;
                } else {
                    int32_t offset = get_local_variable_offset(sym);
                    VirtReg vreg =
                        add_offset_to_phys_reg(PhysReg::S0_FP, offset);
                    return vreg;
                }
            } else {
                int32_t offset = get_local_variable_offset(sym);
                VirtReg vreg = mfn_.get_next_vreg();

                bool is_float = qual_type.type && is_float_type(qual_type.type);
                MachineOpcode load_op =
                    get_load_opcode(qual_type.type, is_float);

                if (is_float) {
                    float_vregs_.insert(vreg);
                }

                emit_load_from_fp(vreg, offset, load_op);
                return vreg;
            }
        } else {
            // It's a global symbol (STATIC storage class)
            auto qual_type = sym->get_type();

            if (qual_type.type && is_aggregate_type(qual_type.type)) {
                // For arrays and structs, return the address
                VirtReg vreg = mfn_.get_next_vreg();
                mfn_.add_instruction(make_la(vreg, sym_name));
                return vreg;
            } else {
                // For scalar types, load the address then dereference
                VirtReg addr_vreg = mfn_.get_next_vreg();
                mfn_.add_instruction(make_la(addr_vreg, sym_name));

                VirtReg vreg = mfn_.get_next_vreg();
                bool is_float = qual_type.type && is_float_type(qual_type.type);
                MachineOpcode load_op =
                    get_load_opcode(qual_type.type, is_float);

                if (is_float) {
                    float_vregs_.insert(vreg);
                }

                MachineInstr load(load_op);
                load.add_def(vreg);
                load.add_use(addr_vreg);
                load.add_operand(VRegOperand(vreg));
                load.add_operand(VRegOperand(addr_vreg));
                load.add_operand(ImmOperand(0));
                mfn_.add_instruction(std::move(load));
                return vreg;
            }
        }
    }
    case TACOperand::Kind::TEMPORARY: {
        std::string temp_name = std::get<std::string>(operand.value);

        if (auto offset_it = temp_to_offset_.find(temp_name);
            offset_it != temp_to_offset_.end()) {
            VirtReg vreg = mfn_.get_next_vreg();
            int32_t offset = offset_it->second;

            // For member access, use base_type; otherwise use type
            TypePtr temp_type =
                operand.base_type ? operand.base_type : operand.type;

            // Check if this is an aggregate type - return address instead of
            // loading
            if (temp_type && is_aggregate_type(temp_type)) {
                vreg = add_offset_to_phys_reg(PhysReg::S0_FP, offset);
                return vreg;
            }

            bool is_float = operand.type && is_float_type(operand.type);
            MachineOpcode load_op = get_load_opcode(operand.type, is_float);

            if (is_float) {
                float_vregs_.insert(vreg);
            }

            emit_load_from_fp(vreg, offset, load_op);
            return vreg;
        }

        // For member access, use base_type; otherwise use type
        TypePtr temp_type =
            operand.base_type ? operand.base_type : operand.type;

        // Check if this temporary needs stack allocation (aggregate type)
        if (temp_type && is_aggregate_type(temp_type)) {
            // Allocate stack space for aggregate temporary
            uint32_t size = get_type_size(temp_type);
            uint32_t alignment = std::min(size, 4u);
            int32_t temp_offset =
                mfn_.get_frame().allocate_slot(size, alignment);
            temp_to_offset_[temp_name] = temp_offset;

            // Return address of allocated stack space
            VirtReg vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                     .add_def(vreg)
                                     .add_operand(VRegOperand(vreg))
                                     .add_operand(RegOperand(PhysReg::S0_FP))
                                     .add_operand(ImmOperand(temp_offset)));
            return vreg;
        }

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

        // Check if this temporary already has a vreg assigned
        auto it = temp_to_vreg_.find(temp_name);
        if (it != temp_to_vreg_.end()) {
            VirtReg dest_vreg = it->second;

            if (vreg != dest_vreg) {
                bool is_float = float_vregs_.contains(vreg) ||
                                float_vregs_.contains(dest_vreg);

                if (is_float) {
                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::FMV_D)
                            .add_def(dest_vreg)
                            .add_use(vreg)
                            .add_operand(VRegOperand(dest_vreg))
                            .add_operand(VRegOperand(vreg)));
                } else {
                    mfn_.add_instruction(
                        MachineInstr(MachineOpcode::ADDI)
                            .add_def(dest_vreg)
                            .add_use(vreg)
                            .add_operand(VRegOperand(dest_vreg))
                            .add_operand(VRegOperand(vreg))
                            .add_operand(ImmOperand(0)));
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
            MachineOpcode store_op = get_store_opcode(dest.type, is_float);

            emit_store_to_fp(vreg, offset, store_op);
        } else {
            // Global variable (STATIC storage class)
            VirtReg addr_vreg = mfn_.get_next_vreg();
            mfn_.add_instruction(make_la(addr_vreg, sym->get_name()));

            bool is_float = float_vregs_.contains(vreg);
            MachineOpcode store_op = get_store_opcode(dest.type, is_float);

            MachineInstr store(store_op);
            store.add_use(vreg);
            store.add_use(addr_vreg);
            store.add_operand(VRegOperand(vreg));
            store.add_operand(VRegOperand(addr_vreg));
            store.add_operand(ImmOperand(0));
            mfn_.add_instruction(std::move(store));
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

bool InstructionSelector::is_aggregate_type(TypePtr type) const
{
    if (!type)
        return false;

    return type->kind == TypeKind::RECORD || type->kind == TypeKind::ARRAY ||
           type->kind == TypeKind::CLASS;
}

MachineOpcode InstructionSelector::get_load_opcode(TypePtr type,
                                                   bool is_float) const
{
    if (is_float) {
        return MachineOpcode::FLD;
    }

    if (!type || !type->has_layout()) {
        return MachineOpcode::LD;
    }

    constexpr auto size_to_opcode = [](size_t size) constexpr {
        switch (size) {
        case 1:
            return MachineOpcode::LB;
        case 2:
            return MachineOpcode::LH;
        case 4:
            return MachineOpcode::LW;
        default:
            return MachineOpcode::LD;
        }
    };

    return size_to_opcode(type->layout.size);
}

MachineOpcode InstructionSelector::get_store_opcode(TypePtr type,
                                                    bool is_float) const
{
    if (is_float) {
        return MachineOpcode::FSD;
    }

    if (!type || !type->has_layout()) {
        return MachineOpcode::SD;
    }

    constexpr auto size_to_opcode = [](size_t size) constexpr {
        switch (size) {
        case 1:
            return MachineOpcode::SB;
        case 2:
            return MachineOpcode::SH;
        case 4:
            return MachineOpcode::SW;
        default:
            return MachineOpcode::SD;
        }
    };

    return size_to_opcode(type->layout.size);
}

VirtReg
InstructionSelector::get_or_create_vreg_for_temp(const std::string &temp_name)
{
    auto [it, inserted] = temp_to_vreg_.try_emplace(temp_name, INVALID_VREG);
    if (inserted) {
        it->second = mfn_.get_next_vreg();
    }
    return it->second;
}

int32_t InstructionSelector::allocate_local_variable(SymbolPtr sym)
{
    const auto unique_key =
        std::to_string(sym->get_scope_id()) + "_" + sym->get_name();

    if (auto it = local_var_offsets_.find(unique_key);
        it != local_var_offsets_.end()) {
        return it->second;
    }

    const auto size = get_type_size(sym->get_type().type);
    const auto alignment = std::min(size, 4u);
    const auto offset = mfn_.get_frame().allocate_slot(size, alignment);

    local_var_offsets_[unique_key] = offset;
    sym->set_stack_offset(offset);

    return offset;
}

int32_t InstructionSelector::get_local_variable_offset(SymbolPtr sym)
{
    const auto unique_key =
        std::to_string(sym->get_scope_id()) + "_" + sym->get_name();

    if (auto it = local_var_offsets_.find(unique_key);
        it != local_var_offsets_.end()) {
        return it->second;
    }

    return allocate_local_variable(sym);
}

VirtReg InstructionSelector::add_offset_to_reg(VirtReg base, int64_t offset)
{
    if (offset == 0) {
        return base;
    }

    VirtReg result = mfn_.get_next_vreg();

    if (is_valid_imm12(offset)) {
        // Offset fits in immediate field, use ADDI directly
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                 .add_def(result)
                                 .add_use(base)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(VRegOperand(base))
                                 .add_operand(ImmOperand(offset)));
    } else {
        // Offset doesn't fit, need to load it into a register first
        VirtReg offset_reg = mfn_.get_next_vreg();
        mfn_.add_instruction(make_li(offset_reg, offset));
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADD)
                                 .add_def(result)
                                 .add_use(base)
                                 .add_use(offset_reg)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(VRegOperand(base))
                                 .add_operand(VRegOperand(offset_reg)));
    }

    return result;
}

VirtReg InstructionSelector::add_offset_to_phys_reg(PhysReg base,
                                                    int64_t offset)
{
    VirtReg result = mfn_.get_next_vreg();

    if (offset == 0) {
        // Just move from physical register to virtual register
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                 .add_def(result)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(RegOperand(base))
                                 .add_operand(ImmOperand(0)));
    } else if (is_valid_imm12(offset)) {
        // Offset fits in immediate field, use ADDI directly
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                 .add_def(result)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(RegOperand(base))
                                 .add_operand(ImmOperand(offset)));
    } else {
        // Offset doesn't fit, need to load it into a register first
        VirtReg offset_reg = mfn_.get_next_vreg();
        mfn_.add_instruction(make_li(offset_reg, offset));

        // Move base to a vreg first
        VirtReg base_vreg = mfn_.get_next_vreg();
        mfn_.add_instruction(MachineInstr(MachineOpcode::ADDI)
                                 .add_def(base_vreg)
                                 .add_operand(VRegOperand(base_vreg))
                                 .add_operand(RegOperand(base))
                                 .add_operand(ImmOperand(0)));

        mfn_.add_instruction(MachineInstr(MachineOpcode::ADD)
                                 .add_def(result)
                                 .add_use(base_vreg)
                                 .add_use(offset_reg)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(VRegOperand(base_vreg))
                                 .add_operand(VRegOperand(offset_reg)));
    }

    return result;
}

void InstructionSelector::emit_load_from_fp(VirtReg dst,
                                            int64_t offset,
                                            MachineOpcode load_op)
{
    if (is_valid_imm12(offset)) {
        // Offset fits in immediate field
        mfn_.add_instruction(
            MachineInstr(load_op)
                .add_def(dst)
                .add_operand(VRegOperand(dst))
                .add_operand(MemOperand(PhysReg::S0_FP, offset)));
    } else {
        // Offset doesn't fit, compute address first
        VirtReg addr = add_offset_to_phys_reg(PhysReg::S0_FP, offset);
        mfn_.add_instruction(MachineInstr(load_op)
                                 .add_def(dst)
                                 .add_use(addr)
                                 .add_operand(VRegOperand(dst))
                                 .add_operand(VRegOperand(addr))
                                 .add_operand(ImmOperand(0)));
    }
}

void InstructionSelector::emit_store_to_fp(VirtReg src,
                                           int64_t offset,
                                           MachineOpcode store_op)
{
    if (is_valid_imm12(offset)) {
        // Offset fits in immediate field
        mfn_.add_instruction(
            MachineInstr(store_op)
                .add_use(src)
                .add_operand(VRegOperand(src))
                .add_operand(MemOperand(PhysReg::S0_FP, offset)));
    } else {
        // Offset doesn't fit, compute address first
        VirtReg addr = add_offset_to_phys_reg(PhysReg::S0_FP, offset);
        mfn_.add_instruction(MachineInstr(store_op)
                                 .add_use(src)
                                 .add_use(addr)
                                 .add_operand(VRegOperand(src))
                                 .add_operand(VRegOperand(addr))
                                 .add_operand(ImmOperand(0)));
    }
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

std::string
RiscV32Backend::add_jump_table(const std::vector<std::string> &labels)
{
    std::string table_label =
        ".LJUMPTABLE" + std::to_string(next_jump_table_id_++);
    jump_tables_.emplace_back(table_label, labels);
    return table_label;
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
    emit_libc_aliases(os);
    emit_rodata(os);
    emit_globals(os);
    emit_init_array(os);
    emit_fini_array(os);
    emit_text(os);
}

void RiscV32Backend::emit_preamble(std::ostream &os) const
{
    os << "    .option nopic\n";
    os << "    .attribute arch, \"rv64imfd\"\n";
}

void RiscV32Backend::emit_libc_aliases(std::ostream &os) const
{
    // Emit .set directives to alias mangled function names to unmangled C names
    // This allows linking with the C runtime library (libc)
    //
    // Mangling scheme: _Z<name-length><name><params>
    // Types: v=void, i=int, j=unsigned, c=char, b=bool, f=float
    //        P<type>=pointer, z=variadic
    //

    // Signature reference:
    // int printf(char* fmt, ...);          -> _Z6printfPcz
    // int scanf(char* fmt, ...);           -> _Z5scanfPcz
    // void* malloc(unsigned size);         -> _Z6mallocj
    // void free(void* ptr);                -> _Z4freePv
    // void* calloc(unsigned nmemb, unsigned size); -> _Z6callocjj
    // void* realloc(void* ptr, unsigned size); -> _Z7reallocPvj
    // int open(char* path, int flags, ...) -> _Z4openPciz
    // int read(int fd, void* buf, int count) -> _Z4readiPvi
    // int write(int fd, void* buf, int count) -> _Z5writeiPvi
    // int close(int fd);                   -> _Z5closei
    // int strlen(char* str);               -> _Z6strlenPc
    // char* strcpy(char* dst, char* src);  -> _Z6strcpyPcPc
    // char* strcat(char* dst, char* src);  -> _Z6strcatPcPc
    // int strcmp(char* s1, char* s2);      -> _Z6strcmpPcPc
    // void* memcpy(void* dst, void* src, int n); -> _Z6memcpyPvPvi
    // void* memset(void* s, int c, int n); -> _Z6memsetPvii
    // int memcmp(void* s1, void* s2, int n); -> _Z6memcmpPvPvi
    // void exit(int status);               -> _Z4exiti
    // int putchar(int c);                  -> _Z7putchari
    // int getchar();                       -> _Z7getcharv

    // I/O functions
    os << "    .set _Z6printfPcz, printf\n";
    os << "    .set _Z5scanfPcz, scanf\n";
    os << "    .set _Z7putchari, putchar\n";
    os << "    .set _Z7getcharv, getchar\n";

    // Memory allocation
    os << "    .set _Z6mallocj, malloc\n";
    os << "    .set _Z4freePv, free\n";
    os << "    .set _Z6callocjj, calloc\n";
    os << "    .set _Z7reallocPvj, realloc\n";

    // String functions
    os << "    .set _Z6strlenPc, strlen\n";
    os << "    .set _Z6strcpyPcPc, strcpy\n";
    os << "    .set _Z6strcatPcPc, strcat\n";
    os << "    .set _Z6strcmpPcPc, strcmp\n";

    // Memory functions
    os << "    .set _Z6memcpyPvPvi, memcpy\n";
    os << "    .set _Z6memsetPvii, memset\n";
    os << "    .set _Z6memcmpPvPvi, memcmp\n";

    // File operations
    os << "    .set _Z4openPciz, open\n";
    os << "    .set _Z4readiPvi, read\n";
    os << "    .set _Z5writeiPvi, write\n";
    os << "    .set _Z5closei, close\n";

    // System functions
    os << "    .set _Z4exiti, exit\n";
}

void RiscV32Backend::emit_rodata(std::ostream &os) const
{
    bool has_data = !program_.string_literals.empty() ||
                    !float_constants_.empty() || !jump_tables_.empty();
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

    for (const auto &[table_label, case_labels] : jump_tables_) {
        os << "    .balign 8\n";
        os << table_label << ":\n";
        for (const auto &label : case_labels) {
            os << "    .dword " << label << "\n";
        }
    }
}

void RiscV32Backend::emit_globals(std::ostream &os) const
{
    if (program_.global_variables.empty()) {
        return;
    }

    auto is_zero_initializer =
        [](const Symbol::InitializerValue &init_value) -> bool {
        if (std::holds_alternative<int64_t>(init_value)) {
            return std::get<int64_t>(init_value) == 0;
        } else if (std::holds_alternative<double>(init_value)) {
            return std::get<double>(init_value) == 0.0;
        } else if (std::holds_alternative<std::string>(init_value)) {
            return false; // String initializers are never zero
        }
        return false; // std::monostate
    };

    // Separate globals into .data (initialized with non-zero) and .bss
    // (uninitialized or zero)
    std::vector<SymbolPtr> data_section_globals;
    std::vector<SymbolPtr> bss_section_globals;

    for (const auto &global : program_.global_variables) {
        if (global->has_initializer() &&
            !is_zero_initializer(global->get_initializer())) {
            // Non-zero initialized -> .data
            data_section_globals.push_back(global);
        } else {
            // Uninitialized or zero-initialized -> .bss
            bss_section_globals.push_back(global);
        }
    }

    // Emit .data section for initialized globals
    if (!data_section_globals.empty()) {
        os << "\n    .section .data\n";
        for (const auto &global : data_section_globals) {
            auto type = global->get_type().type;
            size_t size = type->has_layout() ? type->layout.size : 4;
            size_t align = type->has_layout() ? type->layout.alignment : 4;

            os << "    .balign " << align << "\n";
            os << "    .globl " << global->get_name() << "\n";
            os << global->get_name() << ":\n";

            // Emit the initializer value
            const auto &init_value = global->get_initializer();
            if (std::holds_alternative<int64_t>(init_value)) {
                int64_t value = std::get<int64_t>(init_value);
                if (size == 1) {
                    os << "    .byte " << (value & 0xFF) << "\n";
                } else if (size == 2) {
                    os << "    .half " << (value & 0xFFFF) << "\n";
                } else if (size == 4) {
                    os << "    .word " << value << "\n";
                } else if (size == 8) {
                    os << "    .dword " << value << "\n";
                }
            } else if (std::holds_alternative<double>(init_value)) {
                double value = std::get<double>(init_value);

                uint64_t bits;
                std::memcpy(&bits, &value, sizeof(double));

                os << "    .dword 0x" << std::hex << bits << std::dec << "\n";
            } else if (std::holds_alternative<std::string>(init_value)) {
                std::string str_value = std::get<std::string>(init_value);
                std::string str_label;
                bool found = false;

                for (const auto &str_lit : program_.string_literals) {
                    if (str_lit.value == str_value) {
                        str_label = str_lit.label;
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    str_label = ".str_init_" +
                                std::to_string(data_section_globals.size());
                }
                // Emit as 64-bit pointer on RISC-V 64
                os << "    .dword " << str_label << "\n";
            }
        }
    }

    // Emit .bss section for uninitialized/zero-initialized globals
    if (!bss_section_globals.empty()) {
        os << "\n    .section .bss\n";
        for (const auto &global : bss_section_globals) {
            auto type = global->get_type().type;
            size_t size = type->has_layout() ? type->layout.size : 4;
            size_t align = type->has_layout() ? type->layout.alignment : 4;

            os << "    .balign " << align << "\n";
            os << "    .globl " << global->get_name() << "\n";
            os << global->get_name() << ":\n";
            os << "    .zero " << size << "\n";
        }
    }
}

void RiscV32Backend::emit_init_array(std::ostream &os) const
{
    bool has_init_function = false;
    for (const auto &func : program_.functions) {
        if (func->mangled_name == "__ciel_global_init") {
            has_init_function = true;
            break;
        }
    }

    if (!has_init_function) {
        return;
    }

    os << "\n    .section .init_array,\"aw\",@init_array\n";
    os << "    .balign 8\n";
    os << "    .dword __ciel_global_init\n";
}

void RiscV32Backend::emit_fini_array(std::ostream &os) const
{
    bool has_fini_function = false;
    for (const auto &func : program_.functions) {
        if (func->mangled_name == "__ciel_global_fini") {
            has_fini_function = true;
            break;
        }
    }

    if (!has_fini_function) {
        return;
    }

    os << "\n    .section .fini_array,\"aw\",@fini_array\n";
    os << "    .balign 8\n";
    os << "    .dword __ciel_global_fini\n";
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
        if (label_name == "_Z4mainv" || label_name == "_Z4mainiPPc") {
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

    auto emit_sd_with_offset = [&](std::string_view reg, int32_t offset) {
        if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
            os << "    sd " << reg << ", " << offset << "(sp)\n";
        } else {
            // Offset doesn't fit in immediate, use temp register
            os << "    li t0, " << offset << "\n";
            os << "    add t0, sp, t0\n";
            os << "    sd " << reg << ", 0(t0)\n";
        }
    };

    auto emit_addi_with_offset = [&](std::string_view dst,
                                     std::string_view src,
                                     int32_t offset) {
        if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
            os << "    addi " << dst << ", " << src << ", " << offset << "\n";
        } else {
            // Offset doesn't fit in immediate, use temp register
            os << "    li t0, " << offset << "\n";
            os << "    add " << dst << ", " << src << ", t0\n";
        }
    };

    // Allocate stack frame
    emit_addi_with_offset("sp", "sp", -frame_size);

    // Save return address (8 bytes in RV64)
    emit_sd_with_offset("ra", frame_size + frame.get_ra_offset());

    // Save frame pointer (8 bytes in RV64)
    emit_sd_with_offset("s0", frame_size + frame.get_fp_offset());

    // Set up frame pointer
    emit_addi_with_offset("s0", "sp", frame_size);

    // Save callee-saved registers (8 bytes each in RV64)
    for (auto reg : frame.get_used_callee_regs()) {
        int32_t offset = frame.get_callee_save_offset(reg);
        if (reg >= PhysReg::FT0 && reg <= PhysReg::FA7) {
            if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
                os << "    fsd " << get_reg_name(reg) << ", "
                   << (frame_size + offset) << "(sp)\n";
            } else {
                os << "    li t0, " << (frame_size + offset) << "\n";
                os << "    add t0, sp, t0\n";
                os << "    fsd " << get_reg_name(reg) << ", 0(t0)\n";
            }
        } else {
            emit_sd_with_offset(get_reg_name(reg), frame_size + offset);
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

    auto emit_ld_with_offset = [&](std::string_view reg, int32_t offset) {
        if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
            os << "    ld " << reg << ", " << offset << "(sp)\n";
        } else {
            // Offset doesn't fit in immediate, use temp register
            os << "    li t0, " << offset << "\n";
            os << "    add t0, sp, t0\n";
            os << "    ld " << reg << ", 0(t0)\n";
        }
    };

    auto emit_addi_with_offset = [&](std::string_view dst,
                                     std::string_view src,
                                     int32_t offset) {
        if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
            os << "    addi " << dst << ", " << src << ", " << offset << "\n";
        } else {
            // Offset doesn't fit in immediate, use temp register
            os << "    li t0, " << offset << "\n";
            os << "    add " << dst << ", " << src << ", t0\n";
        }
    };

    // Restore callee-saved registers
    for (auto reg : frame.get_used_callee_regs()) {
        int32_t offset = frame.get_callee_save_offset(reg);
        if (reg >= PhysReg::FT0 && reg <= PhysReg::FA7) {
            if (offset >= MIN_IMM12 && offset <= MAX_IMM12) {
                os << "    fld " << get_reg_name(reg) << ", "
                   << (frame_size + offset) << "(sp)\n";
            } else {
                os << "    li t0, " << (frame_size + offset) << "\n";
                os << "    add t0, sp, t0\n";
                os << "    fld " << get_reg_name(reg) << ", 0(t0)\n";
            }
        } else {
            emit_ld_with_offset(get_reg_name(reg), frame_size + offset);
        }
    }

    // Restore frame pointer (8 bytes in RV64)
    emit_ld_with_offset("s0", frame_size + frame.get_fp_offset());

    // Restore return address (8 bytes in RV64)
    emit_ld_with_offset("ra", frame_size + frame.get_ra_offset());

    // Deallocate stack frame
    emit_addi_with_offset("sp", "sp", frame_size);

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
