#include "codegen/riscv32_backend.hpp"
#include "codegen/riscv32_regalloc.hpp"
#include <ostream>

namespace ciel {
namespace codegen {
namespace riscv32 {

// ============================================================================
// MachineFunction Implementation
// ============================================================================

MachineFunction::MachineFunction(const TACFunction &tac_fn, TypeFactory &types)
    : name_(tac_fn.mangled_name), frame_(tac_fn.mangled_name), next_vreg_(1),
      types_(types)
{
}

// ============================================================================
// InstructionSelector Implementation
// ============================================================================

InstructionSelector::InstructionSelector(MachineFunction &mfn,
                                         TypeFactory &types)
    : mfn_(mfn), types_(types)
{
}

void InstructionSelector::select(const TACFunction &tac_fn)
{
    // Process each basic block
    for (const auto &bb : tac_fn.basic_blocks) {
        // Emit basic block label if present
        if (!bb->label.empty()) {
            mfn_.add_instruction(make_label(bb->label));
        }

        // Select instructions for each TAC instruction
        for (const auto &tac_instr : bb->instructions) {
            select_instruction(*tac_instr);
        }
    }
}

void InstructionSelector::select_instruction(const TACInstruction &instr)
{
    // For minimal baseline: map common TAC ops to RV32 instructions
    // This is a simplified implementation you can expand

    switch (instr.opcode) {
    case TACOpcode::ENTER:
        // Handled by prologue
        break;

    case TACOpcode::RETURN:
        select_return(instr);
        break;

    case TACOpcode::ASSIGN:
        select_assign(instr);
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
        // Already handled above
        break;

    case TACOpcode::JUMP_TABLE:
        select_jump_table(instr);
        break;

    default:
        // For unimplemented opcodes, emit a comment
        break;
    }
}

void InstructionSelector::select_return(const TACInstruction &instr)
{
    if (instr.operand1.is_valid()) {
        // Load return value into a0
        VirtReg val = load_operand(instr.operand1);
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_use(val)
                                 .add_operand(RegOperand(PhysReg::A0))
                                 .add_operand(VRegOperand(val)));
    }
    mfn_.add_instruction(make_ret());
}

void InstructionSelector::select_assign(const TACInstruction &instr)
{
    VirtReg src = load_operand(instr.operand1);
    store_result(src, instr.result);
}

void InstructionSelector::select_binary_op(const TACInstruction &instr)
{
    VirtReg op1 = load_operand(instr.operand1);
    VirtReg op2 = load_operand(instr.operand2);
    VirtReg dst = mfn_.get_next_vreg();

    MachineOpcode opc;
    bool is_signed = is_signed_type(instr.result.type);

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
        return; // Unsupported
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

    bool is_signed = is_signed_type(instr.operand1.type);

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
    VirtReg cond = load_operand(instr.result);

    if (instr.operand1.kind == TACOperand::Kind::LABEL) {
        std::string target = std::get<std::string>(instr.operand1.value);

        // IF_TRUE: branch if non-zero
        // IF_FALSE: branch if zero
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
    VirtReg param = load_operand(instr.result);
    pending_params_.push_back(param);
}

void InstructionSelector::select_call(const TACInstruction &instr)
{
    // Marshal parameters into a0-a7
    for (size_t i = 0; i < pending_params_.size() && i < 8; ++i) {
        PhysReg arg_reg =
            static_cast<PhysReg>(static_cast<uint8_t>(PhysReg::A0) + i);
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_use(pending_params_[i])
                                 .add_operand(RegOperand(arg_reg))
                                 .add_operand(VRegOperand(pending_params_[i])));
    }

    // Update max call args for frame layout
    if (pending_params_.size() > 8) {
        mfn_.get_frame().update_max_call_args(
            static_cast<uint32_t>(pending_params_.size()));
    }

    // Emit call
    if (instr.operand1.kind == TACOperand::Kind::SYMBOL) {
        SymbolPtr func_sym = std::get<SymbolPtr>(instr.operand1.value);
        mfn_.add_instruction(make_call(func_sym->get_name()));
    } else if (instr.operand1.kind == TACOperand::Kind::LABEL) {
        std::string func_name = std::get<std::string>(instr.operand1.value);
        mfn_.add_instruction(make_call(func_name));
    }

    // Store return value if needed
    if (instr.result.is_valid()) {
        VirtReg result = mfn_.get_next_vreg();
        mfn_.add_instruction(MachineInstr(MachineOpcode::MV)
                                 .add_def(result)
                                 .add_operand(VRegOperand(result))
                                 .add_operand(RegOperand(PhysReg::A0)));
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

VirtReg InstructionSelector::load_operand(const TACOperand &operand)
{
    switch (operand.kind) {
    case TACOperand::Kind::CONSTANT: {
        VirtReg vreg = mfn_.get_next_vreg();
        int64_t val = std::get<int64_t>(operand.value);
        mfn_.add_instruction(make_li(vreg, static_cast<int32_t>(val)));
        return vreg;
    }
    case TACOperand::Kind::LABEL: {
        VirtReg vreg = mfn_.get_next_vreg();
        std::string label = std::get<std::string>(operand.value);
        mfn_.add_instruction(make_la(vreg, label));
        return vreg;
    }
    case TACOperand::Kind::SYMBOL: {
        VirtReg vreg = mfn_.get_next_vreg();
        SymbolPtr sym = std::get<SymbolPtr>(operand.value);
        mfn_.add_instruction(make_la(vreg, sym->get_name()));
        return vreg;
    }
    case TACOperand::Kind::TEMPORARY: {
        std::string temp_name = std::get<std::string>(operand.value);
        return get_or_create_vreg_for_temp(temp_name);
    }
    default:
        return INVALID_VREG;
    }
}

void InstructionSelector::store_result(VirtReg vreg, const TACOperand &dest)
{
    if (dest.kind == TACOperand::Kind::TEMPORARY) {
        std::string temp_name = std::get<std::string>(dest.value);
        VirtReg dest_vreg = get_or_create_vreg_for_temp(temp_name);
        if (dest_vreg != vreg) {
            mfn_.add_instruction(make_mv(dest_vreg, vreg));
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

VirtReg
InstructionSelector::get_or_create_vreg_for_temp(const std::string &temp_name)
{
    auto it = temp_to_vreg_.find(temp_name);
    if (it != temp_to_vreg_.end()) {
        return it->second;
    }

    VirtReg vreg = mfn_.get_next_vreg();
    temp_to_vreg_[temp_name] = vreg;

    // Allocate stack slot
    mfn_.get_frame().allocate_temp(temp_name, 4);

    return vreg;
}

// ============================================================================
// RiscV32Backend Implementation
// ============================================================================

RiscV32Backend::RiscV32Backend(const TACProgram &program,
                               SymbolTable &symtab,
                               TypeFactory &types)
    : program_(program), symtab_(symtab), types_(types)
{
    lower_functions();
}

void RiscV32Backend::lower_functions()
{
    for (const auto &tac_fn : program_.functions) {
        auto mfn = std::make_unique<MachineFunction>(*tac_fn, types_);

        // Instruction selection
        InstructionSelector selector(*mfn, types_);
        selector.select(*tac_fn);

        // Register allocation (use simple stack-only for baseline)
        StackOnlyAllocator allocator(mfn->get_instructions(), mfn->get_frame());
        allocator.run();

        // Finalize frame layout
        mfn->get_frame().finalize();

        machine_functions_.push_back(std::move(mfn));
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
    os << "    .attribute arch, \"rv32im\"\n";
}

void RiscV32Backend::emit_rodata(std::ostream &os) const
{
    if (program_.string_literals.empty()) {
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
    os << "\n    .globl " << mfn.get_name() << "\n";
    os << "    .type " << mfn.get_name() << ", @function\n";
    os << mfn.get_name() << ":\n";

    emit_prologue(os, mfn);

    // Emit instructions
    for (const auto &instr : mfn.get_instructions()) {
        emit_instruction(os, instr);
    }

    // Note: epilogue is emitted as part of RET instruction handling
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

    // Save return address
    os << "    sw ra, " << (frame_size + frame.get_ra_offset()) << "(sp)\n";

    // Save frame pointer
    os << "    sw s0, " << (frame_size + frame.get_fp_offset()) << "(sp)\n";

    // Set up frame pointer
    os << "    addi s0, sp, " << frame_size << "\n";

    // Save callee-saved registers
    for (auto reg : frame.get_used_callee_regs()) {
        int32_t offset = frame.get_callee_save_offset(reg);
        os << "    sw " << get_reg_name(reg) << ", " << (frame_size + offset)
           << "(sp)\n";
    }
}

void RiscV32Backend::emit_epilogue(std::ostream &os,
                                   const MachineFunction &mfn) const
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
        os << "    lw " << get_reg_name(reg) << ", " << (frame_size + offset)
           << "(sp)\n";
    }

    // Restore frame pointer
    os << "    lw s0, " << (frame_size + frame.get_fp_offset()) << "(sp)\n";

    // Restore return address
    os << "    lw ra, " << (frame_size + frame.get_ra_offset()) << "(sp)\n";

    // Deallocate stack frame
    os << "    addi sp, sp, " << frame_size << "\n";

    // Return
    os << "    ret\n";
}

void RiscV32Backend::emit_instruction(std::ostream &os,
                                      const MachineInstr &instr) const
{
    // Special handling for RET - emit epilogue
    if (instr.get_opcode() == MachineOpcode::RET) {
        // Find the machine function this belongs to
        for (const auto &mfn : machine_functions_) {
            // Check if this instruction is in this function (simplified check)
            emit_epilogue(os, *mfn);
            return;
        }
    }

    // For all other instructions, use the instruction's emit method
    instr.emit(os);
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
