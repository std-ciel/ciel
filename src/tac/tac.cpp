#include "tac/tac.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type.hpp"
#include <iostream>
#include <sstream>

TACOperand TACOperand::symbol(SymbolPtr sym, TypePtr t)
{
    TACOperand op;
    op.kind = Kind::SYMBOL;
    op.value = sym;
    op.type = t ? t : (sym ? sym->get_type().type : nullptr);
    return op;
}

TACOperand TACOperand::temporary(const std::string &name, TypePtr t)
{
    TACOperand op;
    op.kind = Kind::TEMPORARY;
    op.value = name;
    op.type = t;
    return op;
}

TACOperand TACOperand::constant_int(int64_t val, TypePtr t)
{
    TACOperand op;
    op.kind = Kind::CONSTANT;
    op.value = val;
    op.type = t;
    return op;
}

TACOperand TACOperand::constant_float(double val, TypePtr t)
{
    TACOperand op;
    op.kind = Kind::CONSTANT;
    op.value = val;
    op.type = t;
    return op;
}

TACOperand TACOperand::label(const std::string &name)
{
    TACOperand op;
    op.kind = Kind::LABEL;
    op.value = name;
    return op;
}

TACOperand TACOperand::member_access(const TACOperand &base,
                                     const std::string &member,
                                     size_t offset,
                                     TypePtr member_type)
{
    TACOperand op;
    op.kind = Kind::TEMPORARY; // Member access results in a temporary
    op.value = base.to_string() + "." + member;
    op.type = member_type;
    op.member_name = member;
    op.member_offset = offset;
    return op;
}

std::string TACOperand::to_string() const
{
    switch (kind) {
    case Kind::SYMBOL:
        if (auto sym = std::get_if<SymbolPtr>(&value)) {
            return (*sym)->get_name();
        }
        return "<invalid_symbol>";
    case Kind::TEMPORARY:
        if (!member_name.empty()) {
            // Handle member access like "obj.x"
            std::string base = std::get<std::string>(value);
            // Extract base name before the dot if it exists
            size_t dot_pos = base.find('.');
            if (dot_pos != std::string::npos) {
                base = base.substr(0, dot_pos);
            }
            return base + "." + member_name;
        }
        if (auto name = std::get_if<std::string>(&value)) {
            return *name;
        }
        return "<invalid_temp>";
    case Kind::CONSTANT:
        if (auto i = std::get_if<int64_t>(&value)) {
            return std::to_string(*i);
        }
        if (auto f = std::get_if<double>(&value)) {
            return std::to_string(*f);
        }
        return "<invalid_const>";
    case Kind::LABEL:
        if (auto name = std::get_if<std::string>(&value)) {
            return *name;
        }
        return "<invalid_label>";
    case Kind::NONE:
        return "<none>";
    }
    return "<unknown>";
}

std::string opcode_to_string(TACOpcode op)
{
    switch (op) {
    case TACOpcode::ADD:
        return "ADD";
    case TACOpcode::SUB:
        return "SUB";
    case TACOpcode::MUL:
        return "MUL";
    case TACOpcode::DIV:
        return "DIV";
    case TACOpcode::MOD:
        return "MOD";
    case TACOpcode::NEG:
        return "NEG";
    case TACOpcode::AND:
        return "AND";
    case TACOpcode::OR:
        return "OR";
    case TACOpcode::XOR:
        return "XOR";
    case TACOpcode::NOT:
        return "NOT";
    case TACOpcode::SHL:
        return "SHL";
    case TACOpcode::SHR:
        return "SHR";
    case TACOpcode::LAND:
        return "LAND";
    case TACOpcode::LOR:
        return "LOR";
    case TACOpcode::LNOT:
        return "LNOT";
    case TACOpcode::EQ:
        return "EQ";
    case TACOpcode::NE:
        return "NE";
    case TACOpcode::LT:
        return "LT";
    case TACOpcode::LE:
        return "LE";
    case TACOpcode::GT:
        return "GT";
    case TACOpcode::GE:
        return "GE";
    case TACOpcode::ASSIGN:
        return "ASSIGN";
    case TACOpcode::COPY:
        return "COPY";
    case TACOpcode::LOAD:
        return "LOAD";
    case TACOpcode::STORE:
        return "STORE";
    case TACOpcode::ADDR_OF:
        return "ADDR_OF";
    case TACOpcode::DEREF:
        return "DEREF";
    case TACOpcode::LABEL:
        return "LABEL";
    case TACOpcode::GOTO:
        return "GOTO";
    case TACOpcode::IF_FALSE:
        return "IF_FALSE";
    case TACOpcode::IF_TRUE:
        return "IF_TRUE";
    case TACOpcode::PARAM:
        return "PARAM";
    case TACOpcode::CALL:
        return "CALL";
    case TACOpcode::RETURN:
        return "RETURN";
    case TACOpcode::ENTER:
        return "ENTER";
    case TACOpcode::LEAVE:
        return "LEAVE";
    case TACOpcode::INDEX_ADDR:
        return "INDEX_ADDR";
    case TACOpcode::MEMBER_ADDR:
        return "MEMBER_ADDR";
    case TACOpcode::CAST:
        return "CAST";
    case TACOpcode::NOP:
        return "NOP";
    case TACOpcode::PHI:
        return "PHI";
    case TACOpcode::MEMBER_ACCESS:
        return "MEMBER_ACCESS";
    case TACOpcode::LOAD_MEMBER:
        return "LOAD_MEMBER";
    case TACOpcode::STORE_MEMBER:
        return "STORE_MEMBER";
    case TACOpcode::MEMBER_OFFSET:
        return "MEMBER_OFFSET";
    default:
        return "UNKNOWN";
    }
}

std::string TACInstruction::to_string() const
{
    std::ostringstream oss;

    // Special cases
    if (opcode == TACOpcode::LABEL) {
        oss << operand1.to_string() << ":";
        return oss.str();
    }

    if (opcode == TACOpcode::GOTO) {
        oss << "    GOTO " << operand1.to_string();
        return oss.str();
    }

    if (opcode == TACOpcode::IF_FALSE || opcode == TACOpcode::IF_TRUE) {
        oss << "    " << opcode_to_string(opcode) << " " << operand1.to_string()
            << " GOTO " << operand2.to_string();
        return oss.str();
    }

    if (opcode == TACOpcode::PARAM) {
        oss << "    PARAM " << operand1.to_string();
        return oss.str();
    }

    if (opcode == TACOpcode::CALL) {
        oss << "    ";
        if (result.is_valid()) {
            oss << result.to_string() << " = ";
        }
        oss << "CALL " << operand1.to_string();
        if (operand2.is_valid()) {
            oss << " " << operand2.to_string();
        }
        return oss.str();
    }

    if (opcode == TACOpcode::RETURN) {
        oss << "    RETURN";
        if (operand1.is_valid()) {
            oss << " " << operand1.to_string();
        }
        return oss.str();
    }

    // Handle member access operations specially
    if (opcode == TACOpcode::LOAD_MEMBER) {
        oss << "    " << result.to_string() << " = LOAD_MEMBER "
            << operand1.to_string() << ", " << operand1.member_offset;
        if (!comment.empty()) {
            oss << "  // " << comment;
        }
        return oss.str();
    }

    if (opcode == TACOpcode::STORE_MEMBER) {
        oss << "    STORE_MEMBER " << result.to_string() << ", "
            << result.member_offset << ", " << operand1.to_string();
        if (!comment.empty()) {
            oss << "  // " << comment;
        }
        return oss.str();
    }

    if (opcode == TACOpcode::MEMBER_OFFSET) {
        oss << "    " << result.to_string() << " = &" << operand1.to_string()
            << " + " << operand2.to_string();
        if (!comment.empty()) {
            oss << "  // " << comment;
        }
        return oss.str();
    }

    // Standard three-address format
    oss << "    ";
    if (result.is_valid()) {
        oss << result.to_string() << " = ";
    }

    oss << opcode_to_string(opcode);

    if (operand1.is_valid()) {
        oss << " " << operand1.to_string();
    }

    if (operand2.is_valid()) {
        oss << ", " << operand2.to_string();
    }

    if (!comment.empty()) {
        oss << "  // " << comment;
    }

    return oss.str();
}

// Format: __<mangled_name>_<scope_id>_
std::string TACFunction::get_function_prefix() const
{
    return "__" + mangled_name + "_" + std::to_string(body_scope_id) + "_";
}

// Format: __<mangled_name>_<scope_id>_t<counter>__
std::string TACFunction::mangle_temporary_name(int counter) const
{
    return get_function_prefix() + "t" + std::to_string(counter) + "__";
}

// Format: __<mangled_name>_<scope_id>_<prefix><counter>__
std::string TACFunction::mangle_label_name(const std::string &prefix,
                                           int counter) const
{
    return get_function_prefix() + prefix + std::to_string(counter) + "__";
}

// Format: __<mangled_name>_<scope_id>_entry__
std::string TACFunction::get_entry_label() const
{
    return get_function_prefix() + "entry__";
}

// Format: __<mangled_name>_<scope_id>_exit__
std::string TACFunction::get_exit_label() const
{
    return get_function_prefix() + "exit__";
}

std::string TACFunction::new_temp(TypePtr type)
{
    std::string temp_name = mangle_temporary_name(temp_counter++);

    // Add the temporary to the symbol table if we have access to it
    if (body_scope_id > 0 && type) {
        // Get the parent scope from the symbol table
        auto scope_iter = sym_table.get_scope_chain();
        ScopeID parent_scope = body_scope_id; // Default to same scope

        // Find parent of body_scope_id
        for (size_t i = 0; i < scope_iter.size(); ++i) {
            if (scope_iter[i] == body_scope_id && i > 0) {
                parent_scope = scope_iter[i - 1];
                break;
            }
        }

        // Create a qualified type (use NONE qualifier for temporaries)
        QualifiedType qt(type, Qualifier::NONE);

        // Create the symbol
        auto temp_symbol = std::make_shared<Symbol>(temp_name,
                                                    qt,
                                                    StorageClass::AUTO,
                                                    body_scope_id,
                                                    parent_scope);

        // Add to the function's body scope
        sym_table.add_symbol_in_scope(temp_name, temp_symbol, body_scope_id);
    }

    return temp_name;
}

std::string TACFunction::new_label(const std::string &prefix)
{
    return mangle_label_name(prefix, label_counter++);
}

void TACFunction::add_block(TACBasicBlockPtr block)
{
    basic_blocks.push_back(block);
}

void TACFunction::print() const
{
    std::cout << "\nFunction: " << name << " (mangled: " << mangled_name
              << ")\n";
    std::cout << "Return type: "
              << (return_type ? return_type->debug_name() : "void") << "\n";
    std::cout << "Parameters: ";
    for (const auto &param : parameters) {
        std::cout << param.to_string() << " ";
    }
    std::cout << "\n\n";

    for (const auto &block : basic_blocks) {
        if (!block->label.empty()) {
            std::cout << block->label << ":\n";
        }
        for (const auto &instr : block->instructions) {
            std::cout << instr->to_string() << "\n";
        }
        std::cout << "\n";
    }
}

void TACProgram::print() const
{
    std::cout << "========== THREE ADDRESS CODE ==========\n";

    if (!global_variables.empty()) {
        std::cout << "\nGlobal Variables:\n";
        for (const auto &var : global_variables) {
            std::cout << "  " << var->get_name() << " : "
                      << var->get_type().debug_name() << "\n";
        }
    }

    for (const auto &func : functions) {
        func->print();
    }

    std::cout << "========================================\n";
}
