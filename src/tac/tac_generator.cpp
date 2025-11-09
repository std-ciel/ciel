#include "tac/tac_generator.hpp"
#include "parser/parser_helper.hpp"
#include "symbol_table/type.hpp"
#include <algorithm>

TACGenerator::TACGenerator()
    : symbol_table(get_symbol_table()), type_factory(get_type_factory())
{
}

void TACGenerator::record_error(const std::string &message)
{
    errors.push_back(
        TACErrorInfo(TACError::UNHANDLED_EXPRESSION_TYPE, message));
}

void TACGenerator::emit(TACInstructionPtr instr)
{
    if (current_block) {
        current_block->add_instruction(instr);

        // Increment instruction counter after adding to block
        if (current_function) {
            current_function->current_instruction_number++;
        }
    }
}

void TACGenerator::emit_label(const std::string &label)
{
    // The label will be at the next instruction number
    size_t target_line = 0;
    if (current_function) {
        target_line = current_function->current_instruction_number + 1;

        // Record this label's line number for backward jumps
        current_function->emitted_labels[label] = target_line;

        // Backpatch all pending forward jumps to this label
        auto it = current_function->pending_jumps.find(label);
        if (it != current_function->pending_jumps.end()) {
            for (auto &jump_instr : it->second) {
                jump_instr->target_line_number = target_line;
            }
            // Clear the pending jumps for this label
            current_function->pending_jumps.erase(it);
        }
    }

    auto label_instr =
        std::make_shared<TACInstruction>(TACOpcode::LABEL,
                                         TACOperand(),
                                         TACOperand::label(label));
    emit(label_instr);
}

void TACGenerator::emit_goto(const std::string &label)
{
    auto goto_instr =
        std::make_shared<TACInstruction>(TACOpcode::GOTO,
                                         TACOperand(),
                                         TACOperand::label(label));
    goto_instr->is_backpatch_target = true;

    if (current_function) {
        // Check if this is a backward jump (label already emitted)
        auto it = current_function->emitted_labels.find(label);
        if (it != current_function->emitted_labels.end()) {
            // Backward jump - resolve immediately
            goto_instr->target_line_number = it->second;
        } else {
            // Forward jump - add to pending jumps
            current_function->pending_jumps[label].push_back(goto_instr);
        }
    }

    emit(goto_instr);
}

void TACGenerator::emit_conditional_jump(TACOpcode opcode,
                                         const TACOperand &condition,
                                         const std::string &label)
{
    auto jump_instr =
        std::make_shared<TACInstruction>(opcode,
                                         TACOperand(),
                                         condition,
                                         TACOperand::label(label));
    jump_instr->is_backpatch_target = true;

    if (current_function) {
        // Check if this is a backward jump (label already emitted)
        auto it = current_function->emitted_labels.find(label);
        if (it != current_function->emitted_labels.end()) {
            // Backward jump - resolve immediately
            jump_instr->target_line_number = it->second;
        } else {
            // Forward jump - add to pending jumps
            current_function->pending_jumps[label].push_back(jump_instr);
        }
    }

    emit(jump_instr);
}

std::string TACGenerator::new_temp(TypePtr type)
{
    if (current_function) {
        return current_function->new_temp(type);
    }
    errors.push_back(TACErrorInfo(TACError::NO_CURRENT_FUNCTION, "new_temp"));
    return ""; // Return empty string on error
}

std::string TACGenerator::new_label(const std::string &prefix)
{
    if (current_function) {
        return current_function->new_label(prefix);
    }
    errors.push_back(TACErrorInfo(TACError::NO_CURRENT_FUNCTION, "new_label"));
    return ""; // Return empty string on error
}

// Helper function to compute the size of a type, handling arrays recursively
static size_t compute_type_size(TypePtr type)
{
    if (!type)
        return 0;

    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return 0;

    // If layout is already computed, use it
    if (canonical->has_layout()) {
        return canonical->layout.size;
    }

    // For array types, compute size recursively
    if (canonical->kind == TypeKind::ARRAY) {
        auto array_type = std::static_pointer_cast<ArrayType>(canonical);
        size_t element_size = compute_type_size(array_type->element_type.type);
        return element_size * array_type->size;
    }

    // For other types, try to get from layout (might be 0)
    return canonical->layout.size;
}

TACOpcode TACGenerator::operator_to_tac_opcode(Operator op)
{
    switch (op) {
    // Arithmetic operators
    case Operator::ADD:
        return TACOpcode::ADD;
    case Operator::SUBTRACT:
        return TACOpcode::SUB;
    case Operator::MULTIPLY:
        return TACOpcode::MUL;
    case Operator::DIVIDE:
        return TACOpcode::DIV;
    case Operator::MODULO:
        return TACOpcode::MOD;

    // Bitwise operators
    case Operator::BITWISE_AND:
        return TACOpcode::AND;
    case Operator::BITWISE_OR:
        return TACOpcode::OR;
    case Operator::BITWISE_XOR:
        return TACOpcode::XOR;
    case Operator::BITWISE_NOT:
        return TACOpcode::NOT;
    case Operator::LEFT_SHIFT:
        return TACOpcode::SHL;
    case Operator::RIGHT_SHIFT:
        return TACOpcode::SHR;

    // Logical operators
    case Operator::LOGICAL_AND:
        return TACOpcode::LAND;
    case Operator::LOGICAL_OR:
        return TACOpcode::LOR;
    case Operator::LOGICAL_NOT:
        return TACOpcode::LNOT;

    // Relational operators
    case Operator::EQUAL:
        return TACOpcode::EQ;
    case Operator::NOT_EQUAL:
        return TACOpcode::NE;
    case Operator::LESS_THAN:
        return TACOpcode::LT;
    case Operator::LESS_EQUAL:
        return TACOpcode::LE;
    case Operator::GREATER_THAN:
        return TACOpcode::GT;
    case Operator::GREATER_EQUAL:
        return TACOpcode::GE;

    // Unary operators
    case Operator::UNARY_MINUS:
        return TACOpcode::NEG;
    case Operator::ADDRESS_OF:
        return TACOpcode::ADDR_OF;
    case Operator::POINTER_DEREF:
        return TACOpcode::DEREF;

    // Compound assignment operators - map to their base operations
    case Operator::ADD_ASSIGN:
        return TACOpcode::ADD;
    case Operator::SUBTRACT_ASSIGN:
        return TACOpcode::SUB;
    case Operator::MULTIPLY_ASSIGN:
        return TACOpcode::MUL;
    case Operator::DIVIDE_ASSIGN:
        return TACOpcode::DIV;
    case Operator::MODULO_ASSIGN:
        return TACOpcode::MOD;
    case Operator::BITWISE_AND_ASSIGN:
        return TACOpcode::AND;
    case Operator::BITWISE_OR_ASSIGN:
        return TACOpcode::OR;
    case Operator::BITWISE_XOR_ASSIGN:
        return TACOpcode::XOR;
    case Operator::LEFT_SHIFT_ASSIGN:
        return TACOpcode::SHL;
    case Operator::RIGHT_SHIFT_ASSIGN:
        return TACOpcode::SHR;

    default:
        return TACOpcode::NOP;
    }
}

TACOperand TACGenerator::generate_expression(ASTNodePtr node)
{
    if (!node)
        return TACOperand();

    switch (node->type) {
    case ASTNodeType::LITERAL_EXPR: {
        auto lit = std::static_pointer_cast<LiteralExpr>(node);
        if (std::holds_alternative<int64_t>(lit->value)) {
            return TACOperand::constant_int(std::get<int64_t>(lit->value),
                                            lit->expr_type);
        } else if (std::holds_alternative<uint64_t>(lit->value)) {
            return TACOperand::constant_int(
                static_cast<int64_t>(std::get<uint64_t>(lit->value)),
                lit->expr_type);
        } else if (std::holds_alternative<double>(lit->value)) {
            return TACOperand::constant_float(std::get<double>(lit->value),
                                              lit->expr_type);
        } else if (std::holds_alternative<char>(lit->value)) {
            return TACOperand::constant_int(
                static_cast<int64_t>(std::get<char>(lit->value)),
                lit->expr_type);
        } else if (std::holds_alternative<bool>(lit->value)) {
            return TACOperand::constant_int(std::get<bool>(lit->value) ? 1 : 0,
                                            lit->expr_type);
        } else if (std::holds_alternative<std::string>(lit->value)) {
            // String literals need special handling - create a global string
            // constant
            std::string str_value = std::get<std::string>(lit->value);
            std::string str_label =
                ".str" + std::to_string(string_literal_counter++);

            // Add string literal to the program
            program.add_string_literal(str_label, str_value);

            // Return a label operand that references this string
            return TACOperand::label(str_label);
        }
        return TACOperand();
    }

    case ASTNodeType::IDENTIFIER_EXPR: {
        auto id = std::static_pointer_cast<IdentifierExpr>(node);
        return TACOperand::symbol(id->symbol, id->expr_type);
    }

    case ASTNodeType::THIS_EXPR: {
        auto this_expr = std::static_pointer_cast<ThisExpr>(node);
        // 'this' is an implicit parameter (first parameter) for class methods
        // It's not stored in the symbol table but is passed as the first
        // argument We represent it as a special temporary with the name "this"
        return TACOperand::temporary("this", this_expr->expr_type);
    }

    case ASTNodeType::ENUM_IDENTIFIER_EXPR: {
        auto enum_id = std::static_pointer_cast<EnumIdentifierExpr>(node);
        // Enum identifiers are constant integers
        // Would need to look up the enum value from the type
        return TACOperand::constant_int(0, enum_id->expr_type); // Placeholder
    }

    case ASTNodeType::BINARY_EXPR:
        return generate_binary_op(std::static_pointer_cast<BinaryExpr>(node));

    case ASTNodeType::UNARY_EXPR:
        return generate_unary_op(std::static_pointer_cast<UnaryExpr>(node));

    case ASTNodeType::ASSIGNMENT_EXPR:
        return generate_assignment(
            std::static_pointer_cast<AssignmentExpr>(node));

    case ASTNodeType::CALL_EXPR:
        return generate_call(std::static_pointer_cast<CallExpr>(node));

    case ASTNodeType::TERNARY_EXPR:
        return generate_ternary(std::static_pointer_cast<TernaryExpr>(node));

    case ASTNodeType::CAST_EXPR: {
        auto cast = std::static_pointer_cast<CastExpr>(node);
        auto operand = generate_expression(cast->expression);
        auto result_temp = new_temp(cast->target_type);
        auto result = TACOperand::temporary(result_temp, cast->target_type);

        auto instr =
            std::make_shared<TACInstruction>(TACOpcode::CAST, result, operand);
        instr->comment = "Cast to " + cast->target_type->debug_name();
        emit(instr);

        return result;
    }

    case ASTNodeType::SIZEOF_EXPR: {
        auto sizeof_expr = std::static_pointer_cast<SizeofExpr>(node);
        TypePtr operand_type = sizeof_expr->operand_type;

        // Compute the size using the layout information
        TypePtr canonical = strip_typedefs(operand_type);
        size_t type_size = compute_type_size(canonical);

        // Return as a constant
        return TACOperand::constant_int(static_cast<int64_t>(type_size),
                                        sizeof_expr->expr_type);
    }

    case ASTNodeType::MEMBER_EXPR: {
        const auto member = std::static_pointer_cast<MemberExpr>(node);
        auto base = generate_expression(member->object);

        const auto base_type_result = get_expression_type(member->object);
        if (!base_type_result.is_ok()) {
            record_error("Cannot determine base type for member access");
            return TACOperand();
        }
        auto base_type = strip_typedefs(base_type_result.value());

        // Handle pointer member access (ptr->member)
        if (member->op == Operator::MEMBER_ACCESS_PTR) {
            TypePtr pointee_type = base_type;
            if (base_type->kind == TypeKind::POINTER) {
                auto ptr_type =
                    std::static_pointer_cast<PointerType>(base_type);
                pointee_type = strip_typedefs(ptr_type->pointee.type);
            }
            base_type = pointee_type;
        }

        if (base.type) {
            base_type = strip_typedefs(base.type);
        }

        return generate_member_access(base, member->member_name, base_type);
    }

    case ASTNodeType::NEW_EXPR: {
        auto new_expr = std::static_pointer_cast<NewExpr>(node);
        size_t size = get_type_size(new_expr->allocated_type);

        // Call malloc or allocation function
        auto size_operand = TACOperand::constant_int(size, nullptr);
        auto result_temp = new_temp(new_expr->expr_type);
        auto result = TACOperand::temporary(result_temp, new_expr->expr_type);

        // Emit allocation call
        auto alloc_instr =
            std::make_shared<TACInstruction>(TACOpcode::CALL,
                                             result,
                                             TACOperand::label("malloc"),
                                             size_operand);
        alloc_instr->comment =
            "Allocate " + new_expr->allocated_type->debug_name();
        emit(alloc_instr);

        return result;
    }

    case ASTNodeType::DELETE_EXPR: {
        auto delete_expr = std::static_pointer_cast<DeleteExpr>(node);
        auto operand = generate_expression(delete_expr->operand);

        // Call free or deallocation function
        auto free_instr =
            std::make_shared<TACInstruction>(TACOpcode::CALL,
                                             TACOperand(),
                                             TACOperand::label("free"),
                                             operand);
        free_instr->comment = "Deallocate memory";
        emit(free_instr);

        return TACOperand(); // Delete doesn't produce a value
    }

    case ASTNodeType::COMPOUND_LITERAL_EXPR: {
        auto compound = std::static_pointer_cast<CompoundLiteralExpr>(node);

        // Create a temporary for the compound literal
        auto result_temp = new_temp(compound->expr_type);
        auto result = TACOperand::temporary(result_temp, compound->expr_type);

        // Process each initializer
        for (const auto &init : compound->initializers) {
            if (init->type == ASTNodeType::DESIGNATED_INITIALIZER_EXPR) {
                auto desig =
                    std::static_pointer_cast<DesignatedInitializerExpr>(init);

                // Determine the type of the target member
                TypePtr member_type = compound->type;
                auto record_type =
                    std::static_pointer_cast<RecordType>(compound->type);
                for (const std::string &member : desig->member_path) {
                    if (record_type) {
                        member_type = get_member_type(record_type, member);
                        record_type =
                            std::static_pointer_cast<RecordType>(member_type);
                    }
                }

                // Generate the value (handles nested BLOCK_STMT recursively)
                auto value =
                    generate_initializer_value(desig->value, member_type);

                // Store to the designated member
                generate_designated_member_store(result,
                                                 compound->type,
                                                 desig->member_path,
                                                 value);
            } else {
                // Regular initializer (non-designated)
                generate_expression(init);
            }
        }

        return result;
    }

    case ASTNodeType::DESIGNATED_INITIALIZER_EXPR: {
        // This should generally be handled as part of COMPOUND_LITERAL_EXPR
        // If encountered standalone, just generate the value
        auto desig = std::static_pointer_cast<DesignatedInitializerExpr>(node);
        return generate_expression(desig->value);
    }

    default:
        record_error("Unhandled expression type in TAC generation");
        return TACOperand(); // Return empty operand on error
    }
}

TACOperand TACGenerator::generate_binary_op(std::shared_ptr<BinaryExpr> expr)
{
    // Handle short-circuit evaluation for logical operators
    if (expr->op == Operator::LOGICAL_AND || expr->op == Operator::LOGICAL_OR) {
        auto result_temp = new_temp(expr->expr_type);
        auto result = TACOperand::temporary(result_temp, expr->expr_type);

        auto short_circuit_label = new_label("short_circuit");
        auto end_label = new_label("end_logical");

        auto left = generate_expression(expr->left);

        if (expr->op == Operator::LOGICAL_AND) {
            // If left is false, short-circuit to false
            emit_conditional_jump(TACOpcode::IF_FALSE,
                                  left,
                                  short_circuit_label);

            auto right = generate_expression(expr->right);
            emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                                  result,
                                                  right));
            emit_goto(end_label);

            emit_label(short_circuit_label);
            emit(std::make_shared<TACInstruction>(
                TACOpcode::ASSIGN,
                result,
                TACOperand::constant_int(0, expr->expr_type)));
        } else {
            // If left is true, short-circuit to true
            emit_conditional_jump(TACOpcode::IF_TRUE,
                                  left,
                                  short_circuit_label);

            auto right = generate_expression(expr->right);
            emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                                  result,
                                                  right));
            emit_goto(end_label);

            emit_label(short_circuit_label);
            emit(std::make_shared<TACInstruction>(
                TACOpcode::ASSIGN,
                result,
                TACOperand::constant_int(1, expr->expr_type)));
        }

        emit_label(end_label);
        return result;
    }

    // Handle comma operator - evaluate left for side effects, return right
    if (expr->op == Operator::COMMA_OP) {
        generate_expression(expr->left); // Evaluate for side effects
        return generate_expression(expr->right);
    }

    // Handle array subscript operator
    if (expr->op == Operator::SUBSCRIPT_OP) {
        auto base = generate_expression(expr->left);
        auto index = generate_expression(expr->right);

        // Calculate address: base + (index * element_size)
        // The expr_type is the element type of the array (e.g., for int[3][4],
        // subscripting gives int[4], which is the correct element type)
        TypePtr element_type = expr->expr_type;
        size_t element_size = compute_type_size(element_type);

        auto size_operand = TACOperand::constant_int(element_size, nullptr);
        auto offset_temp = new_temp(nullptr);
        auto offset = TACOperand::temporary(offset_temp, nullptr);

        emit(std::make_shared<TACInstruction>(TACOpcode::MUL,
                                              offset,
                                              index,
                                              size_operand));

        auto addr_temp = new_temp(nullptr);
        auto addr = TACOperand::temporary(addr_temp, nullptr);
        emit(std::make_shared<TACInstruction>(TACOpcode::ADD,
                                              addr,
                                              base,
                                              offset));

        bool is_array_member_access = false;
        if (expr->left->type == ASTNodeType::MEMBER_EXPR) {
            auto member_expr = std::static_pointer_cast<MemberExpr>(expr->left);
            auto member_type_result = get_expression_type(expr->left);
            if (member_type_result.is_ok()) {
                TypePtr member_type =
                    strip_typedefs(member_type_result.value());
                if (member_type && member_type->kind == TypeKind::ARRAY) {
                    is_array_member_access = true;
                }
            }
        }

        // General case: Check base type from expression
        auto base_type_result = get_expression_type(expr->left);
        TypePtr base_type = nullptr;
        if (base_type_result.is_ok()) {
            base_type = strip_typedefs(base_type_result.value());
        }

        TypePtr result_type = strip_typedefs(element_type);
        bool result_is_array =
            (result_type && result_type->kind == TypeKind::ARRAY);
        bool result_is_struct =
            (result_type && result_type->kind == TypeKind::RECORD);

        // The type of the RESULT determines whether we return an address or
        // load a value.
        if (result_is_array || result_is_struct) {
            // Multi-dimensional array OR array of structs - return address
            // (lvalue)
            return addr;
        } else {
            auto result_temp = new_temp(element_type);
            auto result = TACOperand::temporary(result_temp, element_type);
            emit(std::make_shared<TACInstruction>(TACOpcode::LOAD,
                                                  result,
                                                  addr));
            return result;
        }
    }

    // Regular binary operations
    auto left = generate_expression(expr->left);
    auto right = generate_expression(expr->right);

    auto result_temp = new_temp(expr->expr_type);
    auto result = TACOperand::temporary(result_temp, expr->expr_type);

    auto opcode = operator_to_tac_opcode(expr->op);

    if (opcode == TACOpcode::ADD || opcode == TACOpcode::SUB) {
        auto left_type_result = get_expression_type(expr->left);
        auto right_type_result = get_expression_type(expr->right);

        if (left_type_result.is_ok() && right_type_result.is_ok()) {
            TypePtr left_type = strip_typedefs(left_type_result.value());
            TypePtr right_type = strip_typedefs(right_type_result.value());

            // For array type, we treat it as pointer to element type
            bool left_is_array =
                (left_type && left_type->kind == TypeKind::ARRAY);
            bool right_is_array =
                (right_type && right_type->kind == TypeKind::ARRAY);

            TypePtr left_effective_type = left_type;
            TypePtr right_effective_type = right_type;
            size_t left_element_size = 0;
            size_t right_element_size = 0;

            if (left_is_array) {
                auto array_type =
                    std::static_pointer_cast<ArrayType>(left_type);
                left_element_size =
                    compute_type_size(array_type->element_type.type);
            } else if (left_type && left_type->kind == TypeKind::POINTER) {
                auto ptr_type =
                    std::static_pointer_cast<PointerType>(left_type);
                left_element_size = compute_type_size(ptr_type->pointee.type);
            }

            if (right_is_array) {
                auto array_type =
                    std::static_pointer_cast<ArrayType>(right_type);
                right_element_size =
                    compute_type_size(array_type->element_type.type);
            } else if (right_type && right_type->kind == TypeKind::POINTER) {
                auto ptr_type =
                    std::static_pointer_cast<PointerType>(right_type);
                right_element_size = compute_type_size(ptr_type->pointee.type);
            }

            bool left_is_pointer_like =
                (left_is_array ||
                 (left_type && left_type->kind == TypeKind::POINTER));
            bool right_is_pointer_like =
                (right_is_array ||
                 (right_type && right_type->kind == TypeKind::POINTER));
            bool left_is_integral = is_integral_type(left_type);
            bool right_is_integral = is_integral_type(right_type);

            // Pointer/array arithmetic: scale the integer operand by element
            // size
            if (left_is_pointer_like && right_is_integral &&
                left_element_size > 1) {
                auto size_operand =
                    TACOperand::constant_int(left_element_size, nullptr);
                auto scaled_temp = new_temp(nullptr);
                auto scaled = TACOperand::temporary(scaled_temp, nullptr);

                emit(std::make_shared<TACInstruction>(TACOpcode::MUL,
                                                      scaled,
                                                      right,
                                                      size_operand));
                right = scaled;
            } else if (opcode == TACOpcode::ADD && left_is_integral &&
                       right_is_pointer_like && right_element_size > 1) {
                auto size_operand =
                    TACOperand::constant_int(right_element_size, nullptr);
                auto scaled_temp = new_temp(nullptr);
                auto scaled = TACOperand::temporary(scaled_temp, nullptr);

                emit(std::make_shared<TACInstruction>(TACOpcode::MUL,
                                                      scaled,
                                                      left,
                                                      size_operand));
                left = scaled;
            }
        }
    }

    auto instr = std::make_shared<TACInstruction>(opcode, result, left, right);
    emit(instr);

    return result;
}

TACOperand TACGenerator::generate_unary_op(std::shared_ptr<UnaryExpr> expr)
{
    // Handle pre/post increment and decrement
    if (expr->op == Operator::INCREMENT || expr->op == Operator::DECREMENT ||
        expr->op == Operator::POST_INCREMENT ||
        expr->op == Operator::POST_DECREMENT) {

        auto operand = generate_expression(expr->operand);
        auto one = TACOperand::constant_int(1, expr->expr_type);

        TACOperand result;
        if (expr->op == Operator::POST_INCREMENT ||
            expr->op == Operator::POST_DECREMENT) {
            // Post-increment/decrement: save old value, modify, return old
            // value
            auto old_value_temp = new_temp(expr->expr_type);
            auto old_value =
                TACOperand::temporary(old_value_temp, expr->expr_type);
            emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                                  old_value,
                                                  operand));

            auto opcode = (expr->op == Operator::POST_INCREMENT)
                              ? TACOpcode::ADD
                              : TACOpcode::SUB;
            emit(std::make_shared<TACInstruction>(opcode,
                                                  operand,
                                                  operand,
                                                  one));

            result = old_value;
        } else {
            // Pre-increment/decrement: modify, return new value
            auto opcode = (expr->op == Operator::INCREMENT) ? TACOpcode::ADD
                                                            : TACOpcode::SUB;
            emit(std::make_shared<TACInstruction>(opcode,
                                                  operand,
                                                  operand,
                                                  one));
            result = operand;
        }

        return result;
    }

    // Handle unary plus (no-op)
    if (expr->op == Operator::UNARY_PLUS) {
        return generate_expression(expr->operand);
    }

    if (expr->op == Operator::ADDRESS_OF &&
        expr->operand->type == ASTNodeType::BINARY_EXPR) {
        auto binary = std::static_pointer_cast<BinaryExpr>(expr->operand);
        if (binary->op == Operator::SUBSCRIPT_OP) {

            auto base = generate_expression(binary->left);
            auto index = generate_expression(binary->right);

            TypePtr element_type = binary->expr_type;
            size_t element_size = compute_type_size(element_type);

            auto size_operand = TACOperand::constant_int(element_size, nullptr);
            auto offset_temp = new_temp(nullptr);
            auto offset = TACOperand::temporary(offset_temp, nullptr);

            emit(std::make_shared<TACInstruction>(TACOpcode::MUL,
                                                  offset,
                                                  index,
                                                  size_operand));

            auto addr_temp = new_temp(nullptr);
            auto addr = TACOperand::temporary(addr_temp, nullptr);
            emit(std::make_shared<TACInstruction>(TACOpcode::ADD,
                                                  addr,
                                                  base,
                                                  offset));

            return addr;
        }
    }

    // Other unary operations
    auto operand = generate_expression(expr->operand);
    auto result_temp = new_temp(expr->expr_type);
    auto result = TACOperand::temporary(result_temp, expr->expr_type);

    auto opcode = operator_to_tac_opcode(expr->op);
    auto instr = std::make_shared<TACInstruction>(opcode, result, operand);
    emit(instr);

    return result;
}

TACOperand
TACGenerator::generate_assignment(std::shared_ptr<AssignmentExpr> expr)
{
    auto value = generate_expression(expr->value);

    // Handle member assignment
    if (expr->target->type == ASTNodeType::MEMBER_EXPR) {
        auto member = std::static_pointer_cast<MemberExpr>(expr->target);
        auto base = generate_expression(member->object);

        auto base_type_result = get_expression_type(member->object);
        if (!base_type_result.is_ok()) {
            record_error("Cannot determine base type for member assignment");
            return TACOperand(); // Return empty operand on error
        }
        TypePtr base_type = base_type_result.value();

        // Handle pointer dereference for -> operator
        if (member->op == Operator::MEMBER_ACCESS_PTR) {
            auto deref_temp = new_temp(base_type);
            auto deref_result = TACOperand::temporary(deref_temp, base_type);
            emit(std::make_shared<TACInstruction>(TACOpcode::DEREF,
                                                  deref_result,
                                                  base));
            base = deref_result;

            // Get pointed-to type - use pointee member of PointerType
            if (base_type->kind == TypeKind::POINTER) {
                auto ptr_type =
                    std::static_pointer_cast<PointerType>(base_type);
                base_type = ptr_type->pointee.type;
            }
        }

        // Handle compound assignment operators
        if (expr->op != Operator::ASSIGN) {
            auto current =
                generate_member_access(base, member->member_name, base_type);
            auto opcode = operator_to_tac_opcode(expr->op);

            auto temp_result = new_temp(expr->expr_type);
            auto temp = TACOperand::temporary(temp_result, expr->expr_type);
            emit(
                std::make_shared<TACInstruction>(opcode, temp, current, value));
            value = temp;
        }

        generate_member_store(base, member->member_name, base_type, value);
        return value;
    }

    if (expr->target->type == ASTNodeType::UNARY_EXPR) {
        auto unary = std::static_pointer_cast<UnaryExpr>(expr->target);
        if (unary->op == Operator::POINTER_DEREF) {

            auto ptr = generate_expression(unary->operand);

            if (expr->op != Operator::ASSIGN) {
                auto current_temp = new_temp(expr->expr_type);
                auto current =
                    TACOperand::temporary(current_temp, expr->expr_type);
                emit(std::make_shared<TACInstruction>(TACOpcode::LOAD,
                                                      current,
                                                      ptr));

                auto opcode = operator_to_tac_opcode(expr->op);

                auto result_temp = new_temp(expr->expr_type);
                auto result =
                    TACOperand::temporary(result_temp, expr->expr_type);
                emit(std::make_shared<TACInstruction>(opcode,
                                                      result,
                                                      current,
                                                      value));
                value = result;
            }

            emit(
                std::make_shared<TACInstruction>(TACOpcode::STORE, ptr, value));
            return value;
        }
    }

    // Handle array subscript assignment
    if (expr->target->type == ASTNodeType::BINARY_EXPR) {
        auto binary = std::static_pointer_cast<BinaryExpr>(expr->target);
        if (binary->op == Operator::SUBSCRIPT_OP) {
            auto base = generate_expression(binary->left);
            auto index = generate_expression(binary->right);

            // Calculate element size from the result type of the subscript
            size_t element_size = get_type_size(expr->expr_type);

            auto size_operand = TACOperand::constant_int(element_size, nullptr);
            auto offset_temp = new_temp(nullptr);
            auto offset = TACOperand::temporary(offset_temp, nullptr);

            emit(std::make_shared<TACInstruction>(TACOpcode::MUL,
                                                  offset,
                                                  index,
                                                  size_operand));

            auto addr_temp = new_temp(nullptr);
            auto addr = TACOperand::temporary(addr_temp, nullptr);
            emit(std::make_shared<TACInstruction>(TACOpcode::ADD,
                                                  addr,
                                                  base,
                                                  offset));

            // Handle compound assignment
            if (expr->op != Operator::ASSIGN) {
                auto current_temp = new_temp(expr->expr_type);
                auto current =
                    TACOperand::temporary(current_temp, expr->expr_type);
                emit(std::make_shared<TACInstruction>(TACOpcode::LOAD,
                                                      current,
                                                      addr));

                auto opcode = operator_to_tac_opcode(expr->op);

                auto result_temp = new_temp(expr->expr_type);
                auto result =
                    TACOperand::temporary(result_temp, expr->expr_type);
                emit(std::make_shared<TACInstruction>(opcode,
                                                      result,
                                                      current,
                                                      value));
                value = result;
            }

            emit(std::make_shared<TACInstruction>(TACOpcode::STORE,
                                                  addr,
                                                  value));
            return value;
        }
    }

    // Simple variable assignment
    auto target = generate_expression(expr->target);

    // Handle compound assignment operators
    if (expr->op != Operator::ASSIGN) {
        auto opcode = operator_to_tac_opcode(expr->op);

        auto temp_result = new_temp(expr->expr_type);
        auto temp = TACOperand::temporary(temp_result, expr->expr_type);
        emit(std::make_shared<TACInstruction>(opcode, temp, target, value));
        value = temp;
    }

    TypePtr target_type = strip_typedefs(expr->expr_type);
    if (target_type && (target_type->kind == TypeKind::RECORD ||
                        target_type->kind == TypeKind::CLASS)) {
        generate_aggregate_copy(target, value, target_type);
        return target;
    }

    emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN, target, value));
    return target;
}

TACOperand TACGenerator::generate_call(std::shared_ptr<CallExpr> expr)
{
    std::vector<TACOperand> args;
    args.reserve(expr->arguments.size());
    for (const auto &arg : expr->arguments) {
        args.push_back(generate_expression(arg));
    }

    for (const auto &arg : args) {
        emit(std::make_shared<TACInstruction>(TACOpcode::PARAM,
                                              TACOperand(),
                                              arg));
    }

    const auto callee = TACOperand::symbol(expr->callee, nullptr);
    TACOperand result;

    if (expr->expr_type && !is_void_type(expr->expr_type)) {
        result =
            TACOperand::temporary(new_temp(expr->expr_type), expr->expr_type);
        emit(std::make_shared<TACInstruction>(
            TACOpcode::CALL,
            result,
            callee,
            TACOperand::constant_int(args.size(), nullptr)));
    } else {
        emit(std::make_shared<TACInstruction>(
            TACOpcode::CALL,
            TACOperand(),
            callee,
            TACOperand::constant_int(args.size(), nullptr)));
    }

    return result;
}

TACOperand TACGenerator::generate_ternary(std::shared_ptr<TernaryExpr> expr)
{
    auto condition = generate_expression(expr->condition);

    auto true_label = new_label("ternary_true");
    auto false_label = new_label("ternary_false");
    auto end_label = new_label("ternary_end");

    auto result_temp = new_temp(expr->expr_type);
    auto result = TACOperand::temporary(result_temp, expr->expr_type);

    emit_conditional_jump(TACOpcode::IF_FALSE, condition, false_label);

    // True branch
    emit_label(true_label);
    auto true_result = generate_expression(expr->true_branch);
    emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                          result,
                                          true_result));
    emit_goto(end_label);

    // False branch
    emit_label(false_label);
    auto false_result = generate_expression(expr->false_branch);
    emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                          result,
                                          false_result));

    emit_label(end_label);
    return result;
}

void TACGenerator::generate_aggregate_copy(const TACOperand &target,
                                           const TACOperand &source,
                                           TypePtr type)
{
    type = strip_typedefs(type);
    if (!type)
        return record_error("Cannot copy: invalid aggregate type");

    const auto kind = type->kind;

    if (kind == TypeKind::RECORD) {
        const auto record = std::static_pointer_cast<RecordType>(type);

        for (const auto &[field_name, field_qual] : record->fields) {
            const auto field_type = strip_typedefs(field_qual.type);
            const auto source_field =
                generate_member_access(source, field_name, type);

            copy_field(
                target,
                source_field,
                type,
                field_type,
                field_name,
                [&](auto &&tgt, auto &&src, auto &&t) {
                    generate_aggregate_copy(tgt, src, t);
                },
                [&](auto &&tgt,
                    auto &&src,
                    auto &&name,
                    auto &&base_t,
                    auto &&arr_t) {
                    generate_array_copy(tgt, src, name, base_t, arr_t);
                },
                [&](auto &&tgt, auto &&name, auto &&base_t, auto &&src_field) {
                    generate_member_store(tgt, name, base_t, src_field);
                });
        }
    } else if (kind == TypeKind::CLASS) {
        const auto cls = std::static_pointer_cast<ClassType>(type);

        if (cls->base.base_type)
            generate_aggregate_copy(target, source, cls->base.base_type);

        for (const auto &[member_name, member_info] : cls->members) {
            const auto member_type = strip_typedefs(member_info.type.type);
            if (member_type && member_type->kind == TypeKind::FUNCTION)
                continue;

            const auto source_member =
                generate_member_access(source, member_name, type);

            copy_field(
                target,
                source_member,
                type,
                member_type,
                member_name,
                [&](auto &&tgt, auto &&src, auto &&t) {
                    generate_aggregate_copy(tgt, src, t);
                },
                [&](auto &&tgt,
                    auto &&src,
                    auto &&name,
                    auto &&base_t,
                    auto &&arr_t) {
                    generate_array_copy(tgt, src, name, base_t, arr_t);
                },
                [&](auto &&tgt, auto &&name, auto &&base_t, auto &&src_field) {
                    generate_member_store(tgt, name, base_t, src_field);
                });
        }
    } else {
        record_error("Cannot copy: type is not a struct, union, or class");
    }
}

void TACGenerator::generate_array_copy(const TACOperand &target_base,
                                       const TACOperand &source_base,
                                       const std::string &field_name,
                                       TypePtr base_type,
                                       TypePtr array_type)
{
    array_type = strip_typedefs(array_type);
    if (!array_type || array_type->kind != TypeKind::ARRAY)
        return record_error("Expected array type for array copy");

    const auto arr = std::static_pointer_cast<ArrayType>(array_type);
    const auto elem_type = strip_typedefs(arr->element_type.type);
    const auto size = arr->size;

    if (size == 0)
        return record_error("Cannot copy unsized array");

    const auto loop = new_label("array_copy_loop");
    const auto loop_end = new_label("array_copy_end");

    const auto counter = TACOperand::temporary(new_temp(nullptr), nullptr);
    emit(
        std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                         counter,
                                         TACOperand::constant_int(0, nullptr)));

    emit_label(loop);

    const auto cond = TACOperand::temporary(new_temp(nullptr), nullptr);
    emit(std::make_shared<TACInstruction>(
        TACOpcode::LT,
        cond,
        counter,
        TACOperand::constant_int(size, nullptr)));
    emit_conditional_jump(TACOpcode::IF_FALSE, cond, loop_end);

    const auto source_array =
        generate_member_access(source_base, field_name, base_type);
    const auto target_array =
        generate_member_access(target_base, field_name, base_type);

    const auto element_size = get_type_size(elem_type);
    const auto offset = TACOperand::temporary(new_temp(nullptr), nullptr);
    emit(std::make_shared<TACInstruction>(
        TACOpcode::MUL,
        offset,
        counter,
        TACOperand::constant_int(element_size, nullptr)));

    const auto source_addr = TACOperand::temporary(new_temp(nullptr), nullptr);
    const auto target_addr = TACOperand::temporary(new_temp(nullptr), nullptr);

    emit(std::make_shared<TACInstruction>(TACOpcode::ADD,
                                          source_addr,
                                          source_array,
                                          offset));
    emit(std::make_shared<TACInstruction>(TACOpcode::ADD,
                                          target_addr,
                                          target_array,
                                          offset));

    const auto source_elem =
        TACOperand::temporary(new_temp(elem_type), elem_type);
    emit(std::make_shared<TACInstruction>(TACOpcode::LOAD,
                                          source_elem,
                                          source_addr));

    if (elem_type && (elem_type->kind == TypeKind::RECORD ||
                      elem_type->kind == TypeKind::CLASS))
        generate_aggregate_copy(target_addr, source_elem, elem_type);
    else
        emit(std::make_shared<TACInstruction>(TACOpcode::STORE,
                                              target_addr,
                                              source_elem));

    const auto next_counter = TACOperand::temporary(new_temp(nullptr), nullptr);
    emit(
        std::make_shared<TACInstruction>(TACOpcode::ADD,
                                         next_counter,
                                         counter,
                                         TACOperand::constant_int(1, nullptr)));
    emit(std::make_shared<TACInstruction>(TACOpcode::ASSIGN,
                                          counter,
                                          next_counter));

    emit_goto(loop);
    emit_label(loop_end);
}

TACOperand TACGenerator::generate_member_access(const TACOperand &base_object,
                                                const std::string &member_name,
                                                TypePtr base_type)
{
    size_t offset = 0;
    TypePtr member_type = nullptr;

    // Strip typedefs and handle pointer types first
    base_type = strip_typedefs(base_type);

    // If base_type is a pointer, get the pointee type
    if (base_type && base_type->kind == TypeKind::POINTER) {
        auto ptr_type = std::static_pointer_cast<PointerType>(base_type);
        base_type = strip_typedefs(ptr_type->pointee.type);
    }

    // Handle struct/union types
    if (base_type->kind == TypeKind::RECORD) {
        auto record_type = std::static_pointer_cast<RecordType>(base_type);
        // Get member offset from precomputed field_offsets
        auto offset_opt = get_member_offset(record_type, member_name);
        if (!offset_opt.has_value()) {
            record_error("Member '" + member_name +
                         "' not found in struct/union");
            return TACOperand(); // Return empty operand on error
        }

        // For unions, all members are at offset 0 (already handled by layout
        // pass)
        offset = offset_opt.value();

        // Get member type
        member_type = get_member_type(record_type, member_name);
        if (!member_type) {
            record_error("Could not determine member type");
            return TACOperand(); // Return empty operand on error
        }
    }
    // Handle class types
    else if (base_type->kind == TypeKind::CLASS) {
        auto class_type = std::static_pointer_cast<ClassType>(base_type);
        // Get member offset (includes inherited members)
        auto offset_opt = get_class_member_offset(class_type, member_name);
        if (!offset_opt.has_value()) {
            record_error("Member '" + member_name + "' not found in class");
            return TACOperand(); // Return empty operand on error
        }

        offset = offset_opt.value();

        // Get member type (includes inherited members)
        member_type = get_class_member_type(class_type, member_name);
        if (!member_type) {
            record_error("Could not determine member type");
            return TACOperand(); // Return empty operand on error
        }
    } else {
        record_error("Member access on non-struct/class type");
        return TACOperand(); // Return empty operand on error
    }

    // Check if base_object is a pointer type
    TACOperand actual_base = base_object;
    bool base_is_pointer = false;
    TypePtr base_obj_type = strip_typedefs(base_object.type);
    if (base_obj_type && base_obj_type->kind == TypeKind::POINTER) {
        base_is_pointer = true;
        actual_base = base_object;
    }

    if (member_type && member_type->kind == TypeKind::ARRAY) {
        auto array_type = std::static_pointer_cast<ArrayType>(member_type);
        TypePtr element_type = array_type->element_type.type;

        // Create pointer type to the array element
        auto ptr_result = type_factory.get_pointer(QualifiedType{element_type});
        if (ptr_result.is_err()) {
            record_error("Failed to create pointer type for array member");
            return TACOperand();
        }
        TypePtr ptr_type = ptr_result.value();

        // Get base address
        TACOperand base_addr;
        if (base_is_pointer) {
            base_addr = actual_base;
        } else {
            // For struct variables, get the address of the variable
            std::string addr_temp = new_temp(ptr_type);
            base_addr = TACOperand::temporary(addr_temp, ptr_type);
            emit(std::make_shared<TACInstruction>(TACOpcode::ADDR_OF,
                                                  base_addr,
                                                  actual_base));
        }

        // Add offset to get address of array member
        std::string array_addr_temp = new_temp(ptr_type);
        TACOperand array_addr =
            TACOperand::temporary(array_addr_temp, ptr_type);
        TACOperand offset_operand = TACOperand::constant_int(offset, nullptr);
        auto add_instr = std::make_shared<TACInstruction>(TACOpcode::ADD,
                                                          array_addr,
                                                          base_addr,
                                                          offset_operand);
        add_instr->comment =
            "Address of " + actual_base.to_string() + "." + member_name;
        emit(add_instr);

        return array_addr;
    }

    // Create temporary for result
    std::string temp_name = new_temp(member_type);
    TACOperand result = TACOperand::temporary(temp_name, member_type);

    if (base_is_pointer && offset == 0) {
        // Offset is 0, can load directly from the pointer
        emit(std::make_shared<TACInstruction>(TACOpcode::LOAD,
                                              result,
                                              actual_base));
    } else if (base_is_pointer) {
        // Need to add offset to pointer first
        auto addr_temp = new_temp(nullptr);
        auto addr = TACOperand::temporary(addr_temp, nullptr);
        emit(std::make_shared<TACInstruction>(
            TACOpcode::ADD,
            addr,
            actual_base,
            TACOperand::constant_int(offset, nullptr)));
        emit(std::make_shared<TACInstruction>(TACOpcode::LOAD, result, addr));
    } else {
        // Normal member access on struct variable
        TACOperand member_op = TACOperand::member_access(actual_base,
                                                         member_name,
                                                         offset,
                                                         member_type);

        auto instr = std::make_shared<TACInstruction>(TACOpcode::LOAD_MEMBER,
                                                      result,
                                                      member_op);
        instr->comment = "Load " + actual_base.to_string() + "." + member_name;
        emit(instr);
    }

    return result;
}

// Generate member store: obj.member = value
void TACGenerator::generate_member_store(const TACOperand &base_object,
                                         const std::string &member_name,
                                         TypePtr base_type,
                                         const TACOperand &value)
{
    size_t offset = 0;
    TypePtr member_type = nullptr;

    // Strip typedefs and handle pointer types first
    base_type = strip_typedefs(base_type);

    // If base_type is a pointer, get the pointee type
    if (base_type && base_type->kind == TypeKind::POINTER) {
        auto ptr_type = std::static_pointer_cast<PointerType>(base_type);
        base_type = strip_typedefs(ptr_type->pointee.type);
    }

    // Handle struct/union types
    if (base_type->kind == TypeKind::RECORD) {
        auto record_type = std::static_pointer_cast<RecordType>(base_type);
        // Get member offset from precomputed field_offsets
        auto offset_opt = get_member_offset(record_type, member_name);
        if (!offset_opt.has_value()) {
            record_error("Member '" + member_name +
                         "' not found in struct/union");
            return; // Return early on error
        }

        // For unions, all members are at offset 0 (already handled by layout
        // pass)
        offset = offset_opt.value();

        // Get member type
        member_type = get_member_type(record_type, member_name);
        if (!member_type) {
            record_error("Could not determine member type");
            return; // Return early on error
        }
    }
    // Handle class types
    else if (base_type->kind == TypeKind::CLASS) {
        auto class_type = std::static_pointer_cast<ClassType>(base_type);
        // Get member offset (includes inherited members)
        auto offset_opt = get_class_member_offset(class_type, member_name);
        if (!offset_opt.has_value()) {
            record_error("Member '" + member_name + "' not found in class");
            return; // Return early on error
        }

        offset = offset_opt.value();

        // Get member type (includes inherited members)
        member_type = get_class_member_type(class_type, member_name);
        if (!member_type) {
            record_error("Could not determine member type");
            return; // Return early on error
        }
    } else {
        record_error("Member access on non-struct/class type");
        return; // Return early on error
    }

    // Create member operand with offset info
    TACOperand member_op = TACOperand::member_access(base_object,
                                                     member_name,
                                                     offset,
                                                     member_type);

    // Emit STORE_MEMBER instruction: base.member = value
    auto instr = std::make_shared<TACInstruction>(TACOpcode::STORE_MEMBER,
                                                  member_op,
                                                  value);
    instr->comment = "Store to " + base_object.to_string() + "." + member_name;
    emit(instr);
}

// Helper function to generate TAC for initializer values (handles nested
// BLOCK_STMT)
TACOperand TACGenerator::generate_initializer_value(ASTNodePtr value_node,
                                                    TypePtr target_type)
{
    if (!value_node) {
        return TACOperand();
    }

    // Check if the value is a nested initializer list (BLOCK_STMT)
    if (value_node->type == ASTNodeType::BLOCK_STMT) {
        auto block = std::static_pointer_cast<BlockStmt>(value_node);

        // Create a temporary for the nested struct
        auto nested_temp = new_temp(target_type);
        auto result = TACOperand::temporary(nested_temp, target_type);

        // Process each initializer in the nested list
        for (const auto &init : block->statements) {
            if (init->type == ASTNodeType::DESIGNATED_INITIALIZER_EXPR) {
                auto desig =
                    std::static_pointer_cast<DesignatedInitializerExpr>(init);

                // Determine the type of the nested member
                TypePtr member_type = target_type;
                auto record_type =
                    std::static_pointer_cast<RecordType>(target_type);
                for (const std::string &member : desig->member_path) {
                    if (record_type) {
                        member_type = get_member_type(record_type, member);
                        record_type =
                            std::static_pointer_cast<RecordType>(member_type);
                    }
                }

                // Recursively handle the value (which might also be a
                // BLOCK_STMT)
                auto value =
                    generate_initializer_value(desig->value, member_type);

                // Store to the nested member
                generate_designated_member_store(result,
                                                 target_type,
                                                 desig->member_path,
                                                 value);
            }
        }

        return result;
    } else {
        // Regular expression value
        return generate_expression(value_node);
    }
}

// Generate designated member store for compound literals: obj.member1.member2 =
// value
void TACGenerator::generate_designated_member_store(
    const TACOperand &base_object,
    TypePtr base_type,
    const std::vector<std::string> &member_path,
    const TACOperand &value)
{
    if (member_path.empty()) {
        return;
    }

    // Navigate through the member path to find the final member to store
    TACOperand current_object = base_object;
    TypePtr current_type = base_type;

    // For all but the last member in the path, we need to access nested members
    for (size_t i = 0; i < member_path.size() - 1; ++i) {
        const std::string &member = member_path[i];

        // Get the nested member as an intermediate value
        auto intermediate_temp = new_temp(nullptr);
        auto intermediate = TACOperand::temporary(intermediate_temp, nullptr);

        // Get member type
        auto record_type = std::static_pointer_cast<RecordType>(current_type);
        if (!record_type) {
            record_error("Cannot access member on non-struct type");
            return; // Return early on error
        }

        TypePtr member_type = get_member_type(record_type, member);
        if (!member_type) {
            record_error("Member '" + member + "' not found");
            return; // Return early on error
        }

        // Load the intermediate member
        auto offset_opt = get_member_offset(record_type, member);
        size_t offset = 0;
        if (!record_type->is_union && offset_opt.has_value()) {
            offset = offset_opt.value();
        }

        TACOperand member_op = TACOperand::member_access(current_object,
                                                         member,
                                                         offset,
                                                         member_type);

        auto instr = std::make_shared<TACInstruction>(TACOpcode::LOAD_MEMBER,
                                                      intermediate,
                                                      member_op);
        emit(instr);

        current_object = intermediate;
        current_type = member_type;
    }

    const std::string &final_member = member_path.back();
    generate_member_store(current_object, final_member, current_type, value);
}

void TACGenerator::generate_function(std::shared_ptr<FunctionDef> func_def)
{
    // Create function
    auto func_type = std::static_pointer_cast<FunctionType>(
        func_def->function_symbol->get_type().type);

    std::string mangled_name = func_def->function_symbol->get_name();

    auto func_meta_opt = func_def->function_symbol->get_function_meta();
    ScopeID body_scope = 0;
    if (func_meta_opt.has_value()) {
        body_scope = func_meta_opt.value().body_scope_id;
    }
    current_function = std::make_shared<TACFunction>(mangled_name,
                                                     mangled_name,
                                                     func_def->return_type,
                                                     body_scope,
                                                     get_symbol_table());

    current_block = std::make_shared<TACBasicBlock>("");
    current_function->entry_block = current_block;
    current_function->add_block(current_block);

    // Emit ENTER instruction
    emit(std::make_shared<TACInstruction>(TACOpcode::ENTER));

    // Add parameters
    for (const auto &param : func_def->parameters) {
        current_function->parameters.push_back(
            TACOperand::symbol(param, param->get_type().type));
    }

    // Generate body
    if (func_def->body) {
        generate_statement(func_def->body);
    }

    // For void functions, automatically add a return statement if not already
    // present
    bool has_return = false;
    if (current_block && !current_block->instructions.empty()) {
        auto last_instr = current_block->instructions.back();
        if (last_instr && last_instr->opcode == TACOpcode::RETURN) {
            has_return = true;
        }
    }

    if (!has_return && func_def->return_type &&
        is_void_type(func_def->return_type)) {
        emit(std::make_shared<TACInstruction>(TACOpcode::RETURN));
    }

    // Add function to program
    program.add_function(current_function);

    // Clean up
    current_function = nullptr;
    current_block = nullptr;
}

void TACGenerator::generate_global_declaration(ASTNodePtr node)
{
    if (!node)
        return;

    if (node->type == ASTNodeType::BLOCK_STMT) {
        auto block = std::static_pointer_cast<BlockStmt>(node);

        if (block->statements.empty())
            return;

        // Process each global variable initialization
        for (const auto &stmt : block->statements) {
            if (stmt && stmt->type == ASTNodeType::ASSIGNMENT_EXPR) {
                auto assign = std::static_pointer_cast<AssignmentExpr>(stmt);

                if (assign->target &&
                    assign->target->type == ASTNodeType::IDENTIFIER_EXPR) {
                    auto id_expr = std::static_pointer_cast<IdentifierExpr>(
                        assign->target);
                    auto symbol = id_expr->symbol;

                    program.add_global(symbol);

                    if (assign->value) {
                        // Extract constant value from the initializer
                        if (assign->value->type == ASTNodeType::LITERAL_EXPR) {
                            auto lit = std::static_pointer_cast<LiteralExpr>(
                                assign->value);

                            // Convert LiteralValue to Symbol::InitializerValue
                            if (std::holds_alternative<int64_t>(lit->value)) {
                                symbol->set_initializer(
                                    std::get<int64_t>(lit->value));
                            } else if (std::holds_alternative<uint64_t>(
                                           lit->value)) {
                                symbol->set_initializer(static_cast<int64_t>(
                                    std::get<uint64_t>(lit->value)));
                            } else if (std::holds_alternative<double>(
                                           lit->value)) {
                                symbol->set_initializer(
                                    std::get<double>(lit->value));
                            } else if (std::holds_alternative<char>(
                                           lit->value)) {
                                symbol->set_initializer(static_cast<int64_t>(
                                    std::get<char>(lit->value)));
                            } else if (std::holds_alternative<bool>(
                                           lit->value)) {
                                symbol->set_initializer(static_cast<int64_t>(
                                    std::get<bool>(lit->value) ? 1 : 0));
                            } else if (std::holds_alternative<std::string>(
                                           lit->value)) {
                                std::string str_value =
                                    std::get<std::string>(lit->value);

                                std::string str_label =
                                    ".str" +
                                    std::to_string(string_literal_counter++);
                                program.add_string_literal(str_label,
                                                           str_value);
                                // Store the label in the symbol's initializer
                                symbol->set_initializer(str_value);
                            }
                        }

                        // TODO: Handle more complex initializers
                    }
                }
            }
        }
    }
}

Result<bool, std::vector<TACErrorInfo>>
TACGenerator::generate(const std::vector<ASTNodePtr> &translation_unit)
{
    errors.clear();

    // First pass: process all global declarations
    for (const auto &node : translation_unit) {
        if (!node)
            continue;

        if (node->type != ASTNodeType::FUNCTION_DEF) {
            // Handle global declarations
            generate_global_declaration(node);
        }
    }

    // Collect all global variables from the symbol table
    // This includes both initialized and uninitialized globals
    auto global_symbols = symbol_table.get_symbols_in_scope(GLOBAL_SCOPE_ID);
    for (const auto &[name, symbol] : global_symbols) {
        if (symbol->get_type().type->kind != TypeKind::FUNCTION) {
            // Check if already added (to avoid duplicates from initialized
            // globals)
            bool already_added = false;
            for (const auto &existing : program.global_variables) {
                if (existing->get_name() == symbol->get_name()) {
                    already_added = true;
                    break;
                }
            }
            if (!already_added) {
                program.add_global(symbol);
            }
        }
    }

    // Second pass: process all functions in global scope
    for (const auto &node : translation_unit) {
        if (!node)
            continue;

        if (node->type == ASTNodeType::FUNCTION_DEF) {
            generate_function(std::static_pointer_cast<FunctionDef>(node));
        }
    }

    // Third pass: process all class methods (constructors, destructors,
    // methods)
    auto class_methods = get_parsed_class_methods();
    for (const auto &method_def : class_methods) {
        if (method_def) {
            generate_function(method_def);
        }
    }

    if (!errors.empty()) {
        return std::vector<TACErrorInfo>(errors);
    }

    return true;
}

void TACGenerator::generate_statement(ASTNodePtr node)
{
    if (!node)
        return;

    switch (node->type) {
    case ASTNodeType::COMPOUND_STMT:
    case ASTNodeType::BLOCK_STMT:
        generate_block_stmt(std::static_pointer_cast<BlockStmt>(node));
        break;

    case ASTNodeType::IF_STMT:
        generate_if_stmt(std::static_pointer_cast<IfStmt>(node));
        break;

    case ASTNodeType::ELSE_STMT: {
        auto stmt = std::static_pointer_cast<ElseStmt>(node);
        generate_statement(stmt->body);
        break;
    }

    case ASTNodeType::WHILE_STMT:
        generate_while_stmt(std::static_pointer_cast<WhileStmt>(node));
        break;

    case ASTNodeType::DO_WHILE_STMT: {
        auto stmt = std::static_pointer_cast<DoWhileStmt>(node);
        auto body_label = new_label("do_body");
        auto cond_label = new_label("do_cond");
        auto end_label = new_label("do_end");

        loop_labels.push({end_label, cond_label});

        emit_label(body_label);
        generate_statement(stmt->body);

        emit_label(cond_label);
        auto condition = generate_expression(stmt->condition);
        emit_conditional_jump(TACOpcode::IF_TRUE, condition, body_label);

        emit_label(end_label);
        loop_labels.pop();
        break;
    }

    case ASTNodeType::UNTIL_STMT: {
        auto stmt = std::static_pointer_cast<UntilStmt>(node);
        auto body_label = new_label("until_body");
        auto cond_label = new_label("until_cond");
        auto end_label = new_label("until_end");

        loop_labels.push({end_label, cond_label});

        emit_label(cond_label);
        auto condition = generate_expression(stmt->condition);
        emit_conditional_jump(TACOpcode::IF_TRUE, condition, end_label);

        emit_label(body_label);
        generate_statement(stmt->body);
        emit_goto(cond_label);

        emit_label(end_label);
        loop_labels.pop();
        break;
    }

    case ASTNodeType::FOR_STMT:
        generate_for_stmt(std::static_pointer_cast<ForStmt>(node));
        break;

    case ASTNodeType::SWITCH_STMT:
        generate_switch_stmt(std::static_pointer_cast<SwitchStmt>(node));
        break;

    case ASTNodeType::RET_EXPR:
        generate_return_stmt(std::static_pointer_cast<RetExpr>(node));
        break;

    case ASTNodeType::BREAK_STMT:
        if (!loop_labels.empty()) {
            emit_goto(loop_labels.top().first);
        }
        break;

    case ASTNodeType::CONTINUE_STMT:
        if (!loop_labels.empty() && !loop_labels.top().second.empty()) {
            emit_goto(loop_labels.top().second);
        }
        break;

    case ASTNodeType::GOTO_STMT: {
        auto stmt = std::static_pointer_cast<GotoStmt>(node);
        if (stmt->target_label) {
            emit_goto(stmt->target_label->get_name());
        }
        break;
    }

    case ASTNodeType::LABEL_STMT: {
        auto stmt = std::static_pointer_cast<LabelStmt>(node);
        if (stmt->label) {
            emit_label(stmt->label->get_name());
        }
        generate_statement(stmt->statement);
        break;
    }

    case ASTNodeType::CASE_STMT: {
        auto stmt = std::static_pointer_cast<CaseStmt>(node);
        // Emit the label for this case if it exists in the map
        auto it = case_labels_map.find(node);
        if (it != case_labels_map.end()) {
            emit_label(it->second);
        }
        // Process the statement associated with this case
        if (stmt->statement) {
            generate_statement(stmt->statement);
        }
        break;
    }

    case ASTNodeType::DEFAULT_STMT: {
        auto stmt = std::static_pointer_cast<DefaultStmt>(node);
        // Emit the label for default if it exists in the map
        auto it = case_labels_map.find(node);
        if (it != case_labels_map.end()) {
            emit_label(it->second);
        }
        // Process the statement associated with default
        if (stmt->statement) {
            generate_statement(stmt->statement);
        }
        break;
    }

    // Expression statements
    default:
        if (is_expression_node(node->type)) {
            generate_expression(node);
        }
        break;
    }
}

void TACGenerator::generate_block_stmt(std::shared_ptr<BlockStmt> stmt)
{
    for (auto &s : stmt->statements) {
        generate_statement(s);
    }
}

void TACGenerator::generate_if_stmt(std::shared_ptr<IfStmt> stmt)
{
    auto condition = generate_expression(stmt->condition);
    auto else_label = new_label("else");
    auto end_label = new_label("endif");

    emit_conditional_jump(TACOpcode::IF_FALSE, condition, else_label);

    generate_statement(stmt->then_branch);
    emit_goto(end_label);

    emit_label(else_label);
    if (stmt->else_branch.has_value()) {
        generate_statement(stmt->else_branch.value());
    }

    emit_label(end_label);
}

void TACGenerator::generate_while_stmt(std::shared_ptr<WhileStmt> stmt)
{
    auto cond_label = new_label("while_cond");
    auto body_label = new_label("while_body");
    auto end_label = new_label("while_end");

    loop_labels.push({end_label, cond_label});

    emit_label(cond_label);
    auto condition = generate_expression(stmt->condition);
    emit_conditional_jump(TACOpcode::IF_FALSE, condition, end_label);

    emit_label(body_label);
    generate_statement(stmt->body);
    emit_goto(cond_label);

    emit_label(end_label);
    loop_labels.pop();
}

void TACGenerator::generate_for_stmt(std::shared_ptr<ForStmt> stmt)
{
    auto cond_label = new_label("for_cond");
    auto body_label = new_label("for_body");
    auto update_label = new_label("for_update");
    auto end_label = new_label("for_end");

    loop_labels.push({end_label, update_label});

    // Initialization
    if (stmt->initializer.has_value()) {
        generate_statement(stmt->initializer.value());
    }

    // Condition
    emit_label(cond_label);
    if (stmt->condition.has_value()) {
        auto condition = generate_expression(stmt->condition.value());
        emit_conditional_jump(TACOpcode::IF_FALSE, condition, end_label);
    }

    // Body
    emit_label(body_label);
    generate_statement(stmt->body);

    // Update
    emit_label(update_label);
    if (stmt->updation.has_value()) {
        generate_expression(stmt->updation.value());
    }
    emit_goto(cond_label);

    emit_label(end_label);
    loop_labels.pop();
}

void TACGenerator::generate_return_stmt(std::shared_ptr<RetExpr> stmt)
{
    if (stmt->value.has_value()) {
        auto return_value = generate_expression(stmt->value.value());
        emit(std::make_shared<TACInstruction>(TACOpcode::RETURN,
                                              TACOperand(),
                                              return_value));
    } else {
        emit(std::make_shared<TACInstruction>(TACOpcode::RETURN));
    }
}

void TACGenerator::generate_switch_stmt(std::shared_ptr<SwitchStmt> stmt)
{
    auto expr = generate_expression(stmt->expression);
    auto end_label = new_label("switch_end");

    loop_labels.push({end_label, ""});

    case_labels_map.clear();

    // Collect case values and labels
    struct CaseInfo {
        int64_t value;
        std::string label;
        size_t index;
    };
    std::vector<CaseInfo> cases;
    std::string default_label;

    // Generate case labels and extract constant values
    for (size_t i = 0; i < stmt->cases.size(); ++i) {
        if (stmt->cases[i]->type != ASTNodeType::CASE_STMT)
            continue;
        auto case_stmt = std::static_pointer_cast<CaseStmt>(stmt->cases[i]);

        int64_t case_val = 0;
        bool is_constant = false;

        if (case_stmt->value &&
            case_stmt->value->type == ASTNodeType::LITERAL_EXPR) {
            auto lit = std::static_pointer_cast<LiteralExpr>(case_stmt->value);
            if (!lit)
                continue; // Skip if cast fails

            // Try to extract integer or char value
            if (auto *int_val = std::get_if<int64_t>(&lit->value)) {
                case_val = *int_val;
                is_constant = true;
            } else if (auto *uint_val = std::get_if<uint64_t>(&lit->value)) {
                case_val = static_cast<int64_t>(*uint_val);
                is_constant = true;
            } else if (auto *char_val = std::get_if<char>(&lit->value)) {
                case_val = static_cast<int64_t>(*char_val);
                is_constant = true;
            }
        }

        if (is_constant) {
            std::string label = new_label("case");
            cases.push_back({case_val, label, i});
            case_labels_map[stmt->cases[i]] = label;
        }
    }

    if (stmt->default_case.has_value()) {
        default_label = new_label("default");
        case_labels_map[stmt->default_case.value()] = default_label;
    }

    // Determine if we should use a jump table
    // Use jump table if:
    // 1. We have at least 3 cases
    // 2. The case values are reasonably dense (density > 0.4)
    bool use_jump_table = false;
    if (cases.size() >= 3) {
        // Sort cases by value to calculate range
        std::sort(cases.begin(),
                  cases.end(),
                  [](const CaseInfo &a, const CaseInfo &b) {
                      return a.value < b.value;
                  });

        int64_t min_val = cases.front().value;
        int64_t max_val = cases.back().value;
        int64_t range = max_val - min_val + 1;

        double density = static_cast<double>(cases.size()) / range;

        if (density > 0.4 && range <= 256) {
            use_jump_table = true;
        }
    }

    if (use_jump_table) {
        // JUMP TABLE IMPLEMENTATION
        int64_t min_val = cases.front().value;
        int64_t max_val = cases.back().value;
        int64_t range = max_val - min_val + 1;

        std::vector<std::string> jump_table(range);
        std::string fallback_label =
            default_label.empty() ? end_label : default_label;

        for (int64_t i = 0; i < range; ++i) {
            jump_table[i] = fallback_label;
        }

        for (const auto &case_info : cases) {
            int64_t index = case_info.value - min_val;
            jump_table[index] = case_info.label;
        }

        auto min_cmp_temp = new_temp(nullptr);
        auto min_cmp = TACOperand::temporary(min_cmp_temp, nullptr);
        emit(std::make_shared<TACInstruction>(
            TACOpcode::LT,
            min_cmp,
            expr,
            TACOperand::constant_int(min_val, nullptr)));
        emit(std::make_shared<TACInstruction>(
            TACOpcode::IF_TRUE,
            TACOperand(),
            min_cmp,
            TACOperand::label(fallback_label)));

        auto max_cmp_temp = new_temp(nullptr);
        auto max_cmp = TACOperand::temporary(max_cmp_temp, nullptr);
        emit(std::make_shared<TACInstruction>(
            TACOpcode::GT,
            max_cmp,
            expr,
            TACOperand::constant_int(max_val, nullptr)));
        emit(std::make_shared<TACInstruction>(
            TACOpcode::IF_TRUE,
            TACOperand(),
            max_cmp,
            TACOperand::label(fallback_label)));

        auto index_temp = new_temp(nullptr);
        auto index = TACOperand::temporary(index_temp, nullptr);
        emit(std::make_shared<TACInstruction>(
            TACOpcode::SUB,
            index,
            expr,
            TACOperand::constant_int(min_val, nullptr)));

        // Emit jump table instruction
        auto jump_instr =
            std::make_shared<TACInstruction>(TACOpcode::JUMP_TABLE,
                                             TACOperand(),
                                             index);
        jump_instr->jump_table_labels = jump_table;
        jump_instr->jump_table_min = min_val;
        jump_instr->jump_table_max = max_val;
        jump_instr->comment =
            "Jump table with " + std::to_string(cases.size()) + " cases";
        emit(jump_instr);

        for (size_t i = 0; i < stmt->cases.size(); ++i) {
            if (!stmt->cases[i])
                continue; // Skip null entries

            auto case_stmt = std::static_pointer_cast<CaseStmt>(stmt->cases[i]);

            // Find the label for this case
            std::string case_label;
            for (const auto &case_info : cases) {
                if (case_info.index == i) {
                    case_label = case_info.label;
                    break;
                }
            }

            // Only emit label if this case has a valid label (constant
            // expression)
            if (!case_label.empty()) {
                emit_label(case_label);
            }

            // Generate the case body - no automatic goto, allow fall-through
            if (case_stmt->statement) {
                generate_statement(case_stmt->statement);
            }
        }

        if (stmt->default_case.has_value()) {
            emit_label(default_label);
            auto default_stmt = std::static_pointer_cast<DefaultStmt>(
                stmt->default_case.value());
            if (default_stmt->statement) {
                generate_statement(default_stmt->statement);
            }
        }

    } else {
        // FALLBACK: IF-ELSE CHAIN IMPLEMENTATION
        std::vector<std::string> case_labels;

        for (size_t i = 0; i < stmt->cases.size(); ++i) {
            std::string label = new_label("case");
            case_labels.push_back(label);
            case_labels_map[stmt->cases[i]] = label;
        }

        for (size_t i = 0; i < stmt->cases.size(); ++i) {
            auto case_stmt = std::static_pointer_cast<CaseStmt>(stmt->cases[i]);
            if (!case_stmt->value)
                continue; // Skip if invalid

            auto case_value = generate_expression(case_stmt->value);

            auto cmp_temp = new_temp(nullptr);
            auto cmp = TACOperand::temporary(cmp_temp, nullptr);
            emit(std::make_shared<TACInstruction>(TACOpcode::EQ,
                                                  cmp,
                                                  expr,
                                                  case_value));
            emit(std::make_shared<TACInstruction>(
                TACOpcode::IF_TRUE,
                TACOperand(),
                cmp,
                TACOperand::label(case_labels[i])));
        }

        if (!default_label.empty()) {
            emit_goto(default_label);
        } else {
            emit_goto(end_label);
        }

        // Generate case bodies IN ORIGINAL ORDER to support fall-through
        for (size_t i = 0; i < stmt->cases.size(); ++i) {
            emit_label(case_labels[i]);
            if (!stmt->cases[i])
                continue; // Skip null entries

            auto case_stmt = std::static_pointer_cast<CaseStmt>(stmt->cases[i]);

            if (case_stmt->statement) {
                generate_statement(case_stmt->statement);
            }
        }

        if (stmt->default_case.has_value()) {
            emit_label(default_label);
            auto default_stmt = std::static_pointer_cast<DefaultStmt>(
                stmt->default_case.value());
            if (default_stmt->statement) {
                generate_statement(default_stmt->statement);
            }
        }
    }

    emit_label(end_label);
    loop_labels.pop();
}
