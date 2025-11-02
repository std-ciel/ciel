#ifndef TAC_GENERATOR_HPP
#define TAC_GENERATOR_HPP

#include "ast/ast_node.hpp"
#include "common/result.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include "tac/tac.hpp"
#include "tac/tac_errors.hpp"
#include <stack>
#include <unordered_map>

class TACGenerator {
  private:
    TACProgram program;
    TACFunctionPtr current_function;
    TACBasicBlockPtr current_block;

    // References to global instances
    SymbolTable &symbol_table;
    TypeFactory &type_factory;

    std::vector<TACErrorInfo> errors;

    // Stack for loop labels (for break/continue)
    std::stack<std::pair<std::string, std::string>>
        loop_labels; // (break_label, continue_label)

    std::unordered_map<ASTNodePtr, std::string> case_labels_map;

    // Counter for string literals
    int string_literal_counter = 0;

    // Helper methods
    TACOperand generate_expression(ASTNodePtr node);
    TACOperand generate_binary_op(std::shared_ptr<BinaryExpr> expr);
    TACOperand generate_unary_op(std::shared_ptr<UnaryExpr> expr);
    TACOperand generate_assignment(std::shared_ptr<AssignmentExpr> expr);
    TACOperand generate_call(std::shared_ptr<CallExpr> expr);
    TACOperand generate_ternary(std::shared_ptr<TernaryExpr> expr);

    void generate_statement(ASTNodePtr node);
    void generate_if_stmt(std::shared_ptr<IfStmt> stmt);
    void generate_while_stmt(std::shared_ptr<WhileStmt> stmt);
    void generate_for_stmt(std::shared_ptr<ForStmt> stmt);
    void generate_return_stmt(std::shared_ptr<RetExpr> stmt);
    void generate_block_stmt(std::shared_ptr<BlockStmt> stmt);
    void generate_switch_stmt(std::shared_ptr<SwitchStmt> stmt);

    void record_error(const std::string &message);

    TACOpcode operator_to_tac_opcode(Operator op);

    void emit(TACInstructionPtr instr);
    void emit_label(const std::string &label);
    void emit_goto(const std::string &label);
    void emit_conditional_jump(TACOpcode opcode,
                               const TACOperand &condition,
                               const std::string &label);

    std::string new_temp(TypePtr type = nullptr);
    std::string new_label(const std::string &prefix = "L");

  public:
    TACGenerator();

    void generate_function(std::shared_ptr<FunctionDef> func_def);
    void generate_global_declaration(ASTNodePtr node);

    // NEW: Generate TAC for entire translation unit
    Result<bool, std::vector<TACErrorInfo>>
    generate(const std::vector<ASTNodePtr> &translation_unit);

    const TACProgram &get_program() const
    {
        return program;
    }
    TACProgram &get_program()
    {
        return program;
    }

    const std::vector<TACErrorInfo> &get_errors() const
    {
        return errors;
    }

    TACOperand generate_member_access(const TACOperand &base_object,
                                      const std::string &member_name,
                                      TypePtr base_type);

    void generate_member_store(const TACOperand &base_object,
                               const std::string &member_name,
                               TypePtr base_type,
                               const TACOperand &value);

    void generate_designated_member_store(
        const TACOperand &base_object,
        TypePtr base_type,
        const std::vector<std::string> &member_path,
        const TACOperand &value);

    TACOperand generate_initializer_value(ASTNodePtr value_node,
                                          TypePtr target_type);

    void generate_aggregate_copy(const TACOperand &target,
                                 const TACOperand &source,
                                 TypePtr aggregate_type);

    void generate_array_copy(const TACOperand &target_base,
                             const TACOperand &source_base,
                             const std::string &array_field_name,
                             TypePtr base_type,
                             TypePtr array_type);

    template <typename AggregateFn, typename ArrayFn, typename PrimitiveFn>
    void copy_field(const TACOperand &target,
                    const TACOperand &source_field,
                    const TypePtr &base_type,
                    const TypePtr &field_type,
                    const std::string &field_name,
                    AggregateFn &&handle_aggregate,
                    ArrayFn &&handle_array,
                    PrimitiveFn &&handle_primitive)
    {
        if (!field_type)
            return;

        const auto make_member_op = [&](auto &&getter) -> TACOperand {
            size_t offset = getter().value_or(0);
            return TACOperand::member_access(target,
                                             field_name,
                                             offset,
                                             field_type);
        };

        if (field_type->kind == TypeKind::RECORD ||
            field_type->kind == TypeKind::CLASS) {
            handle_aggregate(
                make_member_op([&] {
                    if (auto rec =
                            std::dynamic_pointer_cast<RecordType>(base_type))
                        return get_member_offset(rec, field_name);
                    if (auto cls =
                            std::dynamic_pointer_cast<ClassType>(base_type))
                        return get_class_member_offset(cls, field_name);
                    return std::optional<size_t>{};
                }),
                source_field,
                field_type);
        } else if (field_type->kind == TypeKind::ARRAY) {
            handle_array(target,
                         source_field,
                         field_name,
                         base_type,
                         field_type);
        } else {
            handle_primitive(target, field_name, base_type, source_field);
        }
    }
};

#endif // TAC_GENERATOR_HPP
