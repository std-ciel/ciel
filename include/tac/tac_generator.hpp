#ifndef TAC_GENERATOR_HPP
#define TAC_GENERATOR_HPP

#include "ast/ast_node.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include "tac/tac.hpp"
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

    // Stack for loop labels (for break/continue)
    std::stack<std::pair<std::string, std::string>>
        loop_labels; // (break_label, continue_label)

    std::unordered_map<ASTNodePtr, std::string> case_labels_map;

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

    TACOpcode operator_to_tac_opcode(Operator op);

    void emit(TACInstructionPtr instr);
    void emit_label(const std::string &label);
    void emit_goto(const std::string &label);

    std::string new_temp(TypePtr type = nullptr);
    std::string new_label(const std::string &prefix = "L");

  public:
    TACGenerator();

    void generate_function(std::shared_ptr<FunctionDef> func_def);
    void generate_global_declaration(ASTNodePtr node);

    // NEW: Generate TAC for entire translation unit
    void generate(const std::vector<ASTNodePtr> &translation_unit);

    const TACProgram &get_program() const
    {
        return program;
    }
    TACProgram &get_program()
    {
        return program;
    }

    TACOperand generate_member_access(const TACOperand &base_object,
                                      const std::string &member_name,
                                      TypePtr base_type);

    void generate_member_store(const TACOperand &base_object,
                               const std::string &member_name,
                               TypePtr base_type,
                               const TACOperand &value);
};

#endif // TAC_GENERATOR_HPP
