#ifndef AST_NODE_HPP
#define AST_NODE_HPP

#include "symbol_table/result.hpp"
#include "symbol_table/symbol.hpp"
#include "symbol_table/type.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

enum class Operator {
    // Arithmetic Operators
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    MODULO,
    INCREMENT,
    DECREMENT,
    POST_INCREMENT,
    POST_DECREMENT,

    // Bitwise operators
    BITWISE_AND,
    BITWISE_OR,
    BITWISE_XOR,
    BITWISE_NOT,
    LEFT_SHIFT,
    RIGHT_SHIFT,

    // Logical Operators
    LOGICAL_AND,
    LOGICAL_OR,
    LOGICAL_NOT,

    // Relational Operators
    EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    LESS_EQUAL,
    GREATER_THAN,
    GREATER_EQUAL,

    // Assignment Operators
    ASSIGN,
    ADD_ASSIGN,
    SUBTRACT_ASSIGN,
    MULTIPLY_ASSIGN,
    DIVIDE_ASSIGN,
    MODULO_ASSIGN,

    BITWISE_AND_ASSIGN,
    BITWISE_OR_ASSIGN,
    BITWISE_XOR_ASSIGN,
    LEFT_SHIFT_ASSIGN,
    RIGHT_SHIFT_ASSIGN,

    // Other Operators
    TERNARY_CONDITIONAL,
    ADDRESS_OF,
    UNARY_PLUS,
    UNARY_MINUS,
    POINTER_DEREF,
    MEMBER_ACCESS,
    MEMBER_ACCESS_PTR,
    SUBSCRIPT_OP,
    COMMA_OP,
};

inline const std::unordered_map<Operator, std::pair<std::string, std::string>>
    operator_to_string = {
        // Arithmetic Operators
        {Operator::ADD, {"+", "addition"}},
        {Operator::SUBTRACT, {"-", "subtraction"}},
        {Operator::MULTIPLY, {"*", "multiplication"}},
        {Operator::DIVIDE, {"/", "division"}},
        {Operator::MODULO, {"%", "modulo"}},
        {Operator::INCREMENT, {"++", "increment"}},
        {Operator::DECREMENT, {"--", "decrement"}},
        {Operator::POST_INCREMENT, {"++", "post-increment"}},
        {Operator::POST_DECREMENT, {"--", "post-decrement"}},

        // Bitwise operators
        {Operator::BITWISE_AND, {"&", "bitwise and"}},
        {Operator::BITWISE_OR, {"|", "bitwise or"}},
        {Operator::BITWISE_XOR, {"^", "bitwise xor"}},
        {Operator::BITWISE_NOT, {"~", "bitwise not"}},
        {Operator::LEFT_SHIFT, {"<<", "left shift"}},
        {Operator::RIGHT_SHIFT, {">>", "right shift"}},

        // Logical Operators
        {Operator::LOGICAL_AND, {"&&", "logical and"}},
        {Operator::LOGICAL_OR, {"||", "logical or"}},
        {Operator::LOGICAL_NOT, {"!", "logical not"}},

        // Relational Operators
        {Operator::EQUAL, {"==", "equal"}},
        {Operator::NOT_EQUAL, {"!=", "not equal"}},
        {Operator::LESS_THAN, {"<", "less than"}},
        {Operator::LESS_EQUAL, {"<=", "less equal"}},
        {Operator::GREATER_THAN, {">", "greater than"}},
        {Operator::GREATER_EQUAL, {">=", "greater equal"}},

        // Assignment Operators
        {Operator::ASSIGN, {"=", "assignment"}},
        {Operator::ADD_ASSIGN, {"+=", "add assignment"}},
        {Operator::SUBTRACT_ASSIGN, {"-=", "subtract assignment"}},
        {Operator::MULTIPLY_ASSIGN, {"*=", "multiply assignment"}},
        {Operator::DIVIDE_ASSIGN, {"/=", "divide assignment"}},
        {Operator::MODULO_ASSIGN, {"%=", "modulo assignment"}},
        {Operator::BITWISE_AND_ASSIGN, {"&=", "bitwise and assignment"}},
        {Operator::BITWISE_OR_ASSIGN, {"|=", "bitwise or assignment"}},
        {Operator::BITWISE_XOR_ASSIGN, {"^=", "bitwise xor assignment"}},
        {Operator::LEFT_SHIFT_ASSIGN, {"<<=", "left shift assignment"}},
        {Operator::RIGHT_SHIFT_ASSIGN, {">>=", "right shift assignment"}},

        // Other Operators
        {Operator::TERNARY_CONDITIONAL, {"?:", "ternary conditional"}},
        {Operator::ADDRESS_OF, {"&", "address of"}},
        {Operator::UNARY_PLUS, {"+", "unary plus"}},
        {Operator::UNARY_MINUS, {"-", "unary minus"}},
        {Operator::POINTER_DEREF, {"*", "pointer dereference"}},
        {Operator::MEMBER_ACCESS, {".", "member access"}},
        {Operator::MEMBER_ACCESS_PTR, {"->", "member access pointer"}},
        {Operator::COMMA_OP, {",", "comma"}},
        {Operator::SUBSCRIPT_OP, {"[]", "subscript"}},
};

inline std::string get_operator_string(Operator op)
{
    auto it = operator_to_string.find(op);
    if (it != operator_to_string.end()) {
        return it->second.first;
    }
    return "<unknown_operator>";
}

inline std::string get_operator_name(Operator op)
{
    auto it = operator_to_string.find(op);
    if (it != operator_to_string.end()) {
        return it->second.second;
    }
    return "<unknown_operator>";
}

enum class ASTNodeType {
    LITERAL_EXPR,
    IDENTIFIER_EXPR,
    ENUM_IDENTIFIER_EXPR,
    FUNCTION_IDENTIFIER_EXPR,
    THIS_EXPR,

    CALL_EXPR,
    RET_EXPR,
    CAST_EXPR,

    UNARY_EXPR,
    MEMBER_EXPR,
    BINARY_EXPR,

    TERNARY_EXPR,
    ASSIGNMENT_EXPR,

    NEW_EXPR,
    DELETE_EXPR,

    BLOCK_STMT,
    IF_STMT,
    ELSE_STMT,
    SWITCH_STMT,
    CASE_STMT,
    DEFAULT_STMT,
    FOR_STMT,
    WHILE_STMT,
    DO_WHILE_STMT,
    UNTIL_STMT,
    BREAK_STMT,
    CONTINUE_STMT,
    GOTO_STMT,
    LABEL_STMT,
    COMPOUND_STMT,
    FUNCTION_DEF,
};

class ASTNode {
  public:
    ASTNodeType type;

    ASTNode(ASTNodeType type) : type(type) {}
    virtual ~ASTNode() = default;
};

using ASTNodePtr = std::shared_ptr<ASTNode>;
using LiteralValue =
    std::variant<int64_t, uint64_t, double, char, bool, std::string>;

class LiteralExpr : public ASTNode {
  public:
    LiteralValue value;
    TypePtr expr_type;

    LiteralExpr(LiteralValue value, TypePtr expr_type)
        : ASTNode(ASTNodeType::LITERAL_EXPR), value(std::move(value)),
          expr_type(std::move(expr_type))
    {
    }
    ~LiteralExpr() override = default;
};

class ThisExpr : public ASTNode {
  public:
    TypePtr expr_type;

    ThisExpr(TypePtr expr_type)
        : ASTNode(ASTNodeType::THIS_EXPR), expr_type(std::move(expr_type))
    {
    }
    ~ThisExpr() override = default;
};

class IdentifierExpr : public ASTNode {
  public:
    SymbolPtr symbol;
    TypePtr expr_type;

    IdentifierExpr(SymbolPtr symbol, TypePtr expr_type)
        : ASTNode(ASTNodeType::IDENTIFIER_EXPR), symbol(std::move(symbol)),
          expr_type(std::move(expr_type))
    {
    }
    ~IdentifierExpr() override = default;
};

class EnumIdentifierExpr : public ASTNode {
  public:
    TypePtr expr_type;

    EnumIdentifierExpr(TypePtr expr_type)
        : ASTNode(ASTNodeType::ENUM_IDENTIFIER_EXPR),
          expr_type(std::move(expr_type))
    {
    }
    ~EnumIdentifierExpr() override = default;
};

class FunctionIdentifierExpr : public ASTNode {
  public:
    std::string function_name;

    explicit FunctionIdentifierExpr(std::string function_name)
        : ASTNode(ASTNodeType::FUNCTION_IDENTIFIER_EXPR),
          function_name(std::move(function_name))
    {
    }
    ~FunctionIdentifierExpr() override = default;
};

class CallExpr : public ASTNode {
  public:
    SymbolPtr callee;
    std::vector<ASTNodePtr> arguments;
    TypePtr expr_type;

    CallExpr(SymbolPtr callee,
             std::vector<ASTNodePtr> arguments,
             TypePtr expr_type)
        : ASTNode(ASTNodeType::CALL_EXPR), callee(std::move(callee)),
          arguments(std::move(arguments)), expr_type(std::move(expr_type))
    {
    }
    ~CallExpr() override = default;
};

class RetExpr : public ASTNode {
  public:
    std::optional<ASTNodePtr> value;
    TypePtr expr_type;

    RetExpr(std::optional<ASTNodePtr> value, TypePtr expr_type)
        : ASTNode(ASTNodeType::RET_EXPR), value(std::move(value)),
          expr_type(std::move(expr_type))
    {
    }
    ~RetExpr() override = default;
};

class CastExpr : public ASTNode {
  public:
    TypePtr target_type;
    ASTNodePtr expression;
    TypePtr expr_type;

    CastExpr(TypePtr target_type, ASTNodePtr expression)
        : ASTNode(ASTNodeType::CAST_EXPR), target_type(std::move(target_type)),
          expression(std::move(expression))
    {
        expr_type = this->target_type;
    }
    ~CastExpr() override = default;
};

class UnaryExpr : public ASTNode {
  public:
    Operator op;
    ASTNodePtr operand;
    TypePtr expr_type;

    UnaryExpr(Operator op, ASTNodePtr operand, TypePtr expr_type)
        : ASTNode(ASTNodeType::UNARY_EXPR), op(op), operand(std::move(operand)),
          expr_type(std::move(expr_type))
    {
    }
    ~UnaryExpr() override = default;
};

class MemberExpr : public ASTNode {
  public:
    Operator op; // Either MEMBER_ACCESS (.) or MEMBER_ACCESS_PTR (->)
    ASTNodePtr object;
    std::string member_name;
    TypePtr expr_type;

    MemberExpr(Operator op,
               ASTNodePtr object,
               std::string member_name,
               TypePtr expr_type)
        : ASTNode(ASTNodeType::MEMBER_EXPR), op(op), object(std::move(object)),
          member_name(std::move(member_name)), expr_type(std::move(expr_type))
    {
    }
    ~MemberExpr() override = default;
};

class BinaryExpr : public ASTNode {
  public:
    Operator op;
    ASTNodePtr left;
    ASTNodePtr right;
    TypePtr expr_type;

    BinaryExpr(Operator op,
               ASTNodePtr left,
               ASTNodePtr right,
               TypePtr expr_type)
        : ASTNode(ASTNodeType::BINARY_EXPR), op(op), left(std::move(left)),
          right(std::move(right)), expr_type(std::move(expr_type))
    {
    }
    ~BinaryExpr() override = default;
};

class TernaryExpr : public ASTNode {
  public:
    ASTNodePtr condition;
    ASTNodePtr true_branch;
    ASTNodePtr false_branch;
    TypePtr expr_type;

    TernaryExpr(ASTNodePtr condition,
                ASTNodePtr true_branch,
                ASTNodePtr false_branch,
                TypePtr expr_type)
        : ASTNode(ASTNodeType::TERNARY_EXPR), condition(std::move(condition)),
          true_branch(std::move(true_branch)),
          false_branch(std::move(false_branch)), expr_type(std::move(expr_type))
    {
    }
    ~TernaryExpr() override = default;
};

class AssignmentExpr : public ASTNode {
  public:
    Operator op;
    ASTNodePtr target;
    ASTNodePtr value;
    TypePtr expr_type;

    AssignmentExpr(Operator op,
                   ASTNodePtr target,
                   ASTNodePtr value,
                   TypePtr expr_type)
        : ASTNode(ASTNodeType::ASSIGNMENT_EXPR), op(op),
          target(std::move(target)), value(std::move(value)),
          expr_type(std::move(expr_type))
    {
    }
    ~AssignmentExpr() override = default;
};

class NewExpr : public ASTNode {
  public:
    TypePtr allocated_type;
    TypePtr expr_type;

    NewExpr(TypePtr allocated_type, TypePtr expr_type)
        : ASTNode(ASTNodeType::NEW_EXPR),
          allocated_type(std::move(allocated_type)),
          expr_type(std::move(expr_type))
    {
    }
    ~NewExpr() override = default;
};

class DeleteExpr : public ASTNode {
  public:
    ASTNodePtr operand;
    TypePtr expr_type;

    DeleteExpr(ASTNodePtr operand, TypePtr expr_type)
        : ASTNode(ASTNodeType::DELETE_EXPR), operand(std::move(operand)),
          expr_type(std::move(expr_type))
    {
    }
    ~DeleteExpr() override = default;
};

class BlockStmt : public ASTNode {
  public:
    std::vector<ASTNodePtr> statements;

    explicit BlockStmt(std::vector<ASTNodePtr> statements)
        : ASTNode(ASTNodeType::BLOCK_STMT), statements(std::move(statements))
    {
    }
    ~BlockStmt() override = default;
};

class IfStmt : public ASTNode {
  public:
    ASTNodePtr condition;
    ASTNodePtr then_branch;
    std::optional<ASTNodePtr> else_branch;

    IfStmt(ASTNodePtr condition,
           ASTNodePtr then_branch,
           std::optional<ASTNodePtr> else_branch = std::nullopt)
        : ASTNode(ASTNodeType::IF_STMT), condition(std::move(condition)),
          then_branch(std::move(then_branch)),
          else_branch(std::move(else_branch))
    {
    }
    ~IfStmt() override = default;
};

class ElseStmt : public ASTNode {
  public:
    ASTNodePtr body;

    explicit ElseStmt(ASTNodePtr body)
        : ASTNode(ASTNodeType::ELSE_STMT), body(std::move(body))
    {
    }
    ~ElseStmt() override = default;
};

class SwitchStmt : public ASTNode {
  public:
    ASTNodePtr expression;
    std::vector<ASTNodePtr> cases;
    std::optional<ASTNodePtr> default_case;

    SwitchStmt(ASTNodePtr expression,
               std::vector<ASTNodePtr> cases,
               std::optional<ASTNodePtr> default_case = std::nullopt)
        : ASTNode(ASTNodeType::SWITCH_STMT), expression(std::move(expression)),
          cases(std::move(cases)), default_case(std::move(default_case))
    {
    }
    ~SwitchStmt() override = default;
};

class CaseStmt : public ASTNode {
  public:
    ASTNodePtr value;
    ASTNodePtr statement;

    CaseStmt(ASTNodePtr value, ASTNodePtr statement)
        : ASTNode(ASTNodeType::CASE_STMT), value(std::move(value)),
          statement(std::move(statement))
    {
    }
    ~CaseStmt() override = default;
};

class DefaultStmt : public ASTNode {
  public:
    ASTNodePtr statement;

    explicit DefaultStmt(ASTNodePtr statement)
        : ASTNode(ASTNodeType::DEFAULT_STMT), statement(std::move(statement))
    {
    }
    ~DefaultStmt() override = default;
};

class ForStmt : public ASTNode {
  public:
    std::optional<ASTNodePtr> initializer;
    std::optional<ASTNodePtr> condition;
    std::optional<ASTNodePtr> updation;
    ASTNodePtr body;

    ForStmt(std::optional<ASTNodePtr> initializer,
            std::optional<ASTNodePtr> condition,
            std::optional<ASTNodePtr> updation,
            ASTNodePtr body)
        : ASTNode(ASTNodeType::FOR_STMT), initializer(std::move(initializer)),
          condition(std::move(condition)), updation(std::move(updation)),
          body(std::move(body))
    {
    }
    ~ForStmt() override = default;
};

class WhileStmt : public ASTNode {
  public:
    ASTNodePtr condition;
    ASTNodePtr body;

    WhileStmt(ASTNodePtr condition, ASTNodePtr body)
        : ASTNode(ASTNodeType::WHILE_STMT), condition(std::move(condition)),
          body(std::move(body))
    {
    }
    ~WhileStmt() override = default;
};

class DoWhileStmt : public ASTNode {
  public:
    ASTNodePtr body;
    ASTNodePtr condition;

    DoWhileStmt(ASTNodePtr body, ASTNodePtr condition)
        : ASTNode(ASTNodeType::DO_WHILE_STMT), body(std::move(body)),
          condition(std::move(condition))
    {
    }
    ~DoWhileStmt() override = default;
};

class UntilStmt : public ASTNode {
  public:
    ASTNodePtr condition;
    ASTNodePtr body;

    UntilStmt(ASTNodePtr condition, ASTNodePtr body)
        : ASTNode(ASTNodeType::UNTIL_STMT), condition(std::move(condition)),
          body(std::move(body))
    {
    }
    ~UntilStmt() override = default;
};

class BreakStmt : public ASTNode {
  public:
    BreakStmt() : ASTNode(ASTNodeType::BREAK_STMT) {}
    ~BreakStmt() override = default;
};

class ContinueStmt : public ASTNode {
  public:
    ContinueStmt() : ASTNode(ASTNodeType::CONTINUE_STMT) {}
    ~ContinueStmt() override = default;
};

class GotoStmt : public ASTNode {
  public:
    SymbolPtr target_label;

    explicit GotoStmt(SymbolPtr target_label)
        : ASTNode(ASTNodeType::GOTO_STMT), target_label(std::move(target_label))
    {
    }
    ~GotoStmt() override = default;
};

class LabelStmt : public ASTNode {
  public:
    SymbolPtr label;
    ASTNodePtr statement;

    LabelStmt(SymbolPtr label, ASTNodePtr statement)
        : ASTNode(ASTNodeType::LABEL_STMT), label(std::move(label)),
          statement(std::move(statement))
    {
    }
    ~LabelStmt() override = default;
};

class CompoundStmt : public ASTNode {
  public:
    std::vector<ASTNodePtr> statements;

    explicit CompoundStmt(std::vector<ASTNodePtr> statements)
        : ASTNode(ASTNodeType::COMPOUND_STMT), statements(std::move(statements))
    {
    }
    ~CompoundStmt() override = default;
};

class FunctionDef : public ASTNode {
  public:
    SymbolPtr function_symbol;
    TypePtr return_type;
    std::vector<SymbolPtr> parameters;
    ASTNodePtr body;

    FunctionDef(SymbolPtr function_symbol,
                TypePtr return_type,
                std::vector<SymbolPtr> parameters,
                ASTNodePtr body)
        : ASTNode(ASTNodeType::FUNCTION_DEF),
          function_symbol(std::move(function_symbol)),
          return_type(std::move(return_type)),
          parameters(std::move(parameters)), body(std::move(body))
    {
    }
    ~FunctionDef() override = default;
};

inline bool is_expression_node(ASTNodeType type)
{
    switch (type) {
    case ASTNodeType::LITERAL_EXPR:
    case ASTNodeType::IDENTIFIER_EXPR:
    case ASTNodeType::THIS_EXPR:
    case ASTNodeType::CALL_EXPR:
    case ASTNodeType::RET_EXPR:
    case ASTNodeType::CAST_EXPR:
    case ASTNodeType::UNARY_EXPR:
    case ASTNodeType::BINARY_EXPR:
    case ASTNodeType::TERNARY_EXPR:
    case ASTNodeType::ASSIGNMENT_EXPR:
    case ASTNodeType::NEW_EXPR:
    case ASTNodeType::DELETE_EXPR:
    case ASTNodeType::MEMBER_EXPR:
    case ASTNodeType::ENUM_IDENTIFIER_EXPR:
    case ASTNodeType::FUNCTION_IDENTIFIER_EXPR:
        return true;
    default:
        return false;
    }
}

enum class ExpressionTypeError {
    NullNode,
    NotExpression,
};

inline Result<TypePtr, ExpressionTypeError>
get_expression_type(const ASTNodePtr &node)
{
    if (!node) {
        return ExpressionTypeError::NullNode;
    }

    switch (node->type) {
    case ASTNodeType::LITERAL_EXPR:
        return static_cast<LiteralExpr *>(node.get())->expr_type;
    case ASTNodeType::IDENTIFIER_EXPR:
        return static_cast<IdentifierExpr *>(node.get())->expr_type;
    case ASTNodeType::THIS_EXPR:
        return static_cast<ThisExpr *>(node.get())->expr_type;
    case ASTNodeType::CALL_EXPR:
        return static_cast<CallExpr *>(node.get())->expr_type;
    case ASTNodeType::RET_EXPR:
        return static_cast<RetExpr *>(node.get())->expr_type;
    case ASTNodeType::CAST_EXPR:
        return static_cast<CastExpr *>(node.get())->expr_type;
    case ASTNodeType::UNARY_EXPR:
        return static_cast<UnaryExpr *>(node.get())->expr_type;
    case ASTNodeType::BINARY_EXPR:
        return static_cast<BinaryExpr *>(node.get())->expr_type;
    case ASTNodeType::TERNARY_EXPR:
        return static_cast<TernaryExpr *>(node.get())->expr_type;
    case ASTNodeType::ASSIGNMENT_EXPR:
        return static_cast<AssignmentExpr *>(node.get())->expr_type;
    case ASTNodeType::NEW_EXPR:
        return static_cast<NewExpr *>(node.get())->expr_type;
    case ASTNodeType::DELETE_EXPR:
        return static_cast<DeleteExpr *>(node.get())->expr_type;
    case ASTNodeType::MEMBER_EXPR:
        return static_cast<MemberExpr *>(node.get())->expr_type;
    case ASTNodeType::ENUM_IDENTIFIER_EXPR:
        return static_cast<EnumIdentifierExpr *>(node.get())->expr_type;
    default:
        return ExpressionTypeError::NotExpression;
    }
}

#endif // AST_NODE_HPP
