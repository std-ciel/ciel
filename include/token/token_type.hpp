#ifndef TOKEN_TYPE_HPP
#define TOKEN_TYPE_HPP

#include <optional>
#include <string>

enum class TokenType {
    INT,
    CHAR,
    BOOL,
    FLOAT,
    VOID,
    INT_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,
    BOOL_LITERAL,
    FLOAT_LITERAL,
    IDENTIFIER,
    FN,
    RETURN,
    IF,
    BREAK,
    CONTINUE,
    GOTO,
    ELSE,
    SWITCH,
    CASE,
    DEFAULT,
    FOR,
    DO,
    WHILE,
    UNTIL,
    STATIC,
    ENUM,
    STRUCT,
    UNION,
    TYPEDEF,
    CLASS,
    PUBLIC,
    PRIVATE,
    PROTECTED,

    ARROW_OP,
    DOT_OP,
    QUESTION_OP,

    ASSIGN_OP,
    PLUS_ASSIGN_OP,
    MINUS_ASSIGN_OP,
    STAR_ASSIGN_OP,
    DIVIDE_ASSIGN_OP,
    MOD_ASSIGN_OP,
    AMPERSAND_ASSIGN_OP,
    PIPE_ASSIGN_OP,
    CARET_ASSIGN_OP,
    LSHIFT_ASSIGN_OP,
    RSHIFT_ASSIGN_OP,

    LOGICAL_AND_OP,
    LOGICAL_OR_OP,
    LOGICAL_NOT_OP,
    REL_OP,
    EQ_OP,
    NE_OP,

    INCREMENT_OP,
    DECREMENT_OP,
    PLUS_OP,
    MINUS_OP,
    STAR_OP,
    DIVIDE_OP,
    MOD_OP,

    AMPERSAND_OP,
    PIPE_OP,
    CARET_OP,
    TILDE_OP,
    LSHIFT_OP,
    RSHIFT_OP,

    COMMA_OP,
    SEMICOLON_OP,
    COLON_OP,

    OPEN_PAREN_OP,
    CLOSE_PAREN_OP,
    OPEN_BRACE_OP,
    CLOSE_BRACE_OP,
    OPEN_BRACKET_OP,
    CLOSE_BRACKET_OP,
};

std::string to_string(TokenType type);
std::optional<TokenType> reserved_word(const std::string &str);
std::optional<TokenType> operator_token(const std::string &str);

#endif // TOKEN_TYPE_HPP
