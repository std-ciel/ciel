#ifndef TOKEN_TYPE_HPP
#define TOKEN_TYPE_HPP

#include <optional>
#include <string>

enum class token_type {
    INT,
    CHAR,
    BOOL,
    INT_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,
    BOOL_TRUE,
    BOOL_FALSE,
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

    ARROW_OP,
    DOT_OP,

    ASSIGN_OP,
    PLUS_ASSIGN_OP,
    MINUS_ASSIGN_OP,
    STAR_ASSIGN_OP,
    DIVIDE_ASSIGN_OP,
    MOD_ASSIGN_OP,
    LAND_ASSIGN_OP,
    LOR_ASSIGN_OP,
    BITWISE_AND_ASSIGN_OP,
    BITWISE_OR_ASSIGN_OP,
    BITWISE_XOR_ASSIGN_OP,
    LSHIFT_ASSIGN_OP,
    RSHIFT_ASSIGN_OP,

    LAND_OP,
    LOR_OP,
    LNOT_OP,
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

    BITWISE_AND_OP,
    BITWISE_OR_OP,
    BITWISE_XOR_OP,
    BITWISE_NOT_OP,
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

std::string to_string(token_type type);
std::optional<token_type> reserved_word(const std::string &str);
std::optional<token_type> operator_token(const std::string &str);

#endif // TOKEN_TYPE_HPP
