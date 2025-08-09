#include "token_type.hpp"
#include <unordered_map>

static const std::unordered_map<std::string, token_type> keyword_map = {
    {"int", token_type::INT},
    {"char", token_type::CHAR},
    {"bool", token_type::BOOL},
    {"void", token_type::VOID},
    {"fn", token_type::FN},
    {"return", token_type::RETURN},
    {"if", token_type::IF},
    {"break", token_type::BREAK},
    {"continue", token_type::CONTINUE},
    {"goto", token_type::GOTO},
    {"else", token_type::ELSE},
    {"switch", token_type::SWITCH},
    {"case", token_type::CASE},
    {"default", token_type::DEFAULT},
    {"for", token_type::FOR},
    {"do", token_type::DO},
    {"while", token_type::WHILE},
    {"until", token_type::UNTIL},
    {"static", token_type::STATIC},
    {"enum", token_type::ENUM},
    {"struct", token_type::STRUCT},
    {"union", token_type::UNION},
    {"typedef", token_type::TYPEDEF},
    {"true", token_type::BOOL_TRUE},
    {"false", token_type::BOOL_FALSE}};

static const std::unordered_map<std::string, token_type> operator_map = {

    {"->", token_type::ARROW_OP},
    {".", token_type::DOT_OP},
    {"?", token_type::QUESTION_OP},

    {"=", token_type::ASSIGN_OP},
    {"+=", token_type::PLUS_ASSIGN_OP},
    {"-=", token_type::MINUS_ASSIGN_OP},
    {"*=", token_type::STAR_ASSIGN_OP},
    {"/=", token_type::DIVIDE_ASSIGN_OP},
    {"%=", token_type::MOD_ASSIGN_OP},
    {"&&=", token_type::LAND_ASSIGN_OP},
    {"||=", token_type::LOR_ASSIGN_OP},
    {"&=", token_type::AMPERSAND_ASSIGN_OP},
    {"|=", token_type::PIPE_ASSIGN_OP},
    {"^=", token_type::CARET_ASSIGN_OP},
    {"<<=", token_type::LSHIFT_ASSIGN_OP},
    {">>=", token_type::RSHIFT_ASSIGN_OP},

    {"&&", token_type::LAND_OP},
    {"||", token_type::LOR_OP},
    {"!", token_type::LNOT_OP},
    {"==", token_type::EQ_OP},
    {"!=", token_type::NE_OP},

    {"<", token_type::REL_OP},
    {">", token_type::REL_OP},
    {"<=", token_type::REL_OP},
    {">=", token_type::REL_OP},

    {"++", token_type::INCREMENT_OP},
    {"--", token_type::DECREMENT_OP},

    {"+", token_type::PLUS_OP},
    {"-", token_type::MINUS_OP},
    {"*", token_type::STAR_OP},
    {"/", token_type::DIVIDE_OP},
    {"%", token_type::MOD_OP},

    {"&", token_type::AMPERSAND_OP},
    {"|", token_type::PIPE_OP},
    {"^", token_type::CARET_OP},
    {"~", token_type::TILDE_OP},
    {"<<", token_type::LSHIFT_OP},
    {">>", token_type::RSHIFT_OP},

    {",", token_type::COMMA_OP},
    {";", token_type::SEMICOLON_OP},
    {":", token_type::COLON_OP},

    {"(", token_type::OPEN_PAREN_OP},
    {")", token_type::CLOSE_PAREN_OP},

    {"{", token_type::OPEN_BRACE_OP},
    {"}", token_type::CLOSE_BRACE_OP},

    {"[", token_type::OPEN_BRACKET_OP},
    {"]", token_type::CLOSE_BRACKET_OP}};

static const std::unordered_map<token_type, std::string> token_to_string_map = {
    {token_type::INT, "int"},
    {token_type::CHAR, "char"},
    {token_type::BOOL, "bool"},
    {token_type::VOID, "void"},
    {token_type::BOOL_TRUE, "true"},
    {token_type::BOOL_FALSE, "false"},
    {token_type::INT_LITERAL, "int_literal"},
    {token_type::CHAR_LITERAL, "char_literal"},
    {token_type::STRING_LITERAL, "string_literal"},
    {token_type::IDENTIFIER, "identifier"},
    {token_type::FN, "fn"},
    {token_type::RETURN, "return"},
    {token_type::IF, "if"},
    {token_type::BREAK, "break"},
    {token_type::CONTINUE, "continue"},
    {token_type::GOTO, "goto"},
    {token_type::ELSE, "else"},
    {token_type::SWITCH, "switch"},
    {token_type::CASE, "case"},
    {token_type::DEFAULT, "default"},
    {token_type::FOR, "for"},
    {token_type::DO, "do"},
    {token_type::WHILE, "while"},
    {token_type::UNTIL, "until"},
    {token_type::STATIC, "static"},
    {token_type::ENUM, "enum"},
    {token_type::STRUCT, "struct"},
    {token_type::UNION, "union"},
    {token_type::TYPEDEF, "typedef"},

    {token_type::ARROW_OP, "arrow_op"},
    {token_type::DOT_OP, "dot_op"},
    {token_type::QUESTION_OP, "question_op"},

    {token_type::ASSIGN_OP, "assign_op"},
    {token_type::PLUS_ASSIGN_OP, "plus_assign_op"},
    {token_type::MINUS_ASSIGN_OP, "minus_assign_op"},
    {token_type::STAR_ASSIGN_OP, "star_assign_op"},
    {token_type::DIVIDE_ASSIGN_OP, "divide_assign_op"},
    {token_type::MOD_ASSIGN_OP, "mod_assign_op"},
    {token_type::LAND_ASSIGN_OP, "land_assign_op"},
    {token_type::LOR_ASSIGN_OP, "lor_assign_op"},
    {token_type::AMPERSAND_ASSIGN_OP, "ampersand_assign_op"},
    {token_type::PIPE_ASSIGN_OP, "pipe_assign_op"},
    {token_type::CARET_ASSIGN_OP, "caret_assign_op"},
    {token_type::LSHIFT_ASSIGN_OP, "lshift_assign_op"},
    {token_type::RSHIFT_ASSIGN_OP, "rshift_assign_op"},

    {token_type::LAND_OP, "land_op"},
    {token_type::LOR_OP, "lor_op"},
    {token_type::LNOT_OP, "lnot_op"},
    {token_type::REL_OP, "rel_op"},
    {token_type::EQ_OP, "eq_op"},
    {token_type::NE_OP, "ne_op"},

    {token_type::INCREMENT_OP, "increment_op"},
    {token_type::DECREMENT_OP, "decrement_op"},
    {token_type::PLUS_OP, "plus_op"},
    {token_type::MINUS_OP, "minus_op"},
    {token_type::STAR_OP, "star_op"},
    {token_type::DIVIDE_OP, "divide_op"},
    {token_type::MOD_OP, "mod_op"},

    {token_type::AMPERSAND_OP, "ampersand_op"},
    {token_type::PIPE_OP, "pipe_op"},
    {token_type::CARET_OP, "caret_op"},
    {token_type::TILDE_OP, "tilde_op"},
    {token_type::LSHIFT_OP, "lshift_op"},
    {token_type::RSHIFT_OP, "rshift_op"},

    {token_type::COMMA_OP, "comma_op"},
    {token_type::SEMICOLON_OP, "semicolon_op"},
    {token_type::COLON_OP, "colon_op"},

    {token_type::OPEN_PAREN_OP, "open_paren_op"},
    {token_type::CLOSE_PAREN_OP, "close_paren_op"},

    {token_type::OPEN_BRACE_OP, "open_brace_op"},
    {token_type::CLOSE_BRACE_OP, "close_brace_op"},

    {token_type::OPEN_BRACKET_OP, "open_bracket_op"},
    {token_type::CLOSE_BRACKET_OP, "close_bracket_op"}};

std::string to_string(token_type type)
{
    auto it = token_to_string_map.find(type);
    if (it != token_to_string_map.end()) {
        return it->second;
    }
    return "UNKNOWN";
}

std::optional<token_type> reserved_word(const std::string &str)
{

    auto it = keyword_map.find(str);
    if (it != keyword_map.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<token_type> operator_token(const std::string &str)
{
    auto it = operator_map.find(str);
    if (it != operator_map.end()) {
        return it->second;
    }
    return std::nullopt;
}
