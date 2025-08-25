%option c++
%option noyywrap nodefault
%option yylineno

%{
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "token_type.hpp"
#include "lexer_errors.hpp"
#include "tokens.hpp"

struct YYLTYPE {
    int first_line;
    int first_column;
};
YYLTYPE yylloc;

int column = 1;

#define YY_USER_ACTION \
    yylloc.first_line = yylineno; \
    yylloc.first_column = column; \
    column += yyleng;


std::string string_buf;

static std::vector<Token> g_tokens;
static std::vector<LexError> g_errors;
static bool had_errors = false;

static char escape_char(char *buf, size_t length);

static char escape_hexbyte(char *buf, size_t length);

static char escape_octalbyte(char *buf, size_t length);

void add_token(const std::string &value, TokenType type);

void error_handler(yyFlexLexer *lexer, const std::string &message);

%}

%x STRING
%x COMMENT

letter              [a-zA-Z_]
digit               [0-9]
hex_digit           [0-9a-fA-F]
octal_digit         [0-7]
binary_digit        [01]
escapable_char      [\\abfnrtv'"?]
escaped_hexbyte     \\[xX]{hex_digit}{1,2}
escaped_octalbyte   \\{octal_digit}{1,3}
whitespace          [ \t\r]+
newline             \n
exponent            [Ee][+-]?

%%

{whitespace}   { /* skip whitespace */ }
{newline}      { column = 1; }

"//".*         { /* skip single line comments */ }
"/*"           { BEGIN(COMMENT); }
<COMMENT>"*/"  { BEGIN(INITIAL); }
<COMMENT>\n    { column = 1; }
<COMMENT><<EOF>> {
    error_handler(this, "unterminated comment at end of file");
    BEGIN(INITIAL);
}
<COMMENT>.     { /* skip */ }


\"             { BEGIN(STRING); string_buf.clear(); }

<STRING>{escaped_hexbyte} {
    char out = escape_hexbyte(yytext, yyleng);
    string_buf.push_back(out);
}

<STRING>{escaped_octalbyte} {
    char out = escape_octalbyte(yytext, yyleng);
    string_buf.push_back(out);
}

<STRING>\\{escapable_char} {
    char out = escape_char(yytext, yyleng);
    string_buf.push_back(out);
}

<STRING>\\[^\n] {
    error_handler(this, std::string("unknown escape sequence '\\") + yytext[1] + "'");
    string_buf.push_back('\\');
    string_buf.push_back(yytext[1]);
}

<STRING>\"  {
    BEGIN(INITIAL);
    add_token(string_buf, TokenType::STRING_LITERAL);
}
<STRING>\n  {
    error_handler(this, "string cannot span multiple lines");
    BEGIN(INITIAL);
}

<STRING><<EOF>> {
    error_handler(this, "unterminated string at end of file");
    BEGIN(INITIAL);
    string_buf.clear();
}

<STRING>.   { string_buf.push_back(*yytext); }

0[xX]{hex_digit}+       {
    add_token(yytext, TokenType::INT_LITERAL);
}

0[bB]{binary_digit}+    {
    add_token(yytext, TokenType::INT_LITERAL);
}

0{octal_digit}+         {
    add_token(yytext, TokenType::INT_LITERAL);
}

[1-9]{digit}*|0         {
    add_token(yytext, TokenType::INT_LITERAL);
}

\'{escaped_hexbyte}\' {
    char out = escape_hexbyte(yytext + 1, yyleng - 2);
    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
}

\'{escaped_octalbyte}\'  {
    char out = escape_octalbyte(yytext + 1, yyleng - 2);
    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
}

\'\\{escapable_char}\'    {
    char out = escape_char(yytext + 1, yyleng - 2);
    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
}

\'[^\n\\\']\' {
    add_token(std::string(1, yytext[1]), TokenType::CHAR_LITERAL);
}

{digit}+"."{digit}+  {
    add_token(yytext, TokenType::FLOAT_LITERAL);
}

{digit}+{exponent}{digit}+ {
    add_token(yytext, TokenType::FLOAT_LITERAL);
}

{digit}+"."{digit}+{exponent}{digit}+ {
    add_token(yytext, TokenType::FLOAT_LITERAL);
}


{letter}({letter}|{digit})* {
    auto t = reserved_word(yytext);
    if (t.has_value()) {
        add_token(yytext, t.value());
    } else {
        add_token(yytext, TokenType::IDENTIFIER);
    }
}

[-!<>=&|+*/%~^.,;:(){}\[\]\?] {
    std::string op;
    op.push_back(yytext[0]);
    int extra = 0;

    int c1 = yyinput();
    if (c1 != 0 && c1 != EOF) {
        char ch1 = static_cast<char>(c1);
        op.push_back(ch1);

        int c2 = yyinput();
        if (c2 != 0 && c2 != EOF) {
            char ch2 = static_cast<char>(c2);
            op.push_back(ch2);
            if (auto t3 = operator_token(op); t3.has_value()) {
                extra = 2;
            } else {
                unput(op.back());
                op.pop_back();
            }
        }
        if (extra == 0) {
            if (auto t2 = operator_token(op); t2.has_value()) {
                extra = 1;
            } else {
                unput(op.back());
                op.pop_back();
            }
        }
    }

    column += extra;
    if (auto t = operator_token(op); t.has_value()) {
        add_token(op, t.value());
    } else {
        error_handler(this, std::string("unrecognized operator '") + op + "'");
    }
}


.   {
    error_handler(this, std::string("unrecognized character '") + yytext + "'");
}

%%

static char escape_hexbyte(char *buf, size_t length) {
    unsigned int v = 0;
    for (size_t i = 2; i < length; ++i) {
        char c = buf[i];
        v = (v << 4) + (c >= '0' && c <= '9' ? (c - '0')
            : (c >= 'a' && c <= 'f' ? (c - 'a' + 10)
            : (c - 'A' + 10)));
    }
    return static_cast<char>(v & 0xFFu);
}

static char escape_octalbyte(char *buf, size_t length) {
    unsigned int v = 0;
    for (size_t i = 1; i < length; ++i) {
        char c = buf[i];
        v = (v << 3) + (c >= '0' && c <= '7' ? (c - '0') : 0);
    }
    return static_cast<char>(v & 0xFFu);
}

static char escape_char(char *buf, size_t length) {
    char out = 0;
    switch (buf[1]) {
        case 'a': out = '\a'; break;
        case 'b': out = '\b'; break;
        case 'f': out = '\f'; break;
        case 'n': out = '\n'; break;
        case 'r': out = '\r'; break;
        case 't': out = '\t'; break;
        case 'v': out = '\v'; break;
        case '\\': out = '\\'; break;
        case '\'': out = '\''; break;
        case '"': out = '"'; break;
        case '?': out = '?'; break;
        default:  out = buf[1]; break;
    }
    return out;
}
void add_token(const std::string &value, TokenType type) {
    g_tokens.push_back(Token{value, type, yylloc.first_line, yylloc.first_column});
}

void error_handler(yyFlexLexer *lexer, const std::string &message) {
    g_errors.push_back(LexError{lexer->lineno(), yylloc.first_column, message});
    had_errors = true;
}

const std::vector<LexError> &lexer_get_errors() {
    return g_errors;
}

void lexer_clear_errors() {
    g_errors.clear();
    had_errors = false;
}

int lexer_had_errors() {
    return had_errors ? 1 : 0;
}

const std::vector<Token> &lexer_get_tokens() {
    return g_tokens;
}

void lexer_clear_tokens() {
    g_tokens.clear();
}

std::vector<std::pair<std::string, TokenType>> lexer_get_lexeme_token_pairs() {
    std::vector<std::pair<std::string, TokenType>> out;
    out.reserve(g_tokens.size());
    for (const auto &t : g_tokens) {
        out.emplace_back(t.lexeme, t.type);
    }
    return out;
}
