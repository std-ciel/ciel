%option c++
%option noyywrap nodefault
%option yylineno
%option yyclass="Lexer"

%{
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_set>
#include "token_type.hpp"
#include "lexer_errors.hpp"
#include "tokens.hpp"
#include "lexer.hpp"

#include "parser.hpp"
#include "parser/token_converter.hpp"

using yy::Parser;

#undef YY_DECL
#define YY_DECL int Lexer::yylex(yy::Parser::semantic_type* yylval, yy::Parser::location_type* yylloc)

struct YYLTYPE {
    int first_line;
    int first_column;
};
YYLTYPE loc;

int column = 1;

#define YY_USER_ACTION \
    yylloc->step(); \
    yylloc->columns(yyleng); \
    loc.first_line = yylineno; \
    loc.first_column = column; \
    column += yyleng;

std::string string_buf;

static std::vector<Token> g_tokens;
static std::unordered_set<std::string> g_type_names;
static std::vector<LexError> g_errors;
static bool had_errors = false;
static int type_def_depth = 0;

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


{whitespace}        { /* skip whitespace */ }
{newline}           { column = 1; }

"//".*              { /* skip single line comments */ }
"/*"                { BEGIN(COMMENT); }
<COMMENT>"*/"       { BEGIN(INITIAL); }
<COMMENT>\n         { column = 1; }
<COMMENT><<EOF>>    {
                        error_handler(this, "unterminated comment at end of file");
                        BEGIN(INITIAL);
                    }
<COMMENT>.          { /* skip */ }

\"                  { BEGIN(STRING); string_buf.clear(); }

<STRING>{escaped_hexbyte}       {
                                    char out = escape_hexbyte(yytext, yyleng);
                                    string_buf.push_back(out);
                                }

<STRING>{escaped_octalbyte}     {
                                    char out = escape_octalbyte(yytext, yyleng);
                                    string_buf.push_back(out);
                                }

<STRING>\\{escapable_char}      {
                                    char out = escape_char(yytext, yyleng);
                                    string_buf.push_back(out);
                                }

<STRING>\\[^\n]                 {
                                    error_handler(this, std::string("unknown escape sequence '\\") + yytext[1] + "'");
                                    string_buf.push_back('\\');
                                    string_buf.push_back(yytext[1]);
                                }

<STRING>\"                      {
                                    BEGIN(INITIAL);
                                    add_token(string_buf, TokenType::STRING_LITERAL);
                                    yylval->emplace<std::string>(string_buf);
                                    return to_bison_token(TokenType::STRING_LITERAL);
                                }

<STRING>\n                      {
                                    error_handler(this, "string cannot span multiple lines");
                                    BEGIN(INITIAL);
                                }

<STRING><<EOF>>                 {
                                    error_handler(this, "unterminated string at end of file");
                                    BEGIN(INITIAL);
                                    string_buf.clear();
                                }

<STRING>.                       { string_buf.push_back(*yytext); }

0[xX]{hex_digit}+               {
                                    add_token(yytext, TokenType::INT_LITERAL);
                                    yylval->emplace<uint64_t>(std::stol(yytext,nullptr, 16));
                                    return to_bison_token(TokenType::INT_LITERAL);
                                }

0[bB]{binary_digit}+            {
                                    add_token(yytext, TokenType::INT_LITERAL);
                                    yylval->emplace<uint64_t>(std::stol(yytext,nullptr, 2));
                                    return to_bison_token(TokenType::INT_LITERAL);
                                }

0{octal_digit}+                 {
                                    add_token(yytext, TokenType::INT_LITERAL);
                                    yylval->emplace<uint64_t>(std::stol(yytext, nullptr, 8));
                                    return to_bison_token(TokenType::INT_LITERAL);
                                }

[1-9]{digit}*|0                 {
                                    add_token(yytext, TokenType::INT_LITERAL);
                                    yylval->emplace<uint64_t>(std::stol(yytext, nullptr, 10));
                                    return to_bison_token(TokenType::INT_LITERAL);
                                }

\'{escaped_hexbyte}\'           {
                                    char out = escape_hexbyte(yytext + 1, yyleng - 2);
                                    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
                                    yylval->emplace<char>(out);
                                    return to_bison_token(TokenType::CHAR_LITERAL);
                                }

\'{escaped_octalbyte}\'         {
                                    char out = escape_octalbyte(yytext + 1, yyleng - 2);
                                    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
                                    yylval->emplace<char>(out);
                                    return to_bison_token(TokenType::CHAR_LITERAL);
                                }

\'\\{escapable_char}\'          {
                                    char out = escape_char(yytext + 1, yyleng - 2);
                                    add_token(std::string(1, out), TokenType::CHAR_LITERAL);
                                    yylval->emplace<char>(out);
                                    return to_bison_token(TokenType::CHAR_LITERAL);
                                }

\'[^\n\\\']\'                   {
                                    add_token(std::string(1, yytext[1]), TokenType::CHAR_LITERAL);
                                    yylval->emplace<char>(yytext[1]);
                                    return to_bison_token(TokenType::CHAR_LITERAL);
                                }


{digit}+"."{digit}+             {
                                    add_token(yytext, TokenType::FLOAT_LITERAL);
                                    yylval->emplace<float>(std::stof(yytext));
                                    return to_bison_token(TokenType::FLOAT_LITERAL);
                                }

{digit}+{exponent}{digit}+      {
                                    add_token(yytext, TokenType::FLOAT_LITERAL);
                                    yylval->emplace<float>(std::stof(yytext));
                                    return to_bison_token(TokenType::FLOAT_LITERAL);
                                }

{digit}+"."{digit}+{exponent}{digit}+   {
                                            add_token(yytext, TokenType::FLOAT_LITERAL);
                                            yylval->emplace<float>(std::stof(yytext));
                                            return to_bison_token(TokenType::FLOAT_LITERAL);
                                        }

{letter}({letter}|{digit})*     {
                                    auto t = reserved_word(yytext);
                                    if (t.has_value()) {
                                        add_token(yytext, t.value());
                                        if (t.value() == TokenType::TYPEDEF)
                                            type_def_depth = 2;
                                        else if (t.value() != TokenType::STRUCT && t.value() != TokenType::UNION && t.value() != TokenType::ENUM && t.value() != TokenType::CLASS)
                                            --type_def_depth;
                                        if (t.value() == TokenType::BOOL_LITERAL) {
                                            yylval->emplace<bool>(std::string(yytext) == std::string("true"));
                                        }
                                        return to_bison_token(t.value());
                                    } else {
                                        if(type_def_depth == 1) {
                                            add_token(yytext, TokenType::TYPENAME);
                                            type_def_depth=0;
                                            g_type_names.insert(yytext);
                                            yylval->emplace<std::string>(yytext);
                                            return to_bison_token(TokenType::TYPENAME);
                                        }else{
                                            --type_def_depth;
                                            if(g_type_names.find(yytext) != g_type_names.end()) {
                                                add_token(yytext, TokenType::TYPENAME);
                                                yylval->emplace<std::string>(yytext);
                                                return to_bison_token(TokenType::TYPENAME);
                                            }
                                            add_token(yytext, TokenType::IDENTIFIER);
                                            yylval->emplace<std::string>(yytext);
                                            return to_bison_token(TokenType::IDENTIFIER);
                                        }
                                    }
                                }

[-!<>=&|+*/%~^.,;:(){}\[\]\?]   {
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
                                        return to_bison_token(t.value());
                                    } else {
                                        error_handler(this, std::string("unrecognized operator '") + op + "'");
                                    }
                                }

.                               {
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
        case 'a':  out = '\a'; break;
        case 'b':  out = '\b'; break;
        case 'f':  out = '\f'; break;
        case 'n':  out = '\n'; break;
        case 'r':  out = '\r'; break;
        case 't':  out = '\t'; break;
        case 'v':  out = '\v'; break;
        case '\\': out = '\\'; break;
        case '\'': out = '\''; break;
        case '"':  out = '"'; break;
        case '?':  out = '?'; break;
        default:   out = buf[1]; break;
    }
    return out;
}

void add_token(const std::string &value, TokenType type) {
    g_tokens.push_back(Token{value, type, loc.first_line, loc.first_column});
}

void error_handler(yyFlexLexer *lexer, const std::string &message) {
    g_errors.push_back(LexError{lexer->lineno(), loc.first_column, message});
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

int Lexer::yylex_standalone() {
    yy::Parser::semantic_type dummy_yylval;
    yy::Parser::location_type dummy_yylloc;
    return yylex(&dummy_yylval, &dummy_yylloc);
}
