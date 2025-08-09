%option c++
%option noyywrap nodefault
%option yylineno

%{
#include <iostream>
#include <string>
#include <iomanip>
#include <sstream>
#include "token_type.hpp"

#define YY_USER_ACTION \
    yylloc.first_line = yylineno; \
    yylloc.first_column = column; \
    column += yyleng;

struct YYLTYPE {
    int first_line;
    int first_column;
};
YYLTYPE yylloc;

int column = 1;

std::string string_buf;

static const int COL_VALUE_W = 32;
static const int COL_TYPE_W  = 20;
static const int COL_POS_W   = 12;

static bool header_printed = false;

static std::string escape_for_table(const std::string& in) {
    std::string out;
    out.reserve(in.size());
    for (char c : in) {
        switch (c) {
            case '\n': out += "\\n"; break;
            case '\t': out += "\\t"; break;
            case '\r': out += "\\r"; break;
            case '\\': out += "\\\\"; break;
            case '\'': out += "\\'"; break;
            case '\"': out += "\\\""; break;
            default:   out += c; break;
        }
    }
    return out;
}

static void print_header() {
    using std::left;
    using std::setw;
    std::cout << left << setw(COL_VALUE_W) << "VALUE"
              << " "  << left << setw(COL_TYPE_W)  << "TOKEN"
              << " "  << left << setw(COL_POS_W)   << "POSITION"
              << "\n";
    std::cout << std::string(COL_VALUE_W + 1 + COL_TYPE_W + 1 + COL_POS_W, '-') << "\n";
}

void print_token(const std::string& value, token_type type);

void error_handler(yyFlexLexer* lexer, const std::string& message, bool fatal = true);

%}

%x STRING
%x COMMENT

letter     [a-zA-Z_]
digit      [0-9]
hex_digit  [0-9a-fA-F]
octal_digit [0-7]
binary_digit [01]
whitespace [ \t\r]+
newline    \n

%%


{whitespace}   { column += yyleng; }
{newline}      { column = 1; }

"//".*         { /* skip single line comments */ }
"/*"           { BEGIN(COMMENT); }
<COMMENT>"*/"  { BEGIN(INITIAL); }
<COMMENT>\n    { column = 1; }
<COMMENT>.     { /* skip */ }


\"             { BEGIN(STRING); string_buf.clear(); }
<STRING>\\n    { string_buf.push_back('\n'); }
<STRING>\\t    { string_buf.push_back('\t'); }
<STRING>\\\"   { string_buf.push_back('\"'); }
<STRING>\"     {
                  BEGIN(INITIAL);
                  print_token(string_buf, token_type::STRING_LITERAL);
               }
<STRING>\n     { error_handler(this, "unterminated string"); }
<STRING>.      { string_buf.push_back(*yytext); }

(0{octal_digit}+|0[xX]{hex_digit}+|0[bB]{binary_digit}+|[1-9]{digit}*|0)       {
                  print_token(yytext, token_type::INT_LITERAL);
               }

\'[^\\'\n]\'  {
                  print_token(std::string(1, yytext[1]), token_type::CHAR_LITERAL);
               }


{letter}({letter}|{digit})* {
                  auto t = reserved_word(yytext);
                  if(t.has_value()) {
                      print_token(yytext, t.value());
                  }else{
                      print_token(yytext, token_type::IDENTIFIER);
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
        print_token(op, t.value());
    } else {
        error_handler(this, std::string("unrecognized operator '") + op + "'");
    }
}


.              { error_handler(this, std::string("unrecognized character '") + yytext + "'"); }

%%

void print_token(const std::string& value, token_type type) {
    if (!header_printed) {
        print_header();
        header_printed = true;
    }

    std::ostringstream pos;
    pos << yylloc.first_line << ":" << yylloc.first_column;

    const std::string disp = "'" + escape_for_table(value) + "'";

    std::cout << std::left << std::setw(COL_VALUE_W) << disp
              << " "        << std::left << std::setw(COL_TYPE_W) << to_string(type)
              << " "        << std::left << std::setw(COL_POS_W)  << pos.str()
              << "\n";
}

void error_handler(yyFlexLexer* lexer, const std::string& message, bool fatal) {
    std::cerr << "Error: " << message << " at line " << lexer->lineno();
    if (yylloc.first_column > 0) {
        std::cerr << ":" << yylloc.first_column;
    }
    std::cerr << "\n";
    if (fatal) {
        exit(1);
    }
}
