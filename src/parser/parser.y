%skeleton "lalr1.cc"
%require "3.8"
%define api.namespace {yy}
%define api.parser.class {Parser}
%define api.value.type variant
%define parse.error verbose
%locations


%code requires {
  #include <string>
  #include <vector>
  #include <memory>

  class Lexer;
}

%parse-param { Lexer& lexer }

%code {
  #include <iostream>
  #include "lexer.hpp"

  #undef yylex
  #define yylex lexer.yylex
}


/* ----- Token declarations (mirror TokenType enum) ----- */
// Type keywords
%token INT CHAR BOOL FLOAT VOID

// Control flow keywords
%token RETURN IF BREAK CONTINUE GOTO ELSE SWITCH CASE DEFAULT FOR DO WHILE UNTIL

// Storage and type keywords
%token STATIC ENUM STRUCT UNION TYPEDEF CLASS PUBLIC PRIVATE PROTECTED NEW DELETE

// Operators
%token ARROW_OP DOT_OP QUESTION_OP ELLIPSIS_OP
%token ASSIGN_OP PLUS_ASSIGN_OP MINUS_ASSIGN_OP STAR_ASSIGN_OP DIVIDE_ASSIGN_OP MOD_ASSIGN_OP
%token AMPERSAND_ASSIGN_OP PIPE_ASSIGN_OP CARET_ASSIGN_OP LSHIFT_ASSIGN_OP RSHIFT_ASSIGN_OP
%token LOGICAL_AND_OP LOGICAL_OR_OP LOGICAL_NOT_OP REL_OP EQ_OP NE_OP
%token INCREMENT_OP DECREMENT_OP PLUS_OP MINUS_OP STAR_OP DIVIDE_OP MOD_OP
%token AMPERSAND_OP PIPE_OP CARET_OP TILDE_OP LSHIFT_OP RSHIFT_OP
%token COMMA_OP SEMICOLON_OP COLON_OP
%token OPEN_PAREN_OP CLOSE_PAREN_OP OPEN_BRACE_OP CLOSE_BRACE_OP OPEN_BRACKET_OP CLOSE_BRACKET_OP

// Tokens with semantic values
%token <int>          INT_LITERAL
%token <double>       FLOAT_LITERAL
%token <char>         CHAR_LITERAL
%token <std::string>  STRING_LITERAL
%token <std::string>  IDENTIFIER
%token <bool>         BOOL_LITERAL

// Define semantic types for non-terminals (example - will be expanded later)
%type <int> expr


// Operator precedence and associativity
%precedence THEN
%precedence ELSE

%right ASSIGN_OP PLUS_ASSIGN_OP MINUS_ASSIGN_OP STAR_ASSIGN_OP DIVIDE_ASSIGN_OP MOD_ASSIGN_OP AMPERSAND_ASSIGN_OP PIPE_ASSIGN_OP CARET_ASSIGN_OP LSHIFT_ASSIGN_OP RSHIFT_ASSIGN_OP
%left LOGICAL_OR_OP
%left LOGICAL_AND_OP
%left PIPE_OP
%left CARET_OP
%left AMPERSAND_OP
%left EQ_OP NE_OP
%left REL_OP
%left LSHIFT_OP RSHIFT_OP
%left PLUS_OP MINUS_OP
%left STAR_OP DIVIDE_OP MOD_OP
%precedence UMINUS UPLUS USTAR UAMPERSAND UTILDE ULOGICAL_NOT
%left ARROW_OP DOT_OP
%left OPEN_BRACKET_OP
%left OPEN_PAREN_OP

%start program

%%

// Sample grammar for demonstration purposes
program
  : /* empty */                   {  }
  | program expr SEMICOLON_OP     {  }
  ;
expr
  : INT_LITERAL                    {  }
  | expr PLUS_OP expr              {  }
  | expr MINUS_OP expr             {  }
  | OPEN_PAREN_OP expr CLOSE_PAREN_OP {  }
  ;

%%

void yy::Parser::error(const location_type& loc, const std::string& msg) {
    std::cerr << "Parse error at " << loc << ": " << msg << '\n';
}
