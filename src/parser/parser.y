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

%token IDENTIFIER
%token INT_LITERAL FLOAT_LITERAL CHAR_LITERAL STRING_LITERAL BOOL_LITERAL

%token INT SIGNED UNSIGNED CHAR BOOL FLOAT VOID
%token TYPEDEF STATIC CONST VOLATILE
%token ENUM STRUCT UNION CLASS
%token RETURN IF ELSE SWITCH CASE DEFAULT FOR WHILE DO GOTO CONTINUE BREAK UNTIL
%token NEW DELETE
%token PUBLIC PRIVATE PROTECTED

%token TYPE_NAME

%token ARROW_OP        /* -> */
%token DOT_OP          /* .  */
%token QUESTION_OP     /* ? */
%token ELLIPSIS_OP     /* ... */

%token ASSIGN_OP           /* = */
%token PLUS_ASSIGN_OP      /* += */
%token MINUS_ASSIGN_OP     /* -= */
%token STAR_ASSIGN_OP      /* *= */
%token DIVIDE_ASSIGN_OP    /* /= */
%token MOD_ASSIGN_OP       /* %= */
%token AMPERSAND_ASSIGN_OP /* &= */
%token PIPE_ASSIGN_OP      /* |= */
%token CARET_ASSIGN_OP     /* ^= */
%token LSHIFT_ASSIGN_OP    /* <<= */
%token RSHIFT_ASSIGN_OP    /* >>= */

%token LOGICAL_AND_OP  /* && */
%token LOGICAL_OR_OP   /* || */
%token LOGICAL_NOT_OP  /* ! */

%token EQ_OP           /* == */
%token NE_OP           /* != */
%token REL_OP          /* < or > or <= or >= ; lexer may encode as REL_OP with lexeme */

%token INCREMENT_OP    /* ++ */
%token DECREMENT_OP    /* -- */

%token PLUS_OP
%token MINUS_OP
%token STAR_OP
%token DIVIDE_OP
%token MOD_OP

%token AMPERSAND_OP
%token PIPE_OP
%token CARET_OP
%token TILDE_OP

%token LSHIFT_OP
%token RSHIFT_OP

%token COMMA_OP
%token SEMICOLON_OP
%token COLON_OP

%token OPEN_PAREN_OP
%token CLOSE_PAREN_OP
%token OPEN_BRACE_OP
%token CLOSE_BRACE_OP
%token OPEN_BRACKET_OP
%token CLOSE_BRACKET_OP

%token OPERATOR     /* 'operator' keyword for operator overload declarations */

// %nonassoc LOWER_THAN_COMMA

%left COMMA_OP

%right ASSIGN_OP PLUS_ASSIGN_OP MINUS_ASSIGN_OP STAR_ASSIGN_OP DIVIDE_ASSIGN_OP MOD_ASSIGN_OP
%right AMPERSAND_ASSIGN_OP PIPE_ASSIGN_OP CARET_ASSIGN_OP LSHIFT_ASSIGN_OP RSHIFT_ASSIGN_OP

%right QUESTION_OP /* ternary as right-assoc: a ? b : c (we'll implement with explicit rule) */

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

%right UNARY /* unary + - ! ~ * & sizeof (sizeof removed) - mark unary precedence */
// %nonassoc POSTFIX_PREC  /* postfix ++ --, call, indexing has highest precedence */
// %right CAST_PREC

%start translation_unit

%%

primary_expression
    : IDENTIFIER
    // | TYPE_NAME
    | INT_LITERAL
    | FLOAT_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
    | OPEN_PAREN_OP expression CLOSE_PAREN_OP
    ;

postfix_expression
    : primary_expression
    | postfix_expression OPEN_BRACKET_OP expression CLOSE_BRACKET_OP
    | postfix_expression OPEN_PAREN_OP CLOSE_PAREN_OP
    | postfix_expression OPEN_PAREN_OP argument_expression_list CLOSE_PAREN_OP
    | postfix_expression DOT_OP IDENTIFIER
    | postfix_expression ARROW_OP IDENTIFIER
    | postfix_expression INCREMENT_OP
    | postfix_expression DECREMENT_OP
    | OPEN_PAREN_OP type_name CLOSE_PAREN_OP OPEN_BRACE_OP initializer_list CLOSE_BRACE_OP
    ;

argument_expression_list
    : assignment_expression
    | argument_expression_list COMMA_OP assignment_expression
    ;

unary_expression
    : postfix_expression
    | INCREMENT_OP unary_expression
    | DECREMENT_OP unary_expression
    | LOGICAL_NOT_OP cast_expression         /* '!' */
    | TILDE_OP cast_expression               /* '~' */
    | AMPERSAND_OP cast_expression           /* address-of */
    | STAR_OP cast_expression                /* deref */
    | PLUS_OP cast_expression %prec UNARY
    | MINUS_OP cast_expression %prec UNARY
    | NEW type_name                          /* new T or new T[expr] handled later in semantic phase */
    | DELETE unary_expression
    | OPEN_PAREN_OP type_name CLOSE_PAREN_OP cast_expression
    ;

cast_expression
    : unary_expression
    ;

multiplicative_expression
    : cast_expression
    | multiplicative_expression STAR_OP cast_expression
    | multiplicative_expression DIVIDE_OP cast_expression
    | multiplicative_expression MOD_OP cast_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression PLUS_OP multiplicative_expression
    | additive_expression MINUS_OP multiplicative_expression
    ;

shift_expression
    : additive_expression
    | shift_expression LSHIFT_OP additive_expression
    | shift_expression RSHIFT_OP additive_expression
    ;

relational_expression
    : shift_expression
    | relational_expression REL_OP shift_expression
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    ;

and_expression
    : equality_expression
    | and_expression AMPERSAND_OP equality_expression
    ;

exclusive_or_expression
    : and_expression
    | exclusive_or_expression CARET_OP and_expression
    ;

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression PIPE_OP exclusive_or_expression
    ;

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression LOGICAL_AND_OP inclusive_or_expression
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression LOGICAL_OR_OP logical_and_expression
    ;

conditional_expression
    : logical_or_expression
    | logical_or_expression QUESTION_OP expression COLON_OP conditional_expression
    ;

assignment_expression
    : conditional_expression
    | unary_expression ASSIGN_OP assignment_expression
    | unary_expression PLUS_ASSIGN_OP assignment_expression
    | unary_expression MINUS_ASSIGN_OP assignment_expression
    | unary_expression STAR_ASSIGN_OP assignment_expression
    | unary_expression DIVIDE_ASSIGN_OP assignment_expression
    | unary_expression MOD_ASSIGN_OP assignment_expression
    | unary_expression AMPERSAND_ASSIGN_OP assignment_expression
    | unary_expression PIPE_ASSIGN_OP assignment_expression
    | unary_expression CARET_ASSIGN_OP assignment_expression
    | unary_expression LSHIFT_ASSIGN_OP assignment_expression
    | unary_expression RSHIFT_ASSIGN_OP assignment_expression
    ;

expression
    : assignment_expression
    | expression COMMA_OP assignment_expression
    ;

constant_expression
    : conditional_expression
    ;

declaration
    : declaration_specifiers SEMICOLON_OP
    | declaration_specifiers init_declarator_list SEMICOLON_OP
	| TYPEDEF type_specifier pointer_opt TYPE_NAME SEMICOLON_OP
    ;

declaration_specifiers
    : storage_class_specifier declaration_specifiers
    | storage_class_specifier
    | type_specifier declaration_specifiers
    | type_specifier
    | type_qualifier declaration_specifiers
    | type_qualifier
    ;

storage_class_specifier
    : STATIC
    // | TYPEDEF
   
    ;

init_declarator_list
    : init_declarator
    | init_declarator_list COMMA_OP init_declarator
    ;

init_declarator
    : declarator ASSIGN_OP initializer
    | declarator
    ;

type_specifier
    : VOID
    | CHAR
    | INT
    | SIGNED
    | UNSIGNED
    | BOOL
    | FLOAT
    | struct_or_union_specifier
    | enum_specifier
    | CLASS class_specifier_tail   /* class used as a type */
    | TYPE_NAME
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
    | struct_or_union OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
    | struct_or_union IDENTIFIER
	| struct_or_union IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP // to allow empty structs
    ;

struct_or_union
    : STRUCT
    | UNION
    ;

struct_declaration_list
    : struct_declaration
    | struct_declaration_list struct_declaration
    ;

struct_declaration
    : specifier_qualifier_list SEMICOLON_OP
    | specifier_qualifier_list struct_declarator_list SEMICOLON_OP
    ;

specifier_qualifier_list
    : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    ;

struct_declarator_list
    : struct_declarator
    | struct_declarator_list COMMA_OP struct_declarator
    ;

struct_declarator
    : declarator
    | declarator COLON_OP constant_expression
    | COLON_OP constant_expression
    ;

enum_specifier
    : ENUM IDENTIFIER OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
    | ENUM OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
    | ENUM IDENTIFIER
    ;

enumerator_list
    : enumerator
    | enumerator_list COMMA_OP enumerator
    ;

enumerator
    : IDENTIFIER
    | IDENTIFIER ASSIGN_OP constant_expression
    ;

type_qualifier
    : CONST
    | VOLATILE
    ;

declarator
    : pointer direct_declarator
    | direct_declarator
    ;

direct_declarator
    : IDENTIFIER
    | OPEN_PAREN_OP declarator CLOSE_PAREN_OP
    | direct_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
    | direct_declarator OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    | direct_declarator OPEN_PAREN_OP CLOSE_PAREN_OP
    | direct_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
    ;

// pointer
//     : STAR_OP 
//     | STAR_OP pointer
//     | STAR_OP type_qualifier_list
//     | STAR_OP type_qualifier_list pointer
//     ;

pointer
    : STAR_OP type_qualifier_list_opt pointer_opt
    ;

type_qualifier_list_opt
    : /* empty */
    | type_qualifier_list
    ;

pointer_opt
    : /* empty */
    | pointer
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;

parameter_type_list
    : parameter_list
    | parameter_list COMMA_OP ELLIPSIS_OP
    ;

parameter_list
    : parameter_declaration
    | parameter_list COMMA_OP parameter_declaration
    ;

parameter_declaration
    : declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers
    ;

abstract_declarator
    : pointer direct_abstract_declarator
    | pointer
    | direct_abstract_declarator
    ;

direct_abstract_declarator
    : OPEN_PAREN_OP abstract_declarator CLOSE_PAREN_OP
    | OPEN_BRACKET_OP CLOSE_BRACKET_OP
    | OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    | direct_abstract_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
    | direct_abstract_declarator OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    | OPEN_PAREN_OP CLOSE_PAREN_OP
    | OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
    | direct_abstract_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
    ;

type_name
    : specifier_qualifier_list abstract_declarator
    | specifier_qualifier_list
    ;

initializer
    : assignment_expression
    | OPEN_BRACE_OP initializer_list CLOSE_BRACE_OP
    | OPEN_BRACE_OP initializer_list COMMA_OP CLOSE_BRACE_OP
    ;

initializer_list
    : designation initializer
    | initializer
    | initializer_list COMMA_OP designation initializer
    | initializer_list COMMA_OP initializer
    ;

designation
    : designator_list ASSIGN_OP
    ;

designator_list
    : designator
    | designator_list designator
    ;

designator
    : OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    | DOT_OP IDENTIFIER
    ;

statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
	| declaration   // to allow declarations as statements for gotos
    ;

labeled_statement
    : IDENTIFIER COLON_OP statement            /* user labels / goto target */
    | CASE constant_expression COLON_OP statement
    | DEFAULT COLON_OP statement
    ;

compound_statement
    : OPEN_BRACE_OP CLOSE_BRACE_OP
    | OPEN_BRACE_OP block_item_list CLOSE_BRACE_OP
    ;

block_item_list
    : block_item
    | block_item_list block_item
    ;

block_item
    : statement
    ;

expression_statement
    : SEMICOLON_OP
    | expression SEMICOLON_OP
    ;

selection_statement
    : IF OPEN_PAREN_OP expression CLOSE_PAREN_OP statement ELSE statement
    | IF OPEN_PAREN_OP expression CLOSE_PAREN_OP statement
    | SWITCH OPEN_PAREN_OP expression CLOSE_PAREN_OP statement
    ;

iteration_statement
    : WHILE OPEN_PAREN_OP expression CLOSE_PAREN_OP statement
	| UNTIL OPEN_PAREN_OP expression CLOSE_PAREN_OP statement	
    | DO statement WHILE OPEN_PAREN_OP expression CLOSE_PAREN_OP SEMICOLON_OP
    | FOR OPEN_PAREN_OP expression_statement expression_statement CLOSE_PAREN_OP statement
    | FOR OPEN_PAREN_OP expression_statement expression_statement expression CLOSE_PAREN_OP statement
    | FOR OPEN_PAREN_OP declaration expression_statement CLOSE_PAREN_OP statement
    | FOR OPEN_PAREN_OP declaration expression_statement expression CLOSE_PAREN_OP statement
    ;

jump_statement
    : GOTO IDENTIFIER SEMICOLON_OP
    | CONTINUE SEMICOLON_OP
    | BREAK SEMICOLON_OP
    | RETURN SEMICOLON_OP
    | RETURN expression SEMICOLON_OP
    ;

translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : declaration_specifiers declarator compound_statement
    | declaration_specifiers declarator declaration_list compound_statement
    ;

declaration_list
    : declaration
    | declaration_list declaration
    ;

class_specifier_tail
    : inheritance_opt OPEN_BRACE_OP class_member_list CLOSE_BRACE_OP
    | IDENTIFIER inheritance_opt OPEN_BRACE_OP class_member_list CLOSE_BRACE_OP
	| IDENTIFIER inheritance_opt
	| inheritance_opt OPEN_BRACE_OP CLOSE_BRACE_OP // to allow empty classes
    ;

inheritance_opt
	: /* empty */
	| COLON_OP access_specifier IDENTIFIER
	;

class_member_list
    : /* empty */
    | class_member_list class_member
    ;

class_member
    : access_specifier COLON_OP
    | declaration                               /* data member or typedef inside class */
    | function_declaration_or_definition        /* method (declaration or definition) */
    ;

access_specifier
    : PUBLIC
    | PRIVATE
    | PROTECTED
    ;

function_declaration_or_definition
    : declaration_specifiers declarator SEMICOLON_OP
    | declaration_specifiers declarator compound_statement
    | /* constructor */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
    | /* constructor without params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
    | /* destructor */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
    | /* operator overload */ type_specifier OPERATOR operator_token sequence_function_decl // TODO: ensure return type matches class
    ;

operator_token
    : PLUS_OP
    | MINUS_OP
    | STAR_OP
    | DIVIDE_OP
    | MOD_OP
    | AMPERSAND_OP
    | PIPE_OP
    | CARET_OP
    | EQ_OP
    | NE_OP
    | LSHIFT_OP
    | RSHIFT_OP
    | INCREMENT_OP
    | DECREMENT_OP
    | OPEN_PAREN_OP CLOSE_PAREN_OP
    ;

sequence_function_decl
    : OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
    | OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP
    ;
%%

void yy::Parser::error(const location_type& loc, const std::string& msg) {
    std::cerr << "Parse error at " << loc << ": " << msg << '\n';
}
