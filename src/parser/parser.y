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


%token INT CHAR BOOL FLOAT VOID

%token RETURN IF BREAK CONTINUE GOTO ELSE SWITCH CASE DEFAULT FOR DO WHILE UNTIL

%token STATIC ENUM STRUCT UNION TYPEDEF CLASS PUBLIC PRIVATE PROTECTED NEW DELETE

%token CONST VOLATILE UNSIGNED SIGNED

%token ARROW_OP DOT_OP QUESTION_OP ELLIPSIS_OP
%token ASSIGN_OP PLUS_ASSIGN_OP MINUS_ASSIGN_OP STAR_ASSIGN_OP DIVIDE_ASSIGN_OP MOD_ASSIGN_OP
%token AMPERSAND_ASSIGN_OP PIPE_ASSIGN_OP CARET_ASSIGN_OP LSHIFT_ASSIGN_OP RSHIFT_ASSIGN_OP
%token LOGICAL_AND_OP LOGICAL_OR_OP LOGICAL_NOT_OP REL_OP EQ_OP NE_OP
%token INCREMENT_OP DECREMENT_OP PLUS_OP MINUS_OP STAR_OP DIVIDE_OP MOD_OP
%token AMPERSAND_OP PIPE_OP CARET_OP TILDE_OP LSHIFT_OP RSHIFT_OP
%token COMMA_OP SEMICOLON_OP COLON_OP
%token OPEN_PAREN_OP CLOSE_PAREN_OP OPEN_BRACE_OP CLOSE_BRACE_OP OPEN_BRACKET_OP CLOSE_BRACKET_OP

%token <int>          INT_LITERAL
%token <double>       FLOAT_LITERAL
%token <char>         CHAR_LITERAL
%token <std::string>  STRING_LITERAL
%token <std::string>  IDENTIFIER
%token <bool>         BOOL_LITERAL


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
%left ARROW_OP DOT_OP
%left OPEN_BRACKET_OP
%left OPEN_PAREN_OP

%start translation_unit

%%

primary_expression
	: IDENTIFIER
	| constant
	| string
	| OPEN_PAREN_OP expression CLOSE_PAREN_OP
	;

constant
	: INT_LITERAL		/* includes character_constant */
	| FLOAT_LITERAL
    | CHAR_LITERAL
    | BOOL_LITERAL
	;


string
	: STRING_LITERAL
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
	| OPEN_PAREN_OP type_name CLOSE_PAREN_OP OPEN_BRACE_OP initializer_list COMMA_OP CLOSE_BRACE_OP
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list COMMA_OP assignment_expression
	;

unary_expression
	: postfix_expression
	| INCREMENT_OP unary_expression
	| DECREMENT_OP unary_expression
	| unary_operator cast_expression
	| new_expression
	| delete_expression
	;

new_expression
	: NEW type_name
	| NEW type_name OPEN_PAREN_OP argument_expression_list CLOSE_PAREN_OP
	| NEW type_name OPEN_PAREN_OP CLOSE_PAREN_OP
	| NEW OPEN_PAREN_OP type_name CLOSE_PAREN_OP
	| NEW OPEN_BRACKET_OP assignment_expression CLOSE_BRACKET_OP type_name
	;

delete_expression
	: DELETE cast_expression
	| DELETE OPEN_BRACKET_OP CLOSE_BRACKET_OP cast_expression
	;

unary_operator
	: AMPERSAND_OP
	| STAR_OP
	| PLUS_OP
	| MINUS_OP
	| TILDE_OP
	| LOGICAL_NOT_OP
	;

cast_expression
	: unary_expression
	| OPEN_PAREN_OP type_name CLOSE_PAREN_OP cast_expression
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
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: ASSIGN_OP
	| STAR_ASSIGN_OP
	| DIVIDE_ASSIGN_OP
	| MOD_ASSIGN_OP
	| PLUS_ASSIGN_OP
	| MINUS_ASSIGN_OP
	| LSHIFT_ASSIGN_OP
	| RSHIFT_ASSIGN_OP
	| AMPERSAND_ASSIGN_OP
	| CARET_ASSIGN_OP
	| PIPE_ASSIGN_OP
	;

expression
	: assignment_expression
	| expression COMMA_OP assignment_expression
	;

constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers SEMICOLON_OP
	| declaration_specifiers init_declarator_list SEMICOLON_OP
	| user_defined_type_declaration
	;

user_defined_type_declaration
	: IDENTIFIER init_declarator_list SEMICOLON_OP
	| IDENTIFIER SEMICOLON_OP
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers
	| storage_class_specifier
	| type_specifier declaration_specifiers
	| type_specifier
	| type_qualifier declaration_specifiers
	| type_qualifier
	;

init_declarator_list
	: init_declarator
	| init_declarator_list COMMA_OP init_declarator
	;

init_declarator
	: declarator ASSIGN_OP initializer
	| declarator
	;

storage_class_specifier
	: TYPEDEF	/* identifiers must be flagged as TYPEDEF_NAME */
	| STATIC
	;

type_specifier
	: VOID
	| CHAR
	| INT
	| FLOAT
	| UNSIGNED
	| SIGNED
	| BOOL
	| struct_specifier
	| union_specifier
	| class_specifier
	| enum_specifier
	;

struct_specifier
	: STRUCT OPEN_BRACE_OP struct_member_list CLOSE_BRACE_OP
	| STRUCT IDENTIFIER OPEN_BRACE_OP struct_member_list CLOSE_BRACE_OP
	| STRUCT IDENTIFIER
    | STRUCT IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP
	;

union_specifier
	: UNION OPEN_BRACE_OP union_member_list CLOSE_BRACE_OP
	| UNION IDENTIFIER OPEN_BRACE_OP union_member_list CLOSE_BRACE_OP
	| UNION IDENTIFIER
    | UNION IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP
	;

class_specifier
	: CLASS OPEN_BRACE_OP class_member_list CLOSE_BRACE_OP
	| CLASS IDENTIFIER OPEN_BRACE_OP class_member_list CLOSE_BRACE_OP
	| CLASS IDENTIFIER
    | CLASS IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP
	;

type_qualifier
    : CONST
	| VOLATILE
	;

access_specifier
	: PUBLIC
	| PRIVATE
	| PROTECTED
	;

struct_member_list
	: /* empty */
	  struct_member
	| struct_member_list struct_member
	;

struct_member
	: access_specifier COLON_OP
	| member_declaration
	| method_declaration
	| constructor_declaration
	| destructor_declaration
	;

union_member_list
	: /* empty */
	  union_member
	| union_member_list union_member
	;

union_member
	: member_declaration
	;

class_member_list
	: /* empty */
	  class_member
	| class_member_list class_member
	;

class_member
	: access_specifier COLON_OP
	| member_declaration
	| method_declaration
	| constructor_declaration
	| destructor_declaration
	;

member_declaration
	: specifier_qualifier_list SEMICOLON_OP	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list SEMICOLON_OP
	;

method_declaration
	: type_specifier IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
	| type_specifier IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
	| type_specifier IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP  /* declaration only */
	| type_specifier IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP  /* declaration only */
	;

constructor_declaration
	: IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
	| IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
	| IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP  /* declaration only */
	| IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP  /* declaration only */
	;

destructor_declaration
	: TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
	| TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP  /* declaration only */
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
	: COLON_OP constant_expression
	| declarator COLON_OP constant_expression
	| declarator
	;

enum_specifier
	: ENUM OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
	| ENUM OPEN_BRACE_OP enumerator_list COMMA_OP CLOSE_BRACE_OP
	| ENUM IDENTIFIER OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
	| ENUM IDENTIFIER OPEN_BRACE_OP enumerator_list COMMA_OP CLOSE_BRACE_OP
	| ENUM IDENTIFIER
    | ENUM IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP  
	;

enumerator_list:
	  enumerator
	| enumerator_list COMMA_OP enumerator
	;

enumerator:
    IDENTIFIER ASSIGN_OP constant_expression
  | IDENTIFIER
  ;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| OPEN_PAREN_OP declarator CLOSE_PAREN_OP
	| direct_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP STAR_OP CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP STATIC assignment_expression CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP assignment_expression CLOSE_BRACKET_OP
	| direct_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
	| direct_declarator OPEN_PAREN_OP CLOSE_PAREN_OP
	| direct_declarator OPEN_PAREN_OP identifier_list CLOSE_PAREN_OP
	| direct_declarator OPEN_BRACKET_OP STATIC type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP type_qualifier_list STAR_OP CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP type_qualifier_list STATIC assignment_expression CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| direct_declarator OPEN_BRACKET_OP type_qualifier_list CLOSE_BRACKET_OP
	;

pointer
    :
	  STAR_OP type_qualifier_list pointer
	| STAR_OP type_qualifier_list
	| STAR_OP pointer
	| STAR_OP
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

parameter_type_list
	: parameter_list COMMA_OP ELLIPSIS_OP
	| parameter_list
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

identifier_list
	: IDENTIFIER
	| identifier_list COMMA_OP IDENTIFIER
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: OPEN_PAREN_OP abstract_declarator CLOSE_PAREN_OP
	| OPEN_BRACKET_OP CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP STAR_OP CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP STATIC assignment_expression CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP assignment_expression CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP STAR_OP CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP STATIC assignment_expression CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP assignment_expression CLOSE_BRACKET_OP
	| OPEN_PAREN_OP CLOSE_PAREN_OP
	| OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
	| direct_abstract_declarator OPEN_PAREN_OP CLOSE_PAREN_OP
	| direct_abstract_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
	| OPEN_BRACKET_OP STATIC type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP type_qualifier_list STATIC assignment_expression CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| OPEN_BRACKET_OP type_qualifier_list CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP STATIC type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP type_qualifier_list assignment_expression CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP type_qualifier_list STATIC assignment_expression CLOSE_BRACKET_OP
	| direct_abstract_declarator OPEN_BRACKET_OP type_qualifier_list CLOSE_BRACKET_OP
	;

initializer
	: OPEN_BRACE_OP initializer_list CLOSE_BRACE_OP
	| OPEN_BRACE_OP initializer_list COMMA_OP CLOSE_BRACE_OP
	| assignment_expression
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
	;

labeled_statement
	: IDENTIFIER COLON_OP statement
	| CASE constant_expression COLON_OP statement
	| DEFAULT COLON_OP statement
	;

compound_statement
	: OPEN_BRACE_OP CLOSE_BRACE_OP
	| OPEN_BRACE_OP  block_item_list CLOSE_BRACE_OP
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
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
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;

declaration_list
	: declaration
	| declaration_list declaration
	;


%%

void yy::Parser::error(const location_type& loc, const std::string& msg) {
    std::cerr << "Parse error at " << loc << ": " << msg << '\n';
}
