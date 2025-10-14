%skeleton "lalr1.cc"

%require "3.8"

%define api.namespace {yy}
%define api.parser.class {Parser}
%define api.value.type variant
%define parse.error verbose

%locations
%debug
%verbose

%code requires {
  #include <string>
  #include <vector>
  #include <memory>
  #include <optional>
  #include <variant>
  #include <unordered_set>
  #include <unordered_map>

  #include "ast/ast_node.hpp"
  #include "symbol_table/type.hpp"
  #include "symbol_table/symbol.hpp"
  #include "symbol_table/mangling.hpp"
  #include "parser/parser_errors.hpp"


  class Lexer;

  struct DeclaratorInfo {
    std::string name;
    size_t pointer_levels = 0;
    std::vector<size_t> array_dims;
    bool is_function = false;
    bool has_params = false;
    std::vector<QualifiedType> param_types;
    std::vector<std::string> param_names;
    bool is_variadic = false;
  };

  struct ParamDeclInfo {
    QualifiedType type;
    std::string name;
  };

  struct ParamListInfo {
    std::vector<QualifiedType> types;
    std::vector<std::string> names;
    bool variadic = false;
  };

  enum class ContextKind { GLOBAL, BLOCK, STRUCT, UNION, CLASS, ENUM, FUNCTION, SWITCH, LOOP };

  struct GlobalParserState {
    std::vector<ContextKind> ctx_stack = {ContextKind::GLOBAL};
    Access current_access = Access::PRIVATE;
    QualifiedType current_decl_base_type;
    std::vector<Qualifier> current_decl_qualifiers;
	  Access inherited_access = Access::PRIVATE;
	  ClassTypePtr parent_class_type = nullptr;
    ClassTypePtr current_class_type = nullptr;

    std::unordered_map<std::string, std::vector<ASTNodePtr>> unresolved_labels;

    // For switch-case tracking
    std::vector<std::vector<ASTNodePtr>> case_stmt_stack;
    std::vector<std::optional<ASTNodePtr>> default_case;
    std::vector<TypePtr> switch_subject_stack;

    StorageClass current_storage = StorageClass::STATIC;

    // Track forward declarations that need definitions
    std::unordered_set<std::string> forward_declared_types;
    std::unordered_set<std::string> defined_types;
    std::unordered_map<std::string, std::pair<int, int>> forward_decl_locations; // type name -> (line, column)

    void push_ctx(ContextKind k) { ctx_stack.push_back(k); if(k != ContextKind::GLOBAL) current_storage = StorageClass::AUTO; }
    void pop_ctx() { 
      if (!ctx_stack.empty()) {
        if (ctx_stack.back() == ContextKind::FUNCTION) {
          if (!unresolved_labels.empty()) {
            for (const auto& [label, nodes] : unresolved_labels) {
              (void)nodes; // not used for now
              parser_add_error(0, 0, "undefined label '" + label + "'");
            }
            unresolved_labels.clear();
          } 
        }
        ctx_stack.pop_back();
      }
    }

    // Ensure the expr is same as the switch subject type
    TypePtr current_switch_subject() const { return switch_subject_stack.empty() ? nullptr : switch_subject_stack.back(); }
    // Function-context tracking for return statements
    std::vector<TypePtr> function_return_stack;
    void push_function(TypePtr return_type) {
      push_ctx(ContextKind::FUNCTION);
      function_return_stack.push_back(return_type);
    }
    void pop_function() {
      if (!function_return_stack.empty()) function_return_stack.pop_back();
      pop_ctx();
    }
    bool in_function() const { return !function_return_stack.empty(); }
    TypePtr current_function_return() const { return function_return_stack.empty() ? nullptr : function_return_stack.back(); }

    void reset_decl() {
      current_decl_base_type = QualifiedType{};
      current_decl_qualifiers.clear();
      if (ctx_stack.back() == ContextKind::GLOBAL) {
        current_storage = StorageClass::STATIC;
      } else {
        current_storage = StorageClass::AUTO;
      }
    }

    void add_unresolved_label(const std::string& name, const ASTNodePtr& node) {
      unresolved_labels[name].push_back(node);
    }

    void resolve_label(SymbolPtr label) {
      auto it = unresolved_labels.find(label->get_name());
      if (it != unresolved_labels.end()) {
        for (const auto& node : it->second) {
          if (node->type == ASTNodeType::GOTO_STMT) {
            std::static_pointer_cast<GotoStmt>(node)->target_label = label;
          }
        }
        unresolved_labels.erase(it);
      }
    }
  };
}

%parse-param { Lexer& lexer }

%code {
  #include <iostream>
  #include <sstream>
  #include "lexer/lexer.hpp"
  #include "parser/parser_errors.hpp"
  #include "symbol_table/type_factory.hpp"
  #include "symbol_table/symbol_table.hpp"

  #undef yylex
  #define yylex lexer.yylex
  static SymbolTable symbol_table;
  static TypeFactory type_factory;
  static GlobalParserState parser_state;

  void check_forward_declarations(){
     for (const auto& type_name : parser_state.forward_declared_types) {
        if (parser_state.defined_types.find(type_name) == parser_state.defined_types.end()) {
            auto loc = parser_state.forward_decl_locations[type_name];
            parser_add_error(loc.first, loc.second, 
                "forward declaration of '" + type_name + "' is never defined");
        }
    }
  }


  // Counters to uniquely name anonymous aggregates (struct/union/class) in a scope-stable way
  static size_t anon_struct_counter = 0;
  static size_t anon_union_counter = 0;
  static size_t anon_class_counter = 0;

  void print_parse_results()
  {
	  std::cout<< "Custom Types:\n";
    type_factory.print_custom_types();
	  std::cout << "Symbol Table:\n";
	  symbol_table.print_symbols();
  }

  static TypePtr unwrap_type_or_error(Result<TypePtr, TypeFactoryError> result,
                                      const std::string& context,
                                      int line,
                                      int column);

  static void params_to_vectors(const std::vector<ParamDeclInfo>& in,
                                std::vector<QualifiedType>& types,
                                std::vector<std::string>& names) {
    types.clear();
    names.clear();
    types.reserve(in.size());
    names.reserve(in.size());
    for (const auto& p : in) { types.push_back(p.type); names.push_back(p.name); }
  }

  static TypePtr pointer_from(TypePtr base, const yy::location& loc)
  {
    if (!base) {
      return nullptr;
    }

    auto result = type_factory.pointer_from(base);
    return unwrap_type_or_error(result, "pointer creation", loc.begin.line, loc.begin.column);
  }

  static TypePtr dereference_pointer(TypePtr pointer_type, const yy::location& loc, const std::string& context)
  {
    if (!pointer_type) {
      return nullptr;
    }

    auto result = type_factory.dereference_pointer(pointer_type);
    if (result.is_err()) {
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       context + ": " + type_factory_error_to_string(result.error()));
      return nullptr;
    }

    return result.value();
  }

  static TypePtr unwrap_type_or_error(Result<TypePtr, TypeFactoryError> result, const std::string& context, int line, int column) {
    if (result.is_err()) {
      std::string error_msg = context + ": " + type_factory_error_to_string(result.error());
      parser_add_error(line, column, error_msg);

      return nullptr;
    }

    return result.value();
  }

  static QualifiedType unwrap_qualified_or_error(
      Result<QualifiedType, TypeFactoryError> result,
      const std::string& context,
      int line,
      int column,
      QualifiedType fallback)
  {
    if (result.is_err()) {
      std::string error_msg = context + ": " + type_factory_error_to_string(result.error());
      parser_add_error(line, column, error_msg);
      return fallback;
    }

    return result.value();
  }

  static TypePtr require_builtin(const std::string& name,
                                 const yy::location& loc,
                                 const std::string& context = "type lookup")
  {
    auto builtin = type_factory.get_builtin_type(name);
    if (!builtin.has_value()) {
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       context + ": builtin type '" + name + "' is not defined");
      return nullptr;
    }

    return builtin.value();
  }

  static TypePtr make_function_type_or_error(TypePtr ret,
                                             const std::vector<QualifiedType>& params,
                                             bool variadic,
                                             const std::string& context,
                                             int line,
                                             int column)
  {
    auto result = type_factory.make_function_type(ret, params, variadic);
    if (result.is_err()) {
      parser_add_error(line,
                       column,
                       context + ": " + type_factory_error_to_string(result.error()));
      QualifiedType qualified_ret(ret, Qualifier::NONE);
      return std::make_shared<FunctionType>(qualified_ret, params, variadic);
    }

    return result.value();
  }

  static QualifiedType apply_pointer_levels_or_error(QualifiedType base,
                                                     size_t ptr_levels,
                                                     const std::string& context,
                                                     int line,
                                                     int column)
  {
    auto result = type_factory.apply_pointer_levels(base, ptr_levels);
    return unwrap_qualified_or_error(result, context, line, column, base);
  }

  static QualifiedType apply_array_dimensions_or_error(
      QualifiedType base,
      const std::vector<size_t>& dims,
      const std::string& context,
      int line,
      int column)
  {
    auto result = type_factory.apply_array_dimensions(base, dims);
    return unwrap_qualified_or_error(result, context, line, column, base);
  }

  static void add_symbol_if_valid(const std::string& name,
                                  QualifiedType type,
                                  const yy::location& loc,
                                  std::optional<FunctionMeta> function_meta = std::nullopt) {
    if (!name.empty()) {
      auto result = symbol_table.add_symbol(name, type, parser_state.current_storage, function_meta);
      if (result.is_err()) {
        parser_add_error(loc.begin.line,
                         loc.begin.column,
                         symbol_table_error_to_string(result.error()));
      }
    }
  }

  static TypePtr make_string_literal_type(const yy::location& loc) {
    auto char_type = require_builtin("char", loc, "string literal");
    if (!char_type) {
      return nullptr;
    }

    QualifiedType qualified_char(char_type, Qualifier::CONST);
    std::vector<Qualifier> pointer_levels = {Qualifier::NONE};

    auto pointer_result = type_factory.make_pointer_chain(qualified_char, pointer_levels);

    if (pointer_result.is_err()) {
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       std::string("failed to create pointer type for string literal: ") +
                           type_factory_error_to_string(pointer_result.error()));
      return char_type;
    }

    return pointer_result.value().type;
  }

  static TypePtr get_expression_type(const ASTNodePtr& node,
                                         const yy::location& loc,
                                         const std::string& context)
  {
    if (!node) {
      parser_add_error(loc.begin.line,
                        loc.begin.column,
                        context + ": expression is null");
      return nullptr;
    }

    auto type_result = get_expression_type(node);
    if (type_result.is_ok()) {
      return type_result.value();
    }

    switch (type_result.error()) {
    case ExpressionTypeError::NullNode:
      parser_add_error(loc.begin.line,
                        loc.begin.column,
                        context + ": expression is null");
      break;
    case ExpressionTypeError::NotExpression:
      parser_add_error(loc.begin.line,
                        loc.begin.column,
                        context + ": AST node does not model an expression");
      break;
    }

    return nullptr;
  }
}

%token <std::string> IDENTIFIER
%token <uint64_t> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <std::string> STRING_LITERAL
%token <bool> BOOL_LITERAL

%token INT SIGNED UNSIGNED CHAR BOOL FLOAT VOID
%token TYPEDEF STATIC CONST VOLATILE
%token ENUM STRUCT UNION CLASS
%token RETURN IF ELSE SWITCH CASE DEFAULT FOR WHILE DO GOTO CONTINUE BREAK UNTIL
%token NEW DELETE
%token PUBLIC PRIVATE PROTECTED

%token <std::string> TYPE_NAME

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
%token LT_OP LE_OP GT_OP GE_OP /* < or > or <= or >= */

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

%type <TypePtr> type_specifier specifier_qualifier_list type_name declaration_specifiers
%type <unsigned> pointer pointer_opt
%type <DeclaratorInfo> declarator direct_declarator
%type <std::unordered_map<std::string, QualifiedType>> struct_declaration_list
%type <std::unordered_map<std::string, QualifiedType>> struct_declaration
%type <std::vector<DeclaratorInfo>> struct_declarator_list
%type <DeclaratorInfo> struct_declarator

%type <std::vector<DeclaratorInfo>> init_declarator_list
%type <DeclaratorInfo> init_declarator

%type <TypePtr> enum_specifier
%type <TypePtr> struct_or_union_specifier
%type <std::vector<std::string>> enumerator_list
%type <std::string> enumerator

%type <std::string> struct_or_union
%type <Access> access_specifier
%type <TypePtr> class_specifier_tail
%type <ParamListInfo> parameter_type_list
%type <std::vector<ParamDeclInfo>> parameter_list
%type <ParamDeclInfo> parameter_declaration
%type <std::string> operator_token

%type <ASTNodePtr> primary_expression postfix_expression unary_expression cast_expression multiplicative_expression
%type <ASTNodePtr> additive_expression shift_expression relational_expression equality_expression and_expression
%type <ASTNodePtr> exclusive_or_expression inclusive_or_expression logical_and_expression logical_or_expression
%type <ASTNodePtr> conditional_expression assignment_expression expression constant_expression argument_expression_list

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
%left LT_OP LE_OP GT_OP GE_OP

%left LSHIFT_OP RSHIFT_OP

%left PLUS_OP MINUS_OP
%left STAR_OP DIVIDE_OP MOD_OP

%right UNARY /* unary + - ! ~ * & - mark unary precedence */
// %nonassoc POSTFIX_PREC  /* postfix ++ --, call, indexing has highest precedence */
// %right CAST_PREC

%start translation_unit

%%


open_brace
    : OPEN_BRACE_OP { symbol_table.enter_scope(); parser_state.push_ctx(ContextKind::BLOCK); }
    ;

close_brace
    : CLOSE_BRACE_OP { symbol_table.exit_scope(); parser_state.pop_ctx(); check_forward_declarations(); }
    ;

class_open_brace
  : OPEN_BRACE_OP { symbol_table.enter_scope(); parser_state.push_ctx(ContextKind::CLASS); }
  ;

class_close_brace
  : CLOSE_BRACE_OP { symbol_table.exit_scope(); parser_state.pop_ctx(); }
  ;

primary_expression
    : IDENTIFIER
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
    {
        auto type = $1->type;
        if (type == ASTNodeType::IDENTIFIER_EXPR) {
          std::string enum_name = std::string("enum ") + std::static_pointer_cast<IdentifierExpr>($1)->symbol->get_name();
          auto type_ptr_opt = type_factory.lookup(enum_name);
          if (type_ptr_opt.has_value() && type_ptr_opt.value()->kind == TypeKind::ENUM) {
            auto enum_type_ptr = std::static_pointer_cast<EnumType>(type_ptr_opt.value());
            if (enum_type_ptr->enumerators.find($3) == enum_type_ptr->enumerators.end()) {
              parser_add_error(@2.begin.line, @2.begin.column, "enumerator '" + $3 + "' not found in enum '" + enum_name + "'");
            } else {
                // TODO: propagate value
            }
          }else {
            // TODO: handle other types with member access
          }
        }
    }
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
    | relational_expression LT_OP shift_expression
	  | relational_expression LE_OP shift_expression
	  | relational_expression GT_OP shift_expression
	  | relational_expression GE_OP shift_expression
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
      {
        parser_state.reset_decl();
      }
    | declaration_specifiers init_declarator_list SEMICOLON_OP
      {
        QualifiedType base = parser_state.current_decl_base_type;

        if (base.type) {
          for (auto &di : $2) {
            if (di.is_function) {
              bool in_class = !parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type;
              if (!in_class && !di.name.empty()) {
                TypePtr fn = make_function_type_or_error(base.type,
                                                         di.param_types,
                                                         di.is_variadic,
                                                         "function declarator",
                                                         @1.begin.line,
                                                         @1.begin.column);

                FunctionMeta meta(FunctionKind::NORMAL, di.param_names, std::nullopt);

                auto mangled = mangle_function_name(di.name,
                                                    *std::static_pointer_cast<FunctionType>(fn),
                                                    meta,
                                                    std::nullopt);
                if (!mangled.has_value()) {
                  parser_add_error(@1.begin.line,
                                   @1.begin.column,
                                   "unable to mangle function '" + di.name + "'");
                  continue;
                }
                add_symbol_if_valid(*mangled,
                                    QualifiedType(fn, Qualifier::NONE),
                                    @1,
                                    std::optional<FunctionMeta>{meta});

              }
            }
          }
        }
        parser_state.reset_decl();
      }
    | TYPEDEF type_specifier pointer_opt TYPE_NAME SEMICOLON_OP
      {
        size_t ptrs = $3;
        TypePtr base = $2;
        if (base){
          QualifiedType actual = apply_pointer_levels_or_error(QualifiedType(base, Qualifier::NONE),
                                                               ptrs,
                                                               "typedef pointer",
                                                               @1.begin.line,
                                                               @1.begin.column);
          (void)unwrap_type_or_error(type_factory.make<TypedefType>($4, actual), "typedef", @1.begin.line, @1.begin.column);
        }
        parser_state.reset_decl();
      }
    ;

declaration_specifiers
  : storage_class_specifier declaration_specifiers
    {
      $$ = $2;
      if ($$) {
        // Apply qualifiers from the stack one by one
        QualifiedType base($$, Qualifier::NONE);
        size_t num_qualifiers = parser_state.current_decl_qualifiers.size();

        for (const auto& q : parser_state.current_decl_qualifiers) {
          base = QualifiedType(base.type, q);
        }

        parser_state.current_decl_base_type = base;

        // Pop the qualifiers we just used
        for (size_t i = 0; i < num_qualifiers; ++i) {
          parser_state.current_decl_qualifiers.pop_back();
        }
      }
    }
  | storage_class_specifier { $$ = nullptr; }
  | type_specifier declaration_specifiers
    {
      $$ = $1 ? $1 : $2;
      if ($$) {
        // Apply qualifiers from the stack one by one
        QualifiedType base($$, Qualifier::NONE);
        size_t num_qualifiers = parser_state.current_decl_qualifiers.size();

        for (const auto& q : parser_state.current_decl_qualifiers) {
          base = QualifiedType(base.type, q);
        }

        parser_state.current_decl_base_type = base;

        // Pop the qualifiers we just used
        for (size_t i = 0; i < num_qualifiers; ++i) {
          parser_state.current_decl_qualifiers.pop_back();
        }
      }
    }
  | type_specifier
    {
      $$ = $1;
      if ($$) {
        // Apply qualifiers from the stack one by one
        QualifiedType base($$, Qualifier::NONE);
        size_t num_qualifiers = parser_state.current_decl_qualifiers.size();

        for (const auto& q : parser_state.current_decl_qualifiers) {
          base = QualifiedType(base.type, q);
        }

        parser_state.current_decl_base_type = base;

        // Pop the qualifiers we just used
        for (size_t i = 0; i < num_qualifiers; ++i) {
          parser_state.current_decl_qualifiers.pop_back();
        }
      }
    }
  | type_qualifier declaration_specifiers
    {
      $$ = $2;
      if ($$) {
        // Apply qualifiers from the stack one by one
        QualifiedType base($$, Qualifier::NONE);
        size_t num_qualifiers = parser_state.current_decl_qualifiers.size();

        for (const auto& q : parser_state.current_decl_qualifiers) {
          base = QualifiedType(base.type, q);
        }

        parser_state.current_decl_base_type = base;

        // Pop the qualifiers we just used
        for (size_t i = 0; i < num_qualifiers; ++i) {
          parser_state.current_decl_qualifiers.pop_back();
        }
      }
    }
  | type_qualifier
    {
      $$ = nullptr;
    }
  ;

storage_class_specifier
    : STATIC
      {
        parser_state.current_storage = StorageClass::STATIC;
      }
    ;

init_declarator_list
    : init_declarator
      { $$ = std::vector<DeclaratorInfo>{ $1 }; }
    | init_declarator_list COMMA_OP init_declarator
      { $$ = $1; $$.push_back($3); }
    ;

init_declarator
    : declarator ASSIGN_OP initializer
      {
        $$ = $1;
        const DeclaratorInfo &di = $1;
        QualifiedType base = parser_state.current_decl_base_type;
        if (base.type) {
          QualifiedType final_t;

          if(di.pointer_levels > 0){
            final_t = apply_pointer_levels_or_error(base,
                                                   di.pointer_levels,
                                                   "initializer pointer declarator",
                                                   @1.begin.line,
                                                   @1.begin.column);
          }
          else if(!di.array_dims.empty()){
            final_t = apply_array_dimensions_or_error(base,
                                                     di.array_dims,
                                                     "initializer array declarator",
                                                     @1.begin.line,
                                                     @1.begin.column);
          }
          else {
            final_t = base;
          }

          add_symbol_if_valid(di.name, final_t, @1);

          if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
            if (!di.name.empty()) {
              auto mi = MemberInfo{final_t, parser_state.current_access, false};
              std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(di.name, mi);
            }
          }
        }
      }
    | declarator
      {
        $$ = $1;
        const DeclaratorInfo &di = $1;
        QualifiedType base = parser_state.current_decl_base_type;

        if (base.type) {
          QualifiedType final_t;
          if(di.pointer_levels > 0){
            final_t = apply_pointer_levels_or_error(base,
                                                   di.pointer_levels,
                                                   "declarator pointer",
                                                   @1.begin.line,
                                                   @1.begin.column);
          }
          else if(!di.array_dims.empty()){
            final_t = apply_array_dimensions_or_error(base,
                                                     di.array_dims,
                                                     "declarator array",
                                                     @1.begin.line,
                                                     @1.begin.column);
          }
          else {
            final_t = base;
          }

          add_symbol_if_valid(di.name, final_t, @1);

          if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
            if (!di.name.empty()) {
              auto mi = MemberInfo{final_t, parser_state.current_access, false};
              std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(di.name, mi);
            }
          }
        }
      }
    ;

type_specifier
  : VOID     { $$ = require_builtin("void", @1, "type specifier");     parser_state.current_decl_base_type = $$;  }
  | CHAR     { $$ = require_builtin("char", @1, "type specifier");     parser_state.current_decl_base_type = $$; }
  | INT      { $$ = require_builtin("int", @1, "type specifier");      parser_state.current_decl_base_type = $$; }
  | SIGNED   { $$ = require_builtin("signed", @1, "type specifier");   parser_state.current_decl_base_type = $$; }
  | UNSIGNED { $$ = require_builtin("unsigned", @1, "type specifier"); parser_state.current_decl_base_type = $$; }
  | BOOL     { $$ = require_builtin("bool", @1, "type specifier");     parser_state.current_decl_base_type = $$; }
  | FLOAT    { $$ = require_builtin("float", @1, "type specifier");    parser_state.current_decl_base_type = $$; }
  	| struct_or_union_specifier { $$ = $1; parser_state.current_decl_base_type = $$; }
    | enum_specifier { $$ = $1; parser_state.current_decl_base_type = $$; }
    | CLASS class_specifier_tail
      {
        $$ = $2;
        parser_state.current_decl_base_type = $$;
      }
    | TYPE_NAME
      {
        // TYPE_NAME tokens are typedef names, which are stored with "typedef " prefix
        std::string typedef_name = "typedef " + $1;
        auto opt = type_factory.lookup(typedef_name);
        if (opt.has_value()) {
          $$ = opt.value();
          parser_state.current_decl_base_type = $$;
        }
        else {
          parser_add_error(@1.begin.line, @1.begin.column, "unknown type name '" + $1 + "'");
          $$ = nullptr;
        }
      }
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
      {
        std::string tag = $2;
        bool is_union = ($1 == "union");
        std::string full_name = ($1 == "struct" ? std::string("struct ") : std::string("union ")) + tag;
        
        // Mark as defined
        parser_state.defined_types.insert(full_name);
        
        // Create the aggregate type in the current (outer) scope
        $$ = unwrap_type_or_error(type_factory.make<RecordType>(tag, is_union, true), "struct/union definition", @1.begin.line, @2.begin.column);
        // Create a dedicated member scope and add members as symbols
        symbol_table.enter_scope();
        for (const auto &kv : $4) {
          add_symbol_if_valid(kv.first, kv.second, @2);
        }
        symbol_table.exit_scope();
      }
    | struct_or_union OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
      {
        std::ostringstream os;

        TypePtr rec;
        if ($1 == "struct") {
          os << "<anon-struct@" << symbol_table.get_current_scope_id() << ":" << (anon_struct_counter++) << ">";
          rec = unwrap_type_or_error(type_factory.make<RecordType>(os.str(), false, true), "anonymous struct", @1.begin.line, @1.begin.column);
        } else {
          os << "<anon-union@" << symbol_table.get_current_scope_id() << ":" << (anon_union_counter++) << ">";
          rec = unwrap_type_or_error(type_factory.make<RecordType>(os.str(), true, true), "anonymous union", @1.begin.line, @1.begin.column);
        }

        // Add fields to the RecordType
        auto record_type = std::static_pointer_cast<RecordType>(rec);
        for (const auto &kv : $3) {
          record_type->add_field(kv.first, kv.second);
        }

        $$ = rec;

        symbol_table.enter_scope();
        for (const auto &kv : $3) {
          add_symbol_if_valid(kv.first, kv.second, @1);
        }
        symbol_table.exit_scope();
      }
    | struct_or_union IDENTIFIER
      {
        // Tag name without "struct"/"union" prefix (debug_name() adds it)
        std::string tag = $2;
        std::string full_name = ($1 == "struct" ? std::string("struct ") : std::string("union ")) + tag;
        bool is_union = ($1 == "union");

        // Try to find an existing type globally
        auto found = type_factory.lookup(full_name);
        if (found.has_value()) {
          $$ = found.value();
        } else {
          // Type doesn't exist - create a forward declaration and track it
          parser_state.forward_declared_types.insert(full_name);
          parser_state.forward_decl_locations[full_name] = {@1.begin.line, @2.begin.column};
          $$ = unwrap_type_or_error(type_factory.make<RecordType>(tag, is_union, false), "struct/union forward declaration", @1.begin.line, @2.begin.column);
        }
      }
	  | struct_or_union IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP // to allow empty structs/unions
      {
        std::string tag = $2;
        bool is_union = ($1 == "union");
        std::string full_name = ($1 == "struct" ? std::string("struct ") : std::string("union ")) + tag;
        
        // Mark as defined
        parser_state.defined_types.insert(full_name);
        
        $$ = unwrap_type_or_error(type_factory.make<RecordType>(tag, is_union, true), "empty struct/union", @1.begin.line, @2.begin.column);
      }
    ;

struct_or_union
    : STRUCT  { $$ = "struct"; /* context can be tracked if needed */ }
    | UNION   { $$ = "union"; }
    ;

struct_declaration_list
    : struct_declaration {
        $$ = $1;
	  }
    | struct_declaration_list struct_declaration {
        $$ = $1;
        for (auto &kv : $2) {
          $$.insert(kv);
        }
	  }
    ;

struct_declaration
    : specifier_qualifier_list SEMICOLON_OP
      {
        $$ = std::unordered_map<std::string, QualifiedType>{};
      }
    | specifier_qualifier_list struct_declarator_list SEMICOLON_OP
      {
        $$ = std::unordered_map<std::string, QualifiedType>{};
        auto base = $1;
        if (base) {
          for (auto &di : $2) {
            if (!di.name.empty()) {
              QualifiedType final_t;

              if(di.pointer_levels){
                final_t = apply_pointer_levels_or_error(base,
                                                       di.pointer_levels,
                                                       "struct field pointer",
                                                       @1.begin.line,
                                                       @1.begin.column);
              }
              else{
                final_t = apply_array_dimensions_or_error(base,
                                                         di.array_dims,
                                                         "struct field array",
                                                         @1.begin.line,
                                                         @1.begin.column);
              }

              // TODO handle errors
              $$[di.name] = final_t;
            }
          }
        }
      }
    ;

specifier_qualifier_list
  : type_specifier specifier_qualifier_list { $$ = $1 ? $1 : $2; }
  | type_specifier { $$ = $1; }
  | type_qualifier specifier_qualifier_list { $$ = $2; }
  | type_qualifier { $$ = nullptr; }
  ;

struct_declarator_list
    : struct_declarator
      { $$ = std::vector<DeclaratorInfo>{ $1 }; }
    | struct_declarator_list COMMA_OP struct_declarator
      { $$ = $1; $$.push_back($3); }
    ;

struct_declarator
    : declarator
      { $$ = $1; }
    | declarator COLON_OP constant_expression
      { $$ = $1; /* bit-field width ignored for now */ }
    | COLON_OP constant_expression
      { $$ = DeclaratorInfo{}; /* unnamed bit-field: ignore */ }
    ;

enum_specifier
    : ENUM IDENTIFIER OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
      {
        std::string tag = $2;
        std::string full_name = "enum " + tag;
        
        // Mark as defined
        parser_state.defined_types.insert(full_name);
        
        TypePtr t = unwrap_type_or_error(type_factory.make<EnumType>(tag, true), "enum definition", @1.begin.line, @2.begin.column);

        int64_t v = 0;
        for (const auto& nm : $4) {
          std::static_pointer_cast<EnumType>(t)->add_enumerator(nm, v);
          v++;
        }
        $$ = t;
      }
    | ENUM OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
      {
        parser_add_error(
          @1.begin.line,
          @1.begin.column,
          "anonymous enums are not supported in favour of strongly typed enums by default;\n"
          "Only named enums are allowed as we disagree with C's weakly typed enums.\n"
          "Please remove the anonymous enum"
        );
        $$ = nullptr;
      }
    | ENUM IDENTIFIER
      {
        std::string tag = $2;
        std::string full_name = "enum " + tag;
        // Try to find existing enum globally; otherwise create a forward declaration
        auto found = type_factory.lookup(full_name);
        if (found.has_value()) {
          $$ = found.value();
        } else {
          parser_state.forward_declared_types.insert(full_name);
          parser_state.forward_decl_locations[full_name] = {@1.begin.line, @2.begin.column};
          $$ = unwrap_type_or_error(type_factory.make<EnumType>(tag, false), "enum forward declaration", @1.begin.line, @2.begin.column);
        }
      }
    ;

enumerator_list
    : enumerator
      {
        $$ = std::vector<std::string>{ $1 };
      }
    | enumerator_list COMMA_OP enumerator
      {
        $$ = $1;
        $$.push_back($3);
      }
    ;

enumerator
    : IDENTIFIER
      { $$ = $1; }
    | IDENTIFIER ASSIGN_OP constant_expression
      { $$ = $1; /* explicit value ignored for now */ }
    ;

type_qualifier
    : CONST
      { parser_state.current_decl_qualifiers.push_back(Qualifier::CONST); }
    | VOLATILE
      { parser_state.current_decl_qualifiers.push_back(Qualifier::VOLATILE); }
    ;

declarator
    : pointer direct_declarator
      {
        $$ = $2;
        $$.pointer_levels += $1;
      }
    | direct_declarator
      { $$ = $1; }
    ;

direct_declarator
    : IDENTIFIER
      {
        $$ = DeclaratorInfo{};
        $$.name = $1;
      }
    | OPEN_PAREN_OP declarator CLOSE_PAREN_OP
      {
        $$ = $2;
      }
  	| direct_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
      {
        $$ = $1;
        $$.array_dims.push_back(0);
      }
    | direct_declarator OPEN_BRACKET_OP INT_LITERAL CLOSE_BRACKET_OP
      {
        $$ = $1;
        // TODO: add support for constant expressions (low-priority)
        $$.array_dims.push_back($3);
      }
    | direct_declarator OPEN_PAREN_OP CLOSE_PAREN_OP
      {
        $$ = $1;
        $$.is_function = true;
        $$.has_params = false;
      }
    | direct_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
      {
        $$ = $1;
        $$.is_function = true;
        $$.has_params = true;
		    $$.param_types = $3.types;
		    $$.param_names = $3.names;
		    $$.is_variadic = $3.variadic;
      }
    ;

pointer
    : STAR_OP type_qualifier_list_opt pointer_opt
      { $$ = 1 + $3; }
    ;

type_qualifier_list_opt
    : /* empty */
    | type_qualifier_list
    ;

pointer_opt
    : /* empty */ { $$ = 0; }
    | pointer     { $$ = $1; }
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;

parameter_type_list
    : parameter_list
      {
        $$ = ParamListInfo{};
        params_to_vectors($1, $$.types, $$.names);
        $$.variadic = false;
      }
    | parameter_list COMMA_OP ELLIPSIS_OP
      {
        $$ = ParamListInfo{};
        params_to_vectors($1, $$.types, $$.names);
        $$.variadic = true;
      }
    ;

parameter_list
    : parameter_declaration
      { $$ = std::vector<ParamDeclInfo>{ $1 }; }
    | parameter_list COMMA_OP parameter_declaration
      { $$ = $1; $$.push_back($3); }
    ;

parameter_declaration
    : declaration_specifiers declarator
      {
        $$ = ParamDeclInfo{};
        QualifiedType base = parser_state.current_decl_base_type;
        if (base.type) {
          QualifiedType t;

          if($2.pointer_levels){
            t = apply_pointer_levels_or_error(base,
                                              $2.pointer_levels,
                                              "parameter pointer",
                                              @1.begin.line,
                                              @2.begin.column);
          }
          else if(!$2.array_dims.empty()){
            t = apply_array_dimensions_or_error(base,
                                                $2.array_dims,
                                                "parameter array",
                                                @1.begin.line,
                                                @2.begin.column);
          }
          else {
            t = base;
          }

          $$.type = t;
        }
        $$.name = $2.name; // may be empty if unnamed
        parser_state.reset_decl();
      }
    | declaration_specifiers abstract_declarator
      {
        $$ = ParamDeclInfo{};
        QualifiedType base = parser_state.current_decl_base_type;
        if (base.type) {
          $$.type = base;
        }
        $$.name = std::string{};
        parser_state.reset_decl();
      }
    | declaration_specifiers
      {
        $$ = ParamDeclInfo{};
        $$.type = parser_state.current_decl_base_type;
        $$.name = std::string{};
        parser_state.reset_decl();
      }
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
      { $$ = $1; }
    | specifier_qualifier_list
      { $$ = $1; }
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
    | declaration
    ;

labeled_statement
    : IDENTIFIER COLON_OP statement {
        auto label=type_factory.lookup_by_scope($1, symbol_table.get_scope_chain());
        if (!label.has_value()) {
          label = unwrap_type_or_error(type_factory.make<BuiltinType>(BuiltinTypeKind::LABEL), "label", @1.begin.line, @1.begin.column);
        }
        add_symbol_if_valid($1, QualifiedType(label.value(), Qualifier::NONE), @1);
	  }
    | CASE constant_expression COLON_OP statement
    | DEFAULT COLON_OP statement
    ;

compound_statement
    : open_brace close_brace
    | open_brace block_item_list close_brace
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
    : declaration_specifiers declarator compound_statement {
		auto base = $1;
		if (base) {
		  const DeclaratorInfo &di = $2;
			if (di.is_function && !di.name.empty()) {
      TypePtr fn = make_function_type_or_error(base,
                       di.param_types,
                       di.is_variadic,
                       "function definition",
                       @1.begin.line,
                       @2.begin.column);
            FunctionMeta meta(FunctionKind::NORMAL, di.param_names, std::nullopt);
            auto mangled = mangle_function_name(di.name,
                                                *std::static_pointer_cast<FunctionType>(fn),
                                                meta,
                                                std::nullopt);
            if (!mangled.has_value()) {
              parser_add_error(@2.begin.line,
                               @2.begin.column,
                               "unable to mangle function '" + di.name + "'");
            } else {
              add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
            }
			} else {
				parser_add_error(@2.begin.line, @2.begin.column, "function definition requires function declarator");
			}
		}
		parser_state.reset_decl();
	  }
    | declaration_specifiers declarator declaration_list compound_statement
    ;

declaration_list
    : declaration
    | declaration_list declaration
    ;

class_specifier_tail
  : inheritance_opt
    {
      std::ostringstream os;
      os << "<anon-class@" << symbol_table.get_current_scope_id() << ":" << (anon_class_counter++) << ">";
      parser_state.current_class_type = std::static_pointer_cast<ClassType>(unwrap_type_or_error(type_factory.make<ClassType>(os.str(), parser_state.parent_class_type, parser_state.inherited_access, false), "anonymous class", @1.begin.line, @1.begin.column));
      parser_state.current_access = Access::PRIVATE; // default for class
    }
    class_open_brace class_member_list class_close_brace
    {
      $$ = parser_state.current_class_type;
      parser_state.current_class_type = nullptr;
      parser_state.parent_class_type = nullptr;
    }
  | IDENTIFIER inheritance_opt
    {
      std::string name = $1;
      std::string full_name = std::string("class ") + name;
      auto found = type_factory.lookup(full_name);
      if (found.has_value()) {
        parser_state.current_class_type = std::static_pointer_cast<ClassType>(found.value());
      } else {
        parser_state.current_class_type = std::static_pointer_cast<ClassType>(unwrap_type_or_error(type_factory.make<ClassType>(name, parser_state.parent_class_type, parser_state.inherited_access, false), "class definition", @1.begin.line, @1.begin.column));
      }
      parser_state.current_access = Access::PRIVATE;
    }
    class_open_brace class_member_list class_close_brace
    {
      std::string full_name = std::string("class ") + $1;
      parser_state.defined_types.insert(full_name);
      
      $$ = parser_state.current_class_type;
      parser_state.current_class_type = nullptr;
      parser_state.parent_class_type = nullptr;
    }
  | IDENTIFIER inheritance_opt
    {
      std::string name = $1;
      std::string full_name = std::string("class ") + name;
      auto found = type_factory.lookup(full_name);
      if (found.has_value()) {
        $$ = found.value();
      } else {
        parser_state.forward_declared_types.insert(full_name);
        parser_state.forward_decl_locations[full_name] = {@1.begin.line, @1.begin.column};
        $$ = unwrap_type_or_error(type_factory.make<ClassType>(name, parser_state.parent_class_type, parser_state.inherited_access, false), "class forward declaration", @1.begin.line, @1.begin.column);
      }

      if (parser_state.parent_class_type) {
        parser_add_error(@1.begin.line, @1.begin.column, "inheritance list provided without class definition for '" + full_name + "'");
      }
      parser_state.parent_class_type = nullptr;
    }
  | inheritance_opt class_open_brace class_close_brace
    {
      // empty anonymous class
      std::ostringstream os;
      os << "<anon-class@" << symbol_table.get_current_scope_id() << ":" << (anon_class_counter++) << ">";
      TypePtr t = unwrap_type_or_error(type_factory.make<ClassType>(os.str(), parser_state.parent_class_type, parser_state.inherited_access, false), "empty anonymous class", @1.begin.line, @1.begin.column);
      $$ = t;
      parser_state.parent_class_type = nullptr;
    } // to allow empty classes
  ;

inheritance_opt
	: /* empty */
	| COLON_OP inheritance_list
	;

inheritance_list
    : inheritance_specifier
    ;

inheritance_specifier
    : access_specifier IDENTIFIER {
    std::string name = $2;
    std::string full = std::string("class ") + name;
    auto opt = type_factory.lookup(full);
    if (opt.has_value() && opt.value()->kind == TypeKind::CLASS) {
      parser_state.parent_class_type = std::static_pointer_cast<ClassType>(opt.value());
      parser_state.inherited_access = $1;
    } else {
      parser_add_error(@2.begin.line, @2.begin.column, "unknown class name '" + name + "' for inheritance");
      parser_state.parent_class_type = nullptr;
    }
	}
    | IDENTIFIER {
    std::string name = $1;
    std::string full = std::string("class ") + name;
    auto opt = type_factory.lookup(full);
    if (opt.has_value() && opt.value()->kind == TypeKind::CLASS) {
      parser_state.parent_class_type = std::static_pointer_cast<ClassType>(opt.value());
      parser_state.inherited_access = Access::PRIVATE; // default
    } else {
      parser_add_error(@1.begin.line, @1.begin.column, "unknown class name '" + name + "' for inheritance");
      parser_state.parent_class_type = nullptr;
    }
	}
    ;

class_member_list
    : /* empty */
    | class_member_list class_member
    ;

class_member
    : access_specifier COLON_OP
      {
        parser_state.current_access = $1;
      }
    | declaration                               /* data member or typedef inside class */
    | function_declaration_or_definition        /* method (declaration or definition) */
    ;

access_specifier
  : PUBLIC    { $$ = Access::PUBLIC; }
  | PRIVATE   { $$ = Access::PRIVATE; }
  | PROTECTED { $$ = Access::PROTECTED; }
  ;

function_declaration_or_definition
    : declaration_specifiers declarator SEMICOLON_OP
      {
        const DeclaratorInfo &di = $2;
  		TypePtr ret = $1;
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type && !di.name.empty()) {
          if (di.is_function) {
            TypePtr fn = make_function_type_or_error(ret,
                                                     di.param_types,
                                                     di.is_variadic,
                                                     "member function declarator",
                                                     @1.begin.line,
                                                     @2.begin.column);
            FunctionMeta meta(FunctionKind::METHOD, di.param_names, parser_state.current_class_type);

            auto mangled = mangle_function_name(di.name,
                                                *std::static_pointer_cast<FunctionType>(fn),
                                                meta,
                                                *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
            if (!mangled.has_value()) {
              parser_add_error(@2.begin.line,
                               @2.begin.column,
                               "unable to mangle method '" + di.name + "'");
            } else {
              auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
              std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

              add_symbol_if_valid(*mangled,
                                  QualifiedType(fn, Qualifier::NONE),
                                  @1,
                                  std::optional<FunctionMeta>{meta});
            }
          } else {
            QualifiedType final_t;
            if(di.pointer_levels){
              final_t = apply_pointer_levels_or_error(ret,
                                                     di.pointer_levels,
                                                     "member pointer declarator",
                                                     @1.begin.line,
                                                     @2.begin.column);
            }
            else{
              final_t = apply_array_dimensions_or_error(ret,
                                                       di.array_dims,
                                                       "member array declarator",
                                                       @1.begin.line,
                                                       @1.begin.column);
            }

            // TODO error handling
            add_symbol_if_valid(di.name, final_t, @1);
            auto mi = MemberInfo{final_t, parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(di.name, mi);

          }
        }
        parser_state.reset_decl();
      }
    | declaration_specifiers declarator compound_statement
      {
        const DeclaratorInfo &di = $2;
  		TypePtr ret = $1;
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type && !di.name.empty()) {
          if (di.is_function) {
            TypePtr fn = make_function_type_or_error(ret,
                                                     di.param_types,
                                                     di.is_variadic,
                                                     "member function definition",
                                                     @1.begin.line,
                                                     @2.begin.column);

            FunctionMeta meta(FunctionKind::METHOD, di.param_names, parser_state.current_class_type);

            auto mangled = mangle_function_name(di.name,
                                                *std::static_pointer_cast<FunctionType>(fn),
                                                meta,
                                                *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
            if (!mangled.has_value()) {
              parser_add_error(@2.begin.line,
                               @2.begin.column,
                               "unable to mangle method '" + di.name + "'");
            } else {
              auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
              std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

              add_symbol_if_valid(*mangled,
                                  QualifiedType(fn, Qualifier::NONE),
                                  @1,
                                  std::optional<FunctionMeta>{meta});
            }
          } else {
            QualifiedType final_t;
            if(di.pointer_levels){
              final_t = apply_pointer_levels_or_error(ret,
                                                     di.pointer_levels,
                                                     "member pointer definition",
                                                     @1.begin.line,
                                                     @2.begin.column);
            }
            else{
              final_t = apply_array_dimensions_or_error(ret,
                                                       di.array_dims,
                                                       "member array definition",
                                                       @1.begin.line,
                                                       @1.begin.column);
            }

            // TODO error handling
            add_symbol_if_valid(di.name, final_t, @1);
            auto mi = MemberInfo{final_t, parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(di.name, mi);
          }
        }
        parser_state.reset_decl();
      }
    | /* constructor */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          // Optional check: name matches class
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = require_builtin("void", @1, "constructor return type");
          const auto &plist = $3;

          TypePtr fn = make_function_type_or_error(ret,
                                                   plist.types,
                                                   plist.variadic,
                                                   "constructor definition",
                                                   @1.begin.line,
                                                   @1.begin.column);
          FunctionMeta meta(FunctionKind::CONSTRUCTOR, plist.names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $1,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@1.begin.line,
                             @1.begin.column,
                             "unable to mangle constructor '" + $1 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);
            add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
  | /* constructor without params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = require_builtin("void", @1, "constructor return type");
          std::vector<QualifiedType> params; std::vector<std::string> names; bool variadic = false;
          TypePtr fn = make_function_type_or_error(ret,
                                                   params,
                                                   variadic,
                                                   "constructor definition",
                                                   @1.begin.line,
                                                   @1.begin.column);
          FunctionMeta meta(FunctionKind::CONSTRUCTOR, names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $1,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@1.begin.line,
                             @1.begin.column,
                             "unable to mangle constructor '" + $1 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

          add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
  | /* destructor */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $2;
          if (expected != got) {
            std::cerr << "Warning: destructor name '~" << $2 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = require_builtin("void", @1, "destructor return type");
          std::vector<QualifiedType> params; std::vector<std::string> names; bool variadic = false;
          TypePtr fn = make_function_type_or_error(ret,
                                                   params,
                                                   variadic,
                                                   "destructor definition",
                                                   @1.begin.line,
                                                   @2.begin.column);

          FunctionMeta meta(FunctionKind::DESTRUCTOR, names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $2,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type)
            );
          if (!mangled.has_value()) {
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "unable to mangle destructor '~" + $2 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);
            add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
  | /* operator overload def */ IDENTIFIER OPERATOR operator_token OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
      {
		std::string expected = parser_state.current_class_type ? parser_state.current_class_type->debug_name() : std::string{};
		std::string got = std::string("class ") + $1;
		if (expected != got) {
		  std::cerr << "Warning: operator overload function name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
		}
		TypePtr ret = type_factory.lookup($1).value_or(nullptr);
        if (ret && !parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          const auto &plist = $5;
          std::string opname = $3;
          TypePtr fn = make_function_type_or_error(ret,
                                                   plist.types,
                                                   plist.variadic,
                                                   "operator definition",
                                                   @1.begin.line,
                                                   @1.begin.column);
          FunctionMeta meta(FunctionKind::OPERATOR, plist.names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $3,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@3.begin.line,
                             @3.begin.column,
                             "unable to mangle operator '" + $3 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

            add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
        parser_state.reset_decl();
      }
    | /* constructor decl */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          const auto &plist = $3;
          TypePtr ret = require_builtin("void", @1, "constructor return type");
          TypePtr fn = make_function_type_or_error(ret,
                                                   plist.types,
                                                   plist.variadic,
                                                   "constructor declaration",
                                                   @1.begin.line,
                                                   @1.begin.column);
          FunctionMeta meta(FunctionKind::CONSTRUCTOR, plist.names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $1,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@1.begin.line,
                             @1.begin.column,
                             "unable to mangle constructor '" + $1 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

          add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
    | /* constructor decl no params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          std::vector<QualifiedType> params; std::vector<std::string> names; bool variadic = false;
          TypePtr ret = require_builtin("void", @1, "constructor return type");
          TypePtr fn = make_function_type_or_error(ret,
                                                   params,
                                                   variadic,
                                                   "constructor declaration",
                                                   @1.begin.line,
                                                   @1.begin.column);
          FunctionMeta meta(FunctionKind::CONSTRUCTOR, names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $1,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@1.begin.line,
                             @1.begin.column,
                             "unable to mangle constructor '" + $1 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

            add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
    | /* destructor decl */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->debug_name();
          std::string got = std::string("class ") + $2;
          if (expected != got) {
            std::cerr << "Warning: destructor name '~" << $2 << "' does not match enclosing class '" << expected << "'\n";
          }
          std::vector<QualifiedType> params; std::vector<std::string> names; bool variadic = false;
          TypePtr ret = require_builtin("void", @1, "destructor return type");

          TypePtr fn = make_function_type_or_error(ret,
                                                   params,
                                                   variadic,
                                                   "destructor declaration",
                                                   @1.begin.line,
                                                   @2.begin.column);

          FunctionMeta meta(FunctionKind::DESTRUCTOR, names, parser_state.current_class_type);

          auto mangled = mangle_function_name(
            $2,
            *std::static_pointer_cast<FunctionType>(fn),
            meta,
            *std::static_pointer_cast<ClassType>(parser_state.current_class_type));
          if (!mangled.has_value()) {
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "unable to mangle destructor '~" + $2 + "'");
          } else {
            auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

          add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
          }
        }
      }
    ;

operator_token
    : PLUS_OP         { $$ = "+"; }
    | MINUS_OP        { $$ = "-"; }
    | STAR_OP         { $$ = "*"; }
    | DIVIDE_OP       { $$ = "/"; }
    | MOD_OP          { $$ = "%"; }
    | AMPERSAND_OP    { $$ = "&"; }
    | PIPE_OP         { $$ = "|"; }
    | CARET_OP        { $$ = "^"; }
    | EQ_OP           { $$ = "=="; }
    | NE_OP           { $$ = "!="; }
    | LT_OP           { $$ = "<"; }
	  | LE_OP           { $$ = "<="; }
	  | GT_OP           { $$ = ">"; }
	  | GE_OP           { $$ = ">="; }
    | LSHIFT_OP       { $$ = "<<"; }
    | RSHIFT_OP       { $$ = ">>"; }
    | INCREMENT_OP    { $$ = "++"; }
    | DECREMENT_OP    { $$ = "--"; }
    | ASSIGN_OP       { $$ = "="; }
    | PLUS_ASSIGN_OP  { $$ = "+="; }
    | MINUS_ASSIGN_OP { $$ = "-="; }
    | STAR_ASSIGN_OP  { $$ = "*="; }
    | DIVIDE_ASSIGN_OP{ $$ = "/="; }
    | MOD_ASSIGN_OP   { $$ = "%="; }
    | ARROW_OP        { $$ = "->"; }
    | NEW             { $$ = " new"; }
    | DELETE          { $$ = " delete"; }
    ;


%%

void yy::Parser::error(const location_type& loc, const std::string& msg) {
    parser_add_error(loc.begin.line, loc.begin.column, msg);
}
