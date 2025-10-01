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

  #include "symbol_table/type.hpp"

  class Lexer;

  struct DeclaratorInfo {
    std::string name;
    size_t pointer_levels = 0;
    std::vector<size_t> array_dims;
    bool is_function = false;
    bool has_params = false;
    std::vector<TypePtr> param_types;
    std::vector<std::string> param_names;
    bool is_variadic = false;
  };

  struct ParamDeclInfo {
    TypePtr type = nullptr;
    std::string name;
  };

  struct ParamListInfo {
    std::vector<TypePtr> types;
    std::vector<std::string> names;
    bool variadic = false;
  };

  enum class ContextKind { GLOBAL, BLOCK, STRUCT, UNION, CLASS, ENUM, FUNCTION };


  struct GlobalParserState {
    std::vector<ContextKind> ctx_stack;
    AccessSpecifier current_access = AccessSpecifier::PRIVATE;
    TypePtr current_decl_base_type = nullptr;
    std::vector<TypeQualifier> current_decl_qualifiers;
    StorageClass current_storage = StorageClass::NONE;
	AccessSpecifier inherited_access = AccessSpecifier::PRIVATE;
	TypePtr parent_class_type = nullptr;
  TypePtr current_class_type = nullptr;
    void push_ctx(ContextKind k) { ctx_stack.push_back(k); }
    void pop_ctx() { if (!ctx_stack.empty()) ctx_stack.pop_back(); }

    void reset_decl() {
      current_decl_base_type = nullptr;
      current_decl_qualifiers.clear();
      current_storage = StorageClass::NONE;
    }
  };
}

%parse-param { Lexer& lexer }

%code {
  #include <iostream>
  #include <sstream>
  #include "lexer/lexer.hpp"
  #include "symbol_table/type_factory.hpp"
  #include "symbol_table/symbol_table.hpp"

  #undef yylex
  #define yylex lexer.yylex
  static SymbolTable symbol_table;
  static TypeFactory type_factory;
  static GlobalParserState parser_state;

  // Counter to uniquely name anonymous enums in a scope-stable way
  static size_t anon_enum_counter = 0;
  static size_t anon_struct_counter = 0;
  static size_t anon_union_counter = 0;
  static size_t anon_class_counter = 0;

  void print_parse_results()
  {
	std::cout<< "Custom Types:\n";
	symbol_table.print_custom_types();
	std::cout << "Symbol Table:\n";
	symbol_table.print_symbols();
  }

  static TypePtr builtin_type(const std::string& n) {
    auto opt = type_factory.get_type_by_name(n, 0);
    return opt.has_value() ? opt.value() : nullptr;
  }

  static std::string mangle_function_name(const std::string& base,
                                          const std::vector<TypePtr>& params,
                                          bool variadic) {
    std::ostringstream os;
    os << base << '(';
    for (size_t i = 0; i < params.size(); ++i) {
      if (i) os << ',';
      os << (params[i] ? params[i]->name : std::string("<null>"));
    }
    if (variadic) {
      if (!params.empty()) os << ',';
      os << "...";
    }
    os << ')';
    return os.str();
  }

  // Special mangling for constructors, destructors, and operators
  static std::string mangle_special_function_name(
      FunctionType::FunctionKind kind,
      TypePtr parent_class,
      const std::string& op_name,
      const std::vector<TypePtr>& params,
      bool variadic)
  {
    std::ostringstream os;
    switch (kind) {
      case FunctionType::FunctionKind::CONSTRUCTOR:
        os << "ctor " << (parent_class ? parent_class->name : std::string("<no-class>"));
        break;
      case FunctionType::FunctionKind::DESTRUCTOR:
        os << "dtor " << (parent_class ? parent_class->name : std::string("<no-class>"));
        break;
      case FunctionType::FunctionKind::OPERATOR_OVERLOAD:
        os << "op" << op_name;
        if (parent_class) {
          os << ' ' << parent_class->name;
        }
        break;
      default:
        return mangle_function_name(op_name, params, variadic);
    }
    os << '(';
    for (size_t i = 0; i < params.size(); ++i) {
      if (i) os << ',';
      os << (params[i] ? params[i]->name : std::string("<null>"));
    }
    if (variadic) {
      if (!params.empty()) os << ',';
      os << "...";
    }
    os << ')';
    return os.str();
  }

  static TypePtr make_function_type(TypePtr ret,
                                    const std::vector<TypePtr>& params,
                                    const std::vector<std::string>& param_names,
                                    bool variadic,
                                    TypePtr parent_class = nullptr) {
    std::ostringstream os;
    os << "fn(";
    for (size_t i = 0; i < params.size(); ++i) {
      if (i) os << ',';
      os << (params[i] ? params[i]->name : std::string("<null>"));
    }
    if (variadic) {
      if (!params.empty()) os << ',';
      os << "...";
    }
    os << ")->" << (ret ? ret->name : "<null>");
    FunctionType fnty(ret, params, param_names, variadic, FunctionType::FunctionKind::NORMAL, parent_class);
    return type_factory.make(os.str(),
                             symbol_table.get_current_scope_id(),
                             TypeCategory::FUNCTION,
                             TypeQualifier::NONE,
                             fnty);
  }

  // Helper to build special function types (ctor/dtor/operator)
  static TypePtr make_function_type_ex(
      TypePtr ret,
      const std::vector<TypePtr>& params,
      const std::vector<std::string>& param_names,
      bool variadic,
      FunctionType::FunctionKind kind,
      TypePtr parent_class = nullptr,
      const std::string& operator_name = std::string{})
  {
    std::ostringstream os;
    os << "fn(";
    for (size_t i = 0; i < params.size(); ++i) {
      if (i) os << ',';
      os << (params[i] ? params[i]->name : std::string("<null>"));
    }
    if (variadic) {
      if (!params.empty()) os << ',';
      os << "...";
    }
    os << ")->" << (ret ? ret->name : "<null>");
    FunctionType fnty(ret, params, param_names, variadic, kind, parent_class);
    if (kind == FunctionType::FunctionKind::OPERATOR_OVERLOAD) {
      fnty.operator_name = operator_name;
    }
    return type_factory.make(
        os.str(), symbol_table.get_current_scope_id(), TypeCategory::FUNCTION,
        TypeQualifier::NONE, fnty);
  }

  static void params_to_vectors(const std::vector<ParamDeclInfo>& in,
                                std::vector<TypePtr>& types,
                                std::vector<std::string>& names) {
    types.clear();
    names.clear();
    types.reserve(in.size());
    names.reserve(in.size());
    for (const auto& p : in) { types.push_back(p.type); names.push_back(p.name); }
  }

  static TypePtr apply_ptr_and_arrays(TypePtr base, size_t ptr_levels, const std::vector<size_t>& dims) {
    if (!base) return nullptr;
    TypePtr t = (ptr_levels > 0) ? type_factory.make_multi_level_pointer(base, ptr_levels, symbol_table.get_current_scope_id()) : base;
    if (!dims.empty()) {
      t = type_factory.make_multi_dimensional_array(t, dims, symbol_table.get_current_scope_id());
    }
    return t;
  }

  static void add_symbol_if_valid(const DeclaratorInfo& di, TypePtr type) {
    if (!di.name.empty() && type) {
      (void)symbol_table.add_symbol(di.name, type);
    }
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
%type <std::unordered_map<std::string, TypePtr>> struct_declaration_list
%type <std::unordered_map<std::string, TypePtr>> struct_declaration
%type <std::vector<DeclaratorInfo>> struct_declarator_list
%type <DeclaratorInfo> struct_declarator

%type <std::vector<DeclaratorInfo>> init_declarator_list
%type <DeclaratorInfo> init_declarator

%type <TypePtr> enum_specifier
%type <TypePtr> struct_or_union_specifier
%type <std::vector<std::string>> enumerator_list
%type <std::string> enumerator


%type <TypeCategory> struct_or_union
%type <AccessSpecifier> access_specifier
%type <TypePtr> class_specifier_tail
%type <ParamListInfo> parameter_type_list
%type <std::vector<ParamDeclInfo>> parameter_list
%type <ParamDeclInfo> parameter_declaration
%type <std::string> operator_token

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

%right UNARY /* unary + - ! ~ * & sizeof (sizeof removed) - mark unary precedence */
// %nonassoc POSTFIX_PREC  /* postfix ++ --, call, indexing has highest precedence */
// %right CAST_PREC

%start translation_unit

%%


open_brace
    : OPEN_BRACE_OP { symbol_table.enter_scope(); parser_state.push_ctx(ContextKind::BLOCK); }
    ;

close_brace
    : CLOSE_BRACE_OP { symbol_table.exit_scope(); parser_state.pop_ctx(); }
    ;

class_open_brace
  : OPEN_BRACE_OP { symbol_table.enter_scope(); parser_state.push_ctx(ContextKind::CLASS); }
  ;

class_close_brace
  : CLOSE_BRACE_OP { symbol_table.exit_scope(); parser_state.pop_ctx(); }
  ;

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
    	TypePtr base = $1;
        if (base) {
          for (const auto &di : $2) {
            if (di.is_function) {
              bool in_class = !parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type;
              if (!in_class && !di.name.empty()) {
                TypePtr fn = make_function_type(base, di.param_types, di.param_names, di.is_variadic, nullptr);
                std::string mangled = mangle_function_name(di.name, di.param_types, di.is_variadic);
                (void)symbol_table.add_symbol(mangled, fn);
              }
            } else {
              TypePtr final_t = apply_ptr_and_arrays(base, di.pointer_levels, di.array_dims);
              add_symbol_if_valid(di, final_t);
            }
          }
        }
        parser_state.reset_decl();
      }
    | TYPEDEF type_specifier pointer_opt TYPE_NAME SEMICOLON_OP
      {
        size_t ptrs = $3;
        TypePtr base = $2;
        TypePtr actual = apply_ptr_and_arrays(base, ptrs, {});
		if (actual) {
		  (void)type_factory.make($4, symbol_table.get_current_scope_id(), TypeCategory::TYPEDEF, TypeQualifier::NONE, TypedefType(actual));
		}
        parser_state.reset_decl();
      }
    ;

declaration_specifiers
  : storage_class_specifier declaration_specifiers { $$ = $2; parser_state.current_decl_base_type=$$; }
  | storage_class_specifier { $$ = nullptr; }
  | type_specifier declaration_specifiers { $$ = $1 ? $1 : $2; parser_state.current_decl_base_type=$$; }
  | type_specifier { $$ = $1; parser_state.current_decl_base_type=$$; }
  | type_qualifier declaration_specifiers { $$ = $2; parser_state.current_decl_base_type=$$; }
  | type_qualifier { $$ = nullptr; }
  ;

storage_class_specifier
    : STATIC
      {
        parser_state.current_storage = StorageClass::STATIC;
      }
    // | TYPEDEF
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
        TypePtr base = parser_state.current_decl_base_type;
        if (base) {
          TypePtr final_t = apply_ptr_and_arrays(base, di.pointer_levels, di.array_dims);
          add_symbol_if_valid(di, final_t);
          if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
            if (!di.name.empty()) {
              if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
                (*cls).fields[di.name] = type_access(parser_state.current_access, final_t);
              }
            }
          }
        }
      }
    | declarator
      {
        $$ = $1;
        const DeclaratorInfo &di = $1;
        TypePtr base = parser_state.current_decl_base_type;
        if (base) {
          TypePtr final_t = apply_ptr_and_arrays(base, di.pointer_levels, di.array_dims);
          add_symbol_if_valid(di, final_t);
          if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
            if (!di.name.empty()) {
              if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
                (*cls).fields[di.name] = type_access(parser_state.current_access, final_t);
              }
            }
          }
        }
      }
    ;

type_specifier
    : VOID     { $$ = builtin_type("void");     parser_state.current_decl_base_type = $$;  }
    | CHAR     { $$ = builtin_type("char");     parser_state.current_decl_base_type = $$; }
    | INT      { $$ = builtin_type("int");      parser_state.current_decl_base_type = $$; }
    | SIGNED   { $$ = builtin_type("signed");   parser_state.current_decl_base_type = $$; }
    | UNSIGNED { $$ = builtin_type("unsigned"); parser_state.current_decl_base_type = $$; }
    | BOOL     { $$ = builtin_type("bool");     parser_state.current_decl_base_type = $$; }
    | FLOAT    { $$ = builtin_type("float");    parser_state.current_decl_base_type = $$; }
  	| struct_or_union_specifier { $$ = $1; parser_state.current_decl_base_type = $$; }
    | enum_specifier { $$ = $1; parser_state.current_decl_base_type = $$; }
    | CLASS class_specifier_tail
      {
        $$ = $2;
        parser_state.current_decl_base_type = $$;
      }
    | TYPE_NAME
      {
        auto opt = type_factory.lookup_type($1, symbol_table.get_scope_chain());
        if (opt.has_value()) { $$ = opt.value(); parser_state.current_decl_base_type = $$; }
		else {
		  std::cerr << "Error: unknown type name '" << $1 << "'\n";
		  $$ = nullptr;
		}
      }
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
      {
        std::string name = ($1 == TypeCategory::STRUCT ? std::string("struct ") : std::string("union ")) + $2;
        // Create the aggregate type in the current (outer) scope
        if ($1 == TypeCategory::STRUCT) {
          $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::STRUCT, TypeQualifier::NONE, StructType($4));
        } else {
          $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::UNION, TypeQualifier::NONE, UnionType($4));
        }
        // Create a dedicated member scope and add fields as symbols
        symbol_table.enter_scope();
        for (const auto &kv : $4) {
          symbol_table.add_symbol(kv.first, kv.second);
        }
        symbol_table.exit_scope();
      }
    | struct_or_union OPEN_BRACE_OP struct_declaration_list CLOSE_BRACE_OP
      {
        std::ostringstream os;
        if ($1 == TypeCategory::STRUCT) {
          os << "<anon-struct@" << symbol_table.get_current_scope_id() << ":" << (anon_struct_counter++) << ">";
          $$ = type_factory.make(os.str(), symbol_table.get_current_scope_id(), TypeCategory::STRUCT, TypeQualifier::NONE, StructType($3));
        } else {
          os << "<anon-union@" << symbol_table.get_current_scope_id() << ":" << (anon_union_counter++) << ">";
          $$ = type_factory.make(os.str(), symbol_table.get_current_scope_id(), TypeCategory::UNION, TypeQualifier::NONE, UnionType($3));
        }
        symbol_table.enter_scope();
        for (const auto &kv : $3) {
          symbol_table.add_symbol(kv.first, kv.second);
        }
        symbol_table.exit_scope();
      }
    | struct_or_union IDENTIFIER
      {
        std::string name = ($1 == TypeCategory::STRUCT ? std::string("struct ") : std::string("union ")) + $2;
        // Try to find an existing type in the scope chain; otherwise create a forward declaration
        auto found = type_factory.lookup_type(name, symbol_table.get_scope_chain());
        if (found.has_value()) {
          $$ = found.value();
        } else {
          if ($1 == TypeCategory::STRUCT) {
            $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::STRUCT, TypeQualifier::NONE, StructType{});
          } else {
            $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::UNION, TypeQualifier::NONE, UnionType{});
          }
        }
      }
	| struct_or_union IDENTIFIER OPEN_BRACE_OP CLOSE_BRACE_OP // to allow empty structs/unions
      {
        std::string name = ($1 == TypeCategory::STRUCT ? std::string("struct ") : std::string("union ")) + $2;
        if ($1 == TypeCategory::STRUCT) {
          $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::STRUCT, TypeQualifier::NONE, StructType{});
        } else {
          $$ = type_factory.make(name, symbol_table.get_current_scope_id(), TypeCategory::UNION, TypeQualifier::NONE, UnionType{});
        }
      }
    ;

struct_or_union
    : STRUCT  { $$ = TypeCategory::STRUCT; /* context can be tracked if needed */ }
    | UNION   { $$ = TypeCategory::UNION; }
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
        $$ = std::unordered_map<std::string, TypePtr>{};
      }
    | specifier_qualifier_list struct_declarator_list SEMICOLON_OP
      {
        $$ = std::unordered_map<std::string, TypePtr>{};
        TypePtr base = $1;
        if (base) {
          for (const auto &di : $2) {
            if (!di.name.empty()) {
              TypePtr field_t = apply_ptr_and_arrays(base, di.pointer_levels, di.array_dims);
              if (field_t) {
                $$[di.name] = field_t;
              }
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
        std::string enum_name = "enum " + $2;
        TypePtr t = type_factory.make(enum_name, symbol_table.get_current_scope_id(),
                            TypeCategory::ENUM, TypeQualifier::NONE, EnumType{});
        std::unordered_set<EnumConstant, EnumConstant::Hash> values;
        int64_t v = 0;
        for (const auto& nm : $4) {
          values.insert(EnumConstant(v, t));
			type_factory.make(nm, symbol_table.get_current_scope_id(), TypeCategory::ENUM_CONSTANT, TypeQualifier::NONE, EnumConstant(v, t));
		  ++v;
        }
        t->info = EnumType(std::move(values));
        $$ = t;
      }
    | ENUM OPEN_BRACE_OP enumerator_list CLOSE_BRACE_OP
      {
        std::ostringstream os;
        os << "<anon-enum@" << symbol_table.get_current_scope_id() << ":" << (anon_enum_counter++) << ">";
        TypePtr t = type_factory.make(os.str(), symbol_table.get_current_scope_id(),
                            TypeCategory::ENUM, TypeQualifier::NONE, EnumType{});
        std::unordered_set<EnumConstant, EnumConstant::Hash> values;
        int64_t v = 0;
        for (const auto& nm : $3) {
          values.insert(EnumConstant(v, t));
		  type_factory.make(nm, symbol_table.get_current_scope_id(), TypeCategory::ENUM_CONSTANT, TypeQualifier::NONE, EnumConstant(v, t));
        }
        t->info = EnumType(std::move(values));
        $$ = t;
      }
    | ENUM IDENTIFIER
      {
        std::string enum_name = "enum " + $2;
        TypePtr t = type_factory.make(enum_name, symbol_table.get_current_scope_id(),
                            TypeCategory::ENUM, TypeQualifier::NONE, EnumType{});
        $$ = t;
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
      { parser_state.current_decl_qualifiers.push_back(TypeQualifier::CONST); }
    | VOLATILE
      { parser_state.current_decl_qualifiers.push_back(TypeQualifier::VOLATILE); }
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
    | direct_declarator OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
      {
        $$ = $1;
        // For now, we don't evaluate constant_expression; use 0 placeholder.
        $$.array_dims.push_back(0);
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
        TypePtr base = parser_state.current_decl_base_type;
        if (base) {
          TypePtr t = apply_ptr_and_arrays(base, $2.pointer_levels, $2.array_dims);
          $$.type = t;
        }
        $$.name = $2.name; // may be empty if unnamed
        parser_state.reset_decl();
      }
    | declaration_specifiers abstract_declarator
      {
        $$ = ParamDeclInfo{};
        TypePtr base = parser_state.current_decl_base_type;
        if (base) {
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
	| declaration   // to allow declarations as statements for gotos
    ;

labeled_statement
    : IDENTIFIER COLON_OP statement {
		auto label=type_factory.lookup_type($1, symbol_table.get_scope_chain());
		if (!label.has_value()) {
		  label = type_factory.make("label", 0, TypeCategory::LABEL, TypeQualifier::NONE, LabelType{});
		}
		(void)symbol_table.add_symbol($1, label.value());
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
		TypePtr base = $1;
		if (base) {
		  const DeclaratorInfo &di = $2;
			if (di.is_function && !di.name.empty()) {
			TypePtr fn = make_function_type(base, di.param_types, di.param_names, di.is_variadic, nullptr);
			std::string mangled = mangle_function_name(di.name, di.param_types, di.is_variadic);
			(void)symbol_table.add_symbol(mangled, fn);
			} else {
				std::cerr << "Error: function definition requires function declarator\n";
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
      parser_state.current_class_type = type_factory.make(os.str(), symbol_table.get_current_scope_id(), TypeCategory::CLASS, TypeQualifier::NONE, ClassType{});
      parser_state.current_access = AccessSpecifier::PRIVATE; // default for class
    }
    class_open_brace class_member_list class_close_brace
    {
      if (parser_state.parent_class_type) {
        if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
          cls->set_base_class(parser_state.parent_class_type, parser_state.inherited_access);
        }
      }
      $$ = parser_state.current_class_type;
      parser_state.current_class_type = nullptr;
      parser_state.parent_class_type = nullptr;
    }
  | IDENTIFIER inheritance_opt
    {
      std::string full_name = std::string("class ") + $1;
      auto found = type_factory.lookup_type(full_name, symbol_table.get_scope_chain());
      if (found.has_value()) {
        parser_state.current_class_type = found.value();
      } else {
        parser_state.current_class_type = type_factory.make(full_name, symbol_table.get_current_scope_id(), TypeCategory::CLASS, TypeQualifier::NONE, ClassType{});
      }
      parser_state.current_access = AccessSpecifier::PRIVATE;
    }
    class_open_brace class_member_list class_close_brace
    {
      if (parser_state.parent_class_type) {
        if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
          cls->set_base_class(parser_state.parent_class_type, parser_state.inherited_access);
        }
      }
      $$ = parser_state.current_class_type;
      parser_state.current_class_type = nullptr;
      parser_state.parent_class_type = nullptr;
    }
  | IDENTIFIER inheritance_opt
    {
      std::string full_name = std::string("class ") + $1;
      auto found = type_factory.lookup_type(full_name, symbol_table.get_scope_chain());
      if (found.has_value()) {
        $$ = found.value();
      } else {
        $$ = type_factory.make(full_name, symbol_table.get_current_scope_id(), TypeCategory::CLASS, TypeQualifier::NONE, ClassType{});
      }

      if (parser_state.parent_class_type) {
        std::cerr << "Error: inheritance list provided without class definition for '" << full_name << "'\n";
      }
      parser_state.parent_class_type = nullptr;
    }
  | inheritance_opt class_open_brace class_close_brace
    {
      // empty anonymous class
      std::ostringstream os;
      os << "<anon-class@" << symbol_table.get_current_scope_id() << ":" << (anon_class_counter++) << ">";
      TypePtr t = type_factory.make(os.str(), symbol_table.get_current_scope_id(), TypeCategory::CLASS, TypeQualifier::NONE, ClassType{});
      if (parser_state.parent_class_type) {
        if (auto cls = std::get_if<ClassType>(&t->info)) {
          cls->set_base_class(parser_state.parent_class_type, parser_state.inherited_access);
        }
      }
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
    std::string full = std::string("class ") + $2;
    auto opt = type_factory.lookup_type(full, symbol_table.get_scope_chain());
    if (opt.has_value() && opt.value()->category == TypeCategory::CLASS) {
      parser_state.parent_class_type = opt.value();
      parser_state.inherited_access = $1;
    } else {
      std::cerr << "Error: unknown class name '" << $2 << "' for inheritance\n";
      parser_state.parent_class_type = nullptr;
    }
	}
    | IDENTIFIER {
    std::string full = std::string("class ") + $1;
    auto opt = type_factory.lookup_type(full, symbol_table.get_scope_chain());
    if (opt.has_value() && opt.value()->category == TypeCategory::CLASS) {
      parser_state.parent_class_type = opt.value();
      parser_state.inherited_access = AccessSpecifier::PRIVATE; // default
    } else {
      std::cerr << "Error: unknown class name '" << $1 << "' for inheritance\n";
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
  : PUBLIC    { $$ = AccessSpecifier::PUBLIC; }
  | PRIVATE   { $$ = AccessSpecifier::PRIVATE; }
  | PROTECTED { $$ = AccessSpecifier::PROTECTED; }
  ;

function_declaration_or_definition
    : declaration_specifiers declarator SEMICOLON_OP
      {
        const DeclaratorInfo &di = $2;
  		TypePtr ret = $1;
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type && !di.name.empty()) {
          if (di.is_function) {
            TypePtr fn = make_function_type(ret, di.param_types, di.param_names, di.is_variadic, parser_state.current_class_type);
            std::string mangled = mangle_function_name(di.name, di.param_types, di.is_variadic);
            if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
              (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
            }
            symbol_table.add_symbol(mangled, fn);
          } else {
            TypePtr final_t = apply_ptr_and_arrays(ret, di.pointer_levels, di.array_dims);
            if (final_t) {
              symbol_table.add_symbol(di.name, final_t);
              if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
                (*cls).fields[di.name] = type_access(parser_state.current_access, final_t);
              }
            }
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
            TypePtr fn = make_function_type(ret, di.param_types, di.param_names, di.is_variadic, parser_state.current_class_type);
            std::string mangled = mangle_function_name(di.name, di.param_types, di.is_variadic);
            if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
              (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
            }
            symbol_table.add_symbol(mangled, fn);
          } else {
            TypePtr final_t = apply_ptr_and_arrays(ret, di.pointer_levels, di.array_dims);
            if (final_t) {
              symbol_table.add_symbol(di.name, final_t);
              if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
                (*cls).fields[di.name] = type_access(parser_state.current_access, final_t);
              }
            }
          }
        }
        parser_state.reset_decl();
      }
    | /* constructor */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          // Optional check: name matches class
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = builtin_type("void");
          const auto &plist = $3;
          TypePtr fn = make_function_type_ex(ret, plist.types, plist.names, plist.variadic,
                                             FunctionType::FunctionKind::CONSTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::CONSTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        plist.types,
        plist.variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
      }
  | /* constructor without params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = builtin_type("void");
          std::vector<TypePtr> params; std::vector<std::string> names; bool variadic = false;
          TypePtr fn = make_function_type_ex(ret, params, names, variadic,
                                             FunctionType::FunctionKind::CONSTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::CONSTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        params,
        variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
      }
  | /* destructor */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP compound_statement
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $2;
          if (expected != got) {
            std::cerr << "Warning: destructor name '~" << $2 << "' does not match enclosing class '" << expected << "'\n";
          }
          TypePtr ret = builtin_type("void");
          std::vector<TypePtr> params; std::vector<std::string> names; bool variadic = false;
          TypePtr fn = make_function_type_ex(ret, params, names, variadic,
                                             FunctionType::FunctionKind::DESTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::DESTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        params,
        variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
      }
  | /* operator overload def */ IDENTIFIER OPERATOR operator_token OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP compound_statement
      {
		std::string expected = parser_state.current_class_type ? parser_state.current_class_type->name : std::string{};
		std::string got = std::string("class ") + $1;
		if (expected != got) {
		  std::cerr << "Warning: operator overload function name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
		}
		TypePtr ret = type_factory.lookup_type($1, symbol_table.get_scope_chain()).value_or(nullptr);
        if (ret && !parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          const auto &plist = $5;
          std::string opname = $3;
          TypePtr fn = make_function_type_ex(ret, plist.types, plist.names, plist.variadic,
                                             FunctionType::FunctionKind::OPERATOR_OVERLOAD,
                                             parser_state.current_class_type,
                                             opname);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::OPERATOR_OVERLOAD,
        parser_state.current_class_type,
        opname,
        plist.types,
        plist.variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
            // Also record in operator_overloads map (best-effort)
            (*cls).add_operator_overload(opname, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
        parser_state.reset_decl();
      }
    | /* constructor decl */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          const auto &plist = $3;
          TypePtr ret = builtin_type("void");
          TypePtr fn = make_function_type_ex(ret, plist.types, plist.names, plist.variadic,
                                             FunctionType::FunctionKind::CONSTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::CONSTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        plist.types,
        plist.variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
      }
    | /* constructor decl no params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $1;
          if (expected != got) {
            std::cerr << "Warning: constructor name '" << $1 << "' does not match enclosing class '" << expected << "'\n";
          }
          std::vector<TypePtr> params; std::vector<std::string> names; bool variadic = false;
          TypePtr ret = builtin_type("void");
          TypePtr fn = make_function_type_ex(ret, params, names, variadic,
                                             FunctionType::FunctionKind::CONSTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::CONSTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        params,
        variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
        }
      }
    | /* destructor decl */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        if (!parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type) {
          std::string expected = parser_state.current_class_type->name;
          std::string got = std::string("class ") + $2;
          if (expected != got) {
            std::cerr << "Warning: destructor name '~" << $2 << "' does not match enclosing class '" << expected << "'\n";
          }
          std::vector<TypePtr> params; std::vector<std::string> names; bool variadic = false;
          TypePtr ret = builtin_type("void");
          TypePtr fn = make_function_type_ex(ret, params, names, variadic,
                                             FunctionType::FunctionKind::DESTRUCTOR,
                                             parser_state.current_class_type);
      std::string mangled = mangle_special_function_name(
        FunctionType::FunctionKind::DESTRUCTOR,
        parser_state.current_class_type,
        /*op*/ std::string{},
        params,
        variadic);
          if (auto cls = std::get_if<ClassType>(&parser_state.current_class_type->info)) {
            (*cls).methods[mangled] = type_access(parser_state.current_access, fn);
          }
          symbol_table.add_symbol(mangled, fn);
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
    std::cerr << "Parse error at " << loc << ": " << msg << '\n';
}
