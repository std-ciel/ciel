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
  #include <functional>

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
    ASTNodePtr initializer = nullptr;  // Added to store initializer expression
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

  enum class TypeUsageContext {
    VARIABLE_DECLARATION,
    FUNCTION_RETURN_TYPE,
    FUNCTION_PARAMETER,
    CLASS_DATA_MEMBER,
    STRUCT_UNION_MEMBER,
    CLASS_INHERITANCE,
    OPERATOR_OVERLOAD_RETURN
  };

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

    // Track defined functions by mangled name to detect redefinitions
    std::unordered_set<std::string> defined_functions;

    StorageClass current_storage = StorageClass::STATIC;

    // Track forward declarations that need definitions
    std::unordered_set<std::string> forward_declared_types;
    std::unordered_set<std::string> defined_types;
    std::unordered_map<std::string, std::pair<int, int>> forward_decl_locations; // type name -> (line, column)

    std::vector<QualifiedType> pending_param_types;
    std::vector<std::string> pending_param_names;
    bool has_pending_params = false;

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
  std::unordered_set<std::string> encountered_function_names;
  
  // External reference to the global parsed_translation_unit
  extern std::vector<ASTNodePtr> parsed_translation_unit;
  extern SymbolTable global_symbol_table;
  extern TypeFactory global_type_factory;

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
      return strip_typedefs(type_result.value());
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

  // -------- Centralized semantic helpers for statements --------

  static void ensure_condition_is_bool(const ASTNodePtr& expr, const yy::location& loc, const std::string& context) {
    auto t = get_expression_type(expr, loc, context);
    if (!t) return; // type error already reported
    if (!is_bool_type(t)) {
      parser_add_error(loc.begin.line, loc.begin.column, context + ": condition must be of type 'bool', got '" + t->debug_name() + "'");
    }
  }

  // Also sets the switch subject type in parser_state
  static void ensure_switch_subject_type(const ASTNodePtr& expr, const yy::location& loc) {
    auto t = get_expression_type(expr, loc, "switch subject");
    if (!t) return;
    if (!is_integral_or_enum_non_bool(t)) {
      parser_add_error(loc.begin.line, loc.begin.column, "switch subject must be integral or enum (excluding bool), got '" + t->debug_name() + "'");
    }
    parser_state.switch_subject_stack.push_back(t);
  }

  static bool in_loop() {
    for (auto it = parser_state.ctx_stack.rbegin(); it != parser_state.ctx_stack.rend(); ++it) {
      if (*it == ContextKind::LOOP) return true;
    }
    return false;
  }
  static bool in_switch() {
    for (auto it = parser_state.ctx_stack.rbegin(); it != parser_state.ctx_stack.rend(); ++it) {
      if (*it == ContextKind::SWITCH) return true;
    }
    return false;
  }
  static bool in_class() {
    for(auto it = parser_state.ctx_stack.rbegin(); it != parser_state.ctx_stack.rend(); ++it) {
      if(*it == ContextKind::CLASS) return true;
    }
    return false;
  }
  static bool in_loop_or_switch() { return in_loop() || in_switch(); }

  // -------- Function definition semantic helpers --------

  // Check if a non-void function body contains at least one return with a value
  static bool ast_contains_return_with_value(const ASTNodePtr& node) {
    if (!node) return false;

    switch (node->type) {
      case ASTNodeType::RET_EXPR: {
        auto ret = std::static_pointer_cast<RetExpr>(node);
        return ret->value.has_value();
      }
      case ASTNodeType::COMPOUND_STMT: {
        auto blk = std::static_pointer_cast<CompoundStmt>(node);
        for (const auto& s : blk->statements)
          if (ast_contains_return_with_value(s)) return true;
        return false;
      }
      case ASTNodeType::IF_STMT: {
        auto ifs = std::static_pointer_cast<IfStmt>(node);
        if (ast_contains_return_with_value(ifs->then_branch)) return true;
        if (ifs->else_branch && ast_contains_return_with_value(ifs->else_branch.value())) return true;
        return false;
      }
      case ASTNodeType::SWITCH_STMT: {
        auto sw = std::static_pointer_cast<SwitchStmt>(node);
        for (const auto& c : sw->cases)
          if (ast_contains_return_with_value(c)) return true;
        if (sw->default_case && ast_contains_return_with_value(sw->default_case.value())) return true;
        return false;
      }
      case ASTNodeType::CASE_STMT: {
        auto cs = std::static_pointer_cast<CaseStmt>(node);
        return ast_contains_return_with_value(cs->statement);
      }
      case ASTNodeType::DEFAULT_STMT: {
        auto ds = std::static_pointer_cast<DefaultStmt>(node);
        return ast_contains_return_with_value(ds->statement);
      }
      case ASTNodeType::FOR_STMT: {
        auto fs = std::static_pointer_cast<ForStmt>(node);
        return ast_contains_return_with_value(fs->body);
      }
      case ASTNodeType::WHILE_STMT: {
        auto ws = std::static_pointer_cast<WhileStmt>(node);
        return ast_contains_return_with_value(ws->body);
      }
      case ASTNodeType::DO_WHILE_STMT: {
        auto dw = std::static_pointer_cast<DoWhileStmt>(node);
        return ast_contains_return_with_value(dw->body);
      }
      case ASTNodeType::UNTIL_STMT: {
        auto us = std::static_pointer_cast<UntilStmt>(node);
        return ast_contains_return_with_value(us->body);
      }
      case ASTNodeType::LABEL_STMT: {
        auto ls = std::static_pointer_cast<LabelStmt>(node);
        return ast_contains_return_with_value(ls->statement);
      }
      default:
        return false;
    }
  }

  static void check_function_returns(TypePtr ret_type, const ASTNodePtr& body, const yy::location& loc) {
    if (!ret_type || !body) return;
    if (!is_void_type(ret_type) && !ast_contains_return_with_value(body)) {
      parser_add_error(loc.begin.line, loc.begin.column,
        "non-void function may exit without returning a value");
    }
  }

  static bool note_function_definition(const std::string& mangled, const yy::location& loc) {
    auto& set = parser_state.defined_functions;
    if (!set.insert(mangled).second) {
      parser_add_error(loc.begin.line, loc.begin.column,
        "redefinition of function '" + mangled + "'");
      return false;
    }
    return true;
  }


  static bool is_in_member_function_of_class() {
    if (!parser_state.current_class_type) {
      return false;
    }

    bool has_class = false;
    bool has_function = false;
    for (auto ctx : parser_state.ctx_stack) {
      if (ctx == ContextKind::CLASS) has_class = true;
      if (ctx == ContextKind::FUNCTION) has_function = true;
    }
    return has_class && has_function;
  }

  static std::string type_usage_context_to_string(TypeUsageContext ctx) {
    switch (ctx) {
      case TypeUsageContext::VARIABLE_DECLARATION: return "variable declaration";
      case TypeUsageContext::FUNCTION_RETURN_TYPE: return "function return type";
      case TypeUsageContext::FUNCTION_PARAMETER: return "function parameter";
      case TypeUsageContext::CLASS_DATA_MEMBER: return "class data member";
      case TypeUsageContext::STRUCT_UNION_MEMBER: return "struct/union member";
      case TypeUsageContext::CLASS_INHERITANCE: return "class inheritance";
      case TypeUsageContext::OPERATOR_OVERLOAD_RETURN: return "operator overload return type";
      default: return "unknown context";
    }
  }

  static bool check_complete_type(TypePtr type, const yy::location& loc, TypeUsageContext usage_ctx) {
    if (!type) {
      return false;
    }

    // Special case: allow using the current class type within its own definition in specific contexts
    bool is_data_member_context = (usage_ctx == TypeUsageContext::CLASS_DATA_MEMBER ||
                                    usage_ctx == TypeUsageContext::STRUCT_UNION_MEMBER);

    if (!is_data_member_context && parser_state.current_class_type) {
      // Check if the type matches the current class being defined
      if (type->debug_name() == parser_state.current_class_type->debug_name()) {
        // Allow in function signatures (return types, parameters) and local variables inside member functions
        bool is_function_signature = (usage_ctx == TypeUsageContext::FUNCTION_RETURN_TYPE ||
                                       usage_ctx == TypeUsageContext::FUNCTION_PARAMETER ||
                                       usage_ctx == TypeUsageContext::OPERATOR_OVERLOAD_RETURN);
        bool is_local_variable_in_member = (usage_ctx == TypeUsageContext::VARIABLE_DECLARATION &&
                                             is_in_member_function_of_class());

        if (is_function_signature || is_local_variable_in_member) {
          return true; // Allow self-reference in these contexts
        }
      }
    }

    if (!is_complete_type(type)) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "incomplete type '" + type->debug_name() + "' used in " +
                       type_usage_context_to_string(usage_ctx));
      return false;
    }
    return true;
  }

  // Check that all parameters in a function definition have names
  static void check_unnamed_parameters(const std::vector<std::string>& param_names, const yy::location& loc) {
    for (size_t i = 0; i < param_names.size(); ++i) {
      if (param_names[i].empty()) {
        parser_add_error(loc.begin.line, loc.begin.column,
                         "function definition cannot have unnamed parameters (parameter " +
                         std::to_string(i + 1) + ")");
      }
    }
  }

  // Check that all parameters in a function definition have unique names
  static void check_duplicate_parameter_names(const std::vector<std::string>& param_names, const yy::location& loc) {
    std::unordered_set<std::string> seen_names;
    for (size_t i = 0; i < param_names.size(); ++i) {
      const std::string& name = param_names[i];
      if (!name.empty()) {
        if (!seen_names.insert(name).second) {
          parser_add_error(loc.begin.line, loc.begin.column,
                           "duplicate parameter name '" + name + "' in function definition");
        }
      }
    }
  }

  static void prepare_parameters_for_scope(const std::vector<QualifiedType>& param_types,
                                            const std::vector<std::string>& param_names) {
    parser_state.pending_param_types = param_types;
    parser_state.pending_param_names = param_names;
    parser_state.has_pending_params = true;
  }

  static void add_pending_parameters_to_scope(const yy::location& loc) {
    if (parser_state.has_pending_params) {
      for (size_t i = 0; i < parser_state.pending_param_names.size(); ++i) {
        if (!parser_state.pending_param_names[i].empty() && i < parser_state.pending_param_types.size()) {
          add_symbol_if_valid(parser_state.pending_param_names[i], parser_state.pending_param_types[i], loc);
        }
      }
      parser_state.has_pending_params = false;
      parser_state.pending_param_types.clear();
      parser_state.pending_param_names.clear();
    }
  }

  static void handle_operator_overload_definition(
      TypePtr return_type,
      const std::string& operator_name,
      const std::vector<QualifiedType>& param_types,
      const std::vector<std::string>& param_names,
      bool is_variadic,
      const ASTNodePtr& body,
      const yy::location& loc_ret,
      const yy::location& loc_op)
  {
    if (!return_type || parser_state.ctx_stack.empty() ||
        !in_class() ||
        !parser_state.current_class_type) {
      return;
    }

    check_complete_type(return_type, loc_ret, TypeUsageContext::OPERATOR_OVERLOAD_RETURN);

    TypePtr fn = make_function_type_or_error(
        return_type,
        param_types,
        is_variadic,
        "operator definition",
        loc_ret.begin.line,
        loc_ret.begin.column);

    if (!fn) return;

    FunctionMeta meta(FunctionKind::OPERATOR, param_names, parser_state.current_class_type);

    auto mangled = mangle_function_name(
        operator_name,
        *std::static_pointer_cast<FunctionType>(fn),
        meta,
        *std::static_pointer_cast<ClassType>(parser_state.current_class_type));

    if (!mangled.has_value()) {
      parser_add_error(loc_op.begin.line, loc_op.begin.column,
                       "unable to mangle operator '" + operator_name + "'");
      return;
    }

    // Check that the function body returns properly
    check_function_returns(return_type, body, loc_ret);

    // Note this as a defined function
    note_function_definition(*mangled, loc_op);

    auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
    std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

    add_symbol_if_valid(*mangled,
                        QualifiedType(fn, Qualifier::NONE),
                        loc_ret,
                        std::optional<FunctionMeta>{meta});
  }

  static void handle_constructor_definition(
      const std::string& name,
      const std::vector<QualifiedType>& param_types,
      const std::vector<std::string>& param_names,
      bool is_variadic,
      const ASTNodePtr& body,
      const yy::location& loc)
  {
    if (parser_state.ctx_stack.empty() ||
        in_class() ||
        !parser_state.current_class_type) {
      return;
    }

    std::string expected = parser_state.current_class_type->debug_name();
    std::string got = std::string("class ") + name;
    if (expected != got) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "constructor name '" + name + "' does not match class name");
      return;
    }

    TypePtr ret = require_builtin("void", loc, "constructor return type");
    if (!ret) return;

    TypePtr fn = make_function_type_or_error(
        ret, param_types, is_variadic,
        "constructor definition",
        loc.begin.line, loc.begin.column);

    if (!fn) return;

    FunctionMeta meta(FunctionKind::CONSTRUCTOR, param_names, parser_state.current_class_type);

    auto mangled = mangle_function_name(
        name,
        *std::static_pointer_cast<FunctionType>(fn),
        meta,
        *std::static_pointer_cast<ClassType>(parser_state.current_class_type));

    if (!mangled.has_value()) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "unable to mangle constructor '" + name + "'");
      return;
    }

    note_function_definition(*mangled, loc);

    auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
    std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

    add_symbol_if_valid(*mangled,
                        QualifiedType(fn, Qualifier::NONE),
                        loc,
                        std::optional<FunctionMeta>{meta});
  }

  static void handle_destructor_definition(
      const std::string& name,
      const ASTNodePtr& body,
      const yy::location& loc)
  {
    if (parser_state.ctx_stack.empty() ||
        in_class() ||
        !parser_state.current_class_type) {
      return;
    }

    std::string expected = parser_state.current_class_type->debug_name();
    std::string got = std::string("class ") + name;
    if (expected != got) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "destructor name '" + name + "' does not match class name");
      return;
    }

    TypePtr ret = require_builtin("void", loc, "destructor return type");
    if (!ret) return;

    std::vector<QualifiedType> params;
    std::vector<std::string> names;
    bool variadic = false;

    TypePtr fn = make_function_type_or_error(
        ret, params, variadic,
        "destructor definition",
        loc.begin.line, loc.begin.column);

    if (!fn) return;

    FunctionMeta meta(FunctionKind::DESTRUCTOR, names, parser_state.current_class_type);

    auto mangled = mangle_function_name(
        std::string("~") + name,
        *std::static_pointer_cast<FunctionType>(fn),
        meta,
        *std::static_pointer_cast<ClassType>(parser_state.current_class_type));

    if (!mangled.has_value()) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "unable to mangle destructor '~" + name + "'");
      return;
    }

    note_function_definition(*mangled, loc);

    auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
    std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

    add_symbol_if_valid(*mangled,
                        QualifiedType(fn, Qualifier::NONE),
                        loc,
                        std::optional<FunctionMeta>{meta});
  }

  static void handle_constructor_declaration(
      const std::string& name,
      const std::vector<QualifiedType>& param_types,
      const std::vector<std::string>& param_names,
      bool is_variadic,
      const yy::location& loc)
  {
    if (parser_state.ctx_stack.empty() ||
        in_class() ||
        !parser_state.current_class_type) {
      return;
    }

    std::string expected = parser_state.current_class_type->debug_name();
    std::string got = std::string("class ") + name;
    if (expected != got) {
      std::cerr << "Warning: constructor name '" << name << "' does not match enclosing class '" << expected << "'\n";
    }

    TypePtr ret = require_builtin("void", loc, "constructor return type");
    if (!ret) return;

    TypePtr fn = make_function_type_or_error(
        ret, param_types, is_variadic,
        "constructor declaration",
        loc.begin.line, loc.begin.column);

    if (!fn) return;

    FunctionMeta meta(FunctionKind::CONSTRUCTOR, param_names, parser_state.current_class_type);

    auto mangled = mangle_function_name(
        name,
        *std::static_pointer_cast<FunctionType>(fn),
        meta,
        *std::static_pointer_cast<ClassType>(parser_state.current_class_type));

    if (!mangled.has_value()) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "unable to mangle constructor '" + name + "'");
      return;
    }

    auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
    std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

    add_symbol_if_valid(*mangled,
                        QualifiedType(fn, Qualifier::NONE),
                        loc,
                        std::optional<FunctionMeta>{meta});
  }

  static void handle_destructor_declaration(
      const std::string& name,
      const yy::location& loc)
  {
    if (parser_state.ctx_stack.empty() ||
        in_class() ||
        !parser_state.current_class_type) {
      return;
    }

    std::string expected = parser_state.current_class_type->debug_name();
    std::string got = std::string("class ") + name;
    if (expected != got) {
      std::cerr << "Warning: destructor name '~" << name << "' does not match enclosing class '" << expected << "'\n";
    }

    TypePtr ret = require_builtin("void", loc, "destructor return type");
    if (!ret) return;

    std::vector<QualifiedType> params;
    std::vector<std::string> names;
    bool variadic = false;

    TypePtr fn = make_function_type_or_error(
        ret, params, variadic,
        "destructor declaration",
        loc.begin.line, loc.begin.column);

    if (!fn) return;

    FunctionMeta meta(FunctionKind::DESTRUCTOR, names, parser_state.current_class_type);

    auto mangled = mangle_function_name(
        std::string("~") + name,
        *std::static_pointer_cast<FunctionType>(fn),
        meta,
        *std::static_pointer_cast<ClassType>(parser_state.current_class_type));

    if (!mangled.has_value()) {
      parser_add_error(loc.begin.line, loc.begin.column,
                       "unable to mangle destructor '~" + name + "'");
      return;
    }

    auto mi = MemberInfo{QualifiedType(fn, Qualifier::NONE), parser_state.current_access, false};
    std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(*mangled, mi);

    add_symbol_if_valid(*mangled,
                        QualifiedType(fn, Qualifier::NONE),
                        loc,
                        std::optional<FunctionMeta>{meta});
  }

  std::optional<std::string> mangle_operator_function_name(TypePtr left_type,
                                            const std::string& op_symbol,
                                            TypePtr right_type = nullptr) {
    FunctionMeta meta(FunctionKind::OPERATOR, {});
    FunctionType function_type{};

    if(right_type){
      function_type = FunctionType(QualifiedType{},
                                {QualifiedType(right_type, Qualifier::NONE)},
                                false);
    }
    else{
      function_type = FunctionType(QualifiedType{},
                                {},
                                false);
    }

    auto mangled = mangle_function_name(op_symbol, function_type, meta, *std::static_pointer_cast<ClassType>(left_type));
    if (!mangled.has_value()) {
      std::cerr << "Warning: unable to mangle operator '" << op_symbol << "'\n";
      return std::nullopt;
    }
    return mangled;
  }

  static SymbolPtr get_operator_overload(TypePtr left_type, const std::string& op_symbol, TypePtr right_type = nullptr) {

    if (!left_type) {
      return nullptr;
    }

    auto mangled_name = mangle_operator_function_name(left_type, op_symbol, right_type);

    if(!mangled_name.has_value()){
      return nullptr;
    }

    symbol_table.print_symbols();
    auto sym_opt = symbol_table.lookup_operator(mangled_name.value());
    if (sym_opt.has_value()) {
      return sym_opt.value();
    }

    return nullptr;
  }

  static TypePtr get_higher_rank_type(TypePtr lhs, TypePtr rhs) {
    if (!lhs || !rhs) {
      return nullptr;
    }

    auto lhs_canonical = strip_typedefs(lhs);
    auto rhs_canonical = strip_typedefs(rhs);

    auto lhs_builtin = std::static_pointer_cast<BuiltinType>(lhs_canonical);
    auto rhs_builtin = std::static_pointer_cast<BuiltinType>(rhs_canonical);

    if (!lhs_builtin || !rhs_builtin) {
      return nullptr;
    }

    std::unordered_map<BuiltinTypeKind, int> rank_map = {
        {BuiltinTypeKind::CHAR, 1},
        {BuiltinTypeKind::INT, 2},
        {BuiltinTypeKind::UNSIGNED, 3},
        {BuiltinTypeKind::FLOAT, 4}
    };

    int lhs_rank = rank_map[lhs_builtin->builtin_kind];
    int rhs_rank = rank_map[rhs_builtin->builtin_kind];

    return (lhs_rank >= rhs_rank) ? lhs : rhs;
  }

  static std::optional<ASTNodePtr> try_operator_overload(
      TypePtr operand_type,
      const std::string& op_symbol,
      const std::vector<ASTNodePtr>& operands,
      const yy::location& loc,
      const std::string& op_name,
      TypePtr right_type = nullptr)
  {
    if (!is_class_type(operand_type)) {
      return std::nullopt;
    }

    SymbolPtr overload = get_operator_overload(operand_type, op_symbol, right_type);
    if (overload) {
      TypePtr function_type = overload->get_type().type;
      TypePtr result_type = std::static_pointer_cast<FunctionType>(function_type)->return_type.type;
      return std::make_shared<CallExpr>(overload, operands, result_type);
    } else {
      if (op_symbol == "&" && operands.size() == 1) {
        // Special case: address-of operator can always be applied to class types
        QualifiedType ptr = apply_pointer_levels_or_error(
            QualifiedType(operand_type, Qualifier::NONE),
            1,
            "address-of operator",
            loc.begin.line,
            loc.begin.column);
        if (ptr.type == nullptr) {
          return std::nullopt;
        }
        TypePtr result_type = ptr.type;
        return std::make_shared<UnaryExpr>(Operator::ADDRESS_OF, operands[0], result_type);
      }
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       "No operator" + op_symbol + " overload found for type '" + operand_type->debug_name() + "'");
      return std::make_shared<CallExpr>(nullptr, operands, nullptr); // Return error marker
    }
  }

  static ASTNodePtr handle_unary_operator(
      ASTNodePtr operand,
      const yy::location& loc,
      Operator op_enum,
      std::function<bool(TypePtr)> type_validator,
      const std::string& type_requirement_msg)
  {
    std::string op_symbol = get_operator_string(op_enum);
    std::string op_name = get_operator_name(op_enum);

    TypePtr operand_type = get_expression_type(operand, loc, op_name + " operand");
    if (!operand_type) {
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       "Type of " + op_name + " operand could not be inferred");
      return nullptr;
    }

    // Check for operator overload in class types
    auto overload_result = try_operator_overload(operand_type, op_symbol, {operand}, loc, op_name);
    if (overload_result.has_value()) {
      return overload_result.value();
    }

    // Validate builtin type
    if (type_validator(operand_type)) {
      TypePtr result_type = operand_type;
      return std::make_shared<UnaryExpr>(op_enum, operand, result_type);
    } else {
      parser_add_error(loc.begin.line,
                       loc.begin.column,
                       op_name + " operand must be " + type_requirement_msg);
      return nullptr;
    }
  }

  std::unordered_map<std::string,int> operator_returns_bool = {
    {"<",1},
    {"<=",1},
    {">",1},
    {">=",1},
    {"||",1},
    {"&&",1},
    {"==",1},
    {"!=",1}
  };

  static ASTNodePtr handle_binary_operator(
      ASTNodePtr left,
      ASTNodePtr right,
      const yy::location& left_loc,
      const yy::location& right_loc,
      const yy::location& op_loc,
      Operator op_enum,
      std::function<bool(TypePtr, TypePtr)> type_validator,
      const std::string& type_requirement_msg)
  {
    std::string op_symbol = get_operator_string(op_enum);
    std::string op_name = get_operator_name(op_enum);

    TypePtr left_type = get_expression_type(left, left_loc, op_name);
    TypePtr right_type = get_expression_type(right, right_loc, op_name);

    if (!left_type || !right_type) {
      return nullptr;
    }

    // Check for operator overload in class types
    auto overload_result = try_operator_overload(left_type, op_symbol, {left, right}, op_loc, op_name, right_type);
    if (overload_result.has_value()) {
      return overload_result.value();
    }

    // Validate builtin types
    if (type_validator(left_type, right_type)) {
      if (operator_returns_bool[op_symbol]) {
        // For relational operators, result type is bool
        TypePtr bool_type = require_builtin("bool", op_loc, op_name + " operator");
        if (!bool_type) {
          return nullptr;
        }
        return std::make_shared<BinaryExpr>(op_enum, left, right, bool_type);
      }
      else if(op_enum == Operator::SUBSCRIPT_OP)
      {
        if (left_type->kind == TypeKind::ARRAY) {
          auto array_type = std::static_pointer_cast<ArrayType>(left_type);
          return std::make_shared<BinaryExpr>(op_enum, left, right, array_type->element_type.type);
        }
        else if (left_type->kind == TypeKind::POINTER) {
          TypePtr result_type = dereference_pointer(left_type, op_loc, "array subscript");
          if (!result_type) {
            return nullptr;
          }
          return std::make_shared<BinaryExpr>(op_enum, left, right, result_type);
        }
        else
        {
          parser_add_error(op_loc.begin.line,
                           op_loc.begin.column,
                           "array subscript requires array or pointer type on left side");
          return nullptr;
        }
      }
      else{
        TypePtr result_type = get_higher_rank_type(left_type, right_type);
        return std::make_shared<BinaryExpr>(op_enum, left, right, result_type);
      }

    } else {
      parser_add_error(op_loc.begin.line,
                       op_loc.begin.column,
                       op_name + " requires " + type_requirement_msg);
      return nullptr;
    }
  }

  static bool is_valid_lvalue(const ASTNodePtr& node) {
    if (!node) {
      return false;
    }

    // Valid lvalues: identifiers, dereferences, array subscripts, member access
    switch (node->type) {
    case ASTNodeType::IDENTIFIER_EXPR:
      return true;
    case ASTNodeType::ENUM_IDENTIFIER_EXPR:
      return false;
    case ASTNodeType::MEMBER_EXPR:
      return true;
    case ASTNodeType::UNARY_EXPR: {
      auto unary = std::static_pointer_cast<UnaryExpr>(node);
      // Dereference operator produces an lvalue
      return unary->op == Operator::POINTER_DEREF;
    }
    case ASTNodeType::BINARY_EXPR: {
      auto binary = std::static_pointer_cast<BinaryExpr>(node);
      // Array subscript produces an lvalue (handled as binary operation in some ASTs)
      // Member access with dot operator
      return false; // TODO: implement when array subscript and member access are added
    }
    case ASTNodeType::LITERAL_EXPR:
    case ASTNodeType::CALL_EXPR:
    case ASTNodeType::NEW_EXPR:
      return false;
    default:
      return false;
    }
  }

  static ASTNodePtr handle_assignment_operator(
      ASTNodePtr lhs,
      ASTNodePtr rhs,
      const yy::location& lhs_loc,
      const yy::location& rhs_loc,
      const yy::location& op_loc,
      Operator op_enum,
      const std::string& op_name)
  {
    // Check if lhs is a valid lvalue
    if (!is_valid_lvalue(lhs)) {
      parser_add_error(lhs_loc.begin.line,
                       lhs_loc.begin.column,
                       op_name + ": left operand must be a modifiable lvalue");
      return nullptr;
    }

    TypePtr lhs_type = get_expression_type(lhs, lhs_loc, op_name + " left operand");
    TypePtr rhs_type = get_expression_type(rhs, rhs_loc, op_name + " right operand");

    if (!lhs_type || !rhs_type) {
      return nullptr;
    }

    // Check for operator overload in class types
    std::string op_symbol = get_operator_string(op_enum);
    auto overload_result = try_operator_overload(lhs_type, op_symbol, {lhs, rhs}, op_loc, op_name, rhs_type);
    if (overload_result.has_value()) {
      return overload_result.value();
    }

    // For compound assignment operators, check type compatibility
    if (op_enum != Operator::ASSIGN) {
      // Extract the base operation (e.g., ADD from ADD_ASSIGN)
      bool types_compatible = false;

      switch (op_enum) {
      case Operator::ADD_ASSIGN:
      case Operator::SUBTRACT_ASSIGN:
        // Arithmetic or pointer arithmetic
        types_compatible = (is_arithmetic_type(lhs_type) && is_arithmetic_type(rhs_type)) ||
                          (is_pointer_type(lhs_type) && is_integral_type(rhs_type));
        break;
      case Operator::MULTIPLY_ASSIGN:
      case Operator::DIVIDE_ASSIGN:
      case Operator::MODULO_ASSIGN:
        types_compatible = is_arithmetic_type(lhs_type) && is_arithmetic_type(rhs_type);
        break;
      case Operator::BITWISE_AND_ASSIGN:
      case Operator::BITWISE_OR_ASSIGN:
      case Operator::BITWISE_XOR_ASSIGN:
      case Operator::LEFT_SHIFT_ASSIGN:
      case Operator::RIGHT_SHIFT_ASSIGN:
        types_compatible = is_integral_type(lhs_type) && is_integral_type(rhs_type);
        break;
      default:
        types_compatible = false;
      }

      if (!types_compatible) {
        parser_add_error(op_loc.begin.line,
                         op_loc.begin.column,
                         op_name + ": incompatible types for compound assignment");
        return nullptr;
      }
    }

    return std::make_shared<AssignmentExpr>(op_enum, lhs, rhs, lhs_type);
  }
}

%token <std::string> IDENTIFIER
%token <uint64_t> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <std::string> STRING_LITERAL
%token <bool> BOOL_LITERAL

%token INT UNSIGNED CHAR BOOL FLOAT VOID
%token TYPEDEF STATIC CONST VOLATILE
%token ENUM STRUCT UNION CLASS
%token RETURN IF ELSE SWITCH CASE DEFAULT FOR WHILE DO GOTO CONTINUE BREAK UNTIL
%token NEW DELETE THIS
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
%type <DeclaratorInfo> declarator direct_declarator abstract_declarator direct_abstract_declarator
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

%type <ASTNodePtr> statement compound_statement block_item declaration initializer
%type <ASTNodePtr> expression_statement selection_statement iteration_statement jump_statement
%type <ASTNodePtr> labeled_statement
%type <std::vector<ASTNodePtr>> block_item_list translation_unit
%type <ASTNodePtr> external_declaration function_definition

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
    : OPEN_BRACE_OP {
        symbol_table.enter_scope();
        parser_state.push_ctx(ContextKind::BLOCK);
        add_pending_parameters_to_scope(@1);
      }
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
      {
        // At this point, an indentifier can be a variable name, function name, or enum name.

        // Variable name case
        auto sym_opt = symbol_table.lookup_symbol($1);
        if(sym_opt.has_value()){
          $$ = std::make_shared<IdentifierExpr>(sym_opt.value(), sym_opt.value()->get_type().type);
        } else {
          auto enum_opt = type_factory.lookup_by_scope("enum " + $1, symbol_table.get_scope_chain());

          if (enum_opt.has_value()) {
            $$ = std::make_shared<EnumIdentifierExpr>(enum_opt.value());
          }
          else{
            if(encountered_function_names.find($1) != encountered_function_names.end()){
              $$=std::make_shared<FunctionIdentifierExpr>($1);
            }
            else{
              $$=nullptr;
              parser_add_error(@1.begin.line,
                                @1.begin.column,
                                "use of undeclared identifier '" + $1 + "'");
            }
          }
        }
      }
    | INT_LITERAL
      {
        TypePtr expr_type = require_builtin("int", @1, "integer literal");
        LiteralValue literal = static_cast<uint64_t>($1);
        $$ = std::make_shared<LiteralExpr>(literal, expr_type);
      }
    | FLOAT_LITERAL
      {
        TypePtr expr_type = require_builtin("float", @1, "floating literal");
        LiteralValue literal = static_cast<double>($1);
        $$ = std::make_shared<LiteralExpr>(literal, expr_type);
      }
    | CHAR_LITERAL
      {
        TypePtr expr_type = require_builtin("char", @1, "character literal");
        LiteralValue literal = $1;
        $$ = std::make_shared<LiteralExpr>(literal, expr_type);
      }
    | STRING_LITERAL
      {
        TypePtr expr_type = make_string_literal_type(@1);
        LiteralValue literal = std::move($1);
        $$ = std::make_shared<LiteralExpr>(literal, expr_type);
      }
    | BOOL_LITERAL
      {
        TypePtr expr_type = require_builtin("bool", @1, "boolean literal");
        LiteralValue literal = $1;
        $$ = std::make_shared<LiteralExpr>(literal, expr_type);
      }
    | THIS
      {
        // 'this' is only valid inside a class member function
        if (!in_class()|| !is_in_member_function_of_class()) {
          parser_add_error(@1.begin.line, @1.begin.column, "'this' can only be used inside a class member function");
          $$ = nullptr;
        } else {
          // 'this' is a pointer to the current class type
          QualifiedType class_type(parser_state.current_class_type, Qualifier::NONE);
          auto pointer_result = apply_pointer_levels_or_error(class_type, 1, "'this' type", @1.begin.line, @1.begin.column);
          TypePtr this_type = pointer_result.type;
          $$ = std::make_shared<ThisExpr>(this_type);
        }
      }
    | OPEN_PAREN_OP expression CLOSE_PAREN_OP
      {
        $$ = $2;
      }
    ;

postfix_expression
    : primary_expression
    {
      $$ = $1;
    }
    | postfix_expression OPEN_BRACKET_OP expression CLOSE_BRACKET_OP
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2,
          Operator::SUBSCRIPT_OP,
          [](TypePtr left, TypePtr right) {
            return is_pointer_type(left) && is_integral_type(right);
          },
          "pointer and integer types for array subscript");
    }
    | postfix_expression OPEN_PAREN_OP CLOSE_PAREN_OP
    {
      if($1->type == ASTNodeType::FUNCTION_IDENTIFIER_EXPR){
        std::string function_name = std::static_pointer_cast<FunctionIdentifierExpr>($1)->function_name;
        FunctionType non_variadic(QualifiedType{}, {}, false);
        FunctionType variadic(QualifiedType{}, {}, true);

        FunctionMeta meta{};

        auto non_variadic_mangled_name = mangle_function_name(function_name, non_variadic, meta, std::nullopt).value();
        auto variadic_mangled_name = mangle_function_name(function_name, variadic, meta, std::nullopt).value();

        auto non_variadic_symbol = symbol_table.lookup_operator(non_variadic_mangled_name);
        auto variadic_symbol = symbol_table.lookup_operator(variadic_mangled_name);

        if (!non_variadic_symbol.has_value() && !variadic_symbol.has_value()) {
          parser_add_error(@1.begin.line,
                           @1.begin.column,
                           "use of undeclared function '" + function_name + "'");
          $$ = nullptr;
        }
        else if (non_variadic_symbol.has_value() && variadic_symbol.has_value()) {
          parser_add_error(@1.begin.line,
                           @1.begin.column,
                           "ambiguous call to overloaded function '" + function_name + "' with no arguments");
          $$ = nullptr;
        }
        else {
          SymbolPtr func_symbol = non_variadic_symbol.has_value() ? non_variadic_symbol.value() : variadic_symbol.value();
          auto fn_type = std::static_pointer_cast<FunctionType>(func_symbol->get_type().type);

          if (!fn_type->param_types.empty() && !fn_type->is_variadic) {
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "too few arguments to function call, expected " +
                             std::to_string(fn_type->param_types.size()) + ", have 0");
            $$ = nullptr;
          }
          else {
            std::vector<ASTNodePtr> args; // empty argument list
            $$ = std::make_shared<CallExpr>(func_symbol, args, fn_type->return_type.type);
          }
        }
      }
      else if ($1->type == ASTNodeType::MEMBER_EXPR) {
        std::string function_name = std::static_pointer_cast<MemberExpr>($1)->member_name;
        TypePtr base_class = get_expression_type(std::static_pointer_cast<MemberExpr>($1)->object, @1, "member function call base");

        auto mem_node = std::static_pointer_cast<MemberExpr>($1);
        if (mem_node->op == Operator::MEMBER_ACCESS_PTR) {
          // If using '->', dereference pointer to get class type
          base_class = dereference_pointer(base_class, @1, "member function call base");
        }

        if (!base_class || !is_class_type(base_class)) {
          parser_add_error(@1.begin.line,
                          @1.begin.column,
                          "member function call base is not a class type");
          $$ = nullptr;
        }
        else {
          FunctionMeta meta(FunctionKind::METHOD, {});
          SymbolPtr func_symbol = nullptr;

          // Search through class hierarchy for matching function
          auto current_class = std::static_pointer_cast<ClassType>(base_class);

          while (current_class && !func_symbol) {
            FunctionType non_variadic(QualifiedType(), {}, false);
            FunctionType variadic(QualifiedType(), {}, true);

            // Try non-variadic version
            auto non_variadic_mangled = mangle_function_name(function_name, non_variadic, meta, *current_class);
            if (non_variadic_mangled.has_value()) {
              auto non_variadic_symbol = symbol_table.lookup_operator(non_variadic_mangled.value());
              if (non_variadic_symbol.has_value()) {
                func_symbol = non_variadic_symbol.value();
                break;
              }
            }

            // Try variadic version
            if (!func_symbol) {
              auto variadic_mangled = mangle_function_name(function_name, variadic, meta, *current_class);
              if (variadic_mangled.has_value()) {
                auto variadic_symbol = symbol_table.lookup_operator(variadic_mangled.value());
                if (variadic_symbol.has_value()) {
                  func_symbol = variadic_symbol.value();
                  break;
                }
              }
            }

            // Move to base class if accessible (public inheritance only)
            if (current_class->base.base_type && current_class->base.access == Access::PUBLIC) {
              TypePtr base_type = current_class->base.base_type;
              if (is_class_type(base_type)) {
                current_class = std::static_pointer_cast<ClassType>(base_type);
              } else {
                current_class = nullptr;
              }
            } else {
              current_class = nullptr;
            }
          }

          if (!func_symbol) {
            parser_add_error(@1.begin.line,
                            @1.begin.column,
                            "use of undeclared member function '" + function_name + "'");
            $$ = nullptr;
          }
          else {
            auto fn_type = std::static_pointer_cast<FunctionType>(func_symbol->get_type().type);

            if (!fn_type->param_types.empty() && !fn_type->is_variadic) {
              parser_add_error(@2.begin.line,
                              @2.begin.column,
                              "too few arguments to member function call, expected " +
                              std::to_string(fn_type->param_types.size()) + ", have 0");
              $$ = nullptr;
            }
            else {
              std::vector<ASTNodePtr> args = {$1};
              $$ = std::make_shared<CallExpr>(func_symbol, args, fn_type->return_type.type);
            }
          }
        }
      }
      else {
        TypePtr func_type = get_expression_type($1, @1, "function call");

        if (is_class_type(func_type)) {
          SymbolPtr overload = get_operator_overload(func_type, "()");
          if (overload) {
            TypePtr function_type = overload->get_type().type;
            TypePtr result_type = std::static_pointer_cast<FunctionType>(function_type)->return_type.type;
            std::vector<ASTNodePtr> args = {$1};
            $$ = std::make_shared<CallExpr>(overload, args, result_type);
          } else {
            parser_add_error(@2.begin.line,
                            @2.begin.column,
                            "No operator() overload found for type '" + func_type->debug_name() + "'");
            $$ = nullptr;
          }
        }
        else{
          parser_add_error(@1.begin.line,
                           @1.begin.column,
                           "called object is not a function");
          $$ = nullptr;
        }
      }

    }
    | postfix_expression OPEN_PAREN_OP argument_expression_list CLOSE_PAREN_OP
    {
      ASTNodePtr args_list = $3;
      std::vector<ASTNodePtr> arg_nodes;
      std::vector<QualifiedType> arg_types;

      std::function<void(ASTNodePtr)> extract_arg_nodes = [&](ASTNodePtr node) {
        if (!node) return;

        if (node->type == ASTNodeType::BINARY_EXPR) {
          auto binary = std::static_pointer_cast<BinaryExpr>(node);
          if (binary->op == Operator::COMMA_OP) {
            // Recursively extract left side first (for left-to-right order)
            extract_arg_nodes(binary->left);
            // Then extract right side
            extract_arg_nodes(binary->right);
            return;
          }
        }

        arg_nodes.push_back(node);
      };

      extract_arg_nodes(args_list);

      for (const auto& arg : arg_nodes) {
        TypePtr arg_type = get_expression_type(arg, @3, "function argument");
        if (!arg_type) {
          parser_add_error(@3.begin.line,
                           @3.begin.column,
                           "Type of function argument could not be inferred");
        }
        arg_types.push_back(QualifiedType{arg_type, Qualifier::NONE});
      }

      if($1->type == ASTNodeType::FUNCTION_IDENTIFIER_EXPR){
        std::string function_name = std::static_pointer_cast<FunctionIdentifierExpr>($1)->function_name;

        FunctionMeta meta{};
        SymbolPtr func_symbol = nullptr;

        // Step 1: Try exact match with all arguments (non-variadic)
        FunctionType exact_match(QualifiedType(), arg_types, false);
        auto exact_mangled = mangle_function_name(function_name, exact_match, meta, std::nullopt);
        if (exact_mangled.has_value()) {
          auto exact_symbol = symbol_table.lookup_operator(exact_mangled.value());
          if (exact_symbol.has_value()) {
            func_symbol = exact_symbol.value();
          }
        }

        // Step 2: Try exact match with variadic function
        if (!func_symbol) {
          FunctionType variadic_match(QualifiedType(), arg_types, true);
          auto variadic_mangled = mangle_function_name(function_name, variadic_match, meta, std::nullopt);
          if (variadic_mangled.has_value()) {
            auto variadic_symbol = symbol_table.lookup_operator(variadic_mangled.value());
            if (variadic_symbol.has_value()) {
              func_symbol = variadic_symbol.value();
            }
          }
        }

        // Step 3: Try decreasing parameters from right, looking for variadic functions
        if (!func_symbol) {
          for (size_t i = arg_types.size(); i > 0; --i) {
            std::vector<QualifiedType> reduced_args(arg_types.begin(), arg_types.begin() + i - 1);
            FunctionType reduced_variadic(QualifiedType(), reduced_args, true);

            auto reduced_mangled = mangle_function_name(function_name, reduced_variadic, meta, std::nullopt);
            if (reduced_mangled.has_value()) {
              auto reduced_symbol = symbol_table.lookup_operator(reduced_mangled.value());
              if (reduced_symbol.has_value()) {
                func_symbol = reduced_symbol.value();
                break;
              }
            }
          }
        }

        // Step 4: Try with no parameters and variadic
        if (!func_symbol) {
          FunctionType empty_variadic(QualifiedType(), {}, true);
          auto empty_mangled = mangle_function_name(function_name, empty_variadic, meta, std::nullopt);
          if (empty_mangled.has_value()) {
            auto empty_symbol = symbol_table.lookup_operator(empty_mangled.value());
            if (empty_symbol.has_value()) {
              func_symbol = empty_symbol.value();
            }
          }
        }

        // Step 5: Error if no match found
        if (!func_symbol) {
          parser_add_error(@1.begin.line,
                          @1.begin.column,
                          "no matching function for call to '" + function_name + "' with " +
                          std::to_string(arg_types.size()) + " argument(s)");
          $$ = nullptr;
        }
        else {
          auto fn_type = std::static_pointer_cast<FunctionType>(func_symbol->get_type().type);
          $$ = std::make_shared<CallExpr>(func_symbol, arg_nodes, fn_type->return_type.type);
        }
      }
      else if ($1->type == ASTNodeType::MEMBER_EXPR) {
        std::string function_name = std::static_pointer_cast<MemberExpr>($1)->member_name;
        TypePtr base_class = get_expression_type(std::static_pointer_cast<MemberExpr>($1)->object, @1, "member function call base");

        auto mem_node = std::static_pointer_cast<MemberExpr>($1);
        if (mem_node->op == Operator::MEMBER_ACCESS_PTR) {
          // If using '->', dereference pointer to get class type
          base_class = dereference_pointer(base_class, @1, "member function call base");
        }

        if (!base_class || !is_class_type(base_class)) {
          parser_add_error(@1.begin.line,
                          @1.begin.column,
                          "member function call base is not a class type");
          $$ = nullptr;
        }
        else {
          FunctionMeta meta(FunctionKind::METHOD, {});
          SymbolPtr func_symbol = nullptr;

          // Search through class hierarchy for matching function
          auto current_class = std::static_pointer_cast<ClassType>(base_class);

          while (current_class && !func_symbol) {
            // Step 1: Try exact match with all arguments (non-variadic)
            FunctionType exact_match(QualifiedType(), arg_types, false);
            auto exact_mangled = mangle_function_name(function_name, exact_match, meta, *current_class);
            if (exact_mangled.has_value()) {
              auto exact_symbol = symbol_table.lookup_operator(exact_mangled.value());
              if (exact_symbol.has_value()) {
                func_symbol = exact_symbol.value();
                break;
              }
            }

            // Step 2: Try exact match with variadic function
            if (!func_symbol) {
              FunctionType variadic_match(QualifiedType(), arg_types, true);
              auto variadic_mangled = mangle_function_name(function_name, variadic_match, meta, *current_class);
              if (variadic_mangled.has_value()) {
                auto variadic_symbol = symbol_table.lookup_operator(variadic_mangled.value());
                if (variadic_symbol.has_value()) {
                  func_symbol = variadic_symbol.value();
                  break;
                }
              }
            }

            // Step 3: Try decreasing parameters from right, looking for variadic functions
            if (!func_symbol) {
              for (size_t i = arg_types.size(); i > 0; --i) {
                std::vector<QualifiedType> reduced_args(arg_types.begin(), arg_types.begin() + i - 1);
                FunctionType reduced_variadic(QualifiedType(), reduced_args, true);

                auto reduced_mangled = mangle_function_name(function_name, reduced_variadic, meta, *current_class);
                if (reduced_mangled.has_value()) {
                  auto reduced_symbol = symbol_table.lookup_operator(reduced_mangled.value());
                  if (reduced_symbol.has_value()) {
                    func_symbol = reduced_symbol.value();
                    break;
                  }
                }
              }
            }

            // Step 4: Try with no parameters and variadic
            if (!func_symbol) {
              FunctionType empty_variadic(QualifiedType(), {}, true);
              auto empty_mangled = mangle_function_name(function_name, empty_variadic, meta, *current_class);
              if (empty_mangled.has_value()) {
                auto empty_symbol = symbol_table.lookup_operator(empty_mangled.value());
                if (empty_symbol.has_value()) {
                  func_symbol = empty_symbol.value();
                  break;
                }
              }
            }

            // Move to base class if accessible (public inheritance only)
            if (current_class->base.base_type && current_class->base.access == Access::PUBLIC) {
              TypePtr base_type = current_class->base.base_type;
              if (is_class_type(base_type)) {
                current_class = std::static_pointer_cast<ClassType>(base_type);
              } else {
                current_class = nullptr;
              }
            } else {
              current_class = nullptr;
            }
          }

          // Step 5: Error if no match found
          if (!func_symbol) {
            parser_add_error(@1.begin.line,
                            @1.begin.column,
                            "no matching member function for call to '" + function_name + "' with " +
                            std::to_string(arg_types.size()) + " argument(s)");
            $$ = nullptr;
          }
          else {
            auto fn_type = std::static_pointer_cast<FunctionType>(func_symbol->get_type().type);
            std::vector<ASTNodePtr> args = {$1};
            args.insert(args.end(), arg_nodes.begin(), arg_nodes.end());
            $$ = std::make_shared<CallExpr>(func_symbol, args, fn_type->return_type.type);
          }
        }
      }
      else {
        TypePtr func_type = get_expression_type($1, @1, "function call");

        if (is_class_type(func_type)) {
          SymbolPtr overload = get_operator_overload(func_type, "()");
          if (overload) {
            TypePtr function_type = overload->get_type().type;
            TypePtr result_type = std::static_pointer_cast<FunctionType>(function_type)->return_type.type;
            std::vector<ASTNodePtr> args = {$1};
            $$ = std::make_shared<CallExpr>(overload, args, result_type);
          } else {
            parser_add_error(@2.begin.line,
                            @2.begin.column,
                            "No operator() overload found for type '" + func_type->debug_name() + "'");
            $$ = nullptr;
          }
        }
        else{
          parser_add_error(@1.begin.line,
                           @1.begin.column,
                           "called object is not a function");
          $$ = nullptr;
        }
      }
    }
    | postfix_expression DOT_OP IDENTIFIER
    {
      TypePtr base_type = get_expression_type($1, @1, "member access");

      if (!base_type) {
        parser_add_error(@1.begin.line, @1.begin.column,
                     "member access base type could not be determined");
        $$ = nullptr;
      }
      else if ($1->type == ASTNodeType::IDENTIFIER_EXPR) {
          if (is_class_type(base_type)) {
            auto class_type = std::static_pointer_cast<ClassType>(base_type);
            while(class_type){
              if (class_type->members.find($3)!= class_type->members.end()) {
                MemberInfo member = class_type->members.at($3);

                if (member.access != Access::PUBLIC) {
                  parser_add_error(@2.begin.line, @2.begin.column,
                               "member '" + $3 + "' is not accessible (not public)");
                  $$ = nullptr;
                }
                else{
                  TypePtr member_type = member.type.type;
                  $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS, $1, $3, member_type);
                }
                break;
              }
              if(class_type->base.base_type && class_type->base.access == Access::PUBLIC){
                base_type = class_type->base.base_type;
                if(is_class_type(base_type)){
                  class_type = std::static_pointer_cast<ClassType>(base_type);
                }
                else{
                  class_type = nullptr;
                }
              }
              else{
                class_type = nullptr;
              }
            }

            if(!class_type) {
              // Can be a function call, but we can't verify at this point because we can't mangle it yet.
              if(encountered_function_names.find($3) != encountered_function_names.end()){
                $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS, $1, $3, nullptr);
              }
              else{
                parser_add_error(@2.begin.line, @2.begin.column,
                             "member '" + $3 + "' not found in class or its base classes");
                $$ = nullptr;
              }
            }
          }
          else if(base_type->kind == TypeKind::RECORD){
            auto record_type = std::static_pointer_cast<RecordType>(base_type);
            if (record_type->fields.find($3)!= record_type->fields.end()) {
              TypePtr member_type = record_type->fields.at($3).type;
              $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS, $1, $3, member_type);
            } else {
              parser_add_error(@2.begin.line, @2.begin.column,
                           "member '" + $3 + "' not found in struct/union");
              $$ = nullptr;
            }
          }
          else {
            parser_add_error(@1.begin.line, @1.begin.column,
                         "member reference base type is not a structure or union");
            $$ = nullptr;
          }
        }
      else if($1->type == ASTNodeType::ENUM_IDENTIFIER_EXPR){
          TypePtr enum_type = get_expression_type($1, @1, "enum member access");

          if(enum_type && enum_type->kind == TypeKind::ENUM){

            auto enum_ptr = std::static_pointer_cast<EnumType>(enum_type);
            if(enum_ptr->enumerators.find($3) != enum_ptr->enumerators.end()){
              TypePtr member_type = require_builtin("int", @2, "enum underlying type");
              if(!member_type){
                $$ = nullptr;
              }
              else{
                LiteralValue literal = enum_ptr->enumerators.at($3);
                ASTNodePtr enum_member = std::make_shared<LiteralExpr>(literal, enum_type);
                $$ = enum_member;
              }
            }
            else{
              parser_add_error(@2.begin.line, @2.begin.column,
                           "enumerator '" + $3 + "' not found in enum");
              $$ = nullptr;
            }
          }
      }
      else {
          parser_add_error(@1.begin.line, @1.begin.column,
                       "member access requires struct, union, or class type");
          $$ = nullptr;
      }
    }
    | postfix_expression ARROW_OP IDENTIFIER
    {
      TypePtr ptr_type = get_expression_type($1, @1, "member access through pointer");

      if (!ptr_type) {
        $$ = nullptr;
      }
      else if (!is_pointer_type(ptr_type)) {
        parser_add_error(@1.begin.line,
                         @1.begin.column,
                         "member reference type is not a pointer");
        $$ = nullptr;
      }
      else {
        TypePtr base_type = dereference_pointer(ptr_type, @1, "arrow operator");
        if (base_type) {
          if(is_class_type(base_type)){
            auto class_type = std::static_pointer_cast<ClassType>(base_type);
            while(class_type){
              if (class_type->members.find($3)!= class_type->members.end()) {
                MemberInfo member = class_type->members.at($3);

                if (($1->type != ASTNodeType::THIS_EXPR) && (member.access != Access::PUBLIC)) {
                  parser_add_error(@2.begin.line, @2.begin.column,
                               "member '" + $3 + "' is not accessible (not public)");
                  $$ = nullptr;
                }
                else{
                  TypePtr member_type = member.type.type;
                  $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS_PTR, $1, $3, member_type);
                }

                break;
              }
              if(class_type->base.base_type && class_type->base.access == Access::PUBLIC){
                base_type = class_type->base.base_type;
                if(is_class_type(base_type)){
                  class_type = std::static_pointer_cast<ClassType>(base_type);
                }
                else{
                  class_type = nullptr;
                }
              }
              else{
                class_type = nullptr;
              }
            }
            if(!class_type){
              // Can be a function call, but we can't verify at this point because we can't mangle it yet.
              if(encountered_function_names.find($3) != encountered_function_names.end()){
                $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS_PTR, $1, $3, nullptr);
              }
              else{
                parser_add_error(@2.begin.line, @2.begin.column,
                               "member '" + $3 + "' not found in class or its base classes");
                $$ = nullptr;
              }
            }
          }
          else if(base_type->kind == TypeKind::RECORD){
            auto record_type = std::static_pointer_cast<RecordType>(base_type);
            if (record_type->fields.find($3)!= record_type->fields.end()) {
              TypePtr member_type = record_type->fields.at($3).type;
              $$ = std::make_shared<MemberExpr>(Operator::MEMBER_ACCESS_PTR, $1, $3, member_type);
            } else {
              parser_add_error(@2.begin.line, @2.begin.column,
                             "member '" + $3 + "' not found in struct/union");
              $$ = nullptr;
            }
          }
          else{
            parser_add_error(@1.begin.line, @1.begin.column,
                           "member reference base type is not a structure or union");
            $$ = nullptr;
          }
        }
        else {
          parser_add_error(@1.begin.line, @1.begin.column,
                         "member reference base type is not a pointer to structure or union");
          $$ = nullptr;
        }
      }
    }
    | postfix_expression INCREMENT_OP
    {
      $$ = handle_unary_operator($1, @2, Operator::POST_INCREMENT,
                                 [](TypePtr t) { return is_integral_type(t) || is_pointer_type(t); },
                                 "an integer or pointer type");
    }
    | postfix_expression DECREMENT_OP
    {
      $$ = handle_unary_operator($1, @2, Operator::POST_DECREMENT,
                                 [](TypePtr t) { return is_integral_type(t) || is_pointer_type(t); },
                                 "an integer or pointer type");
    }
    /* | OPEN_PAREN_OP type_name CLOSE_PAREN_OP OPEN_BRACE_OP initializer_list CLOSE_BRACE_OP */
    /* { */

    /* } */
    ;

argument_expression_list
    : assignment_expression
    {
      $$ = $1;
    }
    | argument_expression_list COMMA_OP assignment_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::COMMA_OP,
                                  [](TypePtr l, TypePtr r) { return true; },
                                  "any types");
    }
    ;

unary_expression
    : postfix_expression
      {
        $$ = $1;
      }
    | INCREMENT_OP unary_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::INCREMENT,
                                   [](TypePtr t) { return is_integral_type(t) || is_pointer_type(t); },
                                   "an integer or pointer type");
      }
    | DECREMENT_OP unary_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::DECREMENT,
                                   [](TypePtr t) { return is_integral_type(t) || is_pointer_type(t); },
                                   "an integer or pointer type");
      }
    | LOGICAL_NOT_OP cast_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::LOGICAL_NOT,
                                   [](TypePtr t) { return is_bool_type(t); },
                                   "a bool type");
      }
    | TILDE_OP cast_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::BITWISE_NOT,
                                   [](TypePtr t) { return is_integral_type(t); },
                                   "an integral type");
      }
    | AMPERSAND_OP cast_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::ADDRESS_OF,
                                   [](TypePtr t) { return true; },
                                   "any type");
      }
    | STAR_OP cast_expression
      {
        $$ = handle_unary_operator($2, @1, Operator::POINTER_DEREF,
                                   [](TypePtr t) { return is_pointer_type(t); },
                                   "a pointer type");
      }
    | PLUS_OP cast_expression %prec UNARY
      {
        $$ = handle_unary_operator($2, @1, Operator::UNARY_PLUS,
                                   [](TypePtr t) { return is_arithmetic_type(t); },
                                   "an arithmetic type");
      }
    | MINUS_OP cast_expression %prec UNARY
      {
        $$ = handle_unary_operator($2, @1, Operator::UNARY_MINUS,
                                   [](TypePtr t) { return is_arithmetic_type(t); },
                                   "an arithmetic type");
      }
    | NEW type_name
      {
        TypePtr allocated_type = $2;
        auto ptr_result = type_factory.pointer_from(allocated_type);
        TypePtr result_type = unwrap_type_or_error(ptr_result, "new expression", @1.begin.line, @1.begin.column);
        $$ = std::make_shared<NewExpr>(allocated_type, result_type);
      }
    | DELETE unary_expression
      {
        TypePtr operand_type = get_expression_type($2, @2, "delete operand");
        if (operand_type && !is_pointer_type(operand_type)) {
          parser_add_error(@1.begin.line,
                           @1.begin.column,
                           "delete operand is not a pointer type");
        }
        TypePtr result_type = require_builtin("void", @1, "delete expression");
        $$ = std::make_shared<DeleteExpr>($2, result_type);
      }
    | OPEN_PAREN_OP type_name CLOSE_PAREN_OP cast_expression
      {
        TypePtr target_type = $2;
        $$ = std::make_shared<CastExpr>(target_type, $4);
      }
    ;

cast_expression
    : unary_expression
    {
      $$ = $1;
    }
    ;

multiplicative_expression
    : cast_expression
    {
      $$ = $1;
    }
    | multiplicative_expression STAR_OP cast_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::MULTIPLY,
                                  [](TypePtr l, TypePtr r) { return is_arithmetic_type(l) && is_arithmetic_type(r); },
                                  "arithmetic types");
    }
    | multiplicative_expression DIVIDE_OP cast_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::DIVIDE,
                                  [](TypePtr l, TypePtr r) { return is_arithmetic_type(l) && is_arithmetic_type(r); },
                                  "arithmetic types");
    }
    | multiplicative_expression MOD_OP cast_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::MODULO,
                                  [](TypePtr l, TypePtr r) { return is_arithmetic_type(l) && is_arithmetic_type(r); },
                                  "arithmetic types");
    }
    ;

additive_expression
    : multiplicative_expression
    {
      $$ = $1;
    }
    | additive_expression PLUS_OP multiplicative_expression
    {
      TypePtr left_type = get_expression_type($1, @1, "addition");
      TypePtr right_type = get_expression_type($3, @3, "addition");

      if(!left_type || !right_type) {
        $$ = nullptr;
      }
      else
      {
        if(is_class_type(left_type)){
          SymbolPtr overload = get_operator_overload(left_type, "+", right_type);
          if (overload) {
            TypePtr function_type = overload->get_type().type;
            TypePtr result_type = std::static_pointer_cast<FunctionType>(function_type)->return_type.type;

            std::vector<ASTNodePtr> args = {$1, $3};
            $$ = std::make_shared<CallExpr>(overload, args, result_type);
          }
          else{
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "No operator+ overload found for type '" + left_type->debug_name() + "'");
            $$ = nullptr;
          }
        }
        else if (is_arithmetic_type(left_type) && is_arithmetic_type(right_type)) {
          TypePtr result_type = get_higher_rank_type(left_type, right_type);
          $$ = std::make_shared<BinaryExpr>(Operator::ADD, $1, $3, result_type);
        }
        else if(is_pointer_type(left_type) && is_integral_type(right_type)) {
          $$ = std::make_shared<BinaryExpr>(Operator::ADD, $1, $3, left_type);
        }
        else if(is_integral_type(left_type) && is_pointer_type(right_type)) {
          $$ = std::make_shared<BinaryExpr>(Operator::ADD, $3, $1, right_type);
        }
        else{
          parser_add_error(@2.begin.line,
                           @2.begin.column,
                           "addition requires arithmetic or pointer/integral types");
          $$ = nullptr;
        }
      }
    }
    | additive_expression MINUS_OP multiplicative_expression
    {
      TypePtr left_type = get_expression_type($1, @1, "subtraction");
      TypePtr right_type = get_expression_type($3, @3, "subtraction");

      if(!left_type || !right_type) {
        $$ = nullptr;
      }
      else
      {
        if(is_class_type(left_type)){
          SymbolPtr overload = get_operator_overload(left_type, "-", right_type);
          if (overload) {
            TypePtr function_type = overload->get_type().type;
            TypePtr result_type = std::static_pointer_cast<FunctionType>(function_type)->return_type.type;

            std::vector<ASTNodePtr> args = {$1, $3};
            $$ = std::make_shared<CallExpr>(overload, args, result_type);
          }
          else{
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "No operator- overload found for type '" + left_type->debug_name() + "'");
            $$ = nullptr;
          }
        }
        else if (is_arithmetic_type(left_type) && is_arithmetic_type(right_type)) {
          TypePtr result_type = get_higher_rank_type(left_type, right_type);
          $$ = std::make_shared<BinaryExpr>(Operator::SUBTRACT, $1, $3, result_type);
        }
        else if(is_pointer_type(left_type) && is_integral_type(right_type)) {
          $$ = std::make_shared<BinaryExpr>(Operator::SUBTRACT, $1, $3, left_type);
        }
        else{
          parser_add_error(@2.begin.line,
                           @2.begin.column,
                           "subtraction requires arithmetic or pointer/integral types");
          $$ = nullptr;
        }
      }
    }
    ;

shift_expression
    : additive_expression
      {
        $$ = $1;
      }
    | shift_expression LSHIFT_OP additive_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::LEFT_SHIFT,
                                  [](TypePtr l, TypePtr r) { return is_integral_type(l) && is_integral_type(r); },
                                  "integral types");
    }
    | shift_expression RSHIFT_OP additive_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::RIGHT_SHIFT,
                                  [](TypePtr l, TypePtr r) { return is_integral_type(l) && is_integral_type(r); },
                                  "integral types");
    }
    ;

relational_expression
    : shift_expression
      {
        $$ = $1;
      }
    | relational_expression LT_OP shift_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::LESS_THAN,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    | relational_expression LE_OP shift_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::LESS_EQUAL,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    | relational_expression GT_OP shift_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::GREATER_THAN,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    | relational_expression GE_OP shift_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::GREATER_EQUAL,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    ;

equality_expression
    : relational_expression
    {
        $$ = $1;
    }
    | equality_expression EQ_OP relational_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::EQUAL,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    | equality_expression NE_OP relational_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::NOT_EQUAL,
                                  [](TypePtr l, TypePtr r) { return is_scalar_type(l) && is_scalar_type(r) && are_types_equal(l, r); },
                                  "scalar types");
    }
    ;

and_expression
    : equality_expression
    {
        $$ = $1;
    }
    | and_expression AMPERSAND_OP equality_expression
    {
      $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::BITWISE_AND,
                                  [](TypePtr l, TypePtr r) { return is_integral_type(l) && is_integral_type(r); },
                                  "integral types");
    }
    ;

exclusive_or_expression
    : and_expression
      {
        $$ = $1;
      }
    | exclusive_or_expression CARET_OP and_expression
      {
        $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::BITWISE_XOR,
                                    [](TypePtr l, TypePtr r) { return is_integral_type(l) && is_integral_type(r); },
                                    "integral types");
      }
    ;

inclusive_or_expression
    : exclusive_or_expression
      {
        $$ = $1;
      }
    | inclusive_or_expression PIPE_OP exclusive_or_expression
      {
        $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::BITWISE_OR,
                                    [](TypePtr l, TypePtr r) { return is_integral_type(l) && is_integral_type(r); },
                                    "integral types");
      }
    ;

logical_and_expression
    : inclusive_or_expression
      {
        $$ = $1;
      }
    | logical_and_expression LOGICAL_AND_OP inclusive_or_expression
      {
        $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::LOGICAL_AND,
                                    [](TypePtr l, TypePtr r) { return is_bool_type(l) && is_bool_type(r); },
                                    "bool types");
      }
    ;

logical_or_expression
    : logical_and_expression
      {
        $$ = $1;
      }
    | logical_or_expression LOGICAL_OR_OP logical_and_expression
      {
        $$ = handle_binary_operator($1, $3, @1, @3, @2, Operator::LOGICAL_OR,
                                    [](TypePtr l, TypePtr r) { return is_bool_type(l) && is_bool_type(r); },
                                    "bool types");
      }
    ;

conditional_expression
    : logical_or_expression
      {
        $$ = $1;
      }
    | logical_or_expression QUESTION_OP expression COLON_OP conditional_expression
      {
        TypePtr condition_type = get_expression_type($1, @1, "conditional condition");
        TypePtr true_type = get_expression_type($3, @3, "conditional true branch");
        TypePtr false_type = get_expression_type($5, @5, "conditional false branch");

        if(!condition_type || !true_type || !false_type) {
          $$ = nullptr;
        }
        else if(!is_bool_type(condition_type)){
          parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "conditional condition must be a bool type");
            $$ = nullptr;
        }
        else {
          if (are_types_equal(true_type, false_type)) {
            $$ = std::make_shared<TernaryExpr>($1, $3, $5, true_type);
          }
          else{
            parser_add_error(@2.begin.line,
                             @2.begin.column,
                             "conditional true and false branches must be of the same type");
            $$ = nullptr;
          }
        }
      }
    ;

assignment_expression
    : conditional_expression
      {
        $$ = $1;
      }
    | unary_expression ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::ASSIGN, "assignment");
      }
    | unary_expression PLUS_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::ADD_ASSIGN, "plus assignment");
      }
    | unary_expression MINUS_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::SUBTRACT_ASSIGN, "minus assignment");
      }
    | unary_expression STAR_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::MULTIPLY_ASSIGN, "multiply assignment");
      }
    | unary_expression DIVIDE_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::DIVIDE_ASSIGN, "divide assignment");
      }
    | unary_expression MOD_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::MODULO_ASSIGN, "modulo assignment");
      }
    | unary_expression AMPERSAND_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::BITWISE_AND_ASSIGN, "bitwise and assignment");
      }
    | unary_expression PIPE_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::BITWISE_OR_ASSIGN, "bitwise or assignment");
      }
    | unary_expression CARET_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::BITWISE_XOR_ASSIGN, "bitwise xor assignment");
      }
    | unary_expression LSHIFT_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::LEFT_SHIFT_ASSIGN, "left shift assignment");
      }
    | unary_expression RSHIFT_ASSIGN_OP assignment_expression
      {
        $$ = handle_assignment_operator($1, $3, @1, @3, @2, Operator::RIGHT_SHIFT_ASSIGN, "right shift assignment");
      }
    ;

expression
    : assignment_expression
    {
      $$ = $1;
    }
    | expression COMMA_OP assignment_expression
    {
      TypePtr expr_type = get_expression_type($3, @3, "comma expression");
      $$ = std::make_shared<BinaryExpr>(Operator::COMMA_OP, $1, $3, expr_type);
    }
    ;

constant_expression
    : conditional_expression
    {
      $$ = $1;
    }
    ;

declaration
    : declaration_specifiers SEMICOLON_OP
      {
        $$ = nullptr;
        parser_state.reset_decl();
      }
    | declaration_specifiers init_declarator_list SEMICOLON_OP
      {
        // Use $1 directly instead of parser_state.current_decl_base_type
        // because parameter parsing may have reset it
        TypePtr base = $1;
        
        // Collect assignment statements for declarations with initializers
        std::vector<ASTNodePtr> init_stmts;

        if (base) {
          for (auto &di : $2) {
            if (di.is_function) {
              bool in_class = !parser_state.ctx_stack.empty() && parser_state.ctx_stack.back() == ContextKind::CLASS && parser_state.current_class_type;
              if (!in_class && !di.name.empty()) {
                encountered_function_names.insert(di.name);
                // Apply pointer levels to base type for return type
                TypePtr return_type = base;
                if (di.pointer_levels > 0) {
                  QualifiedType qt = apply_pointer_levels_or_error(QualifiedType(base, Qualifier::NONE),
                                                                   di.pointer_levels,
                                                                   "function return type",
                                                                   @1.begin.line,
                                                                   @1.begin.column);
                  return_type = qt.type;
                }

                if (di.pointer_levels == 0 && return_type) {
                  check_complete_type(return_type, @1, TypeUsageContext::FUNCTION_RETURN_TYPE);
                }

                TypePtr fn = make_function_type_or_error(return_type,
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
            } else {
              // Non-function declarator - check for initializer
              if (di.initializer && !di.name.empty()) {
                // Look up the symbol that was just added
                auto sym = symbol_table.lookup_symbol(di.name);
                if (sym.has_value()) {
                  // Create an identifier expression for the variable
                  auto id_expr = std::make_shared<IdentifierExpr>(sym.value(), sym.value()->get_type().type);
                  // Create an assignment expression: var = initializer
                  auto assign_expr = std::make_shared<AssignmentExpr>(
                    Operator::ASSIGN,
                    id_expr,
                    di.initializer,
                    sym.value()->get_type().type
                  );
                  init_stmts.push_back(assign_expr);
                }
              }
            }
          }
        }
        
        // If we have any initialization statements, return a BlockStmt containing them
        // Otherwise return nullptr
        if (!init_stmts.empty()) {
          $$ = std::make_shared<BlockStmt>(init_stmts);
        } else {
          $$ = nullptr;
        }
        
        parser_state.reset_decl();
      }
    | TYPEDEF type_specifier pointer_opt TYPE_NAME SEMICOLON_OP
      {
        $$ = nullptr;
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
        $$.initializer = $3;  // Store the initializer expression
        const DeclaratorInfo &di = $1;
        QualifiedType base = parser_state.current_decl_base_type;
        if (base.type) {
          // Functions cannot have initializers in declarations
          if (di.is_function) {
            parser_add_error(@2.begin.line, @2.begin.column,
                           "function '" + di.name + "' cannot have an initializer");
          } else {
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
              check_complete_type(final_t.type, @1, TypeUsageContext::VARIABLE_DECLARATION);
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
      }
    | declarator
      {
        $$ = $1;
        const DeclaratorInfo &di = $1;
        QualifiedType base = parser_state.current_decl_base_type;

        if (base.type) {
          // Skip adding as a variable if this is a function declarator
          // Function declarations are handled separately in the declaration rule
          if (!di.is_function) {
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
              check_complete_type(final_t.type, @1, TypeUsageContext::VARIABLE_DECLARATION);
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
      }
    ;

type_specifier
  : VOID     { $$ = require_builtin("void", @1, "type specifier");     parser_state.current_decl_base_type = $$;  }
  | CHAR     { $$ = require_builtin("char", @1, "type specifier");     parser_state.current_decl_base_type = $$; }
  | INT      { $$ = require_builtin("int", @1, "type specifier");      parser_state.current_decl_base_type = $$; }
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

        // Check if a forward declaration already exists (e.g., from self-referential pointers)
        auto found = type_factory.lookup(full_name);
        TypePtr rec;
        if (found.has_value()) {
          rec = found.value();
          auto record_type = std::static_pointer_cast<RecordType>(rec);
          record_type->is_defined = true;
        } else {
          rec = unwrap_type_or_error(type_factory.make<RecordType>(tag, is_union, true), "struct/union definition", @1.begin.line, @2.begin.column);
        }

        $$ = rec;

        symbol_table.enter_scope();
        auto record_type = std::static_pointer_cast<RecordType>($$);
        for (const auto &kv : $4) {
          record_type->add_field(kv.first, kv.second);
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
                if (di.array_dims.empty()) {
                  check_complete_type(final_t.type, @1, TypeUsageContext::STRUCT_UNION_MEMBER);
                }
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
    | direct_declarator OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
      {
        $$ = $1;
        if ($3->type == ASTNodeType::LITERAL_EXPR) {
          auto literal = std::static_pointer_cast<LiteralExpr>($3);
          auto expr_type = get_expression_type($3, @3, "array size expression");
          if (expr_type && is_integral_type(expr_type)) {
            int64_t value = std::visit([](auto&& arg) -> int64_t {
                            using T = std::decay_t<decltype(arg)>;
                            if constexpr (std::is_same_v<T, int64_t>)
                                return arg;
                            else if constexpr (std::is_same_v<T, uint64_t>)
                                return static_cast<int64_t>(arg);
                            else if constexpr (std::is_same_v<T, char>)
                                return static_cast<int64_t>(arg);
                            else
                                throw std::runtime_error("Unexpected type in variant");
                        }, literal->value);

            if(value < 0){
              parser_add_error(@2.begin.line, @2.begin.column, "array size expression must be non-negative");
              value = 0;
            }

            $$.array_dims.push_back(value);
          }
          else{
            parser_add_error(@3.begin.line, @3.begin.column, "array size expression must be an integral constant");
            $$.array_dims.push_back(0);
          }

        } else {
          // TODO: add support for constant expressions (low-priority)
          $$.array_dims.push_back(0);
        }
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
            check_complete_type(t.type, @1, TypeUsageContext::FUNCTION_PARAMETER);
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
          check_complete_type(base.type, @1, TypeUsageContext::FUNCTION_PARAMETER);
          $$.type = base;
        }
        $$.name = std::string{};
        parser_state.reset_decl();
      }
    | declaration_specifiers
      {
        $$ = ParamDeclInfo{};
        QualifiedType base_type = parser_state.current_decl_base_type;
        if (base_type.type) {
          check_complete_type(base_type.type, @1, TypeUsageContext::FUNCTION_PARAMETER);
        }
        $$.type = base_type;
        $$.name = std::string{};
        parser_state.reset_decl();
      }
    ;

abstract_declarator
    : pointer direct_abstract_declarator
    {
        $$ = $2;
        $$.pointer_levels += $1;
    }
    | pointer
    {
        $$ = DeclaratorInfo{};
        $$.pointer_levels = $1;
    }
    | direct_abstract_declarator
    {
        $$ = $1;
    }
    ;

direct_abstract_declarator
    : OPEN_PAREN_OP abstract_declarator CLOSE_PAREN_OP
    {
        $$ = $2;
    }
    | OPEN_BRACKET_OP CLOSE_BRACKET_OP
    {
        $$ = DeclaratorInfo{};
        $$.array_dims.push_back(0);
    }
    | OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    {
        $$ = DeclaratorInfo{};
        if ($2->type == ASTNodeType::LITERAL_EXPR) {
          auto literal = std::static_pointer_cast<LiteralExpr>($2);
          auto expr_type = get_expression_type($2, @2, "array size expression");
          if (expr_type && is_integral_type(expr_type)) {
            int64_t value = std::visit([](auto&& arg) -> int64_t {
                            using T = std::decay_t<decltype(arg)>;
                            if constexpr (std::is_same_v<T, int64_t>)
                                return arg;
                            else if constexpr (std::is_same_v<T, uint64_t>)
                                return static_cast<int64_t>(arg);
                            else if constexpr (std::is_same_v<T, char>)
                                return static_cast<int64_t>(arg);
                            else
                                throw std::runtime_error("Unexpected type in variant");
                        }, literal->value);

            if(value < 0){
              parser_add_error(@2.begin.line, @2.begin.column, "array size expression must be non-negative");
              value = 0;
            }

            $$.array_dims.push_back(value);
          }
          else{
            parser_add_error(@2.begin.line, @2.begin.column, "array size expression must be an integral constant");
            $$.array_dims.push_back(0);
          }

        } else {
          // TODO: add support for constant expressions (low-priority)
          $$.array_dims.push_back(0);
        }
    }
    | direct_abstract_declarator OPEN_BRACKET_OP CLOSE_BRACKET_OP
    {
        $$ = $1;
        $$.array_dims.push_back(0);
    }
    | direct_abstract_declarator OPEN_BRACKET_OP constant_expression CLOSE_BRACKET_OP
    {
        $$ = $1;
        if ($3->type == ASTNodeType::LITERAL_EXPR) {
          auto literal = std::static_pointer_cast<LiteralExpr>($3);
          auto expr_type = get_expression_type($3, @3, "array size expression");
          if (expr_type && is_integral_type(expr_type)) {
            int64_t value = std::visit([](auto&& arg) -> int64_t {
                            using T = std::decay_t<decltype(arg)>;
                            if constexpr (std::is_same_v<T, int64_t>)
                                return arg;
                            else if constexpr (std::is_same_v<T, uint64_t>)
                                return static_cast<int64_t>(arg);
                            else if constexpr (std::is_same_v<T, char>)
                                return static_cast<int64_t>(arg);
                            else
                                throw std::runtime_error("Unexpected type in variant");
                        }, literal->value);

            if(value < 0){
              parser_add_error(@2.begin.line, @2.begin.column, "array size expression must be non-negative");
              value = 0;
            }

            $$.array_dims.push_back(value);
          }
          else{
            parser_add_error(@3.begin.line, @3.begin.column, "array size expression must be an integral constant");
            $$.array_dims.push_back(0);
          }

        } else {
          // TODO: add support for constant expressions (low-priority)
          $$.array_dims.push_back(0);
        }
    }
    | OPEN_PAREN_OP CLOSE_PAREN_OP
      {
        $$ = DeclaratorInfo{};
        $$.is_function = true;
        $$.has_params = false;
      }
    | OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
    {
        $$ = DeclaratorInfo{};
        $$.is_function = true;
        $$.has_params = true;
        $$.param_types = $2.types;
        $$.param_names = $2.names;
        $$.is_variadic = $2.variadic;
    }
    | direct_abstract_declarator OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
    {
        $$ = $1;
        $$.is_function = true;
        $$.has_params = true;
        $$.param_types = $3.types;
        $$.param_names = $3.names;
        $$.is_variadic = $3.variadic;
    }
    ;

type_name
    : specifier_qualifier_list abstract_declarator
      {
        QualifiedType base = $1;
        if (base.type) {
          QualifiedType final_t;
          if($2.pointer_levels){
            final_t = apply_pointer_levels_or_error(base,
                                                   $2.pointer_levels,
                                                   "type name pointer",
                                                   @1.begin.line,
                                                   @1.begin.column);
          }
          else if(!$2.array_dims.empty()){
            final_t = apply_array_dimensions_or_error(base,
                                                     $2.array_dims,
                                                     "type name array",
                                                     @1.begin.line,
                                                     @1.begin.column);
          }
          else {
            final_t = base;
          }
          $$ = final_t.type;
        } else {
          $$ = nullptr;
        }
      }
    | specifier_qualifier_list
      { $$ = $1; }
    ;

initializer
    : assignment_expression { $$ = $1; }
    | OPEN_BRACE_OP initializer_list CLOSE_BRACE_OP { $$ = nullptr; /* TODO: handle aggregate initializers */ }
    | OPEN_BRACE_OP initializer_list COMMA_OP CLOSE_BRACE_OP { $$ = nullptr; /* TODO: handle aggregate initializers */ }
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
    : DOT_OP IDENTIFIER
    ;

statement
    : labeled_statement { $$ = $1; }
    | compound_statement { $$ = $1; }
    | expression_statement { $$ = $1; }
    | selection_statement { $$ = $1; }
    | iteration_statement { $$ = $1; }
    | jump_statement { $$ = $1; }
	  | declaration   { $$ = $1;  } // Now returns BlockStmt with initializer assignments
    ;

labeled_statement
    : IDENTIFIER COLON_OP statement {
        auto label=type_factory.lookup_by_scope($1, symbol_table.get_scope_chain());
        if (!label.has_value()) {
          label = unwrap_type_or_error(type_factory.make<BuiltinType>(BuiltinTypeKind::LABEL), "label", @1.begin.line, @1.begin.column);
        }
        add_symbol_if_valid($1, QualifiedType(label.value(), Qualifier::NONE), @1);
        auto label_sym_opt = symbol_table.lookup_symbol($1);
        if (!label_sym_opt.has_value()) {
          parser_add_error(@1.begin.line, @1.begin.column, "label symbol '" + $1 + "' not found after adding it");
          $$ = nullptr; // TODO: replace with error node
        } else {
          $$ = std::make_shared<LabelStmt>(label_sym_opt.value(), $3);
          parser_state.resolve_label(label_sym_opt.value());
        }
	  }
    | CASE constant_expression COLON_OP statement
    {
      if (!in_switch()) {
        parser_add_error(@1.begin.line, @1.begin.column, "'case' label not within a switch statement");
        $$ = nullptr; // TODO: replace with error node
      } else {
        auto t = parser_state.current_switch_subject();
        if (t) {
          auto expr_type = get_expression_type($2);
          if (expr_type && !are_types_equal(t, expr_type.value())) {
            parser_add_error(@2.begin.line, @2.begin.column, "'case' label type does not match switch expression type");
          } else {
            $$ = std::make_shared<CaseStmt>($2, $4);
            parser_state.case_stmt_stack.back().push_back($$);
          }
        }
      }
    }
    | DEFAULT COLON_OP statement
    {
      if (!in_switch()) {
        parser_add_error(@1.begin.line, @1.begin.column, "'default' label not within a switch statement");
        $$ = nullptr; // TODO: replace with error node
      } else if (parser_state.default_case.back().has_value()) {
        parser_add_error(@1.begin.line, @1.begin.column, "multiple 'default' labels in the same switch");
      } else {
        $$ = std::make_shared<DefaultStmt>($3);
        parser_state.default_case.back() = $$;
      }
    }
    ;

compound_statement
    : open_brace close_brace { $$ = std::make_shared<CompoundStmt>(std::vector<ASTNodePtr>{}); }
    | open_brace block_item_list close_brace { $$ = std::make_shared<CompoundStmt>($2); }
    ;

block_item_list
    : block_item { $$ = std::vector<ASTNodePtr>{ $1 }; }
    | block_item_list block_item { $$ = $1; $$.push_back($2); }
    ;

block_item
    : statement { $$ = $1; }
    ;

expression_statement
  : SEMICOLON_OP { $$ = nullptr; }
  | expression SEMICOLON_OP { $$ = $1; }
    ;

selection_statement
    : IF OPEN_PAREN_OP expression CLOSE_PAREN_OP statement ELSE statement
    {
        ensure_condition_is_bool($3, @3, "if");
        auto else_node = std::make_shared<ElseStmt>($7);
        $$ = std::make_shared<IfStmt>($3, $5, else_node);
    }
    | IF OPEN_PAREN_OP expression CLOSE_PAREN_OP statement
    {
        ensure_condition_is_bool($3, @3, "if");
        $$ = std::make_shared<IfStmt>($3, $5);
    }
    | SWITCH OPEN_PAREN_OP expression CLOSE_PAREN_OP
    {
        ensure_switch_subject_type($3, @3);
        parser_state.push_ctx(ContextKind::SWITCH);
        parser_state.case_stmt_stack.emplace_back();
        parser_state.default_case.push_back(std::nullopt);
    }
      statement
    {
        if (parser_state.case_stmt_stack.back().size() == 0 &&
          !parser_state.default_case.back().has_value()) {
          parser_add_error(@1.begin.line, @1.begin.column, "switch statement has no cases");
          $$ = nullptr; // TODO: replace with error node
        } else {
          $$ = std::make_shared<SwitchStmt>($3,
                parser_state.case_stmt_stack.back(),
                parser_state.default_case.back());
        }
        parser_state.pop_ctx();
        parser_state.case_stmt_stack.pop_back();
        parser_state.default_case.pop_back();
    }
    ;

iteration_statement
    : WHILE OPEN_PAREN_OP expression CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
      statement
    {
    ensure_condition_is_bool($3, @3, "while");
    $$ = std::make_shared<WhileStmt>($3, $6);
        parser_state.pop_ctx();
    }
	  | UNTIL OPEN_PAREN_OP expression CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
     statement
    {
    ensure_condition_is_bool($3, @3, "until");
    $$ = std::make_shared<UntilStmt>($3, $6);
        parser_state.pop_ctx();
    }
  | DO { parser_state.push_ctx(ContextKind::LOOP); } statement WHILE OPEN_PAREN_OP expression CLOSE_PAREN_OP
    SEMICOLON_OP
    {
    ensure_condition_is_bool($6, @6, "do-while");
    $$ = std::make_shared<DoWhileStmt>( $3, $6);
        parser_state.pop_ctx();
    }
    | FOR OPEN_PAREN_OP
    { symbol_table.enter_scope(); }
      expression_statement expression_statement CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
      statement
    {
        if ($5) ensure_condition_is_bool($5, @5, "for");
        $$ = std::make_shared<ForStmt>($4, $5, std::nullopt, $8);
        parser_state.pop_ctx();
        symbol_table.exit_scope();
    }
    | FOR OPEN_PAREN_OP
    { symbol_table.enter_scope(); }
      expression_statement expression_statement expression CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
      statement
    {
        if ($5) ensure_condition_is_bool($5, @5, "for");
        $$ = std::make_shared<ForStmt>($4, $5, $6, $9);
        parser_state.pop_ctx();
        symbol_table.exit_scope();
    }
    | FOR OPEN_PAREN_OP
    { symbol_table.enter_scope(); }
      declaration expression_statement CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
      statement
    {
        if ($5) ensure_condition_is_bool($5, @5, "for");
        // TODO: handle intializer declaration
        $$ = std::make_shared<ForStmt>(std::nullopt, $5, std::nullopt, $8);
        parser_state.pop_ctx();
        symbol_table.exit_scope();
    }
    | FOR OPEN_PAREN_OP
    { symbol_table.enter_scope(); }
      declaration expression_statement expression CLOSE_PAREN_OP
    { parser_state.push_ctx(ContextKind::LOOP); }
      statement
    {
        if ($5) ensure_condition_is_bool($5, @5, "for");
        // TODO: handle intializer declaration
        $$ = std::make_shared<ForStmt>(std::nullopt, $5, $6, $9);
        parser_state.pop_ctx();
        symbol_table.exit_scope();
    }
    ;

jump_statement
    : GOTO IDENTIFIER SEMICOLON_OP
    {
        if (!parser_state.in_function()) {
          parser_add_error(@1.begin.line, @1.begin.column, "'goto' statement not within a function");
          $$ = nullptr; // TODO: replace with error node
        } else {
          auto label_sym_opt = symbol_table.lookup_symbol($2);
          auto goto_node = std::make_shared<GotoStmt>(nullptr);
          if (!label_sym_opt.has_value()) {
            parser_state.add_unresolved_label($2, goto_node);
          }
          else {
            goto_node->target_label = label_sym_opt.value();
          }
          $$ = goto_node;
        }
    }
    | CONTINUE SEMICOLON_OP
    {
        if (!in_loop()) {
          parser_add_error(@1.begin.line, @1.begin.column, "'continue' statement not within a loop");
          $$ = nullptr; // TODO: replace with error node
        } else {
          $$ = std::make_shared<ContinueStmt>();
        }
    }
    | BREAK SEMICOLON_OP
    {
        if (!in_loop_or_switch()) {
          parser_add_error(@1.begin.line, @1.begin.column, "'break' statement not within a loop or switch");
          $$ = nullptr; // TODO: replace with error node
        } else {
          $$ = std::make_shared<BreakStmt>();
        }
    }
    | RETURN SEMICOLON_OP
    {
      if (!parser_state.in_function()) {
          parser_add_error(@1.begin.line, @1.begin.column, "'return' statement not within a function");
          $$ = nullptr; // TODO: replace with error node
      } else if (parser_state.current_function_return() && is_void_type(parser_state.current_function_return())) {
          auto void_t = type_factory.get_builtin_type("void");
          $$ = std::make_shared<RetExpr>(std::nullopt, void_t ? void_t.value() : nullptr);
      } else {
          parser_add_error(@1.begin.line, @1.begin.column, "return without expression in non-void function");
          $$ = nullptr; // TODO: replace with error node
      }
    }
    | RETURN expression SEMICOLON_OP
    {
      auto expr_type = get_expression_type($2, @2, "return expression");
      if (!expr_type) {
        parser_add_error(@2.begin.line, @2.begin.column, "unable to determine type of return expression");
        $$ = nullptr; // TODO: replace with error node
      } else if (!parser_state.in_function()) {
        parser_add_error(@1.begin.line, @1.begin.column, "'return' statement not within a function");
        $$ = nullptr; // TODO: replace with error node
      } else {
        auto fn_ret = strip_typedefs(parser_state.current_function_return());
        auto expr_ret = strip_typedefs(expr_type);

        // Check for return with value in void function first
        if (is_void_type(fn_ret)) {
          parser_add_error(@1.begin.line, @1.begin.column, "return with a value in function returning 'void'");
          $$ = nullptr; // TODO: replace with error node
        } else if (fn_ret != expr_ret) {
          parser_add_error(@2.begin.line, @2.begin.column, "return type mismatch: function expects '" + (fn_ret ? fn_ret->debug_name() : std::string("invalid")) + "', but returning expression of type '" + (expr_ret ? expr_ret->debug_name() : std::string("invalid")) + "'");
          $$ = nullptr; // TODO: replace with error node
        } else {
          $$ = std::make_shared<RetExpr>($2, expr_type);
        }
      }
    }
    ;

translation_unit
    : external_declaration { 
        $$ = std::vector<ASTNodePtr>{ $1 }; 
        parsed_translation_unit = $$;
    }
    | translation_unit external_declaration
    {
        $$ = std::move($1);
        $$.push_back($2);
        parsed_translation_unit = $$;
    }
    ;

external_declaration
    : function_definition { $$ = std::move($1); }
    | declaration { $$ = std::move($1); }  // Pass through the declaration AST node
    ;

function_definition
    : declaration_specifiers declarator { /* begin function body: set function return context */
		TypePtr ret_t = $1;
		const DeclaratorInfo &di = $2;
		if (ret_t && di.is_function) {
		  if (di.pointer_levels > 0) {
		    QualifiedType qt = apply_pointer_levels_or_error(QualifiedType(ret_t, Qualifier::NONE),
		                                                     di.pointer_levels,
		                                                     "function return type context",
		                                                     @1.begin.line,
		                                                     @2.begin.column);
		    ret_t = qt.type;
		  }
		  parser_state.push_function(ret_t);
		  prepare_parameters_for_scope(di.param_types, di.param_names);
		}
	 } compound_statement {
		auto base = $1;
		$$ = nullptr;

		if (!base) {
		  parser_add_error(@1.begin.line, @1.begin.column, "function definition requires a return type");
		} else {
		  const DeclaratorInfo &di = $2;
			if (!di.is_function || di.name.empty()) {
				parser_add_error(@2.begin.line, @2.begin.column, "function definition requires a named function declarator");
			} else {
			  // Validate function parameters
			  check_unnamed_parameters(di.param_names, @2);
			  check_duplicate_parameter_names(di.param_names, @2);

        encountered_function_names.insert(di.name);
			  // Apply pointer levels to base type for return type
			  TypePtr return_type = base;
			  if (di.pointer_levels > 0) {
			    QualifiedType qt = apply_pointer_levels_or_error(QualifiedType(base, Qualifier::NONE),
			                                                     di.pointer_levels,
			                                                     "function return type",
			                                                     @1.begin.line,
			                                                     @2.begin.column);
			    return_type = qt.type;
			  }

			  if (di.pointer_levels == 0 && return_type) {
			    check_complete_type(return_type, @1, TypeUsageContext::FUNCTION_RETURN_TYPE);
			  }

			  // Build function type
      TypePtr fn = make_function_type_or_error(return_type,
                       di.param_types,
                       di.is_variadic,
                       "function definition",
                       @1.begin.line,
                       @2.begin.column);
        if (fn) {
            FunctionMeta meta(FunctionKind::NORMAL, di.param_names, std::nullopt);
            auto mangled = mangle_function_name(di.name,
                                                *std::static_pointer_cast<FunctionType>(fn),
                                                meta,
                                                std::nullopt);
            if (!mangled.has_value()) {
            parser_add_error(@2.begin.line, @2.begin.column, "unable to mangle function '" + di.name + "'");
            } else {
            // Check for redefinition
            note_function_definition(*mangled, @2);

            // Reuse existing symbol if present (from prior declaration); otherwise add it
            auto sym_opt = symbol_table.lookup_symbol(*mangled);
            if (!sym_opt.has_value()) {
              add_symbol_if_valid(*mangled,
                                QualifiedType(fn, Qualifier::NONE),
                                @1,
                                std::optional<FunctionMeta>{meta});
              sym_opt = symbol_table.lookup_symbol(*mangled);
            }

            // Build FunctionDef AST node (parameters can be filled later if needed)
            $$ = std::make_shared<FunctionDef>(sym_opt.value_or(nullptr), return_type, std::vector<SymbolPtr>{}, $4);

            // Check that non-void functions have a return statement
            check_function_returns(return_type, $4, @4);
			}
		}
			}
		}

		parser_state.reset_decl();
		if (parser_state.in_function()) parser_state.pop_function();
	  }
    | declaration_specifiers declarator declaration_list compound_statement
    {
      // K&R style function definition - not fully supported yet
      parser_add_error(@1.begin.line, @1.begin.column, "K&R style function definitions are not yet supported");
      $$ = nullptr;
    }
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

      if (parser_state.current_class_type) {
        std::static_pointer_cast<ClassType>(parser_state.current_class_type)->is_defined = true;
      }
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

      if (parser_state.current_class_type) {
        std::static_pointer_cast<ClassType>(parser_state.current_class_type)->is_defined = true;
      }

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
      TypePtr t = unwrap_type_or_error(type_factory.make<ClassType>(os.str(), parser_state.parent_class_type, parser_state.inherited_access, true), "empty anonymous class", @1.begin.line, @1.begin.column);
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
      if (!check_complete_type(opt.value(), @2, TypeUsageContext::CLASS_INHERITANCE)) {
        parser_state.parent_class_type = nullptr;
      } else {
        parser_state.parent_class_type = std::static_pointer_cast<ClassType>(opt.value());
        parser_state.inherited_access = $1;
      }
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
      if (!check_complete_type(opt.value(), @1, TypeUsageContext::CLASS_INHERITANCE)) {
        parser_state.parent_class_type = nullptr;
      } else {
        parser_state.parent_class_type = std::static_pointer_cast<ClassType>(opt.value());
        parser_state.inherited_access = Access::PRIVATE; // default
      }
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
            if (di.pointer_levels == 0 && ret) {
              check_complete_type(ret, @1, TypeUsageContext::FUNCTION_RETURN_TYPE);
            }

            if (ret && di.pointer_levels > 0) {
              QualifiedType qt = apply_pointer_levels_or_error(QualifiedType(ret, Qualifier::NONE),
                                                               di.pointer_levels,
                                                               "member function pointer declarator",
                                                               @1.begin.line,
                                                               @2.begin.column);
              ret = qt.type;
            }

            encountered_function_names.insert(di.name);
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
              if (di.array_dims.empty()) {
                check_complete_type(final_t.type, @1, TypeUsageContext::CLASS_DATA_MEMBER);
              }
            }

            // TODO error handling
            add_symbol_if_valid(di.name, final_t, @1);
            auto mi = MemberInfo{final_t, parser_state.current_access, false};
            std::static_pointer_cast<ClassType>(parser_state.current_class_type)->add_member(di.name, mi);

          }
        }
        parser_state.reset_decl();
      }
    | declaration_specifiers declarator
      { /* enter function context before parsing body if this is a method */
        TypePtr ret = $1;
        const DeclaratorInfo &di = $2;
        if (ret && di.is_function) {
            if (di.pointer_levels > 0) {
              QualifiedType qt = apply_pointer_levels_or_error(QualifiedType(ret, Qualifier::NONE),
                                                               di.pointer_levels,
                                                               "member function pointer definition",
                                                               @1.begin.line,
                                                               @2.begin.column);
              ret = qt.type;
            }
          parser_state.push_function(ret);
          prepare_parameters_for_scope(di.param_types, di.param_names);
        }
      }
      compound_statement
      {
        const DeclaratorInfo &di = $2;
  		  TypePtr ret = $1;
        if (!parser_state.ctx_stack.empty() && in_class() && parser_state.current_class_type && !di.name.empty()) {
          if (di.is_function) {
            if (di.pointer_levels == 0 && ret) {
              check_complete_type(ret, @1, TypeUsageContext::FUNCTION_RETURN_TYPE);
            }

            encountered_function_names.insert(di.name);
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
        if (parser_state.in_function()) parser_state.pop_function();
      }
    | /* constructor with params */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
      { /* push function context (void return) before body */
        TypePtr ret = require_builtin("void", @1, "constructor return type");
        if (ret) {
          parser_state.push_function(ret);
          const auto &plist = $3;
          prepare_parameters_for_scope(plist.types, plist.names);
        }
      }
      compound_statement
      {
        const auto &plist = $3;
        handle_constructor_definition($1, plist.types, plist.names, plist.variadic, $6, @1);
        if (parser_state.in_function()) parser_state.pop_function();
      }
  | /* constructor without params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP
      { /* push function context (void) before body */
        TypePtr ret = require_builtin("void", @1, "constructor return type");
        if (ret) { parser_state.push_function(ret); }
      }
      compound_statement
      {
        std::vector<QualifiedType> empty_params;
        std::vector<std::string> empty_names;
        handle_constructor_definition($1, empty_params, empty_names, false, $5, @1);
        if (parser_state.in_function()) parser_state.pop_function();
      }
  | /* destructor */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP
      { /* push function context (void) before body */
        TypePtr ret = require_builtin("void", @1, "destructor return type");
        if (ret) { parser_state.push_function(ret); }
      }
      compound_statement
      {
        handle_destructor_definition($2, $6, @2);
        if (parser_state.in_function()) parser_state.pop_function();
      }
  | /* operator overload def with params */ type_name OPERATOR operator_token OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP
      { /* push function context before body */
        if ($1) {
          parser_state.push_function($1);
          const auto &plist = $5;
          prepare_parameters_for_scope(plist.types, plist.names);
        }
      }
      compound_statement
      {
        const auto &plist = $5;
        handle_operator_overload_definition(
            $1, $3, plist.types, plist.names, plist.variadic,
            $8, @1, @3);

        parser_state.reset_decl();
        if (parser_state.in_function()) parser_state.pop_function();
      }
  | /* operator overload def without params */ type_name OPERATOR operator_token OPEN_PAREN_OP CLOSE_PAREN_OP
      { /* push function context before body */
        if ($1) { parser_state.push_function($1); }
      }
      compound_statement
      {
        std::vector<QualifiedType> empty_params;
        std::vector<std::string> empty_names;
        handle_operator_overload_definition(
            $1, $3, empty_params, empty_names, false,
            $7, @1, @3);

        parser_state.reset_decl();
        if (parser_state.in_function()) parser_state.pop_function();
      }
    | /* constructor decl with params */ IDENTIFIER OPEN_PAREN_OP parameter_type_list CLOSE_PAREN_OP SEMICOLON_OP
      {
        const auto &plist = $3;
        handle_constructor_declaration($1, plist.types, plist.names, plist.variadic, @1);
      }
    | /* constructor decl no params */ IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        std::vector<QualifiedType> empty_params;
        std::vector<std::string> empty_names;
        handle_constructor_declaration($1, empty_params, empty_names, false, @1);
      }
    | /* destructor decl */ TILDE_OP IDENTIFIER OPEN_PAREN_OP CLOSE_PAREN_OP SEMICOLON_OP
      {
        handle_destructor_declaration($2, @2);
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
    | TILDE_OP        { $$ = "~"; }
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
    | AMPERSAND_ASSIGN_OP { $$ = "&="; }
    | PIPE_ASSIGN_OP  { $$ = "|="; }
    | CARET_ASSIGN_OP { $$ = "^="; }
    | LSHIFT_ASSIGN_OP{ $$ = "<<="; }
    | RSHIFT_ASSIGN_OP{ $$ = ">>="; }
    | ARROW_OP        { $$ = "->"; }
    | DOT_OP          { $$ = "."; }
    | OPEN_BRACKET_OP CLOSE_BRACKET_OP { $$ = "[]"; }
    | OPEN_PAREN_OP CLOSE_PAREN_OP { $$ = "()"; }
    | COMMA_OP        { $$ = ","; }
    | LOGICAL_AND_OP { $$ = "&&"; }
    | LOGICAL_OR_OP  { $$ = "||"; }
    | LOGICAL_NOT_OP { $$ = "!"; }
    | NEW             { $$ = " new"; }
    | DELETE          { $$ = " delete"; }
    ;


%%

void yy::Parser::error(const location_type& loc, const std::string& msg) {
    parser_add_error(loc.begin.line, loc.begin.column, msg);
}
