#ifndef LOCAL_STATIC_PASS_HPP
#define LOCAL_STATIC_PASS_HPP

#include "ast/ast_node.hpp"
#include "common/result.hpp"
#include "passes/local_static_pass_errors.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include <string>
#include <unordered_set>
#include <vector>

class LocalStaticPass {
  public:
    LocalStaticPass(SymbolTable &symbol_table, TypeFactory &type_factory);

    // Main entry point: process the entire AST
    Result<bool, std::vector<LocalStaticPassErrorInfo>>
    process(std::vector<ASTNodePtr> &translation_unit,
            std::vector<std::shared_ptr<FunctionDef>> &class_methods);

    // Process class methods (constructors, destructors, member functions)
    Result<bool, std::vector<LocalStaticPassErrorInfo>> process_class_methods(
        std::vector<std::shared_ptr<FunctionDef>> &class_methods);

    const std::vector<LocalStaticPassErrorInfo> &get_errors() const
    {
        return errors;
    }

  private:
    SymbolTable &symbol_table;
    TypeFactory &type_factory;
    std::vector<LocalStaticPassErrorInfo> errors;

    // Track symbols we've already moved to avoid duplicates
    std::unordered_set<SymbolPtr> moved_symbols;

    // Current function context (for name mangling)
    std::string current_function_mangled_name;
    ScopeID current_function_scope_id;

    // Traverse and transform
    void process_function(ASTNodePtr func_def);
    void process_block(ASTNodePtr block,
                       const std::string &func_mangled_name,
                       ScopeID func_scope_id);
    void process_statement(ASTNodePtr stmt,
                           const std::string &func_mangled_name,
                           ScopeID func_scope_id);

    // Helper: Create guard-protected initialization and move symbol to global
    ASTNodePtr transform_local_static(SymbolPtr symbol,
                                      ASTNodePtr initializer,
                                      const std::string &func_mangled_name,
                                      ScopeID func_scope_id);

    ASTNodePtr create_guarded_init(SymbolPtr symbol,
                                   SymbolPtr guard_symbol,
                                   ASTNodePtr initializer);
};

#endif // LOCAL_STATIC_PASS_HPP
