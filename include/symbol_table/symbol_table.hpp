#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include "symbol_table/scope.hpp"
#include "symbol_table/symbol.hpp"
#include <optional>
#include <unordered_map>
#include <vector>

class SymbolTable {
  public:
    SymbolTable();

    void enter_scope();
    void exit_scope();

    bool add_symbol(const std::string &name,
                    QualType type,
                    StorageClass storage_class = StorageClass::AUTO);

    std::optional<SymbolPtr> lookup_symbol(const std::string &name) const;

    ScopeID get_current_scope_id() const
    {
        return current_scope_id;
    }

    ScopeID get_current_scope_level() const
    {
        return current_scope_level;
    }

    std::vector<ScopeID> get_scope_chain() const
    {
        return scope_stack;
    }

    void print_symbols() const;

  private:
    ScopeID next_scope_id;
    ScopeID current_scope_id;
    size_t current_scope_level;

    std::unordered_map<ScopeID, Scope> scopes;

    std::vector<ScopeID> scope_stack;
};

#endif // SYMBOL_TABLE_HPP
