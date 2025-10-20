#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include "symbol_table/result.hpp"
#include "symbol_table/scope.hpp"
#include "symbol_table/symbol.hpp"
#include <optional>
#include <unordered_map>
#include <vector>

// Error types for SymbolTable operations
enum class SymbolTableError {
    SYMBOL_ALREADY_EXISTS,
    SYMBOL_NOT_FOUND,
    INVALID_SCOPE,
    INVALID_TYPE,
    MISSING_FUNCTION_META,
};

inline const char *symbol_table_error_to_string(SymbolTableError error)
{
    switch (error) {
    case SymbolTableError::SYMBOL_ALREADY_EXISTS:
        return "Symbol already exists in current scope";
    case SymbolTableError::SYMBOL_NOT_FOUND:
        return "Symbol not found";
    case SymbolTableError::INVALID_SCOPE:
        return "Invalid scope";
    case SymbolTableError::INVALID_TYPE:
        return "Invalid type";
    case SymbolTableError::MISSING_FUNCTION_META:
        return "Function symbol requires function metadata";
    default:
        return "Unknown error";
    }
}

class SymbolTable {
  public:
    SymbolTable();

    void enter_scope();
    void exit_scope();

    Result<bool, SymbolTableError>
    add_symbol(const std::string &name,
               QualifiedType type,
               StorageClass storage_class = StorageClass::AUTO,
               std::optional<FunctionMeta> function_meta = std::nullopt);

    Result<bool, SymbolTableError> add_symbol_in_scope(const std::string &name,
                                                       SymbolPtr symbol,
                                                       ScopeID target_scope);

    std::optional<SymbolPtr> lookup_symbol(const std::string &name) const;

    std::optional<SymbolPtr>
    lookup_operator(const std::string &mangled_name) const;

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

    Result<SymbolPtr, SymbolTableError> remove_symbol(SymbolPtr symbol);

    void print_symbols() const;

  private:
    ScopeID next_scope_id;
    ScopeID current_scope_id;
    size_t current_scope_level;

    std::unordered_map<ScopeID, Scope> scopes;

    std::vector<ScopeID> scope_stack;
};

#endif // SYMBOL_TABLE_HPP
