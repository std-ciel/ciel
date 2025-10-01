#ifndef SCOPE_HPP
#define SCOPE_HPP

#include "symbol_table/symbol.hpp"
#include <string>
#include <unordered_map>

constexpr ScopeID GLOBAL_SCOPE_ID = 0;

struct Scope {
    ScopeID id;
    size_t level;
    ScopeID parent_id;
    std::unordered_map<std::string, SymbolPtr> symbols;

    Scope(ScopeID id, size_t level, ScopeID parent_id)
        : id(id), level(level), parent_id(parent_id)
    {
    }

    bool is_present(const std::string &name) const
    {
        return symbols.find(name) != symbols.end();
    }
};

#endif // SCOPE_HPP