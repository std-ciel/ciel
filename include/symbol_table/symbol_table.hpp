#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include "symbol_table/type.hpp"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class Symbol {
  public:
    Symbol(std::string name, TypePtr type, size_t scope_id, size_t parent_scope)
        : name(std::move(name)), type(std::move(type)), scope_id(scope_id),
          parent_scope(parent_scope)
    {
    }

    const std::string &get_name() const
    {
        return name;
    }
    TypePtr get_type() const
    {
        return type;
    }
    size_t get_scope_id() const
    {
        return scope_id;
    }
    size_t get_parent_scope() const
    {
        return parent_scope;
    }

  private:
    std::string name;
    TypePtr type;
    size_t scope_id;
    size_t parent_scope;
};

using SymbolPtr = std::shared_ptr<Symbol>;

struct Scope {
    size_t id;
    size_t level;
    size_t parent_id;
    std::unordered_map<std::string, SymbolPtr> symbols;

    Scope(size_t id, size_t level, size_t parent_id)
        : id(id), level(level), parent_id(parent_id)
    {
    }
};

class SymbolTable {
  public:
    SymbolTable();

    void enter_scope();
    void exit_scope();

    bool add_symbol(const std::string &name, const std::string &type_name);
    bool add_symbol(const std::string &name, TypePtr type);
    bool add_symbol(const std::string &name, TypeId type_id);

    std::optional<SymbolPtr> lookup_symbol(const std::string &name) const;

    size_t get_current_scope_id() const
    {
        return current_scope_id;
    }

    size_t get_current_scope_level() const
    {
        return current_scope_level;
    }

    TypeFactory &get_type_factory()
    {
        return type_factory;
    }
    const TypeFactory &get_type_factory() const
    {
        return type_factory;
    }

    std::vector<size_t> get_scope_chain() const
    {
        return scope_stack;
    }

    void print_symbols() const;
    void print_custom_types() const;

  private:
    TypeFactory type_factory;
    size_t next_scope_id;
    size_t current_scope_id;
    size_t current_scope_level;

    std::unordered_map<size_t, Scope> scopes;

    std::vector<size_t> scope_stack;
};

#endif // SYMBOL_TABLE_HPP
