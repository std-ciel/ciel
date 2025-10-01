#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include "symbol_table/type.hpp"
#include <memory>
#include <string>

using ScopeID = size_t;

enum class StorageClass { STATIC, REGISTER, AUTO };

class Symbol {
  public:
    Symbol(std::string name,
           QualType type,
           StorageClass storage_class,
           ScopeID scope_id,
           ScopeID parent_scope)
        : name(std::move(name)), type(std::move(type)),
          storage_class(storage_class), scope_id(scope_id),
          parent_scope(parent_scope)
    {
    }

    const std::string &get_name() const
    {
        return name;
    }
    QualType get_type() const
    {
        return type;
    }
    ScopeID get_scope_id() const
    {
        return scope_id;
    }
    ScopeID get_parent_scope() const
    {
        return parent_scope;
    }
    StorageClass get_storage_class() const
    {
        return storage_class;
    }

  private:
    std::string name;
    QualType type;
    StorageClass storage_class = StorageClass::AUTO;
    ScopeID scope_id;
    ScopeID parent_scope;
};

using SymbolPtr = std::shared_ptr<Symbol>;

std::string storage_class_to_string(StorageClass storage_class);

#endif // SYMBOL_HPP