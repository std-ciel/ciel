#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include "symbol_table/type.hpp"
#include <memory>
#include <optional>
#include <string>

using ScopeID = size_t;

enum class StorageClass { STATIC, REGISTER, AUTO };

enum class FunctionKind { NORMAL, METHOD, CONSTRUCTOR, DESTRUCTOR, OPERATOR };

struct FunctionMeta {
    FunctionKind function_kind;
    bool is_defined;
    std::vector<std::string> params;
    std::optional<TypePtr> parent_class;
    ScopeID body_scope_id;
    std::string mangled_name;

    FunctionMeta()
        : function_kind(FunctionKind::NORMAL), is_defined(false),
          body_scope_id(0)
    {
    }

    FunctionMeta(FunctionKind kind,
                 std::vector<std::string> params,
                 std::optional<TypePtr> parent_class = std::nullopt)
        : function_kind(kind), is_defined(false), params(std::move(params)),
          parent_class(std::move(parent_class)), body_scope_id(0)
    {
    }
};

class Symbol {
  public:
    Symbol(std::string name,
           QualifiedType type,
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

    void set_name(const std::string &n)
    {
        name = n;
    }

    QualifiedType get_type() const
    {
        return type;
    }

    ScopeID get_scope_id() const
    {
        return scope_id;
    }

    void set_scope_id(ScopeID sid)
    {
        scope_id = sid;
    }

    ScopeID get_parent_scope() const
    {
        return parent_scope;
    }

    void set_parent_scope(ScopeID psid)
    {
        parent_scope = psid;
    }

    StorageClass get_storage_class() const
    {
        return storage_class;
    }

    const std::optional<FunctionMeta> &get_function_meta() const
    {
        return function_meta;
    }

    void set_function_meta(FunctionMeta fm)
    {
        function_meta = std::move(fm);
    }

  private:
    std::string name;
    QualifiedType type;
    StorageClass storage_class = StorageClass::AUTO;
    ScopeID scope_id;
    ScopeID parent_scope;
    std::optional<FunctionMeta> function_meta;
};

using SymbolPtr = std::shared_ptr<Symbol>;

std::string storage_class_to_string(StorageClass storage_class);

#endif // SYMBOL_HPP
