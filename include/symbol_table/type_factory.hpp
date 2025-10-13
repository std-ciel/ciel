#ifndef TYPE_FACTORY_HPP
#define TYPE_FACTORY_HPP

#include "symbol_table/result.hpp"
#include "symbol_table/symbol.hpp"
#include "symbol_table/type.hpp"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// Error types for TypeFactory operations
enum class TypeFactoryError {
    TYPE_ALREADY_DEFINED,
    TYPE_NOT_FOUND,
    INVALID_TYPE,
    POINTER_CREATION_FAILED,
    ARRAY_CREATION_FAILED,
};

// Convert error enums to strings
inline const char *type_factory_error_to_string(TypeFactoryError error)
{
    switch (error) {
    case TypeFactoryError::TYPE_ALREADY_DEFINED:
        return "Type already defined";
    case TypeFactoryError::TYPE_NOT_FOUND:
        return "Type not found";
    case TypeFactoryError::INVALID_TYPE:
        return "Invalid type";
    case TypeFactoryError::POINTER_CREATION_FAILED:
        return "Failed to create pointer type";
    case TypeFactoryError::ARRAY_CREATION_FAILED:
        return "Failed to create array type";
    default:
        return "Unknown error";
    }
}

class TypeFactory {
  private:
    std::unordered_map<TypeId, TypePtr> types;
    TypeId next_id;
    std::unordered_map<std::string, TypeId> name_to_id;
    std::unordered_map<TypeId, std::string> id_to_name;
    // scope id to defined types in the scope
    std::unordered_map<size_t, std::vector<TypeId>> scope_defined_types;
    TypePtr define_builtin(const std::string &name, BuiltinTypeKind kind)
    {
        auto type = std::make_shared<BuiltinType>(kind);
        TypeId id = next_id++;
        types[id] = type;
        name_to_id[name] = id;
        id_to_name[id] = name;
        return type;
    }

  public:
    template <typename T, typename... Args>
    Result<TypePtr, TypeFactoryError> make(Args &&...args)
    {
        auto type = std::make_shared<T>(std::forward<Args>(args)...);
        std::string name = type->debug_name();

        // Check if type already exists
        auto existing = name_to_id.find(name);
        if (existing != name_to_id.end()) {
            auto existing_type = types[existing->second];

            // For custom types (record, class, enum), check is_defined flag
            switch (type->kind) {
            case TypeKind::RECORD: {
                auto record_type =
                    std::static_pointer_cast<RecordType>(existing_type);
                if (!record_type->is_defined) {
                    return Result<TypePtr, TypeFactoryError>(existing_type);
                } else {
                    return Result<TypePtr, TypeFactoryError>(
                        TypeFactoryError::TYPE_ALREADY_DEFINED);
                }
            }
            case TypeKind::CLASS: {
                auto class_type =
                    std::static_pointer_cast<ClassType>(existing_type);
                if (!class_type->is_defined) {
                    return Result<TypePtr, TypeFactoryError>(existing_type);
                } else {
                    return Result<TypePtr, TypeFactoryError>(
                        TypeFactoryError::TYPE_ALREADY_DEFINED);
                }
            }
            case TypeKind::ENUM: {
                auto enum_type =
                    std::static_pointer_cast<EnumType>(existing_type);
                if (!enum_type->is_defined) {
                    return Result<TypePtr, TypeFactoryError>(existing_type);
                } else {
                    return Result<TypePtr, TypeFactoryError>(
                        TypeFactoryError::TYPE_ALREADY_DEFINED);
                }
            }
            default:
                return Result<TypePtr, TypeFactoryError>(existing_type);
            }
        }

        // Type doesn't exist, create new one
        TypeId id = next_id++;
        types[id] = type;
        name_to_id[name] = id;
        id_to_name[id] = name;
        return Result<TypePtr, TypeFactoryError>(type);
    }

    // Overloaded version for custom types with scope chain lookup
    template <typename T, typename... Args>
    Result<TypePtr, TypeFactoryError>
    make(const std::vector<size_t> &scope_chain, Args &&...args)
    {
        auto type = std::make_shared<T>(std::forward<Args>(args)...);
        std::string name = type->debug_name();

        // For custom types, do scope chain lookup first
        if (type->kind == TypeKind::RECORD || type->kind == TypeKind::CLASS ||
            type->kind == TypeKind::ENUM) {
            auto existing_in_scope = lookup_by_scope(name, scope_chain);
            if (existing_in_scope) {
                auto existing_type = *existing_in_scope;

                switch (type->kind) {
                case TypeKind::RECORD: {
                    auto record_type =
                        std::static_pointer_cast<RecordType>(existing_type);
                    if (!record_type->is_defined) {
                        return Result<TypePtr, TypeFactoryError>(existing_type);
                    } else {
                        return Result<TypePtr, TypeFactoryError>(
                            TypeFactoryError::TYPE_ALREADY_DEFINED);
                    }
                }
                case TypeKind::CLASS: {
                    auto class_type =
                        std::static_pointer_cast<ClassType>(existing_type);
                    if (!class_type->is_defined) {
                        return Result<TypePtr, TypeFactoryError>(existing_type);
                    } else {
                        return Result<TypePtr, TypeFactoryError>(
                            TypeFactoryError::TYPE_ALREADY_DEFINED);
                    }
                }
                case TypeKind::ENUM: {
                    auto enum_type =
                        std::static_pointer_cast<EnumType>(existing_type);
                    if (!enum_type->is_defined) {
                        return Result<TypePtr, TypeFactoryError>(existing_type);
                    } else {
                        return Result<TypePtr, TypeFactoryError>(
                            TypeFactoryError::TYPE_ALREADY_DEFINED);
                    }
                }
                }
            }
        }

        // Type doesn't exist in scope chain, create new one
        TypeId id = next_id++;
        types[id] = type;
        name_to_id[name] = id;
        id_to_name[id] = name;

        // Add to scope if we have one
        if (!scope_chain.empty()) {
            size_t current_scope = scope_chain.back();
            scope_defined_types[current_scope].push_back(id);
        }

        return Result<TypePtr, TypeFactoryError>(type);
    }
    TypeFactory();
    // lookup_type by type id and scope chain
    std::optional<TypePtr> lookup(TypeId id) const
    {
        auto it = types.find(id);
        if (it != types.end()) {
            return it->second;
        }
        return std::nullopt;
    }
    std::optional<TypePtr> lookup(const std::string &name) const
    {
        auto it = name_to_id.find(name);
        if (it != name_to_id.end()) {
            return lookup(it->second);
        }
        return std::nullopt;
    }
    std::optional<TypePtr>
    lookup_by_scope(const std::string &name,
                    const std::vector<ScopeID> &scope_chain) const;

    std::optional<TypePtr>
    lookup_by_scope(TypeId id, const std::vector<ScopeID> &scope_chain) const;

    Result<TypePtr, TypeFactoryError> get_pointer(QualifiedType pointee)
    {
        return make<PointerType>(std::move(pointee));
    }

    Result<TypePtr, TypeFactoryError> get_array(QualifiedType element_type,
                                                size_t size)
    {
        return make<ArrayType>(std::move(element_type), size);
    }

    Result<QualifiedType, TypeFactoryError>
    make_pointer_chain(QualifiedType base,
                       const std::vector<Qualifier> &qualifiers);

    Result<QualifiedType, TypeFactoryError>
    make_array_chain(QualifiedType base, const std::vector<size_t> &sizes);

    std::vector<std::pair<TypeId, TypePtr>> get_custom_types() const;

    void print_custom_types() const;

    QualifiedType make_qualified(TypePtr base, Qualifier qualifier)
    {
        return QualifiedType(base, qualifier);
    }

    std::optional<TypePtr> get_builtin_type(const std::string &name) const;

    Result<TypePtr, TypeFactoryError>
    make_function_type(TypePtr ret,
                       const std::vector<QualifiedType> &params,
                       bool variadic);

    Result<QualifiedType, TypeFactoryError>
    apply_pointer_levels(QualifiedType base, size_t pointer_levels);

    Result<QualifiedType, TypeFactoryError>
    apply_array_dimensions(QualifiedType base,
                           const std::vector<size_t> &sizes);

    Result<TypePtr, TypeFactoryError> pointer_from(TypePtr base);

    Result<TypePtr, TypeFactoryError> dereference_pointer(TypePtr pointer_type);
};

#endif // TYPE_FACTORY_HPP
