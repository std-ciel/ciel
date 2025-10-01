#ifndef TYPE_FACTORY_HPP
#define TYPE_FACTORY_HPP

#include "symbol_table/type.hpp"
#include <optional>

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
    std::optional<TypePtr> make(Args &&...args)
    {
        auto type = std::make_shared<T>(std::forward<Args>(args)...);
        std::string name = type->debug_name();

        // Check if type already exists
        auto existing = name_to_id.find(name);
        if (existing != name_to_id.end()) {
            auto existing_type = types[existing->second];

            // For custom types (record, class, enum), check is_defined flag
            switch (type->kind) {
            case TypeKind::Record: {
                auto record_type =
                    std::static_pointer_cast<RecordType>(existing_type);
                if (!record_type->is_defined) {
                    return existing_type;
                } else {
                    // TODO: add proper error handling
                    return std::nullopt; // Type already defined
                }
            }
            case TypeKind::Class: {
                auto class_type =
                    std::static_pointer_cast<ClassType>(existing_type);
                if (!class_type->is_defined) {
                    return existing_type;
                } else {
                    // TODO: add proper error handling
                    return std::nullopt; // Type already defined
                }
            }
            case TypeKind::Enum: {
                auto enum_type =
                    std::static_pointer_cast<EnumType>(existing_type);
                if (!enum_type->is_defined) {
                    return existing_type;
                } else {
                    // TODO: add proper error handling
                    return std::nullopt; // Type already defined
                }
            }
            default:
                return existing_type;
            }
        }

        // Type doesn't exist, create new one
        TypeId id = next_id++;
        types[id] = type;
        name_to_id[name] = id;
        id_to_name[id] = name;
        return type;
    }

    // Overloaded version for custom types with scope chain lookup
    template <typename T, typename... Args>
    std::optional<TypePtr> make(const std::vector<size_t> &scope_chain,
                                Args &&...args)
    {
        auto type = std::make_shared<T>(std::forward<Args>(args)...);
        std::string name = type->debug_name();

        // For custom types, do scope chain lookup first
        if (type->kind == TypeKind::Record || type->kind == TypeKind::Class ||
            type->kind == TypeKind::Enum) {
            auto existing_in_scope = lookup_by_scope(name, scope_chain);
            if (existing_in_scope) {
                auto existing_type = *existing_in_scope;

                switch (type->kind) {
                case TypeKind::Record: {
                    auto record_type =
                        std::static_pointer_cast<RecordType>(existing_type);
                    if (!record_type->is_defined) {
                        return existing_type;
                    } else {
                        // TODO: add proper error handling
                        return std::nullopt; // Type already defined in scope
                    }
                }
                case TypeKind::Class: {
                    auto class_type =
                        std::static_pointer_cast<ClassType>(existing_type);
                    if (!class_type->is_defined) {
                        return existing_type;
                    } else {
                        // TODO: add proper error handling
                        return std::nullopt; // Type already defined in scope
                    }
                }
                case TypeKind::Enum: {
                    auto enum_type =
                        std::static_pointer_cast<EnumType>(existing_type);
                    if (!enum_type->is_defined) {
                        return existing_type;
                    } else {
                        // TODO: add proper error handling
                        return std::nullopt; // Type already defined in scope
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

        return type;
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
                    const std::vector<size_t> &scope_chain) const;

    std::optional<TypePtr>
    lookup_by_scope(TypeId id, const std::vector<size_t> &scope_chain) const;

    std::optional<TypePtr> get_pointer(QualType pointee)
    {
        return make<PointerType>(std::move(pointee));
    }

    std::optional<TypePtr> get_array(QualType element_type,
                                     std::optional<size_t> size)
    {
        return make<ArrayType>(std::move(element_type), size);
    }

    std::optional<QualType>
    make_pointer_chain(QualType base, const std::vector<Qualifier> &qualifiers);

    std::optional<QualType>
    make_array_chain(QualType base,
                     const std::vector<std::optional<size_t>> &sizes);

    std::vector<std::pair<TypeId, TypePtr>> get_custom_types() const;

    void print_custom_types() const;

    QualType make_qualified(TypePtr base, Qualifier qualifier)
    {
        return QualType(base, qualifier);
    }
};

#endif // TYPE_FACTORY_HPP