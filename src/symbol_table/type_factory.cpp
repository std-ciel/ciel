#include "symbol_table/type_factory.hpp"
#include "symbol_table/type.hpp"
#include <iomanip>
#include <iostream>

TypeFactory::TypeFactory() : next_id(0)
{
    define_builtin("void", BuiltinTypeKind::VOID);
    define_builtin("bool", BuiltinTypeKind::BOOL);
    define_builtin("char", BuiltinTypeKind::CHAR);
    define_builtin("int", BuiltinTypeKind::INT);
    define_builtin("float", BuiltinTypeKind::FLOAT);
    define_builtin("unsigned", BuiltinTypeKind::UNSIGNED);
    define_builtin("signed", BuiltinTypeKind::SIGNED);
}

std::optional<TypePtr>
TypeFactory::lookup_by_scope(const std::string &name,
                             const std::vector<size_t> &scope_chain) const
{
    auto it = name_to_id.find(name);
    if (it == name_to_id.end()) {
        return std::nullopt;
    }
    TypeId id = it->second;
    return lookup_by_scope(id, scope_chain);
}
std::optional<TypePtr>
TypeFactory::lookup_by_scope(TypeId id,
                             const std::vector<size_t> &scope_chain) const
{
    for (auto scope_id : scope_chain) {
        auto it = scope_defined_types.find(scope_id);
        if (it != scope_defined_types.end()) {
            for (auto type_id : it->second) {
                if (type_id == id) {
                    return lookup(type_id);
                }
            }
        }
    }
    return std::nullopt;
}

std::optional<QualType>
TypeFactory::make_pointer_chain(QualType base,
                                const std::vector<Qualifier> &qualifiers)
{
    for (const auto &qual : qualifiers) {
        auto pointer_type = get_pointer(base);
        if (!pointer_type.has_value()) {
            return std::nullopt; // Return failure immediately
        }
        base = QualType(pointer_type.value(), qual);
    }
    return base;
}

std::optional<QualType>
TypeFactory::make_array_chain(QualType base,
                              const std::vector<std::optional<size_t>> &sizes)
{
    for (auto size : sizes) {
        if (size.has_value() && size.value() == 0) {
            // TODO: generate error
        }
        auto array_type = get_array(base, size);
        if (!array_type.has_value()) {
            return std::nullopt; // Return failure immediately
        }
        base = QualType(array_type.value(), Qualifier::Q_NONE);
    }
    return base;
}

std::vector<std::pair<TypeId, TypePtr>> TypeFactory::get_custom_types() const
{
    std::vector<std::pair<TypeId, TypePtr>> result;
    for (const auto &[id, type] : types) {
        if (type->kind == TypeKind::Record || type->kind == TypeKind::Enum ||
            type->kind == TypeKind::Class || type->kind == TypeKind::Typedef) {
            result.emplace_back(id, type);
        }
    }
    return result;
}

void TypeFactory::print_custom_types() const
{
    std::size_t w_id = std::string("Type ID").size();
    std::size_t w_name = std::string("Type Name").size();
    std::size_t w_kind = std::string("Kind").size();
    bool is_empty = true;
    for (const auto &type_pair : get_custom_types()) {
        const TypePtr &type = type_pair.second;
        w_id = std::max(w_id, std::to_string(type_pair.first).size());
        w_name = std::max(w_name, type->debug_name().size());
        w_kind = std::max(w_kind, type_kind_to_string(type->kind).size());
        is_empty = false;
    }

    auto make_sep = [&](char fill = '-') {
        std::string s;
        s.reserve(w_id + w_name + w_kind + 16);
        s.push_back('+');
        s.append(w_id + 2, fill);
        s.push_back('+');
        s.append(w_name + 2, fill);
        s.push_back('+');
        s.append(w_kind + 2, fill);
        s.push_back('+');
        return s;
    };

    const std::string sep = make_sep();

    if (is_empty) {
        std::cout << "No custom types defined.\n";
        return;
    }
    // Header
    std::cout << sep << '\n'
              << '|' << ' ' << std::left << std::setw(w_id) << "Type ID" << ' '
              << '|' << ' ' << std::left << std::setw(w_name) << "Type Name"
              << ' ' << '|' << ' ' << std::left << std::setw(w_kind) << "Kind"
              << ' ' << '|' << '\n'
              << sep << '\n';

    // Rows
    for (const auto &type_pair : get_custom_types()) {
        const TypePtr &type = type_pair.second;
        auto kind = type->kind;
        if (kind == TypeKind::Record || kind == TypeKind::Enum ||
            kind == TypeKind::Class || kind == TypeKind::Typedef)
            std::cout << '|' << ' ' << std::left << std::setw(w_id)
                      << type_pair.first << ' ' << '|' << ' ' << std::left
                      << std::setw(w_name) << type->debug_name() << ' ' << '|'
                      << ' ' << std::left << std::setw(w_kind)
                      << type_kind_to_string(type->kind) << ' ' << '|' << '\n';
    }

    std::cout << sep << '\n';
}