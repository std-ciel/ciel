#include "symbol_table/type_factory.hpp"
#include "symbol_table/type.hpp"
#include <iomanip>
#include <iostream>

TypePtr TypeFactory::define_builtin(const std::string &name,
                                    BuiltinTypeKind kind)
{
    auto type = std::make_shared<BuiltinType>(kind);
    // Set layout for builtin types
    type->layout = target_layout.get_builtin_layout(kind);
    TypeId id = next_id++;
    types[id] = type;
    name_to_id[name] = id;
    id_to_name[id] = name;
    return type;
}

TypeFactory::TypeFactory() : next_id(0), target_layout()
{
    define_builtin("void", BuiltinTypeKind::VOID);
    define_builtin("bool", BuiltinTypeKind::BOOL);
    define_builtin("char", BuiltinTypeKind::CHAR);
    define_builtin("int", BuiltinTypeKind::INT);
    define_builtin("float", BuiltinTypeKind::FLOAT);
    define_builtin("unsigned", BuiltinTypeKind::UNSIGNED);
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

Result<QualifiedType, TypeFactoryError>
TypeFactory::make_pointer_chain(QualifiedType base,
                                const std::vector<Qualifier> &qualifiers)
{
    for (const auto &qual : qualifiers) {
        auto pointer_type = get_pointer(base);
        if (pointer_type.is_err()) {
            return Result<QualifiedType, TypeFactoryError>(
                pointer_type.error());
        }
        base = QualifiedType(pointer_type.value(), qual);
    }
    return Result<QualifiedType, TypeFactoryError>(base);
}

Result<QualifiedType, TypeFactoryError>
TypeFactory::make_array_chain(QualifiedType base,
                              const std::vector<size_t> &sizes)
{
    // Build array types from innermost to outermost
    // For int[3][4], sizes = [3, 4]
    // We need to build: int -> int[4] -> (int[4])[3]
    // So iterate in reverse order
    for (auto it = sizes.rbegin(); it != sizes.rend(); ++it) {
        auto size = *it;
        auto array_type = get_array(base, size);
        if (array_type.is_err()) {
            return Result<QualifiedType, TypeFactoryError>(array_type.error());
        }
        base = QualifiedType(array_type.value(), Qualifier::NONE);
    }
    return Result<QualifiedType, TypeFactoryError>(base);
}

std::vector<std::pair<TypeId, TypePtr>> TypeFactory::get_custom_types() const
{
    std::vector<std::pair<TypeId, TypePtr>> result;
    for (const auto &[id, type] : types) {
        if (type->kind == TypeKind::RECORD || type->kind == TypeKind::ENUM ||
            type->kind == TypeKind::CLASS || type->kind == TypeKind::TYPEDEF) {
            result.emplace_back(id, type);
        }
    }
    return result;
}

void TypeFactory::print_custom_types() const
{
    std::size_t w_id = std::string("Type ID").size();
    std::size_t w_name = std::string("Type Name").size();
    std::size_t w_mangled_name = std::string("Mangled Name").size();
    std::size_t w_kind = std::string("Kind").size();
    bool is_empty = true;
    for (const auto &type_pair : get_custom_types()) {
        const TypePtr &type = type_pair.second;
        w_id = std::max(w_id, std::to_string(type_pair.first).size());
        w_name = std::max(w_name, type->debug_name().size());
        w_mangled_name = std::max(w_mangled_name, type->mangled_name().size());
        w_kind = std::max(w_kind, type_kind_to_string(type->kind).size());
        is_empty = false;
    }

    auto make_sep = [&](char fill = '-') {
        std::string s;
        s.reserve(w_id + w_name + w_mangled_name + w_kind + 10);
        s.push_back('+');
        s.append(w_id + 2, fill);
        s.push_back('+');
        s.append(w_name + 2, fill);
        s.push_back('+');
        s.append(w_mangled_name + 2, fill);
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
              << ' ' << '|' << ' ' << std::left << std::setw(w_mangled_name)
              << "Mangled Name" << ' ' << '|' << ' ' << std::left
              << std::setw(w_kind) << "Kind" << ' ' << '|' << '\n'
              << sep << '\n';

    // Rows
    for (const auto &type_pair : get_custom_types()) {
        const TypePtr &type = type_pair.second;
        auto kind = type->kind;
        if (kind == TypeKind::RECORD || kind == TypeKind::ENUM ||
            kind == TypeKind::CLASS || kind == TypeKind::TYPEDEF)
            std::cout << '|' << ' ' << std::left << std::setw(w_id)
                      << type_pair.first << ' ' << '|' << ' ' << std::left
                      << std::setw(w_name) << type->debug_name() << ' ' << '|'
                      << ' ' << std::left << std::setw(w_mangled_name)
                      << type->mangled_name() << ' ' << '|' << ' ' << std::left
                      << std::setw(w_kind) << type_kind_to_string(type->kind)
                      << ' ' << '|' << '\n';
    }

    std::cout << sep << '\n';
}

void TypeFactory::print_type_layouts() const
{
    auto custom_types = get_custom_types();

    for (const auto &[id, type] : custom_types) {
        if (!type->has_layout()) {
            continue;
        }

        std::cout << type->debug_name() << " - Size: " << type->layout.size
                  << " bytes, Alignment: " << type->layout.alignment << " bytes"
                  << std::endl;

        // Print field offsets for records (structs/unions)
        if (type->kind == TypeKind::RECORD) {
            auto record = std::static_pointer_cast<RecordType>(type);
            if (!record->field_offsets.empty()) {
                std::cout << "  Fields:" << std::endl;
                for (const auto &[name, offset] : record->field_offsets) {
                    std::cout << "    " << name << " @ offset " << offset
                              << std::endl;
                }
            }
        }
        // Print member offsets for classes
        else if (type->kind == TypeKind::CLASS) {
            auto class_type = std::static_pointer_cast<ClassType>(type);

            // Recursive lambda to print members including inherited ones
            auto print_class_members = [](auto &&print_class_members,
                                          const ClassTypePtr &cls,
                                          bool is_base = false) -> void {
                // First print base class members if any
                if (cls->base.base_type) {
                    TypePtr base = strip_typedefs(cls->base.base_type);
                    if (base && base->kind == TypeKind::CLASS) {
                        print_class_members(
                            print_class_members,
                            std::static_pointer_cast<ClassType>(base),
                            true);
                    }
                }

                // Then print this class's members
                for (const auto &[name, member] : cls->members) {
                    // Skip static and function members
                    if (member.is_static) {
                        continue;
                    }
                    auto member_type = strip_typedefs(member.type.type);
                    if (member_type &&
                        member_type->kind == TypeKind::FUNCTION) {
                        continue;
                    }
                    std::cout << "    " << name << " @ offset " << member.offset
                              << (is_base ? " (inherited)" : "") << std::endl;
                }
            };

            std::cout << "  Members:" << std::endl;
            print_class_members(print_class_members, class_type);
        }
    }
}

std::optional<TypePtr>
TypeFactory::get_builtin_type(const std::string &name) const
{
    auto type_opt = lookup(name);
    if (!type_opt.has_value()) {
        return std::nullopt;
    }
    if (type_opt.value()->kind != TypeKind::BUILTIN) {
        return std::nullopt;
    }
    return type_opt;
}

Result<TypePtr, TypeFactoryError>
TypeFactory::make_function_type(TypePtr ret,
                                const std::vector<QualifiedType> &params,
                                bool variadic)
{
    if (!ret) {
        return Result<TypePtr, TypeFactoryError>(
            TypeFactoryError::INVALID_TYPE);
    }

    QualifiedType qualified_ret(ret, Qualifier::NONE);
    auto result = make<FunctionType>(qualified_ret, params, variadic);
    if (result.is_err()) {
        return Result<TypePtr, TypeFactoryError>(result.error());
    }
    return Result<TypePtr, TypeFactoryError>(result.value());
}

Result<QualifiedType, TypeFactoryError>
TypeFactory::apply_pointer_levels(QualifiedType base, size_t pointer_levels)
{
    if (pointer_levels == 0) {
        return Result<QualifiedType, TypeFactoryError>(base);
    }

    std::vector<Qualifier> qualifiers(pointer_levels, Qualifier::NONE);
    auto chain_result = make_pointer_chain(base, qualifiers);
    if (chain_result.is_err()) {
        return Result<QualifiedType, TypeFactoryError>(chain_result.error());
    }
    return Result<QualifiedType, TypeFactoryError>(chain_result.value());
}

Result<QualifiedType, TypeFactoryError>
TypeFactory::apply_array_dimensions(QualifiedType base,
                                    const std::vector<size_t> &sizes)
{
    if (sizes.empty()) {
        return Result<QualifiedType, TypeFactoryError>(base);
    }

    auto chain_result = make_array_chain(base, sizes);
    if (chain_result.is_err()) {
        return Result<QualifiedType, TypeFactoryError>(chain_result.error());
    }
    return Result<QualifiedType, TypeFactoryError>(chain_result.value());
}

Result<TypePtr, TypeFactoryError> TypeFactory::pointer_from(TypePtr base)
{
    if (!base) {
        return Result<TypePtr, TypeFactoryError>(
            TypeFactoryError::INVALID_TYPE);
    }

    QualifiedType qualified(base, Qualifier::NONE);
    auto pointer_result = apply_pointer_levels(qualified, 1);
    if (pointer_result.is_err()) {
        return Result<TypePtr, TypeFactoryError>(pointer_result.error());
    }
    return Result<TypePtr, TypeFactoryError>(pointer_result.value().type);
}

Result<TypePtr, TypeFactoryError>
TypeFactory::dereference_pointer(TypePtr pointer_type)
{
    if (!pointer_type) {
        return Result<TypePtr, TypeFactoryError>(
            TypeFactoryError::INVALID_TYPE);
    }

    if (pointer_type->kind != TypeKind::POINTER) {
        return Result<TypePtr, TypeFactoryError>(
            TypeFactoryError::INVALID_TYPE);
    }

    auto ptr_type = std::static_pointer_cast<PointerType>(pointer_type);
    return Result<TypePtr, TypeFactoryError>(ptr_type->pointee.type);
}
