#include "symbol_table/type.hpp"

std::string type_kind_to_string(TypeKind kind)
{
    switch (kind) {
    case TypeKind::BUILTIN:
        return "Builtin";
    case TypeKind::POINTER:
        return "Pointer";
    case TypeKind::ARRAY:
        return "Array";
    case TypeKind::FUNCTION:
        return "Function";
    case TypeKind::RECORD:
        return "Record";
    case TypeKind::ENUM:
        return "Enum";
    case TypeKind::CLASS:
        return "Class";
    case TypeKind::TYPEDEF:
        return "Typedef";
    default:
        return "Unknown";
    }
}

TypePtr strip_typedefs(TypePtr type)
{
    TypePtr current = type;
    size_t depth = 0;
    while (current && current->kind == TypeKind::TYPEDEF && depth < 64) {
        auto typedef_type = std::static_pointer_cast<TypedefType>(current);
        current = typedef_type->underlying_type.type;
        ++depth;
    }
    return current;
}

bool is_pointer_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    return canonical && canonical->kind == TypeKind::POINTER;
}

bool is_integral_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    if (!canonical) {
        return false;
    }

    if (canonical->kind == TypeKind::ENUM) {
        return true;
    }

    if (canonical->kind != TypeKind::BUILTIN) {
        return false;
    }

    auto builtin = std::static_pointer_cast<BuiltinType>(canonical);
    switch (builtin->builtin_kind) {
    case BuiltinTypeKind::BOOL:
    case BuiltinTypeKind::CHAR:
    case BuiltinTypeKind::INT:
    case BuiltinTypeKind::UNSIGNED:
    case BuiltinTypeKind::SIGNED:
        return true;
    default:
        return false;
    }
}

bool is_floating_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    if (!canonical || canonical->kind != TypeKind::BUILTIN) {
        return false;
    }

    auto builtin = std::static_pointer_cast<BuiltinType>(canonical);
    return builtin->builtin_kind == BuiltinTypeKind::FLOAT;
}

bool is_arithmetic_type(TypePtr type)
{
    return is_integral_type(type) || is_floating_type(type);
}

bool is_scalar_type(TypePtr type)
{
    return is_arithmetic_type(type) || is_pointer_type(type);
}

bool is_class_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    return canonical && canonical->kind == TypeKind::CLASS;
}

bool is_bool_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    if (!canonical || canonical->kind != TypeKind::BUILTIN) {
        return false;
    }

    auto builtin = std::static_pointer_cast<BuiltinType>(canonical);
    return builtin->builtin_kind == BuiltinTypeKind::BOOL;
}

bool is_void_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    if (!canonical || canonical->kind != TypeKind::BUILTIN) {
        return false;
    }

    auto builtin = std::static_pointer_cast<BuiltinType>(canonical);
    return builtin->builtin_kind == BuiltinTypeKind::VOID;
}

bool are_types_equal(TypePtr a, TypePtr b)
{
    return a->mangled_name() == b->mangled_name();
}

bool is_integral_or_enum_non_bool(TypePtr type) {
    if (!type) return false;
    // enum is also considered integral
    if (is_integral_type(type)) {
      return !is_bool_type(type); // exclude bool
    }
    return false;
}