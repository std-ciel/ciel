#include "symbol_table/type.hpp"

std::string type_kind_to_string(TypeKind kind)
{
    switch (kind) {
    case TypeKind::Builtin:
        return "Builtin";
    case TypeKind::Pointer:
        return "Pointer";
    case TypeKind::Array:
        return "Array";
    case TypeKind::Function:
        return "Function";
    case TypeKind::Record:
        return "Record";
    case TypeKind::Enum:
        return "Enum";
    case TypeKind::Class:
        return "Class";
    case TypeKind::Typedef:
        return "Typedef";
    default:
        return "Unknown";
    }
}
