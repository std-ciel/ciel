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
