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

    if (canonical->kind != TypeKind::BUILTIN) {
        return false;
    }

    auto builtin = std::static_pointer_cast<BuiltinType>(canonical);
    switch (builtin->builtin_kind) {
    case BuiltinTypeKind::CHAR:
    case BuiltinTypeKind::INT:
    case BuiltinTypeKind::UNSIGNED:
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

bool is_integral_or_enum_non_bool(TypePtr type)
{
    if (!type)
        return false;
    if (is_integral_type(type)) {
        return !is_bool_type(type); // exclude bool
    }
    return type->kind == TypeKind::ENUM;
}

bool is_function_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    return canonical && canonical->kind == TypeKind::FUNCTION;
}

bool is_array_type(TypePtr type)
{
    auto canonical = strip_typedefs(type);
    return canonical && canonical->kind == TypeKind::ARRAY;
}

bool is_user_defined_type(TypePtr type)
{
    if (!type)
        return false;
    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return false;
    return (canonical->kind == TypeKind::CLASS ||
            canonical->kind == TypeKind::RECORD ||
            canonical->kind == TypeKind::ENUM);
}

bool is_complete_type(TypePtr type)
{
    if (!type)
        return false;
    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return false;

    switch (canonical->kind) {
    case TypeKind::BUILTIN:
        return true; // Builtin types are always complete
    case TypeKind::POINTER:
        return true; // Pointers are always complete
    case TypeKind::ARRAY: {
        auto array_type = std::static_pointer_cast<ArrayType>(canonical);
        // An array is complete if its element type is complete and it has a
        // known size
        return is_complete_type(array_type->element_type.type) &&
               array_type->size != 0;
    }
    case TypeKind::FUNCTION:
        return true;
    case TypeKind::RECORD: {
        auto record_type = std::static_pointer_cast<RecordType>(canonical);
        return record_type->is_defined;
    }
    case TypeKind::ENUM: {
        auto enum_type = std::static_pointer_cast<EnumType>(canonical);
        return enum_type->is_defined;
    }
    case TypeKind::CLASS: {
        auto class_type = std::static_pointer_cast<ClassType>(canonical);
        return class_type->is_defined;
    }
    default:
        return false;
    }
}
bool has_layout(TypePtr type)
{
    if (!type)
        return false;
    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return false;
    return canonical->has_layout();
}

uint32_t get_type_size(TypePtr type)
{
    if (!type)
        return 0;
    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return 0;
    return canonical->layout.size;
}

uint32_t get_type_alignment(TypePtr type)
{
    if (!type)
        return 0;
    TypePtr canonical = strip_typedefs(type);
    if (!canonical)
        return 0;
    return canonical->layout.alignment;
}

std::optional<size_t> get_class_member_offset(const ClassType &class_type,
                                              const std::string &member_name)
{
    auto find_member_offset =
        [](auto &&find_member_offset,
           const ClassType &cls,
           const std::string &name) -> std::optional<size_t> {
        // First check this class's own members
        auto it = cls.members.find(name);
        if (it != cls.members.end()) {
            // Skip function members - they don't have offsets
            auto member_type = strip_typedefs(it->second.type.type);
            if (!member_type || member_type->kind == TypeKind::FUNCTION) {
                return std::nullopt;
            }
            return it->second.offset;
        }

        // If not found in this class, search in base class
        if (cls.base.base_type) {
            TypePtr base = strip_typedefs(cls.base.base_type);
            if (base && base->kind == TypeKind::CLASS) {
                auto base_class = std::static_pointer_cast<ClassType>(base);
                return find_member_offset(find_member_offset,
                                          *base_class,
                                          name);
            }
        }

        return std::nullopt; // Member not found
    };

    return find_member_offset(find_member_offset, class_type, member_name);
}

TypePtr get_class_member_type(const ClassType &class_type,
                              const std::string &member_name)
{
    auto find_member_type = [](auto &&find_member_type,
                               const ClassType &cls,
                               const std::string &name) -> TypePtr {
        // First check this class's own members
        auto it = cls.members.find(name);
        if (it != cls.members.end()) {
            return it->second.type.type;
        }

        // If not found in this class, search in base class
        if (cls.base.base_type) {
            TypePtr base = strip_typedefs(cls.base.base_type);
            if (base && base->kind == TypeKind::CLASS) {
                auto base_class = std::static_pointer_cast<ClassType>(base);
                return find_member_type(find_member_type, *base_class, name);
            }
        }

        return nullptr; // Member not found
    };

    return find_member_type(find_member_type, class_type, member_name);
}

std::optional<size_t> get_member_offset(const RecordType &record_type,
                                        const std::string &member_name)
{
    auto it = record_type.field_offsets.find(member_name);
    if (it != record_type.field_offsets.end()) {
        return it->second;
    }
    return std::nullopt; // Member not found
}

TypePtr get_member_type(const RecordType &record_type,
                        const std::string &member_name)
{
    auto it = record_type.fields.find(member_name);
    if (it != record_type.fields.end()) {
        return it->second.type;
    }
    return nullptr;
}
