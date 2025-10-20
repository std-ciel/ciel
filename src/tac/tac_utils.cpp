#include "tac/tac_utils.hpp"
#include <algorithm>

size_t calculate_type_size(TypePtr type)
{
    if (!type)
        return 0;

    if (auto builtin = std::dynamic_pointer_cast<BuiltinType>(type)) {
        switch (builtin->builtin_kind) {
        case BuiltinTypeKind::BOOL:
        case BuiltinTypeKind::CHAR:
            return 1;
        case BuiltinTypeKind::INT:
        case BuiltinTypeKind::FLOAT:
        case BuiltinTypeKind::UNSIGNED:
            return 4;
        case BuiltinTypeKind::VOID:
            return 0;
        default:
            return 4;
        }
    } else if (auto ptr = std::dynamic_pointer_cast<PointerType>(type)) {
        return 8; // 64-bit pointers
    } else if (auto record = std::dynamic_pointer_cast<RecordType>(type)) {
        // Calculate struct size considering padding and alignment
        size_t total_size = 0;
        size_t max_alignment = 1;

        for (const auto &[name, qualified_member] : record->fields) {
            size_t member_size = calculate_type_size(qualified_member.type);
            size_t member_align =
                calculate_type_alignment(qualified_member.type);
            max_alignment = std::max(max_alignment, member_align);

            // Align current position
            if (total_size % member_align != 0) {
                total_size += member_align - (total_size % member_align);
            }

            total_size += member_size;
        }

        // Final padding to make struct size a multiple of its alignment
        if (total_size % max_alignment != 0) {
            total_size += max_alignment - (total_size % max_alignment);
        }

        return total_size;
    }

    return 0;
}

size_t calculate_type_alignment(TypePtr type)
{
    if (!type)
        return 1;

    if (auto builtin = std::dynamic_pointer_cast<BuiltinType>(type)) {
        // Alignment is typically the same as size for basic types
        return calculate_type_size(type);
    } else if (auto ptr = std::dynamic_pointer_cast<PointerType>(type)) {
        return 8; // 64-bit alignment for pointers
    } else if (auto record = std::dynamic_pointer_cast<RecordType>(type)) {
        // Struct alignment is the maximum alignment of its members
        size_t max_alignment = 1;
        for (const auto &[name, qualified_member] : record->fields) {
            max_alignment =
                std::max(max_alignment,
                         calculate_type_alignment(qualified_member.type));
        }
        return max_alignment;
    }

    return 1;
}

std::optional<size_t> get_member_offset(const RecordType &record_type,
                                        const std::string &member_name)
{
    size_t offset = 0;

    for (const auto &[name, qualified_member] : record_type.fields) {
        // Calculate alignment padding
        size_t member_align = calculate_type_alignment(qualified_member.type);
        if (offset % member_align != 0) {
            offset += member_align - (offset % member_align);
        }

        // Found the member
        if (name == member_name) {
            return offset;
        }

        // Move to next member
        offset += calculate_type_size(qualified_member.type);
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
