#include "passes/layout_pass.hpp"
#include "symbol_table/target_layout.hpp"
#include <algorithm>

Result<bool, std::vector<LayoutPassErrorInfo>> LayoutPass::run()
{
    auto custom_types = type_factory.get_custom_types();

    for (const auto &[id, type] : custom_types) {
        auto result = compute_type_layout(type);
        if (result.is_err()) {
            errors.push_back(result.error());
        }
    }

    if (!errors.empty()) {
        return std::vector<LayoutPassErrorInfo>(errors);
    }

    return true;
}

Result<bool, LayoutPassErrorInfo> LayoutPass::compute_type_layout(TypePtr type)
{
    if (!type) {
        return LayoutPassErrorInfo(LayoutPassError::NULL_TYPE_POINTER);
    }

    if (type->has_layout()) {
        return true;
    }

    // FAILSAFE: Check for cycles in type dependencies
    // NOTE: This should NEVER be triggered in valid ciel code, as direct cyclic
    // struct/class member dependencies are forbidden by the language (e.g.,
    // "struct A { struct A a; }" is invalid). Recursive types can only exist
    // through pointers, which have fixed size and don't require recursing into
    // the pointee type. If this error occurs, there is a bug in the parser's
    // semantic checks that allowed an invalid type definition.
    if (visited.count(type)) {
        return LayoutPassErrorInfo(
            LayoutPassError::CYCLIC_DEPENDENCY,
            type->debug_name(),
            "This indicates a bug in the parser - invalid recursive "
            "struct/class definition was not rejected");
    }

    visited.insert(type);
    auto result = compute_layout(type);
    visited.erase(type);

    return result;
}

Result<bool, LayoutPassErrorInfo> LayoutPass::compute_layout(TypePtr type)
{
    switch (type->kind) {
    case TypeKind::BUILTIN:
    case TypeKind::POINTER:
        // These should already have layout from TypeFactory
        if (!type->has_layout()) {
            return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                       type->debug_name(),
                                       "Builtin or pointer type");
        }
        return true;

    case TypeKind::ARRAY:
        return compute_array_layout(std::static_pointer_cast<ArrayType>(type));

    case TypeKind::RECORD:
        return compute_record_layout(
            std::static_pointer_cast<RecordType>(type));

    case TypeKind::ENUM:
        return compute_enum_layout(std::static_pointer_cast<EnumType>(type));

    case TypeKind::CLASS:
        return compute_class_layout(std::static_pointer_cast<ClassType>(type));

    case TypeKind::TYPEDEF: {
        // For typedef, use the underlying type's layout
        auto typedef_type = std::static_pointer_cast<TypedefType>(type);
        TypePtr underlying = strip_typedefs(type);
        if (!underlying) {
            return LayoutPassErrorInfo(LayoutPassError::TYPEDEF_STRIP_FAILED,
                                       type->debug_name());
        }
        auto result = compute_type_layout(underlying);
        if (result.is_err()) {
            return result.error();
        }
        type->layout = underlying->layout;
        return true;
    }

    case TypeKind::FUNCTION:
        // Functions don't have a layout
        return true;

    default:
        return LayoutPassErrorInfo(LayoutPassError::UNKNOWN_TYPE_KIND,
                                   type->debug_name());
    }
}

Result<bool, LayoutPassErrorInfo>
LayoutPass::compute_array_layout(ArrayTypePtr array)
{
    if (!array) {
        return LayoutPassErrorInfo(LayoutPassError::NULL_TYPE_POINTER, "array");
    }

    // Compute layout of element type first
    TypePtr element = strip_typedefs(array->element_type.type);
    if (!element) {
        return LayoutPassErrorInfo(LayoutPassError::TYPEDEF_STRIP_FAILED,
                                   "array element type");
    }

    auto result = compute_type_layout(element);
    if (result.is_err()) {
        return result.error();
    }

    if (!element->has_layout()) {
        return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                   element->debug_name(),
                                   "array element type");
    }

    // Array layout: size = element_size * array_size, alignment =
    // element_alignment
    uint32_t element_size = element->layout.size;
    uint32_t element_alignment = element->layout.alignment;

    if (array->size == 0) {
        // Incomplete array type
        array->layout = TypeLayout(0, element_alignment);
    } else {
        array->layout =
            TypeLayout(element_size * array->size, element_alignment);
    }

    return true;
}

Result<bool, LayoutPassErrorInfo>
LayoutPass::compute_record_layout(RecordTypePtr record)
{
    if (!record) {
        return LayoutPassErrorInfo(LayoutPassError::NULL_TYPE_POINTER,
                                   "record");
    }

    // If not defined, we can't compute layout
    if (!record->is_defined) {
        return true; // Not an error, just incomplete
    }

    if (record->is_union) {
        // For unions: size = max(field sizes), alignment = max(field
        // alignments)
        uint32_t max_size = 0;
        uint32_t max_alignment = 1;

        for (const auto &[name, qualified_type] : record->fields) {
            TypePtr field_type = strip_typedefs(qualified_type.type);
            if (!field_type) {
                return LayoutPassErrorInfo(
                    LayoutPassError::TYPEDEF_STRIP_FAILED,
                    "union field: " + name);
            }

            auto result = compute_type_layout(field_type);
            if (result.is_err()) {
                return result.error();
            }

            if (!field_type->has_layout()) {
                return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                           field_type->debug_name(),
                                           "union field");
            }

            max_size = std::max(max_size, field_type->layout.size);
            max_alignment =
                std::max(max_alignment, field_type->layout.alignment);

            // All fields in a union start at offset 0
            record->set_field_offset(name, 0);
        }

        // Handle empty unions: a union with no fields should have minimum size
        // of 1 byte
        if (max_size == 0) {
            // Empty union - give it size 1, alignment 1
            record->layout = TypeLayout(1, 1);
        } else {
            uint32_t aligned_size =
                TargetLayout::align_to(max_size, max_alignment);
            record->layout = TypeLayout(aligned_size, max_alignment);
        }

    } else {
        // For structs: layout fields sequentially with proper alignment
        uint32_t current_offset = 0;
        uint32_t max_alignment = 1;

        for (const auto &[name, qualified_type] : record->fields) {
            TypePtr field_type = strip_typedefs(qualified_type.type);
            if (!field_type) {
                return LayoutPassErrorInfo(
                    LayoutPassError::TYPEDEF_STRIP_FAILED,
                    "struct field: " + name);
            }

            auto result = compute_type_layout(field_type);
            if (result.is_err()) {
                return result.error();
            }

            if (!field_type->has_layout()) {
                return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                           field_type->debug_name(),
                                           "struct field");
            }

            uint32_t field_alignment = field_type->layout.alignment;
            uint32_t field_size = field_type->layout.size;

            // Align current offset to field's alignment
            current_offset =
                TargetLayout::align_to(current_offset, field_alignment);

            // Set field offset
            record->set_field_offset(name, current_offset);

            // Update offset for next field
            current_offset += field_size;

            // Track maximum alignment
            max_alignment = std::max(max_alignment, field_alignment);
        }

        // Handle empty structs: a struct with no fields should have minimum
        // size of 1 byte
        if (current_offset == 0) {
            // Empty struct - give it size 1, alignment 1
            record->layout = TypeLayout(1, 1);
        } else {
            // Align the total size to the struct's alignment
            uint32_t aligned_size =
                TargetLayout::align_to(current_offset, max_alignment);
            record->layout = TypeLayout(aligned_size, max_alignment);
        }
    }

    return true;
}

Result<bool, LayoutPassErrorInfo>
LayoutPass::compute_class_layout(ClassTypePtr class_type)
{
    if (!class_type) {
        return LayoutPassErrorInfo(LayoutPassError::NULL_TYPE_POINTER, "class");
    }

    // If not defined, we can't compute layout
    if (!class_type->is_defined) {
        return true; // Not an error, just incomplete
    }

    uint32_t current_offset = 0;
    uint32_t max_alignment = 1;

    // First, layout the base class if present
    if (class_type->base.base_type) {
        TypePtr base = strip_typedefs(class_type->base.base_type);
        if (!base) {
            return LayoutPassErrorInfo(LayoutPassError::TYPEDEF_STRIP_FAILED,
                                       "base class");
        }

        // Check if base is actually a class
        if (base->kind != TypeKind::CLASS) {
            return LayoutPassErrorInfo(LayoutPassError::BASE_TYPE_NOT_CLASS,
                                       base->debug_name());
        }

        // Compute base class layout if not already done
        auto result = compute_type_layout(base);
        if (result.is_err()) {
            return result.error();
        }

        if (!base->has_layout()) {
            return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                       base->debug_name(),
                                       "base class");
        }

        // Base class occupies the initial portion of the object
        current_offset = base->layout.size;
        max_alignment = base->layout.alignment;

        // If the base is a class, we need to account for its members' offsets
        // They're already computed in the base class
    }

    // Now layout the derived class members
    for (const auto &[name, member_info] : class_type->members) {
        // Skip static members - they don't contribute to object layout
        if (member_info.is_static) {
            continue;
        }

        TypePtr member_type = strip_typedefs(member_info.type.type);
        if (!member_type) {
            return LayoutPassErrorInfo(LayoutPassError::TYPEDEF_STRIP_FAILED,
                                       "class member: " + name);
        }

        // Skip function types (methods) - they don't contribute to object
        // layout unless we implement vtables later
        if (member_type->kind == TypeKind::FUNCTION) {
            continue;
        }

        auto result = compute_type_layout(member_type);
        if (result.is_err()) {
            return result.error();
        }

        if (!member_type->has_layout()) {
            return LayoutPassErrorInfo(LayoutPassError::MISSING_LAYOUT,
                                       member_type->debug_name(),
                                       "class member");
        }

        uint32_t member_alignment = member_type->layout.alignment;
        uint32_t member_size = member_type->layout.size;

        // Align current offset to member's alignment
        current_offset =
            TargetLayout::align_to(current_offset, member_alignment);

        // Update member info with offset
        // We need to update the member in the map
        auto &mutable_member = const_cast<MemberInfo &>(member_info);
        mutable_member.offset = current_offset;

        // Update offset for next member
        current_offset += member_size;

        // Track maximum alignment
        max_alignment = std::max(max_alignment, member_alignment);
    }

    // Handle empty classes: a class with no non-static data members
    // should have a minimum size of 1 byte (like in C++)
    // However, if this class is used as a base, the derived class can
    // start at offset 0 (Empty Base Optimization)
    if (current_offset == 0) {
        // Empty class - give it size 1, alignment 1
        class_type->layout = TypeLayout(1, 1);
    } else {
        // Normal case: align the total size to the class's alignment
        uint32_t aligned_size =
            TargetLayout::align_to(current_offset, max_alignment);
        class_type->layout = TypeLayout(aligned_size, max_alignment);
    }

    return true;
}

Result<bool, LayoutPassErrorInfo>
LayoutPass::compute_enum_layout(EnumTypePtr enum_type)
{
    if (!enum_type) {
        return LayoutPassErrorInfo(LayoutPassError::NULL_TYPE_POINTER, "enum");
    }

    // Enums are represented as integers
    const TargetLayout &target = type_factory.get_target_layout();
    enum_type->layout = target.get_builtin_layout(BuiltinTypeKind::INT);

    return true;
}
