#include "symbol_table/target_layout.hpp"

TargetLayout::TargetLayout()
{
    // RISC-V 32-bit configuration
    pointer_size = 4;
    pointer_alignment = 4;

    // void: special case, size 1 for incomplete types
    void_layout = {1, 1};

    bool_layout = {1, 1};

    char_layout = {1, 1};

    int_layout = {4, 4};

    unsigned_layout = {4, 4};

    float_layout = {4, 4};
}

TypeLayout TargetLayout::get_builtin_layout(BuiltinTypeKind kind) const
{
    switch (kind) {
    case BuiltinTypeKind::VOID:
        return TypeLayout(void_layout.size, void_layout.alignment);
    case BuiltinTypeKind::BOOL:
        return TypeLayout(bool_layout.size, bool_layout.alignment);
    case BuiltinTypeKind::CHAR:
        return TypeLayout(char_layout.size, char_layout.alignment);
    case BuiltinTypeKind::INT:
        return TypeLayout(int_layout.size, int_layout.alignment);
    case BuiltinTypeKind::UNSIGNED:
        return TypeLayout(unsigned_layout.size, unsigned_layout.alignment);
    case BuiltinTypeKind::FLOAT:
        return TypeLayout(float_layout.size, float_layout.alignment);
    case BuiltinTypeKind::LABEL:
        // Labels don't have a runtime representation
        return TypeLayout(0, 1);
    default:
        return TypeLayout(0, 1);
    }
}

uint32_t TargetLayout::align_to(uint32_t value, uint32_t alignment)
{
    if (alignment == 0)
        return value;
    return (value + alignment - 1) & ~(alignment - 1);
}

uint32_t TargetLayout::padding_needed(uint32_t offset, uint32_t alignment)
{
    if (alignment == 0)
        return 0;
    uint32_t aligned = align_to(offset, alignment);
    return aligned - offset;
}
