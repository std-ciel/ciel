#ifndef TARGET_LAYOUT_HPP
#define TARGET_LAYOUT_HPP

#include "symbol_table/type.hpp"
#include <cstdint>

// Target architecture layout configuration
// Currently configured for RISC-V 32-bit
struct TargetLayout {
    uint32_t pointer_size;
    uint32_t pointer_alignment;

    struct BuiltinLayout {
        uint32_t size;
        uint32_t alignment;
    };

    BuiltinLayout void_layout;
    BuiltinLayout bool_layout;
    BuiltinLayout char_layout;
    BuiltinLayout int_layout;
    BuiltinLayout unsigned_layout;
    BuiltinLayout float_layout;

    // Default constructor initializes for RISC-V 32-bit
    TargetLayout();

    TypeLayout get_builtin_layout(BuiltinTypeKind kind) const;

    static uint32_t align_to(uint32_t value, uint32_t alignment);

    static uint32_t padding_needed(uint32_t offset, uint32_t alignment);
};

#endif // TARGET_LAYOUT_HPP
