#ifndef LAYOUT_PASS_ERRORS_HPP
#define LAYOUT_PASS_ERRORS_HPP

#include "common/errors.hpp"
#include <string>

enum class LayoutPassError {
    NULL_TYPE_POINTER,
    CYCLIC_DEPENDENCY,
    MISSING_LAYOUT,
    TYPEDEF_STRIP_FAILED,
    UNKNOWN_TYPE_KIND,
    BASE_TYPE_NOT_CLASS,
    INCOMPLETE_TYPE,
};

inline const char *layout_pass_error_to_string(LayoutPassError error)
{
    switch (error) {
    case LayoutPassError::NULL_TYPE_POINTER:
        return "Null type pointer encountered";
    case LayoutPassError::CYCLIC_DEPENDENCY:
        return "Cyclic dependency detected in type definition";
    case LayoutPassError::MISSING_LAYOUT:
        return "Type is missing required layout information";
    case LayoutPassError::TYPEDEF_STRIP_FAILED:
        return "Failed to strip typedef";
    case LayoutPassError::UNKNOWN_TYPE_KIND:
        return "Unknown type kind in layout computation";
    case LayoutPassError::BASE_TYPE_NOT_CLASS:
        return "Base type is not a class";
    case LayoutPassError::INCOMPLETE_TYPE:
        return "Incomplete type definition";
    default:
        return "Unknown layout pass error";
    }
}

struct LayoutPassErrorInfo {
    LayoutPassError error_code;
    std::string type_name;
    std::string additional_context;

    LayoutPassErrorInfo(LayoutPassError code,
                        const std::string &name = "",
                        const std::string &context = "")
        : error_code(code), type_name(name), additional_context(context)
    {
    }

    std::string to_string() const
    {
        std::string msg = layout_pass_error_to_string(error_code);
        if (!type_name.empty()) {
            msg += ": " + type_name;
        }
        if (!additional_context.empty()) {
            msg += " (" + additional_context + ")";
        }
        return msg;
    }

    CompilerError to_compiler_error() const
    {
        return CompilerError(to_string());
    }
};

#endif // LAYOUT_PASS_ERRORS_HPP
