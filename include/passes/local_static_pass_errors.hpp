#ifndef LOCAL_STATIC_PASS_ERRORS_HPP
#define LOCAL_STATIC_PASS_ERRORS_HPP

#include "common/errors.hpp"
#include <string>

enum class LocalStaticPassError {
    SYMBOL_REMOVAL_FAILED,
    SYMBOL_ADDITION_FAILED,
    GUARD_SYMBOL_ADDITION_FAILED,
    INVALID_SYMBOL,
};

inline const char *local_static_pass_error_to_string(LocalStaticPassError error)
{
    switch (error) {
    case LocalStaticPassError::SYMBOL_REMOVAL_FAILED:
        return "Failed to remove local static symbol from scope";
    case LocalStaticPassError::SYMBOL_ADDITION_FAILED:
        return "Failed to add global symbol to global scope";
    case LocalStaticPassError::GUARD_SYMBOL_ADDITION_FAILED:
        return "Failed to add guard symbol to global scope";
    case LocalStaticPassError::INVALID_SYMBOL:
        return "Invalid symbol encountered";
    default:
        return "Unknown local static pass error";
    }
}

struct LocalStaticPassErrorInfo {
    LocalStaticPassError error_code;
    std::string symbol_name;
    std::string additional_context;

    LocalStaticPassErrorInfo(LocalStaticPassError code,
                             const std::string &name = "",
                             const std::string &context = "")
        : error_code(code), symbol_name(name), additional_context(context)
    {
    }

    std::string to_string() const
    {
        std::string msg = local_static_pass_error_to_string(error_code);
        if (!symbol_name.empty()) {
            msg += ": " + symbol_name;
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

#endif // LOCAL_STATIC_PASS_ERRORS_HPP
