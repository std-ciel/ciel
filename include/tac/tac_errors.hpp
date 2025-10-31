#ifndef TAC_ERRORS_HPP
#define TAC_ERRORS_HPP

#include "common/errors.hpp"
#include <string>

enum class TACError {
    NO_CURRENT_FUNCTION,
    UNHANDLED_EXPRESSION_TYPE,
    MEMBER_NOT_FOUND,
    MEMBER_ACCESS_ON_NON_STRUCT,
    TYPE_DETERMINATION_FAILED,
    INVALID_BASE_TYPE,
    INVALID_OPERAND,
};

inline const char *tac_error_to_string(TACError error)
{
    switch (error) {
    case TACError::NO_CURRENT_FUNCTION:
        return "No current function context";
    case TACError::UNHANDLED_EXPRESSION_TYPE:
        return "Unhandled expression type in TAC generation";
    case TACError::MEMBER_NOT_FOUND:
        return "Member not found in struct/class";
    case TACError::MEMBER_ACCESS_ON_NON_STRUCT:
        return "Member access on non-struct/class type";
    case TACError::TYPE_DETERMINATION_FAILED:
        return "Could not determine type";
    case TACError::INVALID_BASE_TYPE:
        return "Invalid base type for operation";
    case TACError::INVALID_OPERAND:
        return "Invalid operand";
    default:
        return "Unknown TAC generation error";
    }
}

struct TACErrorInfo {
    TACError error_code;
    std::string context;
    std::string additional_info;

    TACErrorInfo(TACError code,
                 const std::string &ctx = "",
                 const std::string &info = "")
        : error_code(code), context(ctx), additional_info(info)
    {
    }

    std::string to_string() const
    {
        std::string msg = tac_error_to_string(error_code);
        if (!context.empty()) {
            msg += ": " + context;
        }
        if (!additional_info.empty()) {
            msg += " (" + additional_info + ")";
        }
        return msg;
    }

    CompilerError to_compiler_error() const
    {
        return CompilerError(to_string());
    }
};

#endif // TAC_ERRORS_HPP
