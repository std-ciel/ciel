#ifndef COMMON_ERRORS_HPP
#define COMMON_ERRORS_HPP

#include <string>

// Base error structure used throughout the compiler
// Each component can extend this with specific error codes
struct CompilerError {
    std::string message;
    int line;
    int column;

    CompilerError() : line(0), column(0) {}

    CompilerError(const std::string &msg, int l = 0, int c = 0)
        : message(msg), line(l), column(c)
    {
    }
};

#endif // COMMON_ERRORS_HPP
