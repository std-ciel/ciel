#ifndef LEXER_ERRORS_HPP
#define LEXER_ERRORS_HPP

#include <string>
#include <vector>

struct LexError {
    int line;
    int column;
    std::string message;
};

const std::vector<LexError> &lexer_get_errors();

void lexer_clear_errors();

int lexer_had_errors();

#endif // LEXER_ERRORS_HPP
