#ifndef PARSER_ERRORS_HPP
#define PARSER_ERRORS_HPP

#include <string>
#include <vector>

struct ParseError {
    int line;
    int column;
    std::string message;
};

const std::vector<ParseError> &parser_get_errors();

void parser_clear_errors();

int parser_had_errors();

void parser_add_error(int line, int column, const std::string &message);

#endif // PARSER_ERRORS_HPP
