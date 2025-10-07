#include "parser/parser_errors.hpp"

static std::vector<ParseError> g_parse_errors;
static bool had_parse_errors = false;

const std::vector<ParseError> &parser_get_errors()
{
    return g_parse_errors;
}

void parser_clear_errors()
{
    g_parse_errors.clear();
    had_parse_errors = false;
}

int parser_had_errors()
{
    return had_parse_errors ? 1 : 0;
}

void parser_add_error(int line, int column, const std::string &message)
{
    g_parse_errors.push_back(ParseError{line, column, message});
    had_parse_errors = true;
}
