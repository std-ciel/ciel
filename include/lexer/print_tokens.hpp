#ifndef PRINT_TOKENS_HPP
#define PRINT_TOKENS_HPP

#include <iosfwd>
#include <vector>

#include "tokens.hpp"

void print_tokens_table(const std::vector<Token> &tokens, std::ostream &os);

#endif // PRINT_TOKENS_HPP
