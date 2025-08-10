#ifndef LEXER_TOKENS_HPP
#define LEXER_TOKENS_HPP

#include <string>
#include <vector>

#include "token_type.hpp"

struct Token {
    std::string lexeme;
    TokenType type;
    int line;
    int column;
};

const std::vector<Token> &lexer_get_tokens();

std::vector<std::pair<std::string, TokenType>> lexer_get_lexeme_token_pairs();

void lexer_clear_tokens();

#endif // LEXER_TOKENS_HPP
