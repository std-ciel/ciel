#ifndef TOKEN_CONVERTER_HPP
#define TOKEN_CONVERTER_HPP

#include "token/token_type.hpp"

namespace yy {
class Parser;
}

int to_bison_token(TokenType type);

#endif // TOKEN_CONVERTER_HPP
