#ifndef LEXER_HPP
#define LEXER_HPP

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "parser.hpp"

class Lexer : public yyFlexLexer {
  public:
    Lexer(std::istream *in = nullptr, std::ostream *out = nullptr)
        : yyFlexLexer(in, out)
    {
    }

    virtual ~Lexer() = default;
    using FlexLexer::yylex;
    virtual int yylex(yy::Parser::semantic_type *yylval,
                      yy::Parser::location_type *yylloc);
    int yylex_standalone();
};

#endif // LEXER_HPP
