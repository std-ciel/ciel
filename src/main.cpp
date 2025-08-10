#include <FlexLexer.h>
#include <fstream>
#include <iostream>

#include "lexer_errors.hpp"
#include "print_tokens.hpp"
#include "tokens.hpp"

int main(int argc, char *argv[])
{
    yyFlexLexer lexer;
    std::ifstream file;

    if (argc > 1) {
        file.open(argv[1]);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file " << argv[1] << std::endl;
            return 1;
        }
        lexer.switch_streams(&file, &std::cout);
    } else {
        lexer.switch_streams(&std::cin, &std::cout);
    }

    while (lexer.yylex() != 0) {
    }

    if (lexer_had_errors()) {
        auto errors = lexer_get_errors();
        for (const auto &e : errors) {
            std::cerr << "Error: " << e.message << " at line " << e.line;

            if (e.column > 0) {
                std::cerr << ":" << e.column;
            }

            std::cerr << "\n";
        }

        lexer_clear_errors();
        return 1;
    }

    const auto &tokens = lexer_get_tokens();
    print_tokens_table(tokens, std::cout);
    lexer_clear_tokens();
    return 0;
}
