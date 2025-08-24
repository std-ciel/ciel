#include <FlexLexer.h>
#include <fstream>
#include <iostream>

#include <argparse/argparse.hpp>
#include "lexer_errors.hpp"
#include "print_tokens.hpp"
#include "tokens.hpp"

int main(int argc, char *argv[])
{
    argparse::ArgumentParser program("cielc", "1.0.0");

    program.add_description("Ciel language compiler");

    program.add_argument("input_file")
        .help("Input file to compile (if not provided, enters interactive mode)")
        .default_value(std::string(""))
        .nargs(argparse::nargs_pattern::optional);

    program.add_argument("-d", "--debug")
        .help("Enable debug mode to print lexer tokens")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("-l", "--lexer-only")
        .help("Run only the lexical analysis phase and print tokens")
        .default_value(false)
        .implicit_value(true);

    try {
        program.parse_args(argc, argv);
    } catch (const std::runtime_error& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    bool debug_mode = program.get<bool>("--debug");
    bool lexer_only = program.get<bool>("--lexer-only");

    yyFlexLexer lexer;
    std::ifstream file;

    std::string filename = program.get<std::string>("input_file");

    if (filename.empty()) {
        std::cout << "Ciel interactive mode. Enter your code and press Ctrl+D (EOF) when finished.\n";
        lexer.switch_streams(&std::cin, &std::cout);
    } else {
        file.open(filename);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file " << filename << std::endl;
            return 1;
        }
        lexer.switch_streams(&file, &std::cout);
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

    if (debug_mode || lexer_only) {
        print_tokens_table(tokens, std::cout);
    }

    lexer_clear_tokens();

    if (lexer_only) {
        return 0;
    }

    return 0;
}
