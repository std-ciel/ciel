#include <argparse/argparse.hpp>
#include <fstream>
#include <iostream>

#include "lexer/lexer.hpp"
#include "lexer_errors.hpp"
#include "parser.hpp"
#include "parser/parser_errors.hpp"
#include "print_tokens.hpp"
#include "tokens.hpp"

void print_parse_results();

int main(int argc, char *argv[])
{
    argparse::ArgumentParser program("cielc", "1.0.0");

    program.add_description("Ciel language compiler");

    program.add_argument("input_file")
        .help(
            "Input file to compile (if not provided, enters interactive mode)")
        .default_value(std::string(""))
        .nargs(argparse::nargs_pattern::optional);

    program.add_argument("-d", "--debug")
        .help("Enable debug mode to print results of each compilation phase")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("-l", "--lexer-only")
        .help("Run only the lexical analysis phase and print tokens")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("-p", "--parser-only")
        .help("Run lexical analysis and parsing phases")
        .default_value(false)
        .implicit_value(true);

    try {
        program.parse_args(argc, argv);
    } catch (const std::runtime_error &err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    bool debug_mode = program.get<bool>("--debug");
    bool lexer_only = program.get<bool>("--lexer-only");
    bool parser_only = program.get<bool>("--parser-only");

    // Validate mutually exclusive options
    if (lexer_only && parser_only) {
        std::cerr
            << "Error: --lexer-only and --parser-only are mutually exclusive"
            << std::endl;
        return 1;
    }

    Lexer lexer;
    std::ifstream file;

    std::string filename = program.get<std::string>("input_file");

    if (filename.empty()) {
        std::cout << "Ciel interactive mode. Enter your code and press Ctrl+D "
                     "(EOF) when finished.\n";
        lexer.switch_streams(&std::cin, &std::cout);
    } else {
        file.open(filename);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file " << filename << std::endl;
            return 1;
        }
        lexer.switch_streams(&file, &std::cout);
    }

    bool parse_failed = false;

    if (lexer_only) {
        while (lexer.yylex_standalone() != 0) {
        }
    } else {
        yy::Parser parser(lexer);

        std::cout << "\n┌─────────────────────┐\n";
        std::cout << "│   PARSING PHASE     │\n";
        std::cout << "└─────────────────────┘\n";

        // Initialize location tracking
        yy::Parser::location_type initial_loc;
        initial_loc.begin.line = initial_loc.end.line = 1;
        initial_loc.begin.column = initial_loc.end.column = 1;

        // parser.set_debug_level(1);
        int parse_result = parser.parse();

        if (parse_result == 0) {
            std::cout << "Parsing completed successfully" << std::endl;
        } else {
            std::cout << "Parsing failed with errors" << std::endl;
            parse_failed = true;
        }
    }

    if (lexer_had_errors()) {
        auto errors = lexer_get_errors();
        std::cerr << "\n┌─────────────────────────┐\n";
        std::cerr << "│   LEXICAL ERRORS        │\n";
        std::cerr << "└─────────────────────────┘\n";
        for (const auto &e : errors) {
            std::cerr << "Lexer Error: " << e.message << " at line " << e.line;
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
        std::cout << "\n┌─────────────────────────┐\n";
        std::cout << "│   LEXICAL ANALYSIS      │\n";
        std::cout << "└─────────────────────────┘\n";
        print_tokens_table(tokens, std::cout);
        std::cout << std::endl;
    }

    if (lexer_only) {
        lexer_clear_tokens();
        return 0;
    }

    if (parser_had_errors()) {
        auto errors = parser_get_errors();
        std::cerr << "\n┌─────────────────────────┐\n";
        std::cerr << "│   PARSING ERRORS        │\n";
        std::cerr << "└─────────────────────────┘\n";
        for (const auto &e : errors) {
            std::cerr << "Parser Error: " << e.message << " at line " << e.line;
            if (e.column > 0) {
                std::cerr << ":" << e.column;
            }
            std::cerr << "\n";
        }
        parser_clear_errors();
        return 1;
    }

    if (parser_only || debug_mode || (!lexer_only && !parser_only)) {
        if (debug_mode || parser_only) {
            std::cout << "\n┌─────────────────────┐\n";
            std::cout << "│   PARSING RESULTS   │\n";
            std::cout << "└─────────────────────┘\n";
            print_parse_results();
        }
    }

    lexer_clear_tokens();
    if (parse_failed) {
        return 1;
    }
    return 0;
}
