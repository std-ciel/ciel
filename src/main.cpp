#include <argparse/argparse.hpp>
#include <csignal>
#include <cstdlib>
#include <fstream>
#include <iostream>

#include "layout_pass/layout_pass.hpp"
#include "lexer/lexer.hpp"
#include "lexer_errors.hpp"
#include "local_static_pass/local_static_pass.hpp"
#include "parser.hpp"
#include "parser/parser.hpp"
#include "parser/parser_errors.hpp"
#include "parser/parser_helper.hpp"
#include "print_tokens.hpp"
#include "tac/tac_generator.hpp"
#include "tokens.hpp"

void print_parse_results();

void print_lexer_errors()
{
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
    }
}

void print_parser_errors()
{
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
    }
}

void sigsegv_handler(int signum)
{
    std::cerr << "\n┌─────────────────────────────────────┐\n";
    std::cerr << "│   FATAL ERROR: SEGMENTATION FAULT   │\n";
    std::cerr << "└─────────────────────────────────────┘\n";

    // Print lexer and parser errors if any
    print_lexer_errors();
    print_parser_errors();

    std::exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    // Register signal handler for segmentation faults
    std::signal(SIGSEGV, sigsegv_handler);

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

    program.add_argument("-i", "--irgen-only")
        .help("Run all phases up to and including IR generation (TAC)")
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
    bool irgen_only = program.get<bool>("--irgen-only");

    // Validate mutually exclusive options
    int exclusive_count =
        (lexer_only ? 1 : 0) + (parser_only ? 1 : 0) + (irgen_only ? 1 : 0);
    if (exclusive_count > 1) {
        std::cerr << "Error: --lexer-only, --parser-only, and --irgen-only are "
                     "mutually exclusive"
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

    // Lexer-only mode: just tokenize
    if (lexer_only) {
        while (lexer.yylex_standalone() != 0) {
        }

        if (lexer_had_errors()) {
            print_lexer_errors();
            lexer_clear_errors();
            return 1;
        }

        const auto &tokens = lexer_get_tokens();
        std::cout << "\n┌─────────────────────────┐\n";
        std::cout << "│   LEXICAL ANALYSIS      │\n";
        std::cout << "└─────────────────────────┘\n";
        print_tokens_table(tokens, std::cout);
        std::cout << std::endl;

        lexer_clear_tokens();
        return 0;
    }

    // Parse the input
    yy::Parser parser(lexer);

    std::cout << "\n┌─────────────────────┐\n";
    std::cout << "│   PARSING PHASE     │\n";
    std::cout << "└─────────────────────┘\n";

    parser.set_debug_level(debug_mode ? 1 : 0);
    int parse_result = parser.parse();

    // Check global forward declarations after parsing completes
    if (parse_result == 0) {
        check_global_forward_declarations();
    }

    // Check for errors
    if (lexer_had_errors()) {
        print_lexer_errors();
        lexer_clear_errors();
        return 1;
    }

    if (parser_had_errors()) {
        print_parser_errors();
        parser_clear_errors();
        return 1;
    }

    if (parse_result != 0) {
        std::cout << "Parsing failed with errors" << std::endl;
        return 1;
    }

    std::cout << "Parsing completed successfully" << std::endl;

    // Debug: print tokens
    if (debug_mode) {
        const auto &tokens = lexer_get_tokens();
        std::cout << "\n┌─────────────────────────┐\n";
        std::cout << "│   LEXICAL ANALYSIS      │\n";
        std::cout << "└─────────────────────────┘\n";
        print_tokens_table(tokens, std::cout);
        std::cout << std::endl;
    }

    // Debug: print parse results
    if (debug_mode) {
        std::cout << "\n┌─────────────────────┐\n";
        std::cout << "│   PARSING RESULTS   │\n";
        std::cout << "└─────────────────────┘\n";
        print_parse_results();
    }

    // Parser-only mode: stop here
    if (parser_only) {
        lexer_clear_tokens();
        return 0;
    }

    // Apply local static pass
    try {
        std::cout << "\n┌─────────────────────────────┐\n";
        std::cout << "│ LOCAL STATIC TRANSFORMATION │\n";
        std::cout << "└─────────────────────────────┘\n";

        auto translation_unit = get_parsed_translation_unit();
        auto &symbol_table = get_symbol_table();
        auto &type_factory = get_type_factory();

        LocalStaticPass local_static_pass(symbol_table, type_factory);
        local_static_pass.process(translation_unit);

        std::cout << "Local static pass completed successfully" << std::endl;

        if (debug_mode) {
            get_symbol_table().print_symbols();
        }
    } catch (const std::exception &e) {
        std::cerr << "\nLocal static pass error: " << e.what() << std::endl;
        lexer_clear_tokens();
        return 1;
    }

    try {
        std::cout << "\n┌─────────────────────┐\n";
        std::cout << "│   LAYOUT PASS       │\n";
        std::cout << "└─────────────────────┘\n";

        auto &type_factory = get_type_factory();
        LayoutPass layout_pass(type_factory);

        if (!layout_pass.run()) {
            std::cerr << "Layout pass failed" << std::endl;
            return 1;
        }

        std::cout << "Layout pass completed successfully" << std::endl;

        if (debug_mode) {
            std::cout << "\n┌─────────────────────┐\n";
            std::cout << "│   TYPE LAYOUTS      │\n";
            std::cout << "└─────────────────────┘\n";
            type_factory.print_type_layouts();
        }
    } catch (const std::exception &e) {
        std::cerr << "\nLayout pass error: " << e.what() << std::endl;
        lexer_clear_tokens();
        return 1;
    }

    // Generate TAC
    try {
        std::cout << "\n┌─────────────────────┐\n";
        std::cout << "│ TAC GENERATION      │\n";
        std::cout << "└─────────────────────┘\n";

        auto translation_unit = get_parsed_translation_unit();

        TACGenerator tac_gen;
        tac_gen.generate(translation_unit);

        std::cout << "TAC generation completed successfully\n\n";
        tac_gen.get_program().print();
    } catch (const std::exception &e) {
        std::cerr << "\nTAC generation error: " << e.what() << std::endl;
        lexer_clear_tokens();
        return 1;
    }

    // IR generation-only mode: stop here
    if (irgen_only) {
        lexer_clear_tokens();
        return 0;
    }

    lexer_clear_tokens();
    return 0;
}
