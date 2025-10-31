#ifndef ERROR_PRINTER_HPP
#define ERROR_PRINTER_HPP

#include "lexer_errors.hpp"
#include "parser/parser_errors.hpp"
#include "passes/layout_pass_errors.hpp"
#include "passes/local_static_pass_errors.hpp"
#include "tac/tac_errors.hpp"
#include <iostream>
#include <vector>

inline void print_lexer_errors()
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

inline void print_parser_errors()
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

inline void
print_layout_pass_errors(const std::vector<LayoutPassErrorInfo> &errors)
{
    std::cerr << "\n┌─────────────────────────┐\n";
    std::cerr << "│   LAYOUT PASS ERRORS    │\n";
    std::cerr << "└─────────────────────────┘\n";
    for (const auto &err : errors) {
        std::cerr << "Error: " << err.to_string() << std::endl;
    }
}

inline void print_local_static_pass_errors(
    const std::vector<LocalStaticPassErrorInfo> &errors)
{
    std::cerr << "\n┌─────────────────────────────────┐\n";
    std::cerr << "│ LOCAL STATIC PASS ERRORS        │\n";
    std::cerr << "└─────────────────────────────────┘\n";
    for (const auto &err : errors) {
        std::cerr << "Error: " << err.to_string() << std::endl;
    }
}

inline void print_tac_generation_errors(const std::vector<TACErrorInfo> &errors)
{
    std::cerr << "\n┌─────────────────────────┐\n";
    std::cerr << "│   TAC GENERATION ERRORS │\n";
    std::cerr << "└─────────────────────────┘\n";
    for (const auto &err : errors) {
        std::cerr << "Error: " << err.to_string() << std::endl;
    }
}

#endif // ERROR_PRINTER_HPP
