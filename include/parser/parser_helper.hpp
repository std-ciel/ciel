#ifndef PARSER_HELPER_HPP
#define PARSER_HELPER_HPP

#include "ast/ast_node.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include <vector>

// External declarations for global parser state
extern std::vector<ASTNodePtr> parsed_translation_unit;

// Getter functions to access globals from parser.y
std::vector<ASTNodePtr> get_parsed_translation_unit();
SymbolTable &get_symbol_table();
TypeFactory &get_type_factory();

#endif // PARSER_HELPER_HPP
