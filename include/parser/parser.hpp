#ifndef PARSER_HPP
#define PARSER_HPP

#include "ast/ast_node.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include <vector>

// Forward declarations
void print_parse_results();

// Accessor functions for parsed data
std::vector<ASTNodePtr> get_parsed_translation_unit();
SymbolTable &get_symbol_table();
TypeFactory &get_type_factory();

#endif // PARSER_HPP
