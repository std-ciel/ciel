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

// Setter functions (called by parser)
void set_parsed_translation_unit(const std::vector<ASTNodePtr> &tu);
void set_symbol_table(SymbolTable *st);
void set_type_factory(TypeFactory *tf);

#endif // PARSER_HPP
