#ifndef PARSER_HELPER_HPP
#define PARSER_HELPER_HPP

#include "ast/ast_node.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include <vector>

// External declarations for global parser state
extern std::vector<ASTNodePtr> parsed_translation_unit;
extern std::vector<std::shared_ptr<FunctionDef>> parsed_class_methods;

// Getter functions to access globals from parser.y
std::vector<ASTNodePtr> get_parsed_translation_unit();
std::vector<std::shared_ptr<FunctionDef>> get_parsed_class_methods();
SymbolTable &get_symbol_table();
TypeFactory &get_type_factory();

// Forward declaration checking
void check_global_forward_declarations();

#endif // PARSER_HELPER_HPP
