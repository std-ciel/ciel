#include "parser/parser_helper.hpp"
#include "ast/ast_node.hpp"
#include "parser/parser.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"

// Define the global parsed translation unit
std::vector<ASTNodePtr> parsed_translation_unit;

// Define the global parsed class methods
std::vector<std::shared_ptr<FunctionDef>> parsed_class_methods;

// Forward declare the globals from parser.y
extern SymbolTable symbol_table;
extern TypeFactory type_factory;

// Forward declare functions from parser.y
extern void check_global_forward_declarations_impl();

// Implement getter functions to access parser.y globals
std::vector<ASTNodePtr> get_parsed_translation_unit()
{
    return parsed_translation_unit;
}

std::vector<std::shared_ptr<FunctionDef>> get_parsed_class_methods()
{
    return parsed_class_methods;
}

SymbolTable &get_symbol_table()
{
    return symbol_table;
}

TypeFactory &get_type_factory()
{
    return type_factory;
}

void check_global_forward_declarations()
{
    check_global_forward_declarations_impl();
}

