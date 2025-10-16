#include "parser/parser_helper.hpp"
#include "ast/ast_node.hpp"
#include "parser/parser.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"

// Define the global variables
std::vector<ASTNodePtr> parsed_translation_unit;
SymbolTable global_symbol_table;
TypeFactory global_type_factory;

void set_parsed_translation_unit(const std::vector<ASTNodePtr> &tu)
{
    parsed_translation_unit = tu;
}

void set_symbol_table(SymbolTable *st)
{
    if (st) {
        global_symbol_table = *st;
    }
}

void set_type_factory(TypeFactory *tf)
{
    if (tf) {
        global_type_factory = *tf;
    }
}

// Implement getter functions
std::vector<ASTNodePtr> get_parsed_translation_unit()
{
    return parsed_translation_unit;
}

SymbolTable &get_symbol_table()
{
    return global_symbol_table;
}

TypeFactory &get_type_factory()
{
    return global_type_factory;
}
