#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type.hpp"
#include "symbol_table/type_factory.hpp"
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

class SymbolTableTest : public ::testing::Test {
  protected:
    void SetUp() override
    {
        symbol_table = std::make_unique<SymbolTable>();
        type_factory = std::make_unique<TypeFactory>();
    }

    void TearDown() override
    {
        symbol_table.reset();
        type_factory.reset();
    }

    std::unique_ptr<SymbolTable> symbol_table;
    std::unique_ptr<TypeFactory> type_factory;
};

// Test initial state
TEST_F(SymbolTableTest, InitialState)
{
    // Constructor should create an initial scope
    EXPECT_EQ(symbol_table->get_current_scope_id(), 0);
    EXPECT_EQ(symbol_table->get_current_scope_level(), 1);

    auto scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 1);
    EXPECT_EQ(scope_chain[0], 0);
}

// Test scope management - entering and exiting scopes
TEST_F(SymbolTableTest, ScopeManagement)
{
    // Initial state
    ScopeID initial_scope = symbol_table->get_current_scope_id();
    size_t initial_level = symbol_table->get_current_scope_level();

    // Enter a new scope
    symbol_table->enter_scope();
    EXPECT_GT(symbol_table->get_current_scope_id(), initial_scope);
    EXPECT_EQ(symbol_table->get_current_scope_level(), initial_level + 1);

    auto scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 2);
    EXPECT_EQ(scope_chain[0], initial_scope); // parent scope
    EXPECT_EQ(scope_chain[1],
              symbol_table->get_current_scope_id()); // current scope

    // Enter another scope
    ScopeID second_scope = symbol_table->get_current_scope_id();
    symbol_table->enter_scope();
    EXPECT_GT(symbol_table->get_current_scope_id(), second_scope);
    EXPECT_EQ(symbol_table->get_current_scope_level(), initial_level + 2);

    scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 3);

    // Exit one scope
    symbol_table->exit_scope();
    EXPECT_EQ(symbol_table->get_current_scope_id(), second_scope);
    EXPECT_EQ(symbol_table->get_current_scope_level(), initial_level + 1);

    scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 2);

    // Exit back to initial scope
    symbol_table->exit_scope();
    EXPECT_EQ(symbol_table->get_current_scope_id(), initial_scope);
    EXPECT_EQ(symbol_table->get_current_scope_level(), initial_level);

    scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 1);
}

// Test exiting scope when at root scope
TEST_F(SymbolTableTest, ExitScopeAtRoot)
{
    // Should not exit the root scope
    ScopeID initial_scope = symbol_table->get_current_scope_id();
    size_t initial_level = symbol_table->get_current_scope_level();

    symbol_table->exit_scope();

    // Should remain the same
    EXPECT_EQ(symbol_table->get_current_scope_id(), initial_scope);
    EXPECT_EQ(symbol_table->get_current_scope_level(), initial_level);

    auto scope_chain = symbol_table->get_scope_chain();
    EXPECT_EQ(scope_chain.size(), 1);
}

// Test symbol addition with QualifiedType
TEST_F(SymbolTableTest, AddSymbolWithQualifiedType)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Create a QualifiedType from the TypePtr
    QualifiedType int_qual(int_type.value(), Qualifier::NONE);

    // Add a symbol
    auto result = symbol_table->add_symbol("x", int_qual);
    EXPECT_TRUE(result.is_ok());

    // Try to add the same symbol again (should fail)
    result = symbol_table->add_symbol("x", int_qual);
    EXPECT_FALSE(result.is_ok());

    // Add a different symbol
    result = symbol_table->add_symbol("y", int_qual);
    EXPECT_TRUE(result.is_ok());
}

// Test symbol addition with null type (empty TypePtr)
TEST_F(SymbolTableTest, AddSymbolWithInvalidType)
{
    // Create an empty QualifiedType with nullptr TypePtr
    TypePtr null_type = nullptr;
    QualifiedType invalid_qual(null_type, Qualifier::NONE);
    auto result = symbol_table->add_symbol("invalid", invalid_qual);
    ASSERT_FALSE(result.is_ok());
}

// Test symbol lookup
TEST_F(SymbolTableTest, SymbolLookup)
{
    auto int_type = type_factory->lookup("int");
    auto float_type = type_factory->lookup("float");
    ASSERT_TRUE(int_type.has_value());
    ASSERT_TRUE(float_type.has_value());

    // Create QualifiedTypes
    QualifiedType int_qual(int_type.value(), Qualifier::NONE);
    QualifiedType float_qual(float_type.value(), Qualifier::NONE);

    // Add symbols
    symbol_table->add_symbol("x", int_qual);
    symbol_table->add_symbol("y", float_qual);

    // Look them up
    auto x_symbol = symbol_table->lookup_symbol("x");
    auto y_symbol = symbol_table->lookup_symbol("y");

    ASSERT_TRUE(x_symbol.has_value());
    ASSERT_TRUE(y_symbol.has_value());

    EXPECT_EQ(x_symbol.value()->get_name(), "x");
    EXPECT_EQ(y_symbol.value()->get_name(), "y");

    // Check their types
    EXPECT_EQ(x_symbol.value()->get_type().type, int_type.value());
    EXPECT_EQ(y_symbol.value()->get_type().type, float_type.value());
}

// Test lookup of non-existent symbol
TEST_F(SymbolTableTest, LookupNonExistentSymbol)
{
    auto result = symbol_table->lookup_symbol("non_existent");
    EXPECT_FALSE(result.has_value());
}

// Test symbol lookup across scopes
TEST_F(SymbolTableTest, SymbolLookupAcrossScopes)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Add symbol in root scope
    symbol_table->add_symbol(
        "global_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));

    // Enter new scope
    symbol_table->enter_scope();

    // Add symbol in nested scope
    symbol_table->add_symbol(
        "local_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));

    // Should be able to find both symbols
    auto global_symbol = symbol_table->lookup_symbol("global_var");
    auto local_symbol = symbol_table->lookup_symbol("local_var");

    ASSERT_TRUE(global_symbol.has_value());
    ASSERT_TRUE(local_symbol.has_value());

    EXPECT_EQ(global_symbol.value()->get_name(), "global_var");
    EXPECT_EQ(local_symbol.value()->get_name(), "local_var");

    // Exit scope
    symbol_table->exit_scope();

    // Should still find global symbol
    global_symbol = symbol_table->lookup_symbol("global_var");
    ASSERT_TRUE(global_symbol.has_value());

    // Should not find local symbol
    local_symbol = symbol_table->lookup_symbol("local_var");
    EXPECT_FALSE(local_symbol.has_value());
}

// Test symbol shadowing
TEST_F(SymbolTableTest, SymbolShadowing)
{
    auto int_type = type_factory->lookup("int");
    auto float_type = type_factory->lookup("float");
    ASSERT_TRUE(int_type.has_value());
    ASSERT_TRUE(float_type.has_value());

    // Add symbol in outer scope
    symbol_table->add_symbol(
        "var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));

    // Enter new scope
    symbol_table->enter_scope();

    // Add symbol with same name but different type
    symbol_table->add_symbol(
        "var",
        type_factory->make_qualified(float_type.value(), Qualifier::NONE));

    // Should find the inner scope symbol (float)
    auto symbol = symbol_table->lookup_symbol("var");
    ASSERT_TRUE(symbol.has_value());
    EXPECT_EQ(symbol.value()->get_type().type, float_type.value());

    // Exit scope
    symbol_table->exit_scope();

    // Should find the outer scope symbol (int)
    symbol = symbol_table->lookup_symbol("var");
    ASSERT_TRUE(symbol.has_value());
    EXPECT_EQ(symbol.value()->get_type().type, int_type.value());
}

// Test Symbol class functionality
TEST_F(SymbolTableTest, SymbolProperties)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    symbol_table->add_symbol(
        "test_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));

    auto symbol = symbol_table->lookup_symbol("test_var");
    ASSERT_TRUE(symbol.has_value());

    // Test symbol properties
    EXPECT_EQ(symbol.value()->get_name(), "test_var");
    EXPECT_EQ(symbol.value()->get_type().type, int_type.value());
    EXPECT_EQ(symbol.value()->get_scope_id(),
              symbol_table->get_current_scope_id());
    // Global symbols default to STATIC storage class
    EXPECT_EQ(symbol.value()->get_storage_class(), StorageClass::STATIC);
}

// Test multiple symbols in different scopes
TEST_F(SymbolTableTest, MultipleSymbolsMultipleScopes)
{
    auto int_type = type_factory->lookup("int");
    auto float_type = type_factory->lookup("float");
    auto char_type = type_factory->lookup("char");
    ASSERT_TRUE(int_type.has_value());
    ASSERT_TRUE(float_type.has_value());
    ASSERT_TRUE(char_type.has_value());

    // Root scope symbols
    symbol_table->add_symbol(
        "a",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    symbol_table->add_symbol(
        "b",
        type_factory->make_qualified(float_type.value(), Qualifier::NONE));

    // Enter scope 1
    symbol_table->enter_scope();
    symbol_table->add_symbol(
        "c",
        type_factory->make_qualified(char_type.value(), Qualifier::NONE));
    symbol_table->add_symbol(
        "d",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));

    // Enter scope 2
    symbol_table->enter_scope();
    symbol_table->add_symbol(
        "e",
        type_factory->make_qualified(float_type.value(), Qualifier::NONE));

    // All symbols should be accessible
    EXPECT_TRUE(symbol_table->lookup_symbol("a").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("b").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("c").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("d").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("e").has_value());

    // Exit to scope 1
    symbol_table->exit_scope();
    EXPECT_TRUE(symbol_table->lookup_symbol("a").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("b").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("c").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("d").has_value());
    EXPECT_FALSE(
        symbol_table->lookup_symbol("e").has_value()); // not accessible

    // Exit to root scope
    symbol_table->exit_scope();
    EXPECT_TRUE(symbol_table->lookup_symbol("a").has_value());
    EXPECT_TRUE(symbol_table->lookup_symbol("b").has_value());
    EXPECT_FALSE(
        symbol_table->lookup_symbol("c").has_value()); // not accessible
    EXPECT_FALSE(
        symbol_table->lookup_symbol("d").has_value()); // not accessible
    EXPECT_FALSE(
        symbol_table->lookup_symbol("e").has_value()); // not accessible
}

// Test complex type symbols (pointers, arrays)
TEST_F(SymbolTableTest, ComplexTypeSymbols)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Create pointer type
    QualifiedType int_qual(int_type.value(), Qualifier::NONE);
    auto int_ptr = type_factory->get_pointer(int_qual);

    // Create array type
    auto int_array = type_factory->get_array(int_qual, 10);

    // Add symbols with complex types
    ASSERT_TRUE(int_ptr.is_ok());
    ASSERT_TRUE(int_array.is_ok());
    symbol_table->add_symbol(
        "ptr_var",
        type_factory->make_qualified(*int_ptr, Qualifier::NONE));
    symbol_table->add_symbol(
        "array_var",
        type_factory->make_qualified(*int_array, Qualifier::NONE));

    // Look them up
    auto ptr_symbol = symbol_table->lookup_symbol("ptr_var");
    auto array_symbol = symbol_table->lookup_symbol("array_var");

    ASSERT_TRUE(ptr_symbol.has_value());
    ASSERT_TRUE(array_symbol.has_value());

    EXPECT_EQ(ptr_symbol.value()->get_type().type->kind, TypeKind::POINTER);
    EXPECT_EQ(array_symbol.value()->get_type().type->kind, TypeKind::ARRAY);
}

// Test edge cases with empty names
TEST_F(SymbolTableTest, EdgeCasesEmptyNames)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Try to add symbol with empty name
    auto result = symbol_table->add_symbol(
        "",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    // This might succeed or fail depending on implementation
    // The test documents the behavior

    if (result) {
        auto symbol = symbol_table->lookup_symbol("");
        EXPECT_TRUE(symbol.has_value());
    }
}

// Test scope chain consistency
TEST_F(SymbolTableTest, ScopeChainConsistency)
{
    auto initial_chain = symbol_table->get_scope_chain();

    // Enter several scopes
    symbol_table->enter_scope();
    auto chain1 = symbol_table->get_scope_chain();

    symbol_table->enter_scope();
    auto chain2 = symbol_table->get_scope_chain();

    symbol_table->enter_scope();
    auto chain3 = symbol_table->get_scope_chain();

    // Verify chain consistency
    EXPECT_EQ(chain1.size(), initial_chain.size() + 1);
    EXPECT_EQ(chain2.size(), chain1.size() + 1);
    EXPECT_EQ(chain3.size(), chain2.size() + 1);

    // Check that each chain contains all previous scopes
    for (size_t i = 0; i < initial_chain.size(); ++i) {
        EXPECT_EQ(chain1[i], initial_chain[i]);
        EXPECT_EQ(chain2[i], initial_chain[i]);
        EXPECT_EQ(chain3[i], initial_chain[i]);
    }

    // Exit scopes and verify
    symbol_table->exit_scope();
    auto exit_chain1 = symbol_table->get_scope_chain();
    EXPECT_EQ(exit_chain1, chain2);

    symbol_table->exit_scope();
    auto exit_chain2 = symbol_table->get_scope_chain();
    EXPECT_EQ(exit_chain2, chain1);

    symbol_table->exit_scope();
    auto exit_chain3 = symbol_table->get_scope_chain();
    EXPECT_EQ(exit_chain3, initial_chain);
}

// Test storage class functionality
TEST_F(SymbolTableTest, StorageClassFunctionality)
{
    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Test global scope: AUTO should become STATIC
    symbol_table->add_symbol(
        "global_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    auto global_symbol = symbol_table->lookup_symbol("global_var");
    ASSERT_TRUE(global_symbol.has_value());
    // Global symbols with AUTO storage class default to STATIC
    EXPECT_EQ(global_symbol.value()->get_storage_class(), StorageClass::STATIC);

    // Test local scope: AUTO should remain AUTO
    symbol_table->enter_scope();
    symbol_table->add_symbol(
        "local_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    auto local_symbol = symbol_table->lookup_symbol("local_var");
    ASSERT_TRUE(local_symbol.has_value());
    // Local symbols should keep AUTO storage class
    EXPECT_EQ(local_symbol.value()->get_storage_class(), StorageClass::AUTO);
    symbol_table->exit_scope();
}

// Test parent scope relationships
TEST_F(SymbolTableTest, ParentScopeRelationships)
{
    ScopeID root_scope = symbol_table->get_current_scope_id();

    auto int_type = type_factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Add symbol to root
    symbol_table->add_symbol(
        "root_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    auto root_symbol = symbol_table->lookup_symbol("root_var");
    ASSERT_TRUE(root_symbol.has_value());

    // Enter child scope
    symbol_table->enter_scope();
    ScopeID child_scope = symbol_table->get_current_scope_id();

    symbol_table->add_symbol(
        "child_var",
        type_factory->make_qualified(int_type.value(), Qualifier::NONE));
    auto child_symbol = symbol_table->lookup_symbol("child_var");
    ASSERT_TRUE(child_symbol.has_value());

    // Verify parent relationships
    EXPECT_EQ(root_symbol.value()->get_parent_scope(), root_scope);
    EXPECT_EQ(child_symbol.value()->get_parent_scope(), root_scope);
    EXPECT_EQ(child_symbol.value()->get_scope_id(), child_scope);
}
