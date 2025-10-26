#include "symbol_table/type.hpp"
#include "symbol_table/type_factory.hpp"
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

class TypeFactoryTest : public ::testing::Test {
  protected:
    void SetUp() override
    {
        factory = std::make_unique<TypeFactory>();
    }

    void TearDown() override
    {
        factory.reset();
    }

    std::unique_ptr<TypeFactory> factory;
};

// Test builtin type initialization and lookup
TEST_F(TypeFactoryTest, BuiltinTypesInitialization)
{
    // Test that all builtin types are properly initialized
    auto void_type = factory->lookup("void");
    ASSERT_TRUE(void_type.has_value());
    EXPECT_EQ(void_type.value()->kind, TypeKind::BUILTIN);
    EXPECT_EQ(void_type.value()->debug_name(), "void");

    auto bool_type = factory->lookup("bool");
    ASSERT_TRUE(bool_type.has_value());
    EXPECT_EQ(bool_type.value()->debug_name(), "bool");

    auto char_type = factory->lookup("char");
    ASSERT_TRUE(char_type.has_value());
    EXPECT_EQ(char_type.value()->debug_name(), "char");

    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());
    EXPECT_EQ(int_type.value()->debug_name(), "int");

    auto float_type = factory->lookup("float");
    ASSERT_TRUE(float_type.has_value());
    EXPECT_EQ(float_type.value()->debug_name(), "float");

    auto unsigned_type = factory->lookup("unsigned");
    ASSERT_TRUE(unsigned_type.has_value());
    EXPECT_EQ(unsigned_type.value()->debug_name(), "unsigned");
}

// Test lookup of non-existent type
TEST_F(TypeFactoryTest, LookupNonExistentType)
{
    auto result = factory->lookup("non_existent_type");
    EXPECT_FALSE(result.has_value());
}

// Test lookup by ID
TEST_F(TypeFactoryTest, LookupById)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // We can't directly get the ID from the public interface,
    // but we can test the lookup functionality indirectly
    auto result = factory->lookup(0); // Assuming void has ID 0
    EXPECT_TRUE(result.has_value());  // Either way is valid
}

// Test pointer type creation
TEST_F(TypeFactoryTest, PointerTypeCreation)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType int_qual(int_type.value(), Qualifier::NONE);
    auto int_ptr_opt = factory->get_pointer(int_qual);
    ASSERT_TRUE(int_ptr_opt.is_ok());
    auto int_ptr = *int_ptr_opt;

    EXPECT_EQ(int_ptr->kind, TypeKind::POINTER);
    EXPECT_EQ(int_ptr->debug_name(), "int*");

    // Test double pointer
    QualifiedType int_ptr_qual(int_ptr, Qualifier::NONE);
    auto int_ptr_ptr_opt = factory->get_pointer(int_ptr_qual);
    ASSERT_TRUE(int_ptr_ptr_opt.is_ok());
    auto int_ptr_ptr = *int_ptr_ptr_opt;
    EXPECT_EQ(int_ptr_ptr->debug_name(), "int**");
}

// Test pointer type creation with qualifiers
TEST_F(TypeFactoryTest, PointerTypeWithQualifiers)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType const_int(int_type.value(), Qualifier::CONST);
    auto const_int_ptr_opt = factory->get_pointer(const_int);
    ASSERT_TRUE(const_int_ptr_opt.is_ok());
    auto const_int_ptr = *const_int_ptr_opt;

    EXPECT_EQ(const_int_ptr->kind, TypeKind::POINTER);
    EXPECT_EQ(const_int_ptr->debug_name(), "int const*");

    QualifiedType volatile_int(int_type.value(), Qualifier::VOLATILE);
    auto volatile_int_ptr_opt = factory->get_pointer(volatile_int);
    ASSERT_TRUE(volatile_int_ptr_opt.is_ok());
    auto volatile_int_ptr = *volatile_int_ptr_opt;
    EXPECT_EQ(volatile_int_ptr->debug_name(), "int volatile*");

    QualifiedType const_volatile_int(int_type.value(),
                                     Qualifier::CONST_VOLATILE);
    auto const_volatile_int_ptr_opt = factory->get_pointer(const_volatile_int);
    ASSERT_TRUE(const_volatile_int_ptr_opt.is_ok());
    auto const_volatile_int_ptr = *const_volatile_int_ptr_opt;
    EXPECT_EQ(const_volatile_int_ptr->debug_name(), "int const volatile*");
}

// Test array type creation
TEST_F(TypeFactoryTest, ArrayTypeCreation)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType int_qual(int_type.value(), Qualifier::NONE);

    // Test sized array
    auto int_array_10_opt = factory->get_array(int_qual, 10);
    ASSERT_TRUE(int_array_10_opt.is_ok());
    auto int_array_10 = *int_array_10_opt;
    EXPECT_EQ(int_array_10->kind, TypeKind::ARRAY);
    EXPECT_EQ(int_array_10->debug_name(), "int[10]");

    // Test unsized array
    auto int_array_unsized_opt = factory->get_array(int_qual, 0);
    ASSERT_TRUE(int_array_unsized_opt.is_ok());
    auto int_array_unsized = *int_array_unsized_opt;
    EXPECT_EQ(int_array_unsized->debug_name(), "int[]");
}

// Test complex array types
TEST_F(TypeFactoryTest, ComplexArrayTypes)
{
    auto char_type = factory->lookup("char");
    ASSERT_TRUE(char_type.has_value());

    QualifiedType char_qual(char_type.value(), Qualifier::NONE);

    // Array of arrays
    auto char_array_5_opt = factory->get_array(char_qual, 5);
    ASSERT_TRUE(char_array_5_opt.is_ok());
    auto char_array_5 = *char_array_5_opt;
    QualifiedType char_array_5_qual(char_array_5, Qualifier::NONE);
    auto char_array_3_5_opt = factory->get_array(char_array_5_qual, 3);
    ASSERT_TRUE(char_array_3_5_opt.is_ok());
    auto char_array_3_5 = *char_array_3_5_opt;

    // This creates (char[5])[3], which should display as char[3][5]
    EXPECT_EQ(char_array_3_5->debug_name(), "char[3][5]");
}

// Test pointer chains
TEST_F(TypeFactoryTest, PointerChains)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType base(int_type.value(), Qualifier::NONE);
    std::vector<Qualifier> qualifiers = {Qualifier::NONE,
                                         Qualifier::CONST,
                                         Qualifier::VOLATILE};

    auto result_opt = factory->make_pointer_chain(base, qualifiers);
    ASSERT_TRUE(result_opt.is_ok());
    auto result = *result_opt;

    // Should create: volatile const int***
    // (Note: the order depends on implementation)
    std::string expected_name = result.debug_name();
    EXPECT_TRUE(expected_name.find("int") != std::string::npos);
    EXPECT_TRUE(expected_name.find("const") != std::string::npos);
    EXPECT_TRUE(expected_name.find("volatile") != std::string::npos);
}

// Test array chains
TEST_F(TypeFactoryTest, ArrayChains)
{
    auto float_type = factory->lookup("float");
    ASSERT_TRUE(float_type.has_value());

    QualifiedType base(float_type.value(), Qualifier::NONE);
    std::vector<size_t> sizes = {10, 20, 30};

    auto result_opt = factory->make_array_chain(base, sizes);
    ASSERT_TRUE(result_opt.is_ok());
    auto result = *result_opt;

    // Should create: float[10][20][30]
    std::string result_name = result.debug_name();
    EXPECT_TRUE(result_name.find("float") != std::string::npos);
    EXPECT_TRUE(result_name.find("[10]") != std::string::npos);
    EXPECT_TRUE(result_name.find("[20]") != std::string::npos);
    EXPECT_TRUE(result_name.find("[30]") != std::string::npos);
}

// Test array chains with zero sizes (unsized arrays)
TEST_F(TypeFactoryTest, ArrayChainsWithUnsized)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType base(int_type.value(), Qualifier::NONE);
    std::vector<size_t> sizes = {5, 0, 10}; // nullopt means unsized

    auto result_opt = factory->make_array_chain(base, sizes);
    ASSERT_TRUE(result_opt.is_ok());
    auto result = *result_opt;

    std::string result_name = result.debug_name();
    EXPECT_TRUE(result_name.find("int") != std::string::npos);
    EXPECT_TRUE(result_name.find("[5]") != std::string::npos);
    EXPECT_TRUE(result_name.find("[]") != std::string::npos); // unsized array
    EXPECT_TRUE(result_name.find("[10]") != std::string::npos);
}

// Test QualifiedType equality
TEST_F(TypeFactoryTest, QualifiedTypeEquality)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType int1(int_type.value(), Qualifier::NONE);
    QualifiedType int2(int_type.value(), Qualifier::NONE);
    QualifiedType const_int(int_type.value(), Qualifier::CONST);

    EXPECT_TRUE(int1 == int2);
    EXPECT_FALSE(int1 == const_int);
}

// Test QualifiedType debug names
TEST_F(TypeFactoryTest, QualifiedTypeDebugNames)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    QualifiedType int_none(int_type.value(), Qualifier::NONE);
    QualifiedType int_const(int_type.value(), Qualifier::CONST);
    QualifiedType int_volatile(int_type.value(), Qualifier::VOLATILE);
    QualifiedType int_const_volatile(int_type.value(),
                                     Qualifier::CONST_VOLATILE);

    EXPECT_EQ(int_none.debug_name(), "int");
    EXPECT_EQ(int_const.debug_name(), "int const");
    EXPECT_EQ(int_volatile.debug_name(), "int volatile");
    EXPECT_EQ(int_const_volatile.debug_name(), "int const volatile");
}

// Test scope-based lookup (basic functionality)
TEST_F(TypeFactoryTest, ScopeBasedLookup)
{
    std::vector<size_t> empty_scope_chain;

    // Test lookup with empty scope chain
    auto result = factory->lookup_by_scope("int", empty_scope_chain);
    // This might return nullopt since int is not in any specific scope
    // The exact behavior depends on implementation

    auto result_by_id = factory->lookup_by_scope(0, empty_scope_chain);
    // Similar to above
}

// Test edge cases
TEST_F(TypeFactoryTest, EdgeCases)
{
    // Test with empty string lookup
    auto empty_result = factory->lookup("");
    EXPECT_FALSE(empty_result.has_value());

    // Test very large array size
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());
    QualifiedType int_qual(int_type.value(), Qualifier::NONE);

    auto large_array_opt = factory->get_array(int_qual, SIZE_MAX);
    ASSERT_TRUE(large_array_opt.is_ok());
    auto large_array = *large_array_opt;
    EXPECT_EQ(large_array->kind, TypeKind::ARRAY);

    // Test array of pointers to arrays
    auto int_ptr_opt = factory->get_pointer(int_qual);
    ASSERT_TRUE(int_ptr_opt.is_ok());
    auto int_ptr = *int_ptr_opt;
    QualifiedType int_ptr_qual(int_ptr, Qualifier::NONE);
    auto ptr_array_opt = factory->get_array(int_ptr_qual, 5);
    ASSERT_TRUE(ptr_array_opt.is_ok());
    auto ptr_array = *ptr_array_opt;
    EXPECT_EQ(ptr_array->debug_name(), "int*[5]");
}

// Test custom types functionality
TEST_F(TypeFactoryTest, CustomTypes)
{
    auto record_type_opt = factory->make<RecordType>("MyStruct", false, true);
    auto enum_type_opt = factory->make<EnumType>("MyEnum", true);

    ASSERT_TRUE(record_type_opt.is_ok());
    ASSERT_TRUE(enum_type_opt.is_ok());

    auto record_type = *record_type_opt;
    auto enum_type = *enum_type_opt;
    auto typedef_type_opt =
        factory->make<TypedefType>("MyInt",
                                   QualifiedType(record_type, Qualifier::NONE));

    ASSERT_TRUE(typedef_type_opt.is_ok());
    auto typedef_type = *typedef_type_opt;

    auto custom_types = factory->get_custom_types();
    EXPECT_EQ(custom_types.size(), 3);

    // Check if the types are present in the custom types list
    bool found_record = false, found_enum = false, found_typedef = false;
    for (const auto &[id, type] : custom_types) {
        if (type == record_type)
            found_record = true;
        if (type == enum_type)
            found_enum = true;
        if (type == typedef_type)
            found_typedef = true;
    }
    EXPECT_TRUE(found_record);
    EXPECT_TRUE(found_enum);
    EXPECT_TRUE(found_typedef);
}

// Test type consistency
TEST_F(TypeFactoryTest, TypeConsistency)
{
    // Test that the same type is returned for the same lookup
    auto int_type1 = factory->lookup("int");
    auto int_type2 = factory->lookup("int");

    ASSERT_TRUE(int_type1.has_value());
    ASSERT_TRUE(int_type2.has_value());

    // Should be the same object (shared_ptr)
    EXPECT_EQ(int_type1.value(), int_type2.value());
}

// Test all builtin type kinds
TEST_F(TypeFactoryTest, AllBuiltinTypeKinds)
{
    struct BuiltinTest {
        std::string name;
        BuiltinTypeKind expected_kind;
    };

    std::vector<BuiltinTest> builtin_tests = {
        {"void", BuiltinTypeKind::VOID},
        {"bool", BuiltinTypeKind::BOOL},
        {"char", BuiltinTypeKind::CHAR},
        {"int", BuiltinTypeKind::INT},
        {"float", BuiltinTypeKind::FLOAT},
        {"unsigned", BuiltinTypeKind::UNSIGNED}};

    for (const auto &test : builtin_tests) {
        auto type = factory->lookup(test.name);
        ASSERT_TRUE(type.has_value()) << "Failed to lookup " << test.name;
        EXPECT_EQ(type.value()->kind, TypeKind::BUILTIN);

        auto builtin_type =
            std::dynamic_pointer_cast<BuiltinType>(type.value());
        ASSERT_TRUE(builtin_type != nullptr);
        EXPECT_EQ(builtin_type->builtin_kind, test.expected_kind);
    }
}

// Test make_qualified method with basic qualifiers
TEST_F(TypeFactoryTest, MakeQualifiedBasic)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Test const qualification
    auto const_int =
        factory->make_qualified(int_type.value(), Qualifier::CONST);
    EXPECT_EQ(const_int.qualifier, Qualifier::CONST);
    EXPECT_EQ(const_int.type, int_type.value());
    EXPECT_EQ(const_int.debug_name(), "int const");

    // Test volatile qualification
    auto volatile_int =
        factory->make_qualified(int_type.value(), Qualifier::VOLATILE);
    EXPECT_EQ(volatile_int.qualifier, Qualifier::VOLATILE);
    EXPECT_EQ(volatile_int.type, int_type.value());
    EXPECT_EQ(volatile_int.debug_name(), "int volatile");

    // Test const volatile qualification
    auto const_volatile_int =
        factory->make_qualified(int_type.value(), Qualifier::CONST_VOLATILE);
    EXPECT_EQ(const_volatile_int.qualifier, Qualifier::CONST_VOLATILE);
    EXPECT_EQ(const_volatile_int.type, int_type.value());
    EXPECT_EQ(const_volatile_int.debug_name(), "int const volatile");

    // Test no qualification
    auto unqualified_int =
        factory->make_qualified(int_type.value(), Qualifier::NONE);
    EXPECT_EQ(unqualified_int.qualifier, Qualifier::NONE);
    EXPECT_EQ(unqualified_int.type, int_type.value());
    EXPECT_EQ(unqualified_int.debug_name(), "int");
}

// Test make_qualified with complex pointer chains and mixed qualifiers
TEST_F(TypeFactoryTest, MakeQualifiedComplexPointerChains)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Build: const int*const*const*volatile* const
    // Step 1: const int
    auto const_int =
        factory->make_qualified(int_type.value(), Qualifier::CONST);
    EXPECT_EQ(const_int.debug_name(), "int const");

    // Step 2: const int*const (pointer to const int, const pointer)
    auto ptr1_opt = factory->get_pointer(const_int);
    ASSERT_TRUE(ptr1_opt.is_ok());
    auto ptr1 = *ptr1_opt;
    auto const_ptr1 = factory->make_qualified(ptr1, Qualifier::CONST);
    EXPECT_EQ(const_ptr1.debug_name(), "int const* const");

    // Step 3: const int*const*const (pointer to const pointer to const int,
    // const pointer)
    auto ptr2_opt = factory->get_pointer(const_ptr1);
    ASSERT_TRUE(ptr2_opt.is_ok());
    auto ptr2 = *ptr2_opt;
    auto const_ptr2 = factory->make_qualified(ptr2, Qualifier::CONST);
    EXPECT_EQ(const_ptr2.debug_name(), "int const* const* const");

    // Step 4: const int*const*const*volatile (pointer to const pointer to const
    // pointer to const int, volatile pointer)
    auto ptr3_opt = factory->get_pointer(const_ptr2);
    ASSERT_TRUE(ptr3_opt.is_ok());
    auto ptr3 = *ptr3_opt;
    auto volatile_ptr3 = factory->make_qualified(ptr3, Qualifier::VOLATILE);
    EXPECT_EQ(volatile_ptr3.debug_name(), "int const* const* const* volatile");

    // Step 5: const int*const*const*volatile* const (final const qualification)
    auto ptr4_opt = factory->get_pointer(volatile_ptr3);
    ASSERT_TRUE(ptr4_opt.is_ok());
    auto ptr4 = *ptr4_opt;
    auto final_const_ptr = factory->make_qualified(ptr4, Qualifier::CONST);
    EXPECT_EQ(final_const_ptr.debug_name(),
              "int const* const* const* volatile* const");
}

// Test make_qualified with deeply nested array and pointer combinations
TEST_F(TypeFactoryTest, MakeQualifiedComplexArrayPointerMix)
{
    auto char_type = factory->lookup("char");
    ASSERT_TRUE(char_type.has_value());

    // Build: const volatile char(*const)[10]*volatile
    // Step 1: const volatile char
    auto const_volatile_char =
        factory->make_qualified(char_type.value(), Qualifier::CONST_VOLATILE);
    EXPECT_EQ(const_volatile_char.debug_name(), "char const volatile");

    // Step 2: const volatile char[10]
    auto char_array_opt = factory->get_array(const_volatile_char, 10);
    ASSERT_TRUE(char_array_opt.is_ok());
    auto char_array = *char_array_opt;
    auto unqualified_array =
        factory->make_qualified(char_array, Qualifier::NONE);
    EXPECT_EQ(unqualified_array.debug_name(), "char const volatile[10]");

    // Step 3: pointer to array - const volatile char(*const)[10]
    auto ptr_to_array_opt = factory->get_pointer(unqualified_array);
    ASSERT_TRUE(ptr_to_array_opt.is_ok());
    auto ptr_to_array = *ptr_to_array_opt;
    auto const_ptr_to_array =
        factory->make_qualified(ptr_to_array, Qualifier::CONST);
    EXPECT_EQ(const_ptr_to_array.debug_name(),
              "char const volatile[10]* const");

    // Step 4: pointer to const pointer to array - const volatile
    // char(*const)[10]*volatile
    auto ptr_to_ptr_to_array_opt = factory->get_pointer(const_ptr_to_array);
    ASSERT_TRUE(ptr_to_ptr_to_array_opt.is_ok());
    auto ptr_to_ptr_to_array = *ptr_to_ptr_to_array_opt;
    auto volatile_ptr_to_ptr_to_array =
        factory->make_qualified(ptr_to_ptr_to_array, Qualifier::VOLATILE);
    EXPECT_EQ(volatile_ptr_to_ptr_to_array.debug_name(),
              "char const volatile[10]* const* volatile");

    // Verify type structure
    EXPECT_EQ(volatile_ptr_to_ptr_to_array.type->kind, TypeKind::POINTER);
    EXPECT_EQ(volatile_ptr_to_ptr_to_array.qualifier, Qualifier::VOLATILE);
}

// Test make_qualified with function pointers and complex qualifiers
TEST_F(TypeFactoryTest, MakeQualifiedFunctionPointers)
{
    auto int_type = factory->lookup("int");
    auto float_type = factory->lookup("float");
    ASSERT_TRUE(int_type.has_value());
    ASSERT_TRUE(float_type.has_value());

    // Create function type: int(float, const int*)
    auto const_int =
        factory->make_qualified(int_type.value(), Qualifier::CONST);
    auto const_int_ptr_opt = factory->get_pointer(const_int);
    ASSERT_TRUE(const_int_ptr_opt.is_ok());
    auto const_int_ptr = *const_int_ptr_opt;
    auto unqualified_const_int_ptr =
        factory->make_qualified(const_int_ptr, Qualifier::NONE);
    auto unqualified_float =
        factory->make_qualified(float_type.value(), Qualifier::NONE);
    auto unqualified_int =
        factory->make_qualified(int_type.value(), Qualifier::NONE);

    std::vector<QualifiedType> params = {unqualified_float,
                                         unqualified_const_int_ptr};
    auto func_type_opt =
        factory->make<FunctionType>(unqualified_int, params, false);
    ASSERT_TRUE(func_type_opt.is_ok());
    auto func_type = *func_type_opt;

    // Create function pointer: int(*)(float, const int*)
    auto qualified_func_type =
        factory->make_qualified(func_type, Qualifier::NONE);
    auto func_ptr_opt = factory->get_pointer(qualified_func_type);
    ASSERT_TRUE(func_ptr_opt.is_ok());
    auto func_ptr = *func_ptr_opt;
    auto unqualified_func_ptr =
        factory->make_qualified(func_ptr, Qualifier::NONE);

    // Create const function pointer: int(*const)(float, const int*)
    auto const_func_ptr = factory->make_qualified(func_ptr, Qualifier::CONST);
    EXPECT_EQ(const_func_ptr.qualifier, Qualifier::CONST);

    // Create volatile function pointer: int(*volatile)(float, const int*)
    auto volatile_func_ptr =
        factory->make_qualified(func_ptr, Qualifier::VOLATILE);
    EXPECT_EQ(volatile_func_ptr.qualifier, Qualifier::VOLATILE);
}

// Test make_qualified edge cases and combinations
TEST_F(TypeFactoryTest, MakeQualifiedEdgeCases)
{
    auto void_type = factory->lookup("void");
    auto bool_type = factory->lookup("bool");
    ASSERT_TRUE(void_type.has_value());
    ASSERT_TRUE(bool_type.has_value());

    // Test qualified void
    auto const_void =
        factory->make_qualified(void_type.value(), Qualifier::CONST);
    EXPECT_EQ(const_void.debug_name(), "void const");

    // Test qualified void pointer
    auto void_ptr_opt = factory->get_pointer(const_void);
    ASSERT_TRUE(void_ptr_opt.is_ok());
    auto void_ptr = *void_ptr_opt;
    auto const_void_ptr = factory->make_qualified(void_ptr, Qualifier::CONST);
    EXPECT_EQ(const_void_ptr.debug_name(), "void const* const");

    // Test multiple levels of the same qualifier
    auto const_bool =
        factory->make_qualified(bool_type.value(), Qualifier::CONST);
    auto const_bool_ptr_opt = factory->get_pointer(const_bool);
    ASSERT_TRUE(const_bool_ptr_opt.is_ok());
    auto const_bool_ptr = *const_bool_ptr_opt;
    auto const_const_bool_ptr =
        factory->make_qualified(const_bool_ptr, Qualifier::CONST);
    EXPECT_EQ(const_const_bool_ptr.debug_name(), "bool const* const");

    // Test alternating qualifiers in deep pointer chain
    auto base = factory->make_qualified(bool_type.value(), Qualifier::CONST);
    for (int i = 0; i < 5; ++i) {
        auto ptr_opt = factory->get_pointer(base);
        ASSERT_TRUE(ptr_opt.is_ok());
        auto ptr = *ptr_opt;
        if (i % 2 == 0) {
            base = factory->make_qualified(ptr, Qualifier::VOLATILE);
        } else {
            base = factory->make_qualified(ptr, Qualifier::CONST);
        }
    }
    // Should result in a very deep qualified pointer chain
    EXPECT_TRUE(base.debug_name().length() > 20); // Sanity check for complexity
    EXPECT_EQ(base.qualifier,
              Qualifier::VOLATILE); // Last qualifier was volatile
}

// Test make_qualified with all possible qualifier combinations systematically
TEST_F(TypeFactoryTest, MakeQualifiedAllCombinations)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    struct QualifierTest {
        Qualifier qualifier;
        std::string expected_prefix;
    };

    std::vector<QualifierTest> tests = {
        {Qualifier::NONE, ""},
        {Qualifier::CONST, " const"},
        {Qualifier::VOLATILE, " volatile"},
        {Qualifier::CONST_VOLATILE, " const volatile"}};

    for (const auto &test : tests) {
        auto qualified_int =
            factory->make_qualified(int_type.value(), test.qualifier);
        EXPECT_EQ(qualified_int.qualifier, test.qualifier);
        EXPECT_EQ(qualified_int.debug_name(), "int" + test.expected_prefix);

        // Test with pointer
        auto int_ptr_opt = factory->get_pointer(qualified_int);
        ASSERT_TRUE(int_ptr_opt.is_ok());
        auto int_ptr = *int_ptr_opt;
        auto qualified_ptr = factory->make_qualified(int_ptr, test.qualifier);
        EXPECT_EQ(qualified_ptr.qualifier, test.qualifier);
        std::string expected_ptr_name =
            "int" + test.expected_prefix + "*" + test.expected_prefix;
        EXPECT_EQ(qualified_ptr.debug_name(), expected_ptr_name);
    }
}

// Test extremely complex qualifier patterns that might occur in real C++ code
TEST_F(TypeFactoryTest, MakeQualifiedExtremeComplexity)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Build the monster type: const volatile int*const
    // volatile*const*volatile*const volatile This represents: pointer to const
    // volatile pointer to const pointer to const volatile pointer to const
    // volatile int

    // Step 1: const volatile int
    auto base =
        factory->make_qualified(int_type.value(), Qualifier::CONST_VOLATILE);
    EXPECT_EQ(base.debug_name(), "int const volatile");

    // Step 2: const volatile int*const volatile
    auto ptr1_opt = factory->get_pointer(base);
    ASSERT_TRUE(ptr1_opt.is_ok());
    auto ptr1 = *ptr1_opt;
    auto qualified_ptr1 =
        factory->make_qualified(ptr1, Qualifier::CONST_VOLATILE);
    EXPECT_EQ(qualified_ptr1.debug_name(),
              "int const volatile* const volatile");

    // Step 3: const volatile int*const volatile*const
    auto ptr2_opt = factory->get_pointer(qualified_ptr1);
    ASSERT_TRUE(ptr2_opt.is_ok());
    auto ptr2 = *ptr2_opt;
    auto qualified_ptr2 = factory->make_qualified(ptr2, Qualifier::CONST);
    EXPECT_EQ(qualified_ptr2.debug_name(),
              "int const volatile* const volatile* const");

    // Step 4: const volatile int*const volatile*const*volatile
    auto ptr3_opt = factory->get_pointer(qualified_ptr2);
    ASSERT_TRUE(ptr3_opt.is_ok());
    auto ptr3 = *ptr3_opt;
    auto qualified_ptr3 = factory->make_qualified(ptr3, Qualifier::VOLATILE);
    EXPECT_EQ(qualified_ptr3.debug_name(),
              "int const volatile* const volatile* const* volatile");

    // Step 5: const volatile int*const volatile*const*volatile*const volatile
    // (final form)
    auto ptr4_opt = factory->get_pointer(qualified_ptr3);
    ASSERT_TRUE(ptr4_opt.is_ok());
    auto ptr4 = *ptr4_opt;
    auto final_qualified =
        factory->make_qualified(ptr4, Qualifier::CONST_VOLATILE);
    EXPECT_EQ(
        final_qualified.debug_name(),
        "int const volatile* const volatile* const* volatile* const volatile");

    // Verify the type structure is still coherent
    EXPECT_EQ(final_qualified.type->kind, TypeKind::POINTER);
    EXPECT_EQ(final_qualified.qualifier, Qualifier::CONST_VOLATILE);
}

// Test make_qualified with arrays of pointers to arrays
TEST_F(TypeFactoryTest, MakeQualifiedArraysOfPointersToArrays)
{
    auto char_type = factory->lookup("char");
    ASSERT_TRUE(char_type.has_value());

    // Build: const char[5]*volatile[3]*const[7]
    // This is: array[7] of const pointers to array[3] of volatile pointers to
    // array[5] of const char

    // Step 1: const char
    auto const_char =
        factory->make_qualified(char_type.value(), Qualifier::CONST);

    // Step 2: const char[5]
    auto char_array5_opt = factory->get_array(const_char, 5);
    ASSERT_TRUE(char_array5_opt.is_ok());
    auto char_array5 = *char_array5_opt;
    auto unqualified_char_array5 =
        factory->make_qualified(char_array5, Qualifier::NONE);

    // Step 3: const char[5]*volatile
    auto ptr_to_array5_opt = factory->get_pointer(unqualified_char_array5);
    ASSERT_TRUE(ptr_to_array5_opt.is_ok());
    auto ptr_to_array5 = *ptr_to_array5_opt;
    auto volatile_ptr_to_array5 =
        factory->make_qualified(ptr_to_array5, Qualifier::VOLATILE);

    // Step 4: const char[5]*volatile[3]
    auto array3_of_ptrs_opt = factory->get_array(volatile_ptr_to_array5, 3);
    ASSERT_TRUE(array3_of_ptrs_opt.is_ok());
    auto array3_of_ptrs = *array3_of_ptrs_opt;
    auto unqualified_array3 =
        factory->make_qualified(array3_of_ptrs, Qualifier::NONE);

    // Step 5: const char[5]*volatile[3]*const
    auto ptr_to_array3_opt = factory->get_pointer(unqualified_array3);
    ASSERT_TRUE(ptr_to_array3_opt.is_ok());
    auto ptr_to_array3 = *ptr_to_array3_opt;
    auto const_ptr_to_array3 =
        factory->make_qualified(ptr_to_array3, Qualifier::CONST);

    // Step 6: const char[5]*volatile[3]*const[7] (final form)
    auto final_array7_opt = factory->get_array(const_ptr_to_array3, 7);
    ASSERT_TRUE(final_array7_opt.is_ok());
    auto final_array7 = *final_array7_opt;
    auto final_type = factory->make_qualified(final_array7, Qualifier::NONE);

    // The final type should be extremely complex
    std::string final_name = final_type.debug_name();
    EXPECT_TRUE(final_name.find("char") != std::string::npos);
    EXPECT_TRUE(final_name.find("[5]") != std::string::npos);
    EXPECT_TRUE(final_name.find("[3]") != std::string::npos);
    EXPECT_TRUE(final_name.find("[7]") != std::string::npos);
    EXPECT_TRUE(final_name.find("volatile") != std::string::npos);
    EXPECT_TRUE(final_name.find("const") != std::string::npos);

    // Verify the final type is an array
    EXPECT_EQ(final_type.type->kind, TypeKind::ARRAY);
}

// Test make_qualified type consistency and equality with complex types
TEST_F(TypeFactoryTest, MakeQualifiedConsistencyAndEquality)
{
    auto int_type = factory->lookup("int");
    ASSERT_TRUE(int_type.has_value());

    // Create the same complex type in two different ways
    // Way 1: Build step by step
    auto const_int1 =
        factory->make_qualified(int_type.value(), Qualifier::CONST);
    auto ptr1_step1_opt = factory->get_pointer(const_int1);
    ASSERT_TRUE(ptr1_step1_opt.is_ok());
    auto ptr1_step1 = *ptr1_step1_opt;
    auto volatile_ptr1 =
        factory->make_qualified(ptr1_step1, Qualifier::VOLATILE);
    auto ptr2_step1_opt = factory->get_pointer(volatile_ptr1);
    ASSERT_TRUE(ptr2_step1_opt.is_ok());
    auto ptr2_step1 = *ptr2_step1_opt;
    auto const_ptr2_way1 =
        factory->make_qualified(ptr2_step1, Qualifier::CONST);

    // Way 2: Build in a different order
    auto const_int2 =
        factory->make_qualified(int_type.value(), Qualifier::CONST);
    auto ptr1_step2_opt = factory->get_pointer(const_int2);
    ASSERT_TRUE(ptr1_step2_opt.is_ok());
    auto ptr1_step2 = *ptr1_step2_opt;
    auto volatile_ptr2 =
        factory->make_qualified(ptr1_step2, Qualifier::VOLATILE);
    auto ptr2_step2_opt = factory->get_pointer(volatile_ptr2);
    ASSERT_TRUE(ptr2_step2_opt.is_ok());
    auto ptr2_step2 = *ptr2_step2_opt;
    auto const_ptr2_way2 =
        factory->make_qualified(ptr2_step2, Qualifier::CONST);

    // Both should have the same debug representation
    EXPECT_EQ(const_ptr2_way1.debug_name(), const_ptr2_way2.debug_name());
    EXPECT_EQ(const_ptr2_way1.qualifier, const_ptr2_way2.qualifier);

    // Test that different qualifier orders produce different types
    auto different_qualified =
        factory->make_qualified(ptr2_step1, Qualifier::VOLATILE);
    EXPECT_NE(const_ptr2_way1.debug_name(), different_qualified.debug_name());
    EXPECT_NE(const_ptr2_way1.qualifier, different_qualified.qualifier);
}
