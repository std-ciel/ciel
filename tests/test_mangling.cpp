#include "symbol_table/mangling.hpp"
#include "symbol_table/type.hpp"
#include "symbol_table/type_factory.hpp"
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

class ManglingTest : public ::testing::Test {
  protected:
    void SetUp() override
    {
        type_factory = std::make_unique<TypeFactory>();
    }

    void TearDown() override
    {
        type_factory.reset();
    }

    std::unique_ptr<TypeFactory> type_factory;

    TypePtr get_void_type()
    {
        return type_factory->lookup("void").value();
    }
    TypePtr get_int_type()
    {
        return type_factory->lookup("int").value();
    }
    TypePtr get_bool_type()
    {
        return type_factory->lookup("bool").value();
    }
    TypePtr get_char_type()
    {
        return type_factory->lookup("char").value();
    }
    TypePtr get_float_type()
    {
        return type_factory->lookup("float").value();
    }
    TypePtr get_unsigned_type()
    {
        return type_factory->lookup("unsigned").value();
    }

    std::pair<FunctionType, FunctionMeta>
    create_function_type(QualType return_type,
                         std::vector<QualType> params = {},
                         bool variadic = false,
                         FunctionKind kind = FunctionKind::NORMAL)
    {
        FunctionType ftype(return_type, std::move(params), variadic);
        FunctionMeta meta(kind, {});
        return {std::move(ftype), std::move(meta)};
    }

    QualType qual(TypePtr type, Qualifier q = Qualifier::Q_NONE)
    {
        return QualType{type, q};
    }
};

TEST_F(ManglingTest, BasicFunctionNoParams)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()));
    std::string mangled = mangle_function_name("main", func_type, meta);

    EXPECT_EQ(mangled, "_Z4mainv");
}

TEST_F(ManglingTest, FunctionWithSingleParam)
{
    auto [func_type, meta] =
        create_function_type(qual(get_void_type()), {qual(get_int_type())});
    std::string mangled = mangle_function_name("func", func_type, meta);

    EXPECT_EQ(mangled, "_Z4funci");
}

TEST_F(ManglingTest, FunctionWithMultipleParams)
{
    auto [func_type, meta] = create_function_type(
        qual(get_int_type()),
        {qual(get_int_type()), qual(get_bool_type()), qual(get_char_type())});
    std::string mangled = mangle_function_name("add", func_type, meta);

    EXPECT_EQ(mangled, "_Z3addibc");
}

TEST_F(ManglingTest, VariadicFunction)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()),
                                                  {qual(get_char_type())},
                                                  true);
    std::string mangled = mangle_function_name("printf", func_type, meta);

    EXPECT_EQ(mangled, "_Z6printfcz");
}

TEST_F(ManglingTest, ConstructorMangling)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::CONSTRUCTOR);

    ClassType cls("MyClass", nullptr, Access::PUBLIC);

    std::string mangled = mangle_function_name("MyClass", func_type, meta, cls);

    EXPECT_EQ(mangled, "_Z7MyClassC1i");
}

TEST_F(ManglingTest, DestructorMangling)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()),
                                                  {},
                                                  false,
                                                  FunctionKind::DESTRUCTOR);

    ClassType cls("MyClass", nullptr, Access::PUBLIC);
    std::string mangled =
        mangle_function_name("~MyClass", func_type, meta, cls);

    EXPECT_EQ(mangled, "_Z7MyClassD1v");
}

TEST_F(ManglingTest, ArithmeticOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_int_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string plus_mangled = mangle_function_name("+", func_type, meta);
    EXPECT_EQ(plus_mangled, "_Zpli");

    std::string minus_mangled = mangle_function_name("-", func_type, meta);
    EXPECT_EQ(minus_mangled, "_Zmii");

    std::string mult_mangled = mangle_function_name("*", func_type, meta);
    EXPECT_EQ(mult_mangled, "_Zmli");

    std::string div_mangled = mangle_function_name("/", func_type, meta);
    EXPECT_EQ(div_mangled, "_Zdvi");
}

TEST_F(ManglingTest, ComparisonOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_bool_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string eq_mangled = mangle_function_name("==", func_type, meta);
    EXPECT_EQ(eq_mangled, "_Zeqi");

    std::string ne_mangled = mangle_function_name("!=", func_type, meta);
    EXPECT_EQ(ne_mangled, "_Znei");

    std::string lt_mangled = mangle_function_name("<", func_type, meta);
    EXPECT_EQ(lt_mangled, "_Zlti");

    std::string gt_mangled = mangle_function_name(">", func_type, meta);
    EXPECT_EQ(gt_mangled, "_Zgti");
}

TEST_F(ManglingTest, LogicalOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_bool_type()),
                                                  {qual(get_bool_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string and_mangled = mangle_function_name("&&", func_type, meta);
    EXPECT_EQ(and_mangled, "_Zaab");

    std::string or_mangled = mangle_function_name("||", func_type, meta);
    EXPECT_EQ(or_mangled, "_Zoob");
}

TEST_F(ManglingTest, UnaryOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_int_type()),
                                                  {},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string plus_mangled = mangle_function_name("+", func_type, meta);
    EXPECT_EQ(plus_mangled, "_Zpsv");

    std::string minus_mangled = mangle_function_name("-", func_type, meta);
    EXPECT_EQ(minus_mangled, "_Zngv");

    std::string inc_mangled = mangle_function_name("++", func_type, meta);
    EXPECT_EQ(inc_mangled, "_Zppv");

    std::string dec_mangled = mangle_function_name("--", func_type, meta);
    EXPECT_EQ(dec_mangled, "_Zmmv");
}

TEST_F(ManglingTest, AssignmentOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_int_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string assign_mangled = mangle_function_name("=", func_type, meta);
    EXPECT_EQ(assign_mangled, "_ZaSi");

    std::string plus_assign_mangled =
        mangle_function_name("+=", func_type, meta);
    EXPECT_EQ(plus_assign_mangled, "_ZpLi");

    std::string minus_assign_mangled =
        mangle_function_name("-=", func_type, meta);
    EXPECT_EQ(minus_assign_mangled, "_ZmIi");

    std::string mult_assign_mangled =
        mangle_function_name("*=", func_type, meta);
    EXPECT_EQ(mult_assign_mangled, "_ZmLi");
}

TEST_F(ManglingTest, BitwiseOperators)
{
    auto [func_type, meta] = create_function_type(qual(get_int_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    std::string and_mangled = mangle_function_name("&", func_type, meta);
    EXPECT_EQ(and_mangled, "_Zani");

    std::string or_mangled = mangle_function_name("|", func_type, meta);
    EXPECT_EQ(or_mangled, "_Zori");

    std::string xor_mangled = mangle_function_name("^", func_type, meta);
    EXPECT_EQ(xor_mangled, "_Zeoi");

    std::string ls_mangled = mangle_function_name("<<", func_type, meta);
    EXPECT_EQ(ls_mangled, "_Zlsi");

    std::string rs_mangled = mangle_function_name(">>", func_type, meta);
    EXPECT_EQ(rs_mangled, "_Zrsi");
}

TEST_F(ManglingTest, FunctionOverloading)
{
    auto [func1, func1_meta] =
        create_function_type(qual(get_void_type()), {qual(get_int_type())});
    auto [func2, func2_meta] =
        create_function_type(qual(get_void_type()), {qual(get_float_type())});
    auto [func3, func3_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_int_type()), qual(get_float_type())});

    std::string mangled1 = mangle_function_name("func", func1, func1_meta);
    std::string mangled2 = mangle_function_name("func", func2, func2_meta);
    std::string mangled3 = mangle_function_name("func", func3, func3_meta);

    EXPECT_EQ(mangled1, "_Z4funci");
    EXPECT_EQ(mangled2, "_Z4funcf");
    EXPECT_EQ(mangled3, "_Z4funcif");

    EXPECT_NE(mangled1, mangled2);
    EXPECT_NE(mangled1, mangled3);
    EXPECT_NE(mangled2, mangled3);
}

TEST_F(ManglingTest, MethodMangling)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::METHOD);

    ClassType cls("Calculator", nullptr, Access::PUBLIC);
    std::string mangled = mangle_function_name("add", func_type, meta, cls);

    EXPECT_EQ(mangled, "_Z10Calculator3addi");
}

TEST_F(ManglingTest, ComplexParameterTypes)
{
    // int*
    auto int_ptr = type_factory->make<PointerType>(qual(get_int_type()));
    ASSERT_TRUE(int_ptr.has_value());

    // int[10]
    auto int_array = type_factory->make<ArrayType>(qual(get_int_type()), 10);
    ASSERT_TRUE(int_array.has_value());

    auto [func_type, meta] =
        create_function_type(qual(get_void_type()),
                             {qual(int_ptr.value()), qual(int_array.value())});

    std::string mangled = mangle_function_name("process", func_type, meta);

    EXPECT_EQ(mangled, "_Z7processPiA10_i");
}

TEST_F(ManglingTest, UnsignedTypes)
{
    auto [func_type, meta] = create_function_type(
        qual(get_unsigned_type()),
        {qual(get_unsigned_type()), qual(get_unsigned_type())});

    std::string mangled = mangle_function_name("add_unsigned", func_type, meta);

    EXPECT_EQ(mangled, "_Z12add_unsignedjj");
}

TEST_F(ManglingTest, UnknownOperatorError)
{
    auto [func_type, meta] = create_function_type(qual(get_int_type()),
                                                  {qual(get_int_type())},
                                                  false,
                                                  FunctionKind::OPERATOR);

    EXPECT_THROW(mangle_function_name("??", func_type, meta),
                 std::runtime_error);
    EXPECT_THROW(mangle_function_name("@#", func_type, meta),
                 std::runtime_error);
}

TEST_F(ManglingTest, EdgeCases)
{
    auto [func_type, meta] = create_function_type(qual(get_void_type()));
    std::string mangled = mangle_function_name("", func_type, meta);
    EXPECT_EQ(mangled, "_Z0v");

    std::string long_name(100, 'a');
    std::string long_mangled = mangle_function_name(long_name, func_type, meta);
    EXPECT_EQ(long_mangled, "_Z100" + long_name + "v");

    std::string special_name = "func_with_underscores";
    std::string special_mangled =
        mangle_function_name(special_name, func_type, meta);
    EXPECT_EQ(special_mangled, "_Z21func_with_underscoresv");
}

TEST_F(ManglingTest, AllOperatorCoverage)
{
    auto [binary_func_type, binary_func_type_meta] =
        create_function_type(qual(get_int_type()),
                             {qual(get_int_type())},
                             false,
                             FunctionKind::OPERATOR);

    struct {
        std::string op;
        std::string expected_code;
    } binary_operators[] = {
        {"+", "pl"},   {"-", "mi"},  {"*", "ml"},  {"/", "dv"},  {"%", "rm"},
        {"^", "eo"},   {"&", "an"},  {"|", "or"},  {"=", "aS"},  {"+=", "pL"},
        {"-=", "mI"},  {"*=", "mL"}, {"/=", "dV"}, {"%=", "rM"}, {"^=", "eO"},
        {"&=", "aN"},  {"|=", "oR"}, {"<<", "ls"}, {">>", "rs"}, {"<<=", "lS"},
        {">>=", "rS"}, {"==", "eq"}, {"!=", "ne"}, {">", "gt"},  {"<", "lt"},
        {">=", "ge"},  {"<=", "le"}, {"&&", "aa"}, {"||", "oo"}, {",", "cm"}};

    for (const auto &op : binary_operators) {
        std::string mangled = mangle_function_name(op.op,
                                                   binary_func_type,
                                                   binary_func_type_meta);
        std::string expected = "_Z" + op.expected_code + "i";
        EXPECT_EQ(mangled, expected) << "Failed for binary operator: " << op.op;
    }

    auto [unary_func_type, unary_func_type_meta] =
        create_function_type(qual(get_int_type()),
                             {},
                             false,
                             FunctionKind::OPERATOR);

    struct {
        std::string op;
        std::string expected_code;
    } unary_operators[] = {
        {"+", "ps"},  // unary plus
        {"-", "ng"},  // unary minus
        {"*", "de"},  // dereference
        {"&", "ad"},  // address-of
        {"~", "co"},  // complement
        {"!", "nt"},  // not
        {"++", "pp"}, // pre/post increment
        {"--", "mm"}  // pre/post decrement
    };

    for (const auto &op : unary_operators) {
        std::string mangled =
            mangle_function_name(op.op, unary_func_type, unary_func_type_meta);
        std::string expected = "_Z" + op.expected_code + "v";
        EXPECT_EQ(mangled, expected) << "Failed for unary operator: " << op.op;
    }
}

TEST_F(ManglingTest, DifferentReturnTypes)
{
    std::vector<TypePtr> return_types = {get_void_type(),
                                         get_int_type(),
                                         get_bool_type(),
                                         get_char_type(),
                                         get_float_type()};

    std::vector<std::string> mangled_names;

    for (const auto &ret_type : return_types) {
        auto [func_type, meta] = create_function_type(qual(ret_type));
        std::string mangled = mangle_function_name("test", func_type, meta);
        mangled_names.push_back(mangled);
    }

    for (const auto &mangled : mangled_names) {
        EXPECT_EQ(mangled, "_Z4testv");
    }
}

TEST_F(ManglingTest, ManyParameters)
{
    std::vector<QualType> many_params;
    std::string expected_params;

    for (int i = 0; i < 20; ++i) {
        switch (i % 5) {
        case 0:
            many_params.push_back(qual(get_int_type()));
            expected_params += "i";
            break;
        case 1:
            many_params.push_back(qual(get_bool_type()));
            expected_params += "b";
            break;
        case 2:
            many_params.push_back(qual(get_char_type()));
            expected_params += "c";
            break;
        case 3:
            many_params.push_back(qual(get_float_type()));
            expected_params += "f";
            break;
        case 4:
            many_params.push_back(qual(get_unsigned_type()));
            expected_params += "j";
            break;
        }
    }

    auto [func_type, meta] =
        create_function_type(qual(get_void_type()), many_params);
    std::string mangled = mangle_function_name("stress_test", func_type, meta);
    std::string expected = "_Z11stress_test" + expected_params;

    EXPECT_EQ(mangled, expected);
}

// const int, const char, const bool, const float
TEST_F(ManglingTest, ConstQualifiedBasicTypes)
{
    auto [const_int_func, const_int_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_int_type(), Qualifier::Q_CONST)});
    std::string const_int_mangled =
        mangle_function_name("func", const_int_func, const_int_func_meta);
    EXPECT_EQ(const_int_mangled, "_Z4funcKi");

    auto [const_char_func, const_char_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_char_type(), Qualifier::Q_CONST)});
    std::string const_char_mangled =
        mangle_function_name("func", const_char_func, const_char_func_meta);
    EXPECT_EQ(const_char_mangled, "_Z4funcKc");

    auto [const_bool_func, const_bool_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_bool_type(), Qualifier::Q_CONST)});
    std::string const_bool_mangled =
        mangle_function_name("func", const_bool_func, const_bool_func_meta);
    EXPECT_EQ(const_bool_mangled, "_Z4funcKb");

    auto [const_float_func, const_float_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_float_type(), Qualifier::Q_CONST)});
    std::string const_float_mangled =
        mangle_function_name("func", const_float_func, const_float_func_meta);
    EXPECT_EQ(const_float_mangled, "_Z4funcKf");
}

// volatile int, volatile char
TEST_F(ManglingTest, VolatileQualifiedTypes)
{
    auto [volatile_int_func, volatile_int_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_int_type(), Qualifier::Q_VOLATILE)});
    std::string volatile_int_mangled =
        mangle_function_name("func", volatile_int_func, volatile_int_func_meta);
    EXPECT_EQ(volatile_int_mangled, "_Z4funcVi");

    auto [volatile_char_func, volatile_char_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(get_char_type(), Qualifier::Q_VOLATILE)});
    std::string volatile_char_mangled =
        mangle_function_name("func",
                             volatile_char_func,
                             volatile_char_func_meta);
    EXPECT_EQ(volatile_char_mangled, "_Z4funcVc");
}

// const volatile int, const volatile char
TEST_F(ManglingTest, ConstVolatileQualifiedTypes)
{
    auto [const_volatile_int_func, const_volatile_int_func_meta] =
        create_function_type(
            qual(get_void_type()),
            {qual(get_int_type(), Qualifier::Q_CONST_VOLATILE)});
    std::string const_volatile_int_mangled =
        mangle_function_name("func",
                             const_volatile_int_func,
                             const_volatile_int_func_meta);
    EXPECT_EQ(const_volatile_int_mangled, "_Z4funcVKi");

    auto [const_volatile_char_func, const_volatile_char_func_meta] =
        create_function_type(
            qual(get_void_type()),
            {qual(get_char_type(), Qualifier::Q_CONST_VOLATILE)});
    std::string const_volatile_char_mangled =
        mangle_function_name("func",
                             const_volatile_char_func,
                             const_volatile_char_func_meta);
    EXPECT_EQ(const_volatile_char_mangled, "_Z4funcVKc");
}

// const char[10], const int[5], volatile char[7]
TEST_F(ManglingTest, ConstQualifiedArrays)
{
    auto const_char_array =
        type_factory->make<ArrayType>(qual(get_char_type(), Qualifier::Q_CONST),
                                      10);
    ASSERT_TRUE(const_char_array.has_value());

    auto [const_char_array_func, const_char_array_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(const_char_array.value())});
    std::string const_char_array_mangled =
        mangle_function_name("func",
                             const_char_array_func,
                             const_char_array_func_meta);
    EXPECT_EQ(const_char_array_mangled, "_Z4funcA10_Kc");

    auto const_int_array =
        type_factory->make<ArrayType>(qual(get_int_type(), Qualifier::Q_CONST),
                                      5);
    ASSERT_TRUE(const_int_array.has_value());

    auto [const_int_array_func, const_int_array_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(const_int_array.value())});
    std::string const_int_array_mangled =
        mangle_function_name("func",
                             const_int_array_func,
                             const_int_array_func_meta);
    EXPECT_EQ(const_int_array_mangled, "_Z4funcA5_Ki");

    auto volatile_char_array = type_factory->make<ArrayType>(
        qual(get_char_type(), Qualifier::Q_VOLATILE),
        7);
    ASSERT_TRUE(volatile_char_array.has_value());

    auto [volatile_char_array_func, volatile_char_array_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(volatile_char_array.value())});
    std::string volatile_char_array_mangled =
        mangle_function_name("func",
                             volatile_char_array_func,
                             volatile_char_array_func_meta);
    EXPECT_EQ(volatile_char_array_mangled, "_Z4funcA7_Vc");
}

// const int*, const char*, volatile int*
TEST_F(ManglingTest, PointerToConstTypes)
{
    auto const_int_ptr = type_factory->make<PointerType>(
        qual(get_int_type(), Qualifier::Q_CONST));
    ASSERT_TRUE(const_int_ptr.has_value());

    auto [const_int_ptr_func, const_int_ptr_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(const_int_ptr.value())});
    std::string const_int_ptr_mangled =
        mangle_function_name("func",
                             const_int_ptr_func,
                             const_int_ptr_func_meta);
    EXPECT_EQ(const_int_ptr_mangled, "_Z4funcPKi");

    auto const_char_ptr = type_factory->make<PointerType>(
        qual(get_char_type(), Qualifier::Q_CONST));
    ASSERT_TRUE(const_char_ptr.has_value());

    auto [const_char_ptr_func, const_char_ptr_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(const_char_ptr.value())});
    std::string const_char_ptr_mangled =
        mangle_function_name("func",
                             const_char_ptr_func,
                             const_char_ptr_func_meta);
    EXPECT_EQ(const_char_ptr_mangled, "_Z4funcPKc");

    auto volatile_int_ptr = type_factory->make<PointerType>(
        qual(get_int_type(), Qualifier::Q_VOLATILE));
    ASSERT_TRUE(volatile_int_ptr.has_value());

    auto [volatile_int_ptr_func, volatile_int_ptr_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(volatile_int_ptr.value())});
    std::string volatile_int_ptr_mangled =
        mangle_function_name("func",
                             volatile_int_ptr_func,
                             volatile_int_ptr_func_meta);
    EXPECT_EQ(volatile_int_ptr_mangled, "_Z4funcPVi");
}

// int* const, char* const
TEST_F(ManglingTest, ConstPointerTypes)
{
    auto int_ptr = type_factory->make<PointerType>(qual(get_int_type()));
    ASSERT_TRUE(int_ptr.has_value());

    auto [const_int_ptr_func, const_int_ptr_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(int_ptr.value(), Qualifier::Q_CONST)});
    std::string const_int_ptr_mangled =
        mangle_function_name("func",
                             const_int_ptr_func,
                             const_int_ptr_func_meta);
    EXPECT_EQ(const_int_ptr_mangled, "_Z4funcKPi");

    auto char_ptr = type_factory->make<PointerType>(qual(get_char_type()));
    ASSERT_TRUE(char_ptr.has_value());

    auto [const_char_ptr_func, const_char_ptr_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(char_ptr.value(), Qualifier::Q_CONST)});
    std::string const_char_ptr_mangled =
        mangle_function_name("func",
                             const_char_ptr_func,
                             const_char_ptr_func_meta);
    EXPECT_EQ(const_char_ptr_mangled, "_Z4funcKPc");
}

// const int*const *const*const
TEST_F(ManglingTest, ComplexMultiLevelPointers)
{
    // const int
    auto const_int = qual(get_int_type(), Qualifier::Q_CONST);

    // const int* const
    auto const_int_ptr = type_factory->make<PointerType>(const_int);
    ASSERT_TRUE(const_int_ptr.has_value());
    auto const_int_ptr_const = qual(const_int_ptr.value(), Qualifier::Q_CONST);

    // const int* const * const
    auto const_int_ptr_const_ptr =
        type_factory->make<PointerType>(const_int_ptr_const);
    ASSERT_TRUE(const_int_ptr_const_ptr.has_value());
    auto const_int_ptr_const_ptr_const =
        qual(const_int_ptr_const_ptr.value(), Qualifier::Q_CONST);

    // const int* const * const * const
    auto const_int_ptr_const_ptr_const_ptr =
        type_factory->make<PointerType>(const_int_ptr_const_ptr_const);
    ASSERT_TRUE(const_int_ptr_const_ptr_const_ptr.has_value());
    auto const_int_ptr_const_ptr_const_ptr_const =
        qual(const_int_ptr_const_ptr_const_ptr.value(), Qualifier::Q_CONST);

    auto [complex_func, complex_func_meta] =
        create_function_type(qual(get_void_type()),
                             {const_int_ptr_const_ptr_const_ptr_const});
    std::string complex_mangled =
        mangle_function_name("complex_func", complex_func, complex_func_meta);
    EXPECT_EQ(complex_mangled, "_Z12complex_funcKPKPKPKi");
}

// volatile int* const, const volatile int*
TEST_F(ManglingTest, MixedQualifierScenarios)
{
    auto volatile_int_ptr = type_factory->make<PointerType>(
        qual(get_int_type(), Qualifier::Q_VOLATILE));
    ASSERT_TRUE(volatile_int_ptr.has_value());

    auto [const_volatile_int_ptr_func, const_volatile_int_ptr_func_meta] =
        create_function_type(
            qual(get_void_type()),
            {qual(volatile_int_ptr.value(), Qualifier::Q_CONST)});
    std::string const_volatile_int_ptr_mangled =
        mangle_function_name("func",
                             const_volatile_int_ptr_func,
                             const_volatile_int_ptr_func_meta);
    EXPECT_EQ(const_volatile_int_ptr_mangled, "_Z4funcKPVi");

    auto const_volatile_int_ptr2 = type_factory->make<PointerType>(
        qual(get_int_type(), Qualifier::Q_CONST_VOLATILE));
    ASSERT_TRUE(const_volatile_int_ptr2.has_value());

    auto [const_volatile_int_ptr2_func, const_volatile_int_ptr2_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(const_volatile_int_ptr2.value())});
    std::string const_volatile_int_ptr2_mangled =
        mangle_function_name("func",
                             const_volatile_int_ptr2_func,
                             const_volatile_int_ptr2_func_meta);
    EXPECT_EQ(const_volatile_int_ptr2_mangled, "_Z4funcPVKi");
}

// const char (*)[10], char const (*const)[5]
TEST_F(ManglingTest, ComplexArrayPointerCombinations)
{
    auto const_char_array =
        type_factory->make<ArrayType>(qual(get_char_type(), Qualifier::Q_CONST),
                                      10);
    ASSERT_TRUE(const_char_array.has_value());

    auto ptr_to_const_char_array =
        type_factory->make<PointerType>(qual(const_char_array.value()));
    ASSERT_TRUE(ptr_to_const_char_array.has_value());

    auto [ptr_to_const_char_array_func, ptr_to_const_char_array_func_meta] =
        create_function_type(qual(get_void_type()),
                             {qual(ptr_to_const_char_array.value())});
    std::string ptr_to_const_char_array_mangled =
        mangle_function_name("func",
                             ptr_to_const_char_array_func,
                             ptr_to_const_char_array_func_meta);
    EXPECT_EQ(ptr_to_const_char_array_mangled, "_Z4funcPA10_Kc");

    auto const_char_array2 =
        type_factory->make<ArrayType>(qual(get_char_type(), Qualifier::Q_CONST),
                                      5);
    ASSERT_TRUE(const_char_array2.has_value());

    auto const_ptr_to_const_char_array =
        type_factory->make<PointerType>(qual(const_char_array2.value()));
    ASSERT_TRUE(const_ptr_to_const_char_array.has_value());

    auto [const_ptr_to_const_char_array_func,
          const_ptr_to_const_char_array_func_meta] =
        create_function_type(
            qual(get_void_type()),
            {qual(const_ptr_to_const_char_array.value(), Qualifier::Q_CONST)});
    std::string const_ptr_to_const_char_array_mangled =
        mangle_function_name("func",
                             const_ptr_to_const_char_array_func,
                             const_ptr_to_const_char_array_func_meta);
    EXPECT_EQ(const_ptr_to_const_char_array_mangled, "_Z4funcKPA5_Kc");
}

// const int*const *const*const, volatile char[20], const volatile float* const
TEST_F(ManglingTest, ExtremeComplexityStressTest)
{
    std::vector<QualType> complex_params;

    // const int*const *const*const
    auto const_int = qual(get_int_type(), Qualifier::Q_CONST);
    auto const_int_ptr = type_factory->make<PointerType>(const_int);
    ASSERT_TRUE(const_int_ptr.has_value());
    auto const_int_ptr_const = qual(const_int_ptr.value(), Qualifier::Q_CONST);
    auto const_int_ptr_const_ptr =
        type_factory->make<PointerType>(const_int_ptr_const);
    ASSERT_TRUE(const_int_ptr_const_ptr.has_value());
    auto const_int_ptr_const_ptr_const =
        qual(const_int_ptr_const_ptr.value(), Qualifier::Q_CONST);
    auto const_int_ptr_const_ptr_const_ptr =
        type_factory->make<PointerType>(const_int_ptr_const_ptr_const);
    ASSERT_TRUE(const_int_ptr_const_ptr_const_ptr.has_value());
    auto param1 =
        qual(const_int_ptr_const_ptr_const_ptr.value(), Qualifier::Q_CONST);
    complex_params.push_back(param1);

    // volatile char[20]
    auto volatile_char_array = type_factory->make<ArrayType>(
        qual(get_char_type(), Qualifier::Q_VOLATILE),
        20);
    ASSERT_TRUE(volatile_char_array.has_value());
    complex_params.push_back(qual(volatile_char_array.value()));

    // const volatile float* const
    auto const_volatile_float_ptr = type_factory->make<PointerType>(
        qual(get_float_type(), Qualifier::Q_CONST_VOLATILE));
    ASSERT_TRUE(const_volatile_float_ptr.has_value());
    auto param3 = qual(const_volatile_float_ptr.value(), Qualifier::Q_CONST);
    complex_params.push_back(param3);

    auto [extreme_func, extreme_func_meta] =
        create_function_type(qual(get_void_type()), complex_params);
    std::string extreme_mangled = mangle_function_name("extreme_complexity",
                                                       extreme_func,
                                                       extreme_func_meta);

    std::string expected = "_Z18extreme_complexityKPKPKPKiA20_VcKPVKf";
    EXPECT_EQ(extreme_mangled, expected);
}