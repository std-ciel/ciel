#include <FlexLexer.h>
#include <gtest/gtest.h>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

extern int yylineno;
extern int column;

namespace {

std::string run_lexer_on(const std::string &src)
{

    yylineno = 1;
    column = 1;

    std::ostringstream captured;
    auto *oldbuf = std::cout.rdbuf(captured.rdbuf());

    std::istringstream in(src);
    yyFlexLexer lexer;
    lexer.switch_streams(&in, &std::cout);

    while (lexer.yylex() != 0) { /* keep scanning */
    }

    std::cout.rdbuf(oldbuf);
    return captured.str();
}

std::vector<std::string> strip_header(const std::string &out)
{
    std::vector<std::string> lines;
    std::istringstream is(out);
    std::string line;
    while (std::getline(is, line)) {
        if (line.rfind("VALUE", 0) == 0)
            continue;
        bool all_dash =
            !line.empty() && line.find_first_not_of('-') == std::string::npos;
        if (all_dash)
            continue;
        if (!line.empty())
            lines.push_back(line);
    }
    return lines;
}

std::vector<std::pair<std::string, std::string>>
parse_tokens(const std::string &out)
{
    auto lines = strip_header(out);
    std::vector<std::pair<std::string, std::string>> tokens;

    for (const auto &line : lines) {

        size_t start = line.find('\'');
        if (start == std::string::npos)
            continue;

        size_t i = start + 1;
        bool esc = false;
        size_t end = std::string::npos;
        for (; i < line.size(); ++i) {
            char c = line[i];
            if (esc) {
                esc = false;
            } else if (c == '\\') {
                esc = true;
            } else if (c == '\'') {
                end = i;
                break;
            }
        }
        if (end == std::string::npos)
            continue;

        std::string value = line.substr(start + 1, end - (start + 1));

        size_t type_begin = line.find_first_not_of(' ', end + 1);
        if (type_begin == std::string::npos)
            continue;

        size_t type_end = line.find_first_of(' ', type_begin);
        std::string type = (type_end == std::string::npos)
                               ? line.substr(type_begin)
                               : line.substr(type_begin, type_end - type_begin);

        tokens.emplace_back(value, type);
    }
    return tokens;
}

} // namespace

TEST(Lexer, KeywordsAndIdentifiers)
{
    const std::string src = "int x; bool y; fn main { return; }";
    auto out = run_lexer_on(src);
    auto toks = parse_tokens(out);

    std::vector<std::pair<std::string, std::string>> expected = {
        {"int", "int"},
        {"x", "identifier"},
        {";", "semicolon_op"},
        {"bool", "bool"},
        {"y", "identifier"},
        {";", "semicolon_op"},
        {"fn", "fn"},
        {"main", "identifier"},
        {"{", "open_brace_op"},
        {"return", "return"},
        {";", "semicolon_op"},
        {"}", "close_brace_op"},
    };

    ASSERT_EQ(toks.size(), expected.size()) << out;
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(toks[i].first, expected[i].first) << out;
        EXPECT_EQ(toks[i].second, expected[i].second) << out;
    }
}

TEST(Lexer, IntegerLiterals)
{
    const std::string src = "0 012 0x1F 0b101 123";
    auto out = run_lexer_on(src);
    auto toks = parse_tokens(out);

    std::vector<std::pair<std::string, std::string>> expected = {
        {"0", "int_literal"},
        {"012", "int_literal"},
        {"0x1F", "int_literal"},
        {"0b101", "int_literal"},
        {"123", "int_literal"},
    };

    ASSERT_EQ(toks.size(), expected.size()) << out;
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(toks[i].first, expected[i].first) << out;
        EXPECT_EQ(toks[i].second, expected[i].second) << out;
    }
}

TEST(Lexer, StringAndCharLiterals)
{
    const std::string src = "\"hello\\nworld\" 'a'";
    auto out = run_lexer_on(src);
    auto toks = parse_tokens(out);

    std::vector<std::pair<std::string, std::string>> expected = {
        {"hello\\nworld", "string_literal"},
        {"a", "char_literal"},
    };

    ASSERT_EQ(toks.size(), expected.size()) << out;
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(toks[i].first, expected[i].first) << out;
        EXPECT_EQ(toks[i].second, expected[i].second) << out;
    }
}

TEST(Lexer, CommentsAreIgnored)
{
    const std::string src = "int x; // single line\n"
                            "x=1; /* multi\nline */ y=2;";
    auto out = run_lexer_on(src);
    auto toks = parse_tokens(out);

    std::vector<std::pair<std::string, std::string>> expected = {
        {"int", "int"},
        {"x", "identifier"},
        {";", "semicolon_op"},
        {"x", "identifier"},
        {"=", "assign_op"},
        {"1", "int_literal"},
        {";", "semicolon_op"},
        {"y", "identifier"},
        {"=", "assign_op"},
        {"2", "int_literal"},
        {";", "semicolon_op"},
    };

    ASSERT_EQ(toks.size(), expected.size()) << out;
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(toks[i].first, expected[i].first) << out;
        EXPECT_EQ(toks[i].second, expected[i].second) << out;
    }
}

TEST(Lexer, NegativeNumberAndOperators)
{
    const std::string src = "a=-42; b+=c; d&&e || !f;";
    auto out = run_lexer_on(src);
    auto toks = parse_tokens(out);

    std::vector<std::pair<std::string, std::string>> expected = {
        {"a", "identifier"},
        {"=", "assign_op"},
        {"-", "minus_op"},
        {"42", "int_literal"},
        {";", "semicolon_op"},
        {"b", "identifier"},
        {"+=", "plus_assign_op"},
        {"c", "identifier"},
        {";", "semicolon_op"},
        {"d", "identifier"},
        {"&&", "land_op"},
        {"e", "identifier"},
        {"||", "lor_op"},
        {"!", "lnot_op"},
        {"f", "identifier"},
        {";", "semicolon_op"},
    };

    ASSERT_EQ(toks.size(), expected.size()) << out;
    for (size_t i = 0; i < expected.size(); ++i) {
        EXPECT_EQ(toks[i].first, expected[i].first) << out;
        EXPECT_EQ(toks[i].second, expected[i].second) << out;
    }
}
