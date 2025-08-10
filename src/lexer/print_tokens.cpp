#include "print_tokens.hpp"

#include <algorithm>
#include <cctype>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#include "token_type.hpp"

namespace {
std::string render_lexeme(const std::string &in, std::size_t max_len = 40)
{
    std::string out;
    out.reserve(std::min<std::size_t>(in.size() + 8, max_len + 3));
    auto append_hex = [&](unsigned char c) {
        static const char *hex = "0123456789abcdef";
        out += "\\x";
        out.push_back(hex[(c >> 4) & 0xF]);
        out.push_back(hex[c & 0xF]);
    };

    for (unsigned char c : in) {
        if (out.size() >= max_len) {
            out += "..."; // ellipsis
            break;
        }
        
        switch (c) {
        case '\\':
            out += "\\";
            break;
        case '\a':
            out += "\\a";
            break;
        case '\b':
            out += "\\b";
            break;
        case '\f':
            out += "\\f";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\t':
            out += "\\t";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\v':
            out += "\\v";
            break;
        case '"':
            out += "\"";
            break;
        default:
            if (std::isprint(c)) {
                out += c;
            } else {
                append_hex(c);
            }
        }
    }
    return out;
}

std::size_t digits_for(std::size_t v)
{
    std::size_t d = 1;
    while (v >= 10) {
        v /= 10;
        ++d;
    }
    return d;
}
} // namespace

void print_tokens_table(const std::vector<Token> &tokens, std::ostream &os)
{
    std::size_t idx_w = std::max<std::size_t>(3, digits_for(tokens.size()));
    std::size_t lex_w = std::string("Lexeme").size();
    std::size_t type_w = std::string("Type").size();
    std::size_t line_w = std::string("Line").size();
    std::size_t col_w = std::string("Col").size();

    std::vector<std::string> rendered_lex;
    rendered_lex.reserve(tokens.size());

    int max_line = 0, max_col = 0;
    for (const auto &t : tokens) {
        auto s = render_lexeme(t.lexeme);
        lex_w = std::max<std::size_t>(lex_w, s.size());
        type_w = std::max<std::size_t>(type_w, to_string(t.type).size());
        max_line = std::max(max_line, t.line);
        max_col = std::max(max_col, t.column);
        rendered_lex.emplace_back(std::move(s));
    }
    line_w = std::max<std::size_t>(line_w, std::to_string(max_line).size());
    col_w = std::max<std::size_t>(col_w, std::to_string(max_col).size());

    auto print_sep = [&]() {
        auto dash = [&](std::size_t n) {
            for (std::size_t i = 0; i < n; ++i)
                os << '-';
        };
        os << '+';
        dash(idx_w + 2);
        os << '+';
        dash(lex_w + 2);
        os << '+';
        dash(type_w + 2);
        os << '+';
        dash(line_w + 2);
        os << '+';
        dash(col_w + 2);
        os << "+\n";
    };

    auto setw_i = [](std::size_t w) { return std::setw(static_cast<int>(w)); };

    print_sep();
    os << "| " << setw_i(idx_w) << std::right << "Idx"
       << " | " << setw_i(lex_w) << std::left << "Lexeme"
       << " | " << setw_i(type_w) << std::left << "Type"
       << " | " << setw_i(line_w) << std::right << "Line"
       << " | " << setw_i(col_w) << std::right << "Col" << " |\n";
    print_sep();

    for (std::size_t i = 0; i < tokens.size(); ++i) {
        const auto &t = tokens[i];
        const auto &lx = rendered_lex[i];
        os << "| " << setw_i(idx_w) << std::right << (i + 1) << " | "
           << setw_i(lex_w) << std::left << lx << " | " << setw_i(type_w)
           << std::left << to_string(t.type) << " | " << setw_i(line_w)
           << std::right << t.line << " | " << setw_i(col_w) << std::right
           << t.column << " |\n";
    }
    print_sep();
    os << tokens.size() << " token" << (tokens.size() == 1 ? "" : "s") << "\n";
}
