#include "symbol_table/mangling.hpp"
#include <optional>
#include <unordered_map>

static const std::unordered_map<std::string, std::string>
    unary_operator_mangling = {
        {"+", "ps"},  // unary plus
        {"-", "ng"},  // unary minus
        {"*", "de"},  // dereference
        {"&", "ad"},  // address-of
        {"~", "co"},  // complement
        {"!", "nt"},  // not
        {"++", "pp"}, // pre/post increment
        {"--", "mm"}  // pre/post decrement
};

static const std::unordered_map<std::string, std::string>
    binary_operator_mangling = {
        {"+", "pl"},   {"-", "mi"},  {"*", "ml"},  {"/", "dv"},  {"%", "rm"},
        {"^", "eo"},   {"&", "an"},  {"|", "or"},  {"=", "aS"},  {"+=", "pL"},
        {"-=", "mI"},  {"*=", "mL"}, {"/=", "dV"}, {"%=", "rM"}, {"^=", "eO"},
        {"&=", "aN"},  {"|=", "oR"}, {"<<", "ls"}, {">>", "rs"}, {"<<=", "lS"},
        {">>=", "rS"}, {"==", "eq"}, {"!=", "ne"}, {">", "gt"},  {"<", "lt"},
        {">=", "ge"},  {"<=", "le"}, {"&&", "aa"}, {"||", "oo"}, {",", "cm"},
        {".*", "pm"},  {"->", "pt"}};

std::optional<std::string> mangle_function_name(const std::string &name,
                                                const FunctionType &ftype,
                                                const FunctionMeta &meta,
                                                std::optional<ClassType> cls)
{
    std::string result = "_Z";
    if (cls.has_value()) {
        result += cls->mangled_name();
    }

    switch (meta.function_kind) {
    case FunctionKind::CONSTRUCTOR:
        result += "C1"; // C1 for constructor
        break;
    case FunctionKind::DESTRUCTOR:
        result += "D1"; // D1 for destructor
        break;
    case FunctionKind::OPERATOR: {
        bool is_unary = ftype.param_types.size() == 0;

        if (is_unary) {
            auto it = unary_operator_mangling.find(name);
            if (it != unary_operator_mangling.end()) {
                result += it->second;
            } else {
                return std::nullopt;
            }
        } else {
            auto it = binary_operator_mangling.find(name);
            if (it != binary_operator_mangling.end()) {
                result += it->second;
            } else {
                return std::nullopt;
            }
        }
        break;
    }
    case FunctionKind::NORMAL:
    case FunctionKind::METHOD:
    default:
        result += std::to_string(name.length()) + name; // <name-length><name>
        break;
    }

    if (ftype.param_types.empty() && !ftype.is_variadic) {
        result += "v"; // 'v' for void (no parameters)
    } else {
        for (const auto &param : ftype.param_types) {
            result += param.mangled_name();
        }
        if (ftype.is_variadic) {
            result += "z"; // 'z' for variadic
        }
    }
    return result;
}
