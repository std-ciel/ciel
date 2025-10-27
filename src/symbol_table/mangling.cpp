#include "symbol_table/mangling.hpp"
#include <optional>
#include <string>
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
        {".*", "pm"},  {"->", "pt"}, {"[]", "ix"}, {"()", "cl"}};

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
        // Special case for operator() and operator[] - always treated as binary
        if (name == "()" || name == "[]") {
            auto it = binary_operator_mangling.find(name);
            if (it != binary_operator_mangling.end()) {
                result += it->second;
            } else {
                return std::nullopt;
            }
        } else {
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

std::string get_function_prefix(const std::string &mangled_name,
                                ScopeID body_scope_id)
{
    return "__" + mangled_name + "_" + std::to_string(body_scope_id) + "_";
}

std::string mangle_temporary_name(const std::string &mangled_name,
                                  ScopeID body_scope_id,
                                  int counter)
{
    return get_function_prefix(mangled_name, body_scope_id) + "t" +
           std::to_string(counter) + "__";
}

std::string tac_mangle_label_name(const std::string &mangled_name,
                                  ScopeID body_scope_id,
                                  const std::string &prefix,
                                  int counter)
{
    return get_function_prefix(mangled_name, body_scope_id) + prefix +
           std::to_string(counter) + "__";
}

std::string tac_get_entry_label(const std::string &mangled_name,
                                ScopeID body_scope_id)
{
    return get_function_prefix(mangled_name, body_scope_id) + "entry__";
}

std::string tac_get_exit_label(const std::string &mangled_name,
                               ScopeID body_scope_id)
{
    return get_function_prefix(mangled_name, body_scope_id) + "exit__";
}

std::string mangle_local_static_name(const std::string &function_mangled_name,
                                     ScopeID function_scope_id,
                                     const std::string &var_name)
{
    return "__ZL" + function_mangled_name + "_" +
           std::to_string(function_scope_id) + "_" + var_name + "__";
}

std::string
mangle_local_static_guard_name(const std::string &function_mangled_name,
                               ScopeID function_scope_id,
                               const std::string &var_name)
{
    return "__ZGL" + function_mangled_name + "_" +
           std::to_string(function_scope_id) + "_" + var_name + "__";
}
