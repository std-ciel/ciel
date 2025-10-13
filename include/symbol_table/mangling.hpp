#ifndef MANGLING_HPP
#define MANGLING_HPP
#include "symbol_table/symbol.hpp"
#include <optional>
#include <string>

std::optional<std::string>
mangle_function_name(const std::string &name,
                     const FunctionType &ftype,
                     const FunctionMeta &meta,
                     std::optional<ClassType> cls = std::nullopt);
#endif // MANGLING_HPP
