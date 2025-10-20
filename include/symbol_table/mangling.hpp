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

// Generate function-specific prefix: __<mangled_name>_<scope_id>_
std::string get_function_prefix(const std::string &mangled_name,
                                ScopeID body_scope_id);

// Generate mangled temporary name: __<mangled_name>_<scope_id>_t<counter>__
std::string mangle_temporary_name(const std::string &mangled_name,
                                  ScopeID body_scope_id,
                                  int counter);

// Generate mangled label name: __<mangled_name>_<scope_id>_<prefix><counter>__
std::string tac_mangle_label_name(const std::string &mangled_name,
                                  ScopeID body_scope_id,
                                  const std::string &prefix,
                                  int counter);

// Generate entry label: __<mangled_name>_<scope_id>_entry__
std::string tac_get_entry_label(const std::string &mangled_name,
                                ScopeID body_scope_id);

// Generate exit label: __<mangled_name>_<scope_id>_exit__
std::string tac_get_exit_label(const std::string &mangled_name,
                               ScopeID body_scope_id);

// Generate mangled local static name:
// __ZL<function_mangled_name>_<function_scope_id>_<var_name>__
std::string mangle_local_static_name(const std::string &function_mangled_name,
                                     ScopeID function_scope_id,
                                     const std::string &var_name);

// Generate mangled local static guard name:
// __ZGL<function_mangled_name>_<function_scope_id>_<var_name>__
std::string
mangle_local_static_guard_name(const std::string &function_mangled_name,
                               ScopeID function_scope_id,
                               const std::string &var_name);

#endif // MANGLING_HPP
