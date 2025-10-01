#include "symbol_table/symbol_table.hpp"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <string>

SymbolTable::SymbolTable()
    : next_scope_id(0), current_scope_id(0), current_scope_level(0)
{
    enter_scope();
}

void SymbolTable::enter_scope()
{
    ScopeID parent_id = current_scope_id;
    current_scope_level++;
    current_scope_id = next_scope_id++;

    scopes.emplace(current_scope_id,
                   Scope(current_scope_id, current_scope_level, parent_id));
    scope_stack.push_back(current_scope_id);
}

void SymbolTable::exit_scope()
{
    if (scope_stack.size() <= 1) {
        return;
    }

    scope_stack.pop_back();
    current_scope_id = scope_stack.back();
    current_scope_level--;
}

bool SymbolTable::add_symbol(const std::string &name,
                             QualType type,
                             StorageClass storage_class)
{
    auto scope_iter = scopes.find(current_scope_id);
    if (scope_iter == scopes.end()) {
        return false;
    }

    Scope &scope = scope_iter->second;
    if (scope.symbols.find(name) != scope.symbols.end()) {
        return false;
    }

    if (current_scope_id == GLOBAL_SCOPE_ID &&
        storage_class == StorageClass::AUTO) {
        storage_class = StorageClass::STATIC;
        // TODO: Warn that global symbols default to static
    }

    SymbolPtr symbol = std::make_shared<Symbol>(name,
                                                type,
                                                storage_class,
                                                current_scope_id,
                                                scope.parent_id);
    scope.symbols[name] = symbol;

    for (auto it = scope_stack.rbegin(); it != scope_stack.rend(); ++it) {
        ScopeID scope_id = *it;
        auto sit = scopes.find(scope_id);
        if (sit != scopes.end()) {
            Scope &s = sit->second;
            if (s.is_present(name)) {
                // TODO: Warn that symbol shadows another symbol in outer scope
            }
        }
    }
    return true;
}

std::optional<SymbolPtr>
SymbolTable::lookup_symbol(const std::string &name) const
{
    for (auto scope_id_it = scope_stack.rbegin();
         scope_id_it != scope_stack.rend();
         ++scope_id_it) {
        size_t scope_id = *scope_id_it;
        auto scope_iter = scopes.find(scope_id);
        if (scope_iter != scopes.end()) {
            const Scope &scope = scope_iter->second;
            auto symbol_iter = scope.symbols.find(name);

            if (symbol_iter != scope.symbols.end()) {
                return symbol_iter->second;
            }
        }
    }

    return std::nullopt;
}

void SymbolTable::print_symbols() const
{
    // Compute column widths
    std::size_t w_scope = std::string("Scope ID").size();
    std::size_t w_level = std::string("Level").size();
    std::size_t w_parent = std::string("Parent ID").size();
    std::size_t w_name = std::string("Symbol Name").size();
    std::size_t w_type = std::string("Type").size();
    std::size_t w_storage = std::string("Storage Class").size();

    for (const auto &scope_pair : scopes) {
        const Scope &scope = scope_pair.second;
        if (scope.symbols.empty()) {
            continue; // skip empty scopes entirely
        }
        w_scope = std::max(w_scope, std::to_string(scope.id).size());
        w_level = std::max(w_level, std::to_string(scope.level).size());
        w_parent = std::max(w_parent, std::to_string(scope.parent_id).size());
        for (const auto &symbol_pair : scope.symbols) {
            const SymbolPtr &symbol = symbol_pair.second;
            w_name = std::max(w_name, symbol->get_name().size());
            std::string type_str = symbol->get_type().type
                                       ? symbol->get_type().debug_name()
                                       : "nullptr";
            w_type = std::max(w_type, type_str.size());
            std::string storage_str =
                storage_class_to_string(symbol->get_storage_class());
            w_storage = std::max(w_storage, storage_str.size());
        }
    }

    auto make_sep = [&](char fill = '-') {
        std::string s;
        s.reserve(w_scope + w_level + w_parent + w_name + w_type + w_storage +
                  16);
        s.push_back('+');
        s.append(w_scope + 2, fill);
        s.push_back('+');
        s.append(w_level + 2, fill);
        s.push_back('+');
        s.append(w_parent + 2, fill);
        s.push_back('+');
        s.append(w_name + 2, fill);
        s.push_back('+');
        s.append(w_type + 2, fill);
        s.push_back('+');
        s.append(w_storage + 2, fill);
        s.push_back('+');
        return s;
    };

    const std::string sep = make_sep();

    // Header
    std::cout << sep << '\n'
              << '|' << ' ' << std::left << std::setw(w_scope) << "Scope ID"
              << ' ' << '|' << ' ' << std::left << std::setw(w_level) << "Level"
              << ' ' << '|' << ' ' << std::left << std::setw(w_parent)
              << "Parent ID" << ' ' << '|' << ' ' << std::left
              << std::setw(w_name) << "Symbol Name" << ' ' << '|' << ' '
              << std::left << std::setw(w_type) << "Type" << ' ' << '|' << ' '
              << std::left << std::setw(w_storage) << "Storage Class" << ' '
              << '|' << '\n'
              << sep << '\n';

    // Rows
    for (const auto &scope_pair : scopes) {
        const Scope &scope = scope_pair.second;
        if (scope.symbols.empty()) {
            continue; // skip empty scopes entirely
        }
        for (const auto &symbol_pair : scope.symbols) {
            const SymbolPtr &symbol = symbol_pair.second;
            std::string type_str = symbol->get_type().type
                                       ? symbol->get_type().debug_name()
                                       : "nullptr";
            std::string storage_str =
                storage_class_to_string(symbol->get_storage_class());

            std::cout << '|' << ' ' << std::left << std::setw(w_scope)
                      << scope.id << ' ' << '|' << ' ' << std::left
                      << std::setw(w_level) << scope.level << ' ' << '|' << ' '
                      << std::left << std::setw(w_parent) << scope.parent_id
                      << ' ' << '|' << ' ' << std::left << std::setw(w_name)
                      << symbol->get_name() << ' ' << '|' << ' ' << std::left
                      << std::setw(w_type) << type_str << ' ' << '|' << ' '
                      << std::left << std::setw(w_storage) << storage_str << ' '
                      << '|' << '\n';
        }
    }

    std::cout << sep << '\n';
}
