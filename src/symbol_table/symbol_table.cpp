#include "symbol_table/symbol_table.hpp"
#include <algorithm>
#include <iomanip>
#include <iostream>

SymbolTable::SymbolTable()
    : next_scope_id(0), current_scope_id(0), current_scope_level(0)
{
    enter_scope();
}

void SymbolTable::enter_scope()
{
    size_t parent_id = current_scope_id;
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
                             const std::string &type_name)
{
    auto &current_scope = scopes.at(current_scope_id);
    if (current_scope.symbols.find(name) != current_scope.symbols.end()) {
        return false;
    }

    auto type_ptr = type_factory.lookup_type(type_name, scope_stack);
    if (!type_ptr.has_value()) {
        return false;
    }

    auto symbol = std::make_shared<Symbol>(name,
                                           type_ptr.value(),
                                           current_scope_id,
                                           current_scope.parent_id);
    current_scope.symbols[name] = symbol;
    return true;
}

bool SymbolTable::add_symbol(const std::string &name, TypePtr type)
{
    if (!type) {
        return false;
    }

    auto &current_scope = scopes.at(current_scope_id);
    if (current_scope.symbols.find(name) != current_scope.symbols.end()) {
        return false;
    }

    auto symbol = std::make_shared<Symbol>(name,
                                           type,
                                           current_scope_id,
                                           current_scope.parent_id);
    current_scope.symbols[name] = symbol;
    return true;
}

// New method with TypeId
bool SymbolTable::add_symbol(const std::string &name, TypeId type_id)
{
    auto type_ptr = type_factory.get_type_by_id(type_id);
    if (!type_ptr.has_value()) {
        return false;
    }

    return add_symbol(name, type_ptr.value());
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
            std::string type_str =
                symbol->get_type() ? symbol->get_type()->name : "nullptr";
            w_type = std::max(w_type, type_str.size());
        }
    }

    auto make_sep = [&](char fill = '-') {
        std::string s;
        s.reserve(w_scope + w_level + w_parent + w_name + w_type + 13);
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
              << std::left << std::setw(w_type) << "Type" << ' ' << '|' << '\n'
              << sep << '\n';

    // Rows
    for (const auto &scope_pair : scopes) {
        const Scope &scope = scope_pair.second;
        if (scope.symbols.empty()) {
            continue; // skip empty scopes entirely
        }
        for (const auto &symbol_pair : scope.symbols) {
            const SymbolPtr &symbol = symbol_pair.second;
            std::string type_str =
                symbol->get_type() ? symbol->get_type()->name : "nullptr";

            std::cout << '|' << ' ' << std::left << std::setw(w_scope)
                      << scope.id << ' ' << '|' << ' ' << std::left
                      << std::setw(w_level) << scope.level << ' ' << '|' << ' '
                      << std::left << std::setw(w_parent) << scope.parent_id
                      << ' ' << '|' << ' ' << std::left << std::setw(w_name)
                      << symbol->get_name() << ' ' << '|' << ' ' << std::left
                      << std::setw(w_type) << type_str << ' ' << '|' << '\n';
        }
    }

    std::cout << sep << '\n';
}

void SymbolTable::print_custom_types() const
{
    // Compute column widths
    std::size_t w_id = std::string("Type ID").size();
    std::size_t w_name = std::string("Type Name").size();
    std::size_t w_category = std::string("Category").size();
    std::size_t w_qualifier = std::string("Qualifier").size();
    bool is_empty = true;
    for (const auto &type_pair : type_factory.get_defined_types()) {
        const TypePtr &type = type_pair.second;
        if (type->category != TypeCategory::POINTER &&
            type->category != TypeCategory::ARRAY &&
            type->category != TypeCategory::PRIMITIVE &&
            type->category != TypeCategory::LABEL) {
            w_id = std::max(w_id, std::to_string(type->id).size());
            w_name = std::max(w_name, type->name.size());
            w_category =
                std::max(w_category,
                         type_category_to_string(type->category).size());
            w_qualifier =
                std::max(w_qualifier,
                         type_qualifier_to_string(type->type_qualifier).size());
            is_empty = false;
        }
    }

    auto make_sep = [&](char fill = '-') {
        std::string s;
        s.reserve(w_id + w_name + w_category + w_qualifier + 13);
        s.push_back('+');
        s.append(w_id + 2, fill);
        s.push_back('+');
        s.append(w_name + 2, fill);
        s.push_back('+');
        s.append(w_category + 2, fill);
        s.push_back('+');
        s.append(w_qualifier + 2, fill);
        s.push_back('+');
        return s;
    };

    const std::string sep = make_sep();

    if (is_empty) {
        std::cout << "No custom types defined.\n";
        return;
    }
    // Header
    std::cout << sep << '\n'
              << '|' << ' ' << std::left << std::setw(w_id) << "Type ID" << ' '
              << '|' << ' ' << std::left << std::setw(w_name) << "Type Name"
              << ' ' << '|' << ' ' << std::left << std::setw(w_category)
              << "Category" << ' ' << '|' << ' ' << std::left
              << std::setw(w_qualifier) << "Qualifier" << ' ' << '|' << '\n'
              << sep << '\n';

    // Rows
    for (const auto &type_pair : type_factory.get_defined_types()) {
        const TypePtr &type = type_pair.second;
        if (type->category == TypeCategory::POINTER ||
            type->category == TypeCategory::ARRAY ||
            type->category == TypeCategory::PRIMITIVE) {
            continue;
        }
        std::cout << '|' << ' ' << std::left << std::setw(w_id) << type->id
                  << ' ' << '|' << ' ' << std::left << std::setw(w_name)
                  << type->name << ' ' << '|' << ' ' << std::left
                  << std::setw(w_category)
                  << type_category_to_string(type->category) << ' ' << '|'
                  << ' ' << std::left << std::setw(w_qualifier)
                  << type_qualifier_to_string(type->type_qualifier) << ' '
                  << '|' << '\n';
    }

    std::cout << sep << '\n';
}
