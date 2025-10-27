#include "local_static_pass/local_static_pass.hpp"
#include "symbol_table/mangling.hpp"
#include <iostream>

LocalStaticPass::LocalStaticPass(SymbolTable &symbol_table,
                                 TypeFactory &type_factory)
    : symbol_table(symbol_table), type_factory(type_factory),
      current_function_scope_id(0)
{
}

void LocalStaticPass::process(
    std::vector<ASTNodePtr> &translation_unit,
    std::vector<std::shared_ptr<FunctionDef>> &class_methods)
{
    for (const auto &node : translation_unit) {
        if (node && node->type == ASTNodeType::FUNCTION_DEF) {
            process_function(node);
        }
    }
    process_class_methods(class_methods);
}

void LocalStaticPass::process_class_methods(
    std::vector<std::shared_ptr<FunctionDef>> &class_methods)
{
    for (const auto &method : class_methods) {
        if (method) {
            process_function(method);
        }
    }
}

void LocalStaticPass::process_function(ASTNodePtr func_def)
{
    auto func = std::static_pointer_cast<FunctionDef>(func_def);

    const auto &func_meta = func->function_symbol->get_function_meta();
    if (!func_meta.has_value()) {
        return;
    }

    current_function_mangled_name = func_meta->mangled_name;
    current_function_scope_id = func_meta->body_scope_id;

    if (func->body) {
        process_statement(func->body,
                          current_function_mangled_name,
                          current_function_scope_id);
    }
}

void LocalStaticPass::process_block(ASTNodePtr block,
                                    const std::string &func_mangled_name,
                                    ScopeID func_scope_id)
{
    if (!block) {
        return;
    }

    std::vector<ASTNodePtr> *statements = nullptr;

    if (block->type == ASTNodeType::BLOCK_STMT) {
        auto block_stmt = std::static_pointer_cast<BlockStmt>(block);
        statements = &block_stmt->statements;
    } else if (block->type == ASTNodeType::COMPOUND_STMT) {
        auto compound_stmt = std::static_pointer_cast<CompoundStmt>(block);
        statements = &compound_stmt->statements;
    } else {
        return;
    }

    for (size_t i = 0; i < statements->size(); ++i) {
        auto &stmt = (*statements)[i];

        if (stmt && stmt->type == ASTNodeType::ASSIGNMENT_EXPR) {
            auto assign = std::static_pointer_cast<AssignmentExpr>(stmt);

            if (assign->is_static_assignment &&
                assign->target->type == ASTNodeType::IDENTIFIER_EXPR) {

                auto id_expr =
                    std::static_pointer_cast<IdentifierExpr>(assign->target);
                auto symbol = id_expr->symbol;

                // Verify it's a local static (not global)
                if (symbol->get_storage_class() == StorageClass::STATIC &&
                    symbol->get_scope_id() != GLOBAL_SCOPE_ID) {

                    (*statements)[i] = transform_local_static(symbol,
                                                              assign->value,
                                                              func_mangled_name,
                                                              func_scope_id);
                    continue;
                }
            }
        }

        // Recurse into other statements
        process_statement(stmt, func_mangled_name, func_scope_id);
    }
}

void LocalStaticPass::process_statement(ASTNodePtr stmt,
                                        const std::string &func_mangled_name,
                                        ScopeID func_scope_id)
{
    if (!stmt) {
        return;
    }

    switch (stmt->type) {
    case ASTNodeType::BLOCK_STMT:
    case ASTNodeType::COMPOUND_STMT:
        process_block(stmt, func_mangled_name, func_scope_id);
        break;

    case ASTNodeType::IF_STMT: {
        auto if_stmt = std::static_pointer_cast<IfStmt>(stmt);
        process_statement(if_stmt->then_branch,
                          func_mangled_name,
                          func_scope_id);
        if (if_stmt->else_branch.has_value()) {
            process_statement(*if_stmt->else_branch,
                              func_mangled_name,
                              func_scope_id);
        }
        break;
    }

    case ASTNodeType::WHILE_STMT: {
        auto while_stmt = std::static_pointer_cast<WhileStmt>(stmt);
        process_statement(while_stmt->body, func_mangled_name, func_scope_id);
        break;
    }

    case ASTNodeType::DO_WHILE_STMT: {
        auto do_while = std::static_pointer_cast<DoWhileStmt>(stmt);
        process_statement(do_while->body, func_mangled_name, func_scope_id);
        break;
    }

    case ASTNodeType::UNTIL_STMT: {
        auto until = std::static_pointer_cast<UntilStmt>(stmt);
        process_statement(until->body, func_mangled_name, func_scope_id);
        break;
    }

    case ASTNodeType::FOR_STMT: {
        auto for_stmt = std::static_pointer_cast<ForStmt>(stmt);
        if (for_stmt->initializer.has_value()) {
            process_statement(*for_stmt->initializer,
                              func_mangled_name,
                              func_scope_id);
        }
        process_statement(for_stmt->body, func_mangled_name, func_scope_id);
        break;
    }

    case ASTNodeType::SWITCH_STMT: {
        auto switch_stmt = std::static_pointer_cast<SwitchStmt>(stmt);
        for (const auto &case_stmt : switch_stmt->cases) {
            process_statement(case_stmt, func_mangled_name, func_scope_id);
        }
        if (switch_stmt->default_case.has_value()) {
            process_statement(*switch_stmt->default_case,
                              func_mangled_name,
                              func_scope_id);
        }
        break;
    }

    case ASTNodeType::CASE_STMT: {
        auto case_stmt = std::static_pointer_cast<CaseStmt>(stmt);
        process_statement(case_stmt->statement,
                          func_mangled_name,
                          func_scope_id);
        break;
    }

    case ASTNodeType::DEFAULT_STMT: {
        auto default_stmt = std::static_pointer_cast<DefaultStmt>(stmt);
        process_statement(default_stmt->statement,
                          func_mangled_name,
                          func_scope_id);
        break;
    }

    case ASTNodeType::LABEL_STMT: {
        auto label = std::static_pointer_cast<LabelStmt>(stmt);
        process_statement(label->statement, func_mangled_name, func_scope_id);
        break;
    }

    default:
        break;
    }
}

ASTNodePtr
LocalStaticPass::transform_local_static(SymbolPtr symbol,
                                        ASTNodePtr initializer,
                                        const std::string &func_mangled_name,
                                        ScopeID func_scope_id)
{
    if (moved_symbols.find(symbol) != moved_symbols.end()) {
        // Already transformed, just create the guarded init with existing
        // symbol
        auto guard_name = mangle_local_static_guard_name(func_mangled_name,
                                                         func_scope_id,
                                                         symbol->get_name());

        auto guard_symbol_opt = symbol_table.lookup_symbol(guard_name);
        if (!guard_symbol_opt.has_value()) {
            return initializer;
        }

        return create_guarded_init(symbol, *guard_symbol_opt, initializer);
    }

    moved_symbols.insert(symbol);

    std::string mangled_name = mangle_local_static_name(func_mangled_name,
                                                        func_scope_id,
                                                        symbol->get_name());
    std::string guard_name = mangle_local_static_guard_name(func_mangled_name,
                                                            func_scope_id,
                                                            symbol->get_name());

    ScopeID original_scope = symbol->get_scope_id();
    auto remove_result = symbol_table.remove_symbol(symbol);
    if (remove_result.is_err()) {
        std::cerr << "Warning: Failed to remove local static symbol '"
                  << symbol->get_name() << "' from scope " << original_scope
                  << std::endl;
        return initializer;
    }

    symbol->set_name(mangled_name);
    symbol->set_scope_id(GLOBAL_SCOPE_ID);
    symbol->set_parent_scope(GLOBAL_SCOPE_ID);

    auto add_result =
        symbol_table.add_symbol_in_scope(mangled_name, symbol, GLOBAL_SCOPE_ID);

    if (add_result.is_err()) {
        std::cerr << "Warning: Failed to add global symbol '" << mangled_name
                  << "' to global scope" << std::endl;
        return initializer; // Return original on error
    }

    auto guard_symbol = std::make_shared<Symbol>(
        guard_name,
        QualifiedType(type_factory.get_builtin_type("bool").value(),
                      Qualifier::NONE),
        StorageClass::STATIC,
        GLOBAL_SCOPE_ID,
        GLOBAL_SCOPE_ID);

    auto guard_result = symbol_table.add_symbol_in_scope(guard_name,
                                                         guard_symbol,
                                                         GLOBAL_SCOPE_ID);

    if (guard_result.is_err()) {
        std::cerr << "Warning: Failed to add guard symbol '" << guard_name
                  << "' to global scope" << std::endl;
    }

    return create_guarded_init(symbol, guard_symbol, initializer);
}

ASTNodePtr LocalStaticPass::create_guarded_init(SymbolPtr symbol,
                                                SymbolPtr guard_symbol,
                                                ASTNodePtr initializer)
{
    // Create: if (!guard_var) { var = init; guard_var = true; }

    // 1. Guard variable identifier for condition
    auto guard_id_expr =
        std::make_shared<IdentifierExpr>(guard_symbol,
                                         guard_symbol->get_type().type);

    // 2. Logical NOT of guard (!guard)
    auto not_guard = std::make_shared<UnaryExpr>(
        Operator::LOGICAL_NOT,
        guard_id_expr,
        type_factory.get_builtin_type("bool").value());

    // 3. Variable identifier for assignment
    auto var_id_expr =
        std::make_shared<IdentifierExpr>(symbol, symbol->get_type().type);

    // 4. Assignment: var = init
    auto var_assign = std::make_shared<AssignmentExpr>(Operator::ASSIGN,
                                                       var_id_expr,
                                                       initializer,
                                                       symbol->get_type().type);

    // 5. Guard identifier for setting to true
    auto guard_id_expr2 =
        std::make_shared<IdentifierExpr>(guard_symbol,
                                         guard_symbol->get_type().type);

    // 6. True literal
    auto true_literal = std::make_shared<LiteralExpr>(
        true,
        type_factory.get_builtin_type("bool").value());

    // 7. Assignment: guard_var = true
    auto guard_assign = std::make_shared<AssignmentExpr>(
        Operator::ASSIGN,
        guard_id_expr2,
        true_literal,
        type_factory.get_builtin_type("bool").value());

    // 8. Block containing both assignments
    std::vector<ASTNodePtr> then_stmts;
    then_stmts.push_back(var_assign);
    then_stmts.push_back(guard_assign);
    auto then_block = std::make_shared<BlockStmt>(std::move(then_stmts));

    // 9. If statement with negated guard
    return std::make_shared<IfStmt>(not_guard, then_block, std::nullopt);
}
