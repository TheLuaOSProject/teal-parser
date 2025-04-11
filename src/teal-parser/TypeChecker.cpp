#include "TypeChecker.hpp"
#include <algorithm>   // for std::all_of
#include <iostream>    // For debug prints (remove later)
#include <set>
#include <stdexcept>   // For runtime_error

using namespace teal::parser;
using namespace teal::parser::typechecker;

// -----------------------------------------------------------------------------
// Constructor and Main Check
// -----------------------------------------------------------------------------
TypeChecker::TypeChecker() { load_stdlib(); }

std::vector<CheckerError> TypeChecker::check(ast::Block *root)
{
    errors.clear();
    while (not current_function_type.empty()) current_function_type.pop(); // Clear state from previous checks
    // Start checking from the root node
    visit(root);
    // Perform any final checks (e.g., unused variables in global scope)
    if (symbol_table.get_global_scope()) {
        // TODO: Add checks for unused globals if desired
    }
    return errors;
}

// -----------------------------------------------------------------------------
// Error Handling
// -----------------------------------------------------------------------------
void TypeChecker::add_error(const CheckerError::Kind_t &detail, const ast::ASTNode &location_node)
{
    errors.emplace_back(detail, location_node.line, location_node.column);
    // Optional immediate debug printing:
    // std::cerr << "Error (" << location_node.line << ":" << location_node.column << "): "
    //           << std::visit([](const auto& err){ return err.to_string(); }, detail) << std::endl;
}

void TypeChecker::add_error(const CheckerError::Kind_t &detail, size_t line, size_t col) { errors.emplace_back(detail, line, col); }

// -----------------------------------------------------------------------------
// Type Resolution (unchanged from original)
// -----------------------------------------------------------------------------
std::shared_ptr<Type> TypeChecker::resolve_type_node(ast::TypeNode *type_node)
{
    if (not type_node) return get_any_type(*type_node); // Or invalid? Treat missing type as 'any' for now

    if (auto *basic = dynamic_cast<ast::BasicTypeNode *>(type_node)) { return get_primitive_type(basic->name, *basic); }
    if (auto *nominal = dynamic_cast<ast::NominalTypeNode *>(type_node)) {
        std::string full_name;
        if (not nominal->name_parts.empty()) full_name = nominal->name_parts[0];
        auto symbol_opt = symbol_table.find_symbol(full_name);
        if (symbol_opt and (*symbol_opt)->is_type_alias) {
            return (*symbol_opt)->type; // Might require later resolution
        } else if (symbol_opt) {
            add_error(TypeErrorDetail("'" + full_name + "' is not a type"), *nominal);
            return get_invalid_type(*nominal);
        } else {
            add_error(UndefinedVariableError(full_name + " (as type)"), *nominal);
            return get_invalid_type(*nominal);
        }
    }
    if (auto *table = dynamic_cast<ast::TableTypeNode *>(type_node)) {
        if (table->is_map) {
            if (not table->key_type or table->element_types.size() != 1) {
                add_error(TypeErrorDetail("Invalid map type definition"), *table);
                return get_invalid_type(*table);
            }
            auto key_t = resolve_type_node(table->key_type.get());
            auto val_t = resolve_type_node(table->element_types[0].get());
            return std::make_shared<MapType>(*table, key_t, val_t);
        } else { // Array or Tuple
            if (table->element_types.empty()) {
                add_error(TypeErrorDetail("Array/Tuple type needs at least one element type"), *table);
                return get_invalid_type(*table);
            }
            if (table->element_types.size() == 1 and not table->key_type) { // Array {T}
                auto elem_t = resolve_type_node(table->element_types[0].get());
                return std::make_shared<ArrayType>(*table, elem_t);
            } else { // Tuple {T1, T2, ...}
                std::vector<std::shared_ptr<Type>> types;
                for (const auto &elem_node : table->element_types) { types.push_back(resolve_type_node(elem_node.get())); }
                return std::make_shared<TupleType>(*table, std::move(types));
            }
        }
    }
    if (auto *func = dynamic_cast<ast::FunctionTypeNode *>(type_node)) {
        std::vector<FunctionParameter> params;
        bool is_va_param = false;
        for (const auto &p_node : func->parameters) {
            if (is_va_param) {
                add_error(TypeErrorDetail("Vararg '...' must be the last parameter type"), *func);
                return get_invalid_type(*func);
            }
            auto p_type = resolve_type_node(p_node.type.get());
            params.push_back({ p_node.name.value_or(""), p_type, p_node.is_optional, false /*is_va_param*/ });
        }
        auto return_tuple = resolve_type_list(func->return_types, *func);
        return_tuple->is_variadic = func->varadict_return;
        return std::make_shared<FunctionType>(*func, std::move(params), return_tuple);
    }
    if (auto *union_node = dynamic_cast<ast::UnionTypeNode *>(type_node)) {
        std::vector<std::shared_ptr<Type>> options;
        for (const auto &opt_node : union_node->options) { options.push_back(resolve_type_node(opt_node.get())); }
        return UnionType::create_union(*union_node, options);
    }
    if (auto *record_node = dynamic_cast<ast::TypeRecordNode *>(type_node)) {
        auto rec_type = std::make_shared<RecordType>(*record_node, "<anonymous record@" + std::to_string(record_node->line) + ">");
        populate_record_type(rec_type.get(), record_node->body.get());
        return rec_type;
    }
    if (auto *enum_node = dynamic_cast<ast::TypeEnumNode *>(type_node)) {
        auto enum_type = std::make_shared<EnumType>(
            *enum_node, "<anonymous enum@" + std::to_string(enum_node->line) + ">",
            std::set<std::string>(enum_node->elements.begin(), enum_node->elements.end())
        );
        return enum_type;
    }
    add_error(TypeErrorDetail("Unhandled TypeNode kind"), *type_node);
    return get_invalid_type(*type_node);
}

std::shared_ptr<TupleType> TypeChecker::resolve_type_list(
    const std::vector<std::unique_ptr<ast::TypeNode>> &type_nodes, const ast::ASTNode &fallback_node
)
{
    std::vector<std::shared_ptr<Type>> types;
    for (const auto &node : type_nodes) { types.push_back(resolve_type_node(node.get())); }
    return std::make_shared<TupleType>(fallback_node, std::move(types));
}

std::shared_ptr<FunctionType> TypeChecker::resolve_function_body(ast::FunctionBody *body)
{
    std::vector<FunctionParameter> params;
    bool is_va_param = false;
    for (const auto &p_node : body->parameters) {
        if (is_va_param) { add_error(TypeErrorDetail("Vararg '...' must be the last parameter"), *body); }
        auto p_type = resolve_type_node(p_node.type.get());
        is_va_param = p_node.is_varadict;
        params.push_back({ p_node.name, p_type, p_node.is_optional, is_va_param });
    }
    auto return_tuple = resolve_type_list(body->return_types, *body);
    return_tuple->is_variadic = body->varadict_return;
    return std::make_shared<FunctionType>(*body, std::move(params), return_tuple);
}

void TypeChecker::populate_record_type(RecordType *record_type, ast::RecordBody *body)
{
    for (const auto &entry : body->entries) {
        if (entry.entry_kind == ast::RecordBody::Entry::Kind::FIELD) {
            std::string field_name;
            if (entry.name) {
                field_name = *entry.name;
            } else if (entry.key_literal) {
                field_name = *entry.key_literal;
            } else {
                add_error(TypeErrorDetail("Record field entry is missing name or key literal"), *body);
                continue;
            }
            if (record_type->fields.count(field_name) or record_type->metamethods.count(field_name)) {
                add_error(TypeErrorDetail("Duplicate field/metamethod name '" + field_name + "' in record"), *body);
                continue;
            }
            auto field_type = resolve_type_node(entry.type.get());
            if (entry.is_metamethod) {
                if (auto *_ = field_type->as<FunctionType>()) {
                    record_type->metamethods[field_name] = std::static_pointer_cast<FunctionType>(field_type);
                } else {
                    add_error(TypeErrorDetail("Metamethod '" + field_name + "' must be a function type"), *body);
                }
            } else {
                record_type->fields[field_name] = { field_name, field_type };
            }
        } else if (entry.entry_kind == ast::RecordBody::Entry::Kind::USERDATA) {
            record_type->is_userdata_proxy = true;
        }
        // Nested type definitions (TYPE_ALIAS, RECORD, ENUM, INTERFACE) are not fully supported yet.
    }
}

void TypeChecker::populate_enum_type(EnumType *enum_type, ast::EnumBody *body)
{
    for (const auto &elem : body->elements) {
        if (not enum_type->members.insert(elem).second) { add_error(TypeErrorDetail("Duplicate enum member '" + elem + "'"), *body); }
    }
}

// -----------------------------------------------------------------------------
// Assignability Check
// -----------------------------------------------------------------------------
bool TypeChecker::check_assignability(const std::shared_ptr<Type> &target, const std::shared_ptr<Type> &source, const ast::ASTNode &error_node)
{
    if (not target or not source) {
        add_error(TypeErrorDetail("Internal error: checking assignability with null types"), error_node);
        return false;
    }
    if (not Type::check_subtype(source, target)) {
        add_error(TypeMismatchError(target, source), error_node);
        return false;
    }
    return true;
}

// -----------------------------------------------------------------------------
// AST Visitor Dispatcher (Newer version)
// -----------------------------------------------------------------------------
std::shared_ptr<Type> TypeChecker::visit(ast::ASTNode *node)
{
    if (not node) return nullptr;

    if (auto *n = dynamic_cast<ast::NameExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::NumberExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::StringExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::BooleanExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::NilExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::VarargExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::FunctionCallExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::IndexExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::FieldExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::BinaryOperationExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::UnaryOperationExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::FunctionDefinitionExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::CastExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::IsTypeExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::TableConstructorExpression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::Block *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::ReturnStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::BreakStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::GotoStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::LabelStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::DoStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::IfStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::WhileStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::RepeatStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::ForNumericStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::ForInStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::FunctionDeclarationStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::VariableDeclarationStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::RecordDeclarationStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::EnumDeclarationStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::TypeAliasStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::AssignmentStatement *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::CallStatement *>(node)) return visit(n);

    if (auto *n = dynamic_cast<ast::Expression *>(node)) return visit(n);
    if (auto *n = dynamic_cast<ast::Statement *>(node)) return visit(n);

    add_error(TypeErrorDetail("Internal error: Unhandled AST node type in visitor: " + std::string(typeid(*node).name())), *node);
    return get_invalid_type(*node);
}

std::shared_ptr<Type> TypeChecker::visit(ast::Expression *node)
{
    add_error(TypeErrorDetail("Visiting generic Expression node - likely unhandled specific expression type"), *node);
    return get_invalid_type(*node);
}

std::shared_ptr<Type> TypeChecker::visit(ast::Statement *)
{
    // Statements do not produce a type
    return nullptr;
}

// -----------------------------------------------------------------------------
// Visitor Implementations for Expressions and Statements
// (Most implementations remain as in the original code. Below are selected examples.)
// -----------------------------------------------------------------------------

std::shared_ptr<Type> TypeChecker::visit(ast::Block *node)
{
    symbol_table.enter_scope(Scope::ScopeType::BLOCK);
    for (auto &stmt : node->statements) { visit(stmt.get()); }
    symbol_table.leave_scope();
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::NameExpression *node)
{
    auto symbol_opt = symbol_table.find_symbol(node->name);
    if (not symbol_opt) {
        add_error(UndefinedVariableError(node->name), *node);
        return get_invalid_type(*node);
    }
    if ((*symbol_opt)->is_type_alias) {
        add_error(TypeErrorDetail("Cannot use type name '" + node->name + "' as a value"), *node);
        return get_invalid_type(*node);
    }
    return resolve_alias((*symbol_opt)->type);
}

std::shared_ptr<Type> TypeChecker::visit(ast::NumberExpression *node)
{
    if (node->value.find('.') != std::string::npos or node->value.find_first_of("eE") != std::string::npos) {
        return std::make_shared<NumberType>(*node);
    } else {
        return std::make_shared<IntegerType>(*node);
    }
}

std::shared_ptr<Type> TypeChecker::visit(ast::StringExpression *node) { return std::make_shared<StringType>(*node, node->value); }

std::shared_ptr<Type> TypeChecker::visit(ast::BooleanExpression *node) { return std::make_shared<BooleanType>(*node); }

std::shared_ptr<Type> TypeChecker::visit(ast::NilExpression *node) { return get_nil_type(*node); }

std::shared_ptr<Type> TypeChecker::visit(ast::VarargExpression *node)
{
    auto symbol_opt = symbol_table.find_symbol("...");
    if (not symbol_opt) {
        add_error(TypeErrorDetail("Cannot use '...' outside a vararg function"), *node);
        return get_invalid_type(*node);
    }
    return resolve_alias((*symbol_opt)->type);
}

std::shared_ptr<Type> TypeChecker::visit(ast::ReturnStatement *node)
{
    std::vector<std::shared_ptr<Type>> return_val_types;
    for (auto &val_expr : node->values) { return_val_types.push_back(visit(val_expr.get())); }
    auto return_tuple = std::make_shared<TupleType>(*node, std::move(return_val_types));
    if (current_function_type.empty()) {
        // Top-level return handling (module return) if needed
    } else {
        auto expected_return_tuple = current_function_type.top()->return_types;
        check_assignability(expected_return_tuple, return_tuple, *node);
    }
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::VariableDeclarationStatement *node)
{
    std::vector<std::shared_ptr<Type>> declared_types;
    std::vector<std::shared_ptr<Type>> initial_value_types;
    bool is_global = node->visibility == ast::Visibility::GLOBAL;

    // 1. Resolve declared types
    for (const auto &type_node : node->types) { declared_types.push_back(resolve_type_node(type_node.get())); }
    // 2. Visit initial value expressions
    for (const auto &value_expr : node->values) { initial_value_types.push_back(visit(value_expr.get())); }
    std::shared_ptr<TupleType> value_tuple;
    if (initial_value_types.size() == 1 and initial_value_types[0]->is<TupleType>()) {
        value_tuple = std::static_pointer_cast<TupleType>(initial_value_types[0]);
    } else {
        value_tuple = std::make_shared<TupleType>(*node, std::move(initial_value_types));
    }

    size_t num_vars = node->names.size();
    size_t num_decls = declared_types.size();
    size_t num_vals = value_tuple->element_types.size();
    bool vals_are_vararg = value_tuple->is_variadic;

    if (num_decls > 0 and num_decls != num_vars) {
        add_error(
            TypeErrorDetail(
                "Number of type annotations (" + std::to_string(num_decls) + ") does not match number of variables (" + std::to_string(num_vars) + ")"
            ),
            *node
        );
    }

    for (size_t i = 0; i < num_vars; ++i) {
        const auto &var_name_info = node->names[i];
        std::string var_name = var_name_info.name;
        std::shared_ptr<Type> var_type = (i < num_decls) ? declared_types[i] : nullptr;
        std::shared_ptr<Type> value_type = nullptr;
        if (i < num_vals) {
            value_type = value_tuple->element_types[i];
        } else if (vals_are_vararg and num_vals > 0) {
            value_type = value_tuple->element_types.back();
        } else {
            value_type = get_nil_type(*node);
        }

        if (not var_type) {
            if (value_type and not value_type->is<NilType>()) {
                var_type = value_type;
                if (var_type->is<EmptyTableType>()) {
                    add_error(TypeErrorDetail("Cannot infer type from '{}'; add a type annotation for '" + var_name + "'"), *node);
                    var_type = get_invalid_type(*node);
                }
            } else {
                add_error(TypeErrorDetail("Variable '" + var_name + "' must have a type annotation or be initialized with a non-nil value"), *node);
                var_type = get_invalid_type(*node);
            }
        } else {
            if (value_type) { check_assignability(var_type, value_type, *node); }
        }

        bool is_const = var_name_info.attribute == "const";
        bool is_close = var_name_info.attribute == "close";
        bool is_total = var_name_info.attribute == "total";

        SymbolInfo symbol(var_name, var_type, is_const, is_close, is_total, is_global, node->line, node->column);
        if (not symbol_table.add_symbol(symbol)) { add_error(TypeErrorDetail("Variable '" + var_name + "' already declared in this scope"), *node); }
    }

    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::AssignmentStatement *node)
{
    std::vector<std::shared_ptr<Type>> left_types;
    std::vector<std::shared_ptr<Type>> right_types;
    std::vector<ast::Expression *> left_nodes;

    // 1. Visit LHS expressions; they must be valid L-values.
    for (const auto &left_expr : node->left) {
        if (not left_expr->is<ast::NameExpression>() and not left_expr->is<ast::IndexExpression>() and not left_expr->is<ast::FieldExpression>()) {
            add_error(TypeErrorDetail("Invalid target for assignment"), *left_expr);
            left_types.push_back(get_invalid_type(*left_expr));
        } else {
            left_types.push_back(visit(left_expr.get()));
        }
        left_nodes.push_back(left_expr.get());
    }
    // 2. Visit RHS expressions.
    for (const auto &right_expr : node->right) { right_types.push_back(visit(right_expr.get())); }
    std::shared_ptr<TupleType> right_tuple;
    if (right_types.size() == 1 and right_types[0] and right_types[0]->is<TupleType>()) {
        right_tuple = std::static_pointer_cast<TupleType>(right_types[0]);
    } else {
        right_tuple = std::make_shared<TupleType>(*node, std::move(right_types));
    }

    size_t num_left = left_types.size();
    size_t num_right = right_tuple->element_types.size();
    bool rights_are_vararg = right_tuple->is_variadic;

    for (size_t i = 0; i < num_left; ++i) {
        std::shared_ptr<Type> target_type = left_types[i];
        std::shared_ptr<Type> source_type = nullptr;
        if (i < num_right) {
            source_type = right_tuple->element_types[i];
        } else if (rights_are_vararg and num_right > 0) {
            source_type = right_tuple->element_types.back();
        } else {
            source_type = get_nil_type(*node);
        }

        if (target_type->is<InvalidType>()) continue;

        // Check for const violation (if LHS is a NameExpression)
        if (auto *name_expr = dynamic_cast<ast::NameExpression *>(left_nodes[i])) {
            auto symbol_opt = symbol_table.find_symbol(name_expr->name);
            if (symbol_opt and (*symbol_opt)->is_constant) {
                add_error(TypeErrorDetail("Cannot assign to constant variable '" + name_expr->name + "'"), *left_nodes[i]);
                continue;
            }
        }
        check_assignability(target_type, source_type, *left_nodes[i]);
    }

    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::TypeAliasStatement *node)
{
    auto aliased_type = resolve_type_node(node->type.get());
    auto alias_type = std::make_shared<TypeAlias>(*node, node->name, aliased_type);
    bool is_global = node->visibility == ast::Visibility::GLOBAL;
    SymbolInfo symbol(node->name, alias_type, is_global, node->line, node->column);
    if (not symbol_table.add_symbol(symbol)) { add_error(TypeErrorDetail("Type alias '" + node->name + "' already declared in this scope"), *node); }
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::RecordDeclarationStatement *node)
{
    bool is_global = node->visibility == ast::Visibility::GLOBAL;
    auto record_type = std::make_shared<RecordType>(*node, node->name);
    SymbolInfo symbol(node->name, record_type, is_global, node->line, node->column);
    if (not symbol_table.add_symbol(symbol)) { add_error(TypeErrorDetail("Record type '" + node->name + "' already declared in this scope"), *node); }
    populate_record_type(record_type.get(), node->body.get());
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::EnumDeclarationStatement *node)
{
    bool is_global = node->visibility == ast::Visibility::GLOBAL;
    auto enum_type = std::make_shared<EnumType>(*node, node->name, std::set<std::string> {});
    populate_enum_type(enum_type.get(), node->body.get());
    SymbolInfo symbol(node->name, enum_type, is_global, node->line, node->column);
    if (not symbol_table.add_symbol(symbol)) { add_error(TypeErrorDetail("Enum type '" + node->name + "' already declared in this scope"), *node); }
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::FunctionDeclarationStatement *node)
{
    auto func_type = resolve_function_body(node->body.get());
    std::string func_name;
    if (node->is_method and not node->method_name.empty()) {
        func_name = node->method_name;
        add_error(TypeErrorDetail("Method declaration syntax (using ':') outside of record definition not fully supported yet."), *node);
    } else if (not node->name_path.empty()) {
        if (node->name_path.size() > 1) { add_error(TypeErrorDetail("Dotted function names (e.g., table.func) not fully supported yet."), *node); }
        func_name = node->name_path.empty() ? "<anonymous>" : node->name_path[0];
    } else {
        add_error(TypeErrorDetail("Function declaration is missing a name."), *node);
        func_name = "<error>";
    }
    bool is_global = node->visibility == ast::Visibility::GLOBAL;
    SymbolInfo symbol(func_name, func_type, false, false, false, is_global, node->line, node->column);
    if (not symbol_table.add_symbol(symbol)) { add_error(TypeErrorDetail("Function '" + func_name + "' already declared in this scope"), *node); }

    symbol_table.enter_scope(Scope::ScopeType::FUNCTION);
    current_function_type.push(func_type);

    for (const auto &param : func_type->parameters) {
        SymbolInfo param_symbol(param.name, param.type, false, false, false, false, param.type->line, param.type->column);
        param_symbol.is_parameter = true;
        if (not symbol_table.add_symbol(param_symbol)) { add_error(TypeErrorDetail("Duplicate parameter name '" + param.name + "'"), *node); }
    }
    visit(node->body->body.get());
    current_function_type.pop();
    symbol_table.leave_scope();
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::FunctionDefinitionExpression *node)
{
    auto func_type = resolve_function_body(node->body.get());
    symbol_table.enter_scope(Scope::ScopeType::FUNCTION);
    current_function_type.push(func_type);
    for (const auto &param : func_type->parameters) {
        SymbolInfo param_symbol(param.name, param.type, false, false, false, false, param.type->line, param.type->column);
        param_symbol.is_parameter = true;
        symbol_table.add_symbol(param_symbol);
    }
    visit(node->body->body.get());
    current_function_type.pop();
    symbol_table.leave_scope();
    return func_type;
}

std::shared_ptr<Type> TypeChecker::visit(ast::TableConstructorExpression *node) { return infer_table_constructor_type(node); }

std::shared_ptr<Type> TypeChecker::visit(ast::BreakStatement *)
{
    // TODO: Check if inside a loop
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::GotoStatement *)
{
    // TODO: Check if label exists
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::LabelStatement *)
{
    // TODO: Register label in the current scope
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::DoStatement *node)
{
    visit(node->body.get());
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::IfStatement *node)
{
    symbol_table.enter_scope(Scope::ScopeType::BLOCK);
    bool all_branches_return = true;
    for (size_t i = 0; i < node->if_branches.size(); ++i) {
        auto &branch = node->if_branches[i];
        symbol_table.enter_scope(Scope::ScopeType::BLOCK);
        auto cond_type = visit(branch.condition.get());
        visit(branch.block.get());
        symbol_table.leave_scope();
    }
    if (node->else_block) {
        symbol_table.enter_scope(Scope::ScopeType::BLOCK);
        visit(node->else_block.get());
        symbol_table.leave_scope();
    } else {
        all_branches_return = false;
    }
    symbol_table.leave_scope();

    if (not all_branches_return) { add_error(NotAllCodePathsReturn(), *node); }

    return nullptr;
}

// New loop visitors introduced in the newer version:
std::shared_ptr<Type> TypeChecker::visit(ast::WhileStatement *node)
{
    auto cond_type = visit(node->condition.get());
    symbol_table.enter_scope(Scope::ScopeType::BLOCK); // Loop body scope
    visit(node->body.get());
    symbol_table.leave_scope();
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::RepeatStatement *node)
{
    symbol_table.enter_scope(Scope::ScopeType::BLOCK);
    visit(node->body.get());
    auto cond_type = visit(node->condition.get());
    symbol_table.leave_scope();
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::ForNumericStatement *node)
{
    auto start_type = visit(node->expressions.start.get());
    auto end_type = visit(node->expressions.end.get());
    std::shared_ptr<Type> step_type = node->expressions.step ? visit(node->expressions.step.get()) : std::make_shared<IntegerType>(*node);
    auto number_type = std::make_shared<NumberType>(*node);
    check_assignability(number_type, start_type, *node->expressions.start);
    check_assignability(number_type, end_type, *node->expressions.end);
    check_assignability(number_type, step_type, node->expressions.step ? (const ast::ASTNode &)*node->expressions.step : (const ast::ASTNode &)*node);
    symbol_table.enter_scope(Scope::ScopeType::BLOCK);
    SymbolInfo loop_var(node->variable_name, number_type, true, false, false, false, node->line, node->column);
    if (not symbol_table.add_symbol(loop_var)) {
        add_error(TypeErrorDetail("Loop variable '" + node->variable_name + "' shadows existing variable"), *node);
    }
    visit(node->body.get());
    symbol_table.leave_scope();
    return nullptr;
}

std::shared_ptr<Type> TypeChecker::visit(ast::ForInStatement *node)
{
    std::vector<std::shared_ptr<Type>> iter_expr_types;
    for (const auto &expr : node->exprs) { iter_expr_types.push_back(visit(expr.get())); }
    auto iter_tuple = std::make_shared<TupleType>(*node, std::move(iter_expr_types));
    if (iter_tuple->element_types.empty()) {
        add_error(TypeErrorDetail("For..in statement requires at least one iterator expression"), *node);
        return nullptr;
    }
    auto iter_func_type_any = resolve_alias(iter_tuple->element_types[0]);
    std::vector<std::shared_ptr<Type>> loop_var_types;
    if (auto *iter_func_type = iter_func_type_any->as<FunctionType>()) {
        if (iter_func_type->return_types) {
            loop_var_types = iter_func_type->return_types->element_types;
            if (iter_func_type->return_types->is_variadic and not loop_var_types.empty()) {
                auto last_type = loop_var_types.back();
                while (loop_var_types.size() < node->names.size()) { loop_var_types.push_back(last_type); }
            }
        }
    } else {
        add_error(
            TypeErrorDetail("First expression in for..in loop must be an iterator function, got " + iter_func_type_any->to_string()), *node->exprs[0]
        );
        for (size_t i = 0; i < node->names.size(); ++i) { loop_var_types.push_back(get_invalid_type(*node)); }
    }
    symbol_table.enter_scope(Scope::ScopeType::BLOCK);
    for (size_t i = 0; i < node->names.size(); ++i) {
        std::string var_name = node->names[i];
        std::shared_ptr<Type> var_type = (i < loop_var_types.size()) ? loop_var_types[i] : get_any_type(*node);
        SymbolInfo loop_var(var_name, var_type, false, false, false, false, node->line, node->column);
        if (not symbol_table.add_symbol(loop_var)) {
            add_error(TypeErrorDetail("Loop variable '" + var_name + "' shadows existing variable"), *node);
        }
    }
    visit(node->body.get());
    symbol_table.leave_scope();
    return nullptr;
}

// -----------------------------------------------------------------------------
// Standard Library Loading
// -----------------------------------------------------------------------------
void TypeChecker::load_stdlib()
{
    auto global_scope = symbol_table.get_global_scope();
    if (not global_scope) return;
    ast::ASTNode dummy_loc(0, 0);
    // Global print function: print(any...) -> void
    auto print_param_type = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { get_any_type(dummy_loc) }, true);
    auto print_return_type = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> {});
    auto print_func_type = std::make_shared<FunctionType>(
        dummy_loc,
        std::vector<FunctionParameter> {
            { "", print_param_type->element_types[0], false, true }
    },
        print_return_type
    );
    global_scope->add_symbol({ "print", print_func_type, true, false, false, true, 0, 0 });

    // Global type function: type(any) -> string
    auto type_param_type = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { get_any_type(dummy_loc) }, false);
    auto type_return_type
        = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { std::make_shared<StringType>(dummy_loc) }, false);
    auto type_func_type = std::make_shared<FunctionType>(
        dummy_loc,
        std::vector<FunctionParameter> {
            { "v", get_any_type(dummy_loc), false, false }
    },
        type_return_type
    );
    global_scope->add_symbol({ "type", type_func_type, true, false, false, true, 0, 0 });

    // Pairs function example
    auto pairs_param_type = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { get_any_type(dummy_loc) }, false);
    auto iter_ret_type
        = std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { get_any_type(dummy_loc), get_any_type(dummy_loc) }, false);
    auto iter_func_type = std::make_shared<FunctionType>(dummy_loc, std::vector<FunctionParameter> {}, iter_ret_type);
    auto pairs_ret_type = std::make_shared<TupleType>(
        dummy_loc, std::vector<std::shared_ptr<Type>> { iter_func_type, get_any_type(dummy_loc), get_nil_type(dummy_loc) }, false
    );
    auto pairs_func_type = std::make_shared<FunctionType>(
        dummy_loc,
        std::vector<FunctionParameter> {
            { "t", get_any_type(dummy_loc), false, false }
    },
        pairs_ret_type
    );
    global_scope->add_symbol({ "pairs", pairs_func_type, true, false, false, true, 0, 0 });

    // Math library table example
    auto math_record = std::make_shared<RecordType>(dummy_loc, "math");
    math_record->is_global = true;
    auto number_to_number = std::make_shared<FunctionType>(
        dummy_loc,
        std::vector<FunctionParameter> {
            { "x", std::make_shared<NumberType>(dummy_loc), false, false }
    },
        std::make_shared<TupleType>(dummy_loc, std::vector<std::shared_ptr<Type>> { std::make_shared<NumberType>(dummy_loc) })
    );
    math_record->fields["abs"] = { "abs", number_to_number };
    math_record->fields["huge"] = { "huge", std::make_shared<NumberType>(dummy_loc) };
    global_scope->add_symbol({ "math", math_record, true, false, false, true, 0, 0 });
}

// -----------------------------------------------------------------------------
// Metamethod and Operation Checks (Updated in newer version)
// -----------------------------------------------------------------------------
std::shared_ptr<Type> TypeChecker::check_metamethod(
    const std::string &op_name, const ast::ASTNode &error_node, std::shared_ptr<Type> t1, std::shared_ptr<Type> t2
)
{
    auto r1 = resolve_alias(t1);
    auto r2 = t2 ? resolve_alias(t2) : nullptr;
    std::shared_ptr<FunctionType> meta_func = nullptr;
    int self_arg_idx = 1; // Which operand's metamethod is used (1 or 2)

    if (auto *rec1 = r1->as<RecordType>()) {
        auto meta_opt = rec1->get_metamethod_type(op_name);
        if (meta_opt) meta_func = *meta_opt;
    }
    if (not meta_func and r2 and op_name != "__index" and op_name != "__newindex" and op_name != "__call") {
        if (auto *rec2 = r2->as<RecordType>()) {
            auto meta_opt = rec2->get_metamethod_type(op_name);
            if (meta_opt) {
                meta_func = *meta_opt;
                self_arg_idx = 2;
            }
        }
    }
    if (meta_func) {
        size_t expected_params = t2 ? 2 : 1;
        if (meta_func->parameters.size() != expected_params or meta_func->parameters[0].is_variadic) {
            add_error(TypeErrorDetail("Metamethod '" + op_name + "' has incorrect signature"), error_node);
            return get_invalid_type(error_node);
        }
        if (not NamedType::check_equality(self_arg_idx == 1 ? t1 : t2, meta_func->parameters[0].type)) {
            add_error(TypeMismatchError(meta_func->parameters[0].type, self_arg_idx == 1 ? t1 : t2), error_node);
        }
        if (t2 and not NamedType::check_equality(self_arg_idx == 1 ? t2 : t1, meta_func->parameters[1].type)) {
            add_error(TypeMismatchError(meta_func->parameters[1].type, self_arg_idx == 1 ? t2 : t1), error_node);
        }
        if (meta_func->return_types->element_types.empty()) return get_nil_type(error_node);
        if (meta_func->return_types->element_types.size() == 1 and not meta_func->return_types->is_variadic)
            return meta_func->return_types->element_types[0];
        return meta_func->return_types;
    }
    return nullptr; // No applicable metamethod found
}

std::shared_ptr<Type> TypeChecker::check_metamethod(
    const std::string &, const ast::ASTNode &, std::shared_ptr<Type>
) {
    throw std::runtime_error("Unary metamethod not supported yet");
}

std::shared_ptr<Type> TypeChecker::check_binary_operation(
    ast::BinaryOperationExpression *node, std::shared_ptr<Type> left, std::shared_ptr<Type> right
)
{
    TokenType op = node->operation;
    left = resolve_alias(left);
    right = resolve_alias(right);

    if (left->is<InvalidType>() or right->is<InvalidType>()) return get_invalid_type(*node);

    std::string meta_name;
    switch (op) {
    case TokenType::ADD:
        meta_name = "__add";
        break;
    case TokenType::SUB:
        meta_name = "__sub";
        break;
    case TokenType::MUL:
        meta_name = "__mul";
        break;
    case TokenType::DIV:
        meta_name = "__div";
        break;
    case TokenType::FLOOR_DIV:
        meta_name = "__idiv";
        break;
    case TokenType::MOD:
        meta_name = "__mod";
        break;
    case TokenType::POW:
        meta_name = "__pow";
        break;
    case TokenType::EQUALS:
        meta_name = "__eq";
        break;
    case TokenType::LESS:
        meta_name = "__lt";
        break;
    case TokenType::LESS_EQ:
        meta_name = "__le";
        break;
    case TokenType::CONCAT:
        meta_name = "__concat";
        break;
    case TokenType::BIT_AND:
        meta_name = "__band";
        break;
    case TokenType::BIT_OR:
        meta_name = "__bor";
        break;
    case TokenType::BIT_XOR:
        meta_name = "__bxor";
        break;
    case TokenType::SHIFT_L:
        meta_name = "__shl";
        break;
    case TokenType::SHIFT_R:
        meta_name = "__shr";
        break;
    default:
        break;
    }
    if (not meta_name.empty()) {
        auto meta_result = check_metamethod(meta_name, *node, left, right);
        if (meta_result) return meta_result;
    }
    if (op == TokenType::GREATER or op == TokenType::GREATER_EQ) {
        meta_name = (op == TokenType::GREATER) ? "__lt" : "__le";
        auto meta_result = check_metamethod(meta_name, *node, right, left);
        if (meta_result) return meta_result;
    }
    // Arithmetic ops
    if (op == TokenType::ADD or op == TokenType::SUB or op == TokenType::MUL or op == TokenType::DIV or op == TokenType::FLOOR_DIV
        or op == TokenType::MOD or op == TokenType::POW) {
        bool left_is_num = left->is<NumberType>() or left->is<IntegerType>();
        bool right_is_num = right->is<NumberType>() or right->is<IntegerType>();
        if (left_is_num and right_is_num) {
            if (left->is<NumberType>() or right->is<NumberType>() or op == TokenType::DIV or op == TokenType::POW)
                return std::make_shared<NumberType>(*node);
            else return std::make_shared<IntegerType>(*node);
        }
        add_error(InvalidOperationError(Token::type_to_string(op), { left, right }), *node);
        return get_invalid_type(*node);
    }
    // Comparison ops
    if (op == TokenType::EQUALS or op == TokenType::NOT_EQ or op == TokenType::LESS or op == TokenType::LESS_EQ or op == TokenType::GREATER
        or op == TokenType::GREATER_EQ) {
        bool comparable = ((left->is<NumberType>() or left->is<IntegerType>()) and (right->is<NumberType>() or right->is<IntegerType>()))
            or (left->is<StringType>() and right->is<StringType>()) or (left->is<EnumType>() and right->is<StringType>())
            or (left->is<StringType>() and right->is<EnumType>()) or (left->is<EnumType>() and right->is<EnumType>() and left->equals(right))
            or (left->is<BooleanType>() and right->is<BooleanType>()) or left->is<NilType>() or right->is<NilType>() or left->equals(right);
        if (comparable) return std::make_shared<BooleanType>(*node);
        add_error(InvalidOperationError(Token::type_to_string(op) + " (types not comparable)", { left, right }), *node);
        return get_invalid_type(*node);
    }
    // Logical ops
    if (op == TokenType::AND) return UnionType::create_union(*node, { left, right });
    if (op == TokenType::OR) return UnionType::create_union(*node, { left, right });
    // Concatenation
    if (op == TokenType::CONCAT) {
        // auto s = StringType(0, 0, "");
        // auto n = NumberType(0, 0, "");
        auto str_t = get_string_type(*node);
        auto n_t = get_number_type(*node);
        bool left_ok = Type::check_subtype(left, str_t) or Type::check_subtype(left, n_t);
        bool right_ok = Type::check_subtype(right, str_t) or Type::check_subtype(right, n_t);
        if (left_ok and right_ok) return std::make_shared<StringType>(*node);
        add_error(InvalidOperationError(Token::type_to_string(op), { left, right }), *node);
        return get_invalid_type(*node);
    }
    // Bitwise ops
    if (op == TokenType::BIT_AND or op == TokenType::BIT_OR or op == TokenType::BIT_XOR or op == TokenType::SHIFT_L or op == TokenType::SHIFT_R) {
        auto i_t = get_integer_type(*node);
        bool left_is_int = Type::check_subtype(left, i_t);
        bool right_is_int = Type::check_subtype(right, i_t);
        if (left_is_int and right_is_int) return std::make_shared<IntegerType>(*node);
        add_error(InvalidOperationError(Token::type_to_string(op), { left, right }), *node);
        return get_invalid_type(*node);
    }
    add_error(TypeErrorDetail("Unsupported binary operator '" + Token::type_to_string(op) + "'"), *node);
    return get_invalid_type(*node);
}

std::shared_ptr<Type> TypeChecker::check_unary_operation(ast::UnaryOperationExpression *node, std::shared_ptr<Type> operand)
{
    TokenType op = node->operation;
    operand = resolve_alias(operand);
    if (operand->is<InvalidType>()) return operand;

    std::string meta_name;
    switch (op) {
    case TokenType::SUB:
        meta_name = "__unm";
        break;
    case TokenType::LENGTH:
        meta_name = "__len";
        break;
    case TokenType::BIT_XOR:
        meta_name = "__bnot";
        break; // Using BIT_XOR token for unary '~'
    default:
        break;
    }
    if (not meta_name.empty()) {
        auto meta_result = check_metamethod(meta_name, *node, operand);
        if (meta_result) return meta_result;
    }
    if (op == TokenType::SUB) {
        // auto n = NumberType(0, 0, "");
        auto n_t = get_number_type(*node);
        if (Type::check_subtype(operand, n_t)) {
            if (operand->is<IntegerType>()) {
                return std::make_shared<IntegerType>(*node);
            } else {
                return std::make_shared<NumberType>(*node);
            }

            // return operand->is<IntegerType>() ? std::const_pointer_cast<Type>(std::make_shared<IntegerType>(*node))
            //                                   : std::const_pointer_cast<Type>(std::make_shared<NumberType>(*node));
        }
        add_error(InvalidOperationError(Token::type_to_string(op), { operand }), *node);
        return get_invalid_type(*node);
    }
    if (op == TokenType::NOT) return std::make_shared<BooleanType>(*node);
    if (op == TokenType::LENGTH) {
        if (operand->is<StringType>() or operand->is<ArrayType>() or operand->is<TupleType>() or operand->is<MapType>())
            return std::make_shared<IntegerType>(*node);
        add_error(InvalidOperationError(Token::type_to_string(op), { operand }), *node);
        return get_invalid_type(*node);
    }
    if (op == TokenType::BIT_XOR) {
        // auto i = IntegerType(0, 0, "");
        auto i_t = get_integer_type(*node);
        if (Type::check_subtype(operand, i_t)) return std::make_shared<IntegerType>(*node);
        add_error(InvalidOperationError("~ (bitwise not)", { operand }), *node);
        return get_invalid_type(*node);
    }
    add_error(TypeErrorDetail("Unsupported unary operator '" + Token::type_to_string(op) + "'"), *node);
    return get_invalid_type(*node);
}

std::shared_ptr<Type> TypeChecker::infer_table_constructor_type(ast::TableConstructorExpression *node)
{
    std::shared_ptr<Type> inferred_key_type = nullptr;
    std::shared_ptr<Type> inferred_value_type = nullptr;
    std::vector<std::shared_ptr<Type>> tuple_types;
    bool is_potential_array = true;
    bool is_potential_map = true;
    bool is_potential_tuple = true;
    long long next_array_index = 1;

    if (node->fields.empty()) return std::make_shared<EmptyTableType>(*node);

    for (const auto &field_variant : node->fields) {
        std::shared_ptr<Type> key_type = nullptr;
        std::shared_ptr<Type> value_type = nullptr;
        bool is_implicit_key = false;

        if (const auto *expr_ptr = std::get_if<std::unique_ptr<ast::Expression>>(&field_variant)) {
            is_potential_map = false;
            is_implicit_key = true;
            key_type = std::make_shared<IntegerType>(*(*expr_ptr));
            value_type = visit(expr_ptr->get());
            if (value_type->is<TupleType>()) {
                value_type = std::static_pointer_cast<TupleType>(value_type)->element_types.empty()
                    ? get_nil_type(*(*expr_ptr))
                    : std::static_pointer_cast<TupleType>(value_type)->element_types[0];
            }
            tuple_types.push_back(value_type);
            if ((size_t)next_array_index != tuple_types.size()) is_potential_tuple = false;
        } else if (const auto *kv_pair_ptr = std::get_if<ast::TableConstructorExpression::KeyValuePair>(&field_variant)) {
            is_potential_array = false;
            is_potential_tuple = false;
            if (const auto *key_str = std::get_if<std::string>(&kv_pair_ptr->key)) {
                key_type = std::make_shared<StringType>(*node, *key_str);
            } else if (const auto *key_expr_ptr = std::get_if<std::unique_ptr<ast::Expression>>(&kv_pair_ptr->key)) {
                key_type = visit(key_expr_ptr->get());
            } else {
                add_error(TypeErrorDetail("Internal error: Invalid table key variant"), *node);
                key_type = get_invalid_type(*node);
            }
            value_type = visit(kv_pair_ptr->value.get());
            if (value_type->is<TupleType>()) {
                value_type = std::static_pointer_cast<TupleType>(value_type)->element_types.empty()
                    ? get_nil_type(*kv_pair_ptr->value)
                    : std::static_pointer_cast<TupleType>(value_type)->element_types[0];
            }
        }

        if (value_type->is<InvalidType>() or key_type->is<InvalidType>()) return get_invalid_type(*node);

        if (not inferred_value_type) {
            inferred_value_type = value_type;
        } else {
            if (not Type::check_equality(inferred_value_type, value_type)) {
                if (Type::check_subtype(value_type, inferred_value_type)) {
                    // keep existing inferred_value_type
                } else if (Type::check_subtype(inferred_value_type, value_type)) {
                    inferred_value_type = value_type;
                } else {
                    inferred_value_type = get_any_type(*node);
                    is_potential_array = false;
                }
            }
        }
        if (not is_implicit_key) {
            if (not inferred_key_type) {
                inferred_key_type = key_type;
            } else {
                if (not Type::check_equality(inferred_key_type, key_type)) {
                    if (Type::check_subtype(key_type, inferred_key_type)) {
                        // keep inferred_key_type
                    } else if (Type::check_subtype(inferred_key_type, key_type)) {
                        inferred_key_type = key_type;
                    } else {
                        inferred_key_type = get_any_type(*node);
                        is_potential_map = false;
                    }
                }
            }
        }
        if (is_implicit_key) next_array_index++;
        else is_potential_tuple = false;
    }
    if (is_potential_tuple and not is_potential_map and next_array_index > 1) return std::make_shared<TupleType>(*node, std::move(tuple_types));
    else if (is_potential_array and not is_potential_map)
        return std::make_shared<ArrayType>(*node, inferred_value_type ? inferred_value_type : get_any_type(*node));
    else if (is_potential_map and not is_potential_array)
        return std::make_shared<MapType>(
            *node, inferred_key_type ? inferred_key_type : get_any_type(*node), inferred_value_type ? inferred_value_type : get_any_type(*node)
        );
    else {
        add_error(TypeErrorDetail("Cannot reliably infer table type (mixed array/map keys/values?)"), *node);
        return std::make_shared<MapType>(*node, get_any_type(*node), get_any_type(*node));
    }
}
