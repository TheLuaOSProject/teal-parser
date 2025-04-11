#pragma once

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <optional>
#include "Types.hpp"
#include "AST.hpp" // For Visibility

namespace teal::parser::typechecker {

// Information stored for each variable/symbol
struct SymbolInfo {
    std::string name;
    std::shared_ptr<Type> type;
    bool is_constant = false; // For <const>
    bool is_closable = false; // For <close>
    bool is_total = false; // For <total>
    bool is_type_alias = false; // If it's a type alias name
    bool is_global = false;
    bool is_parameter = false; // If it's a function parameter
    size_t declaration_line = 0;
    size_t declaration_col = 0;
    // Add more flags as needed (e.g., is_used, is_reassigned)

    // Constructor for Variables
    SymbolInfo(std::string n, std::shared_ptr<Type> t, bool is_const = false, bool is_close = false, bool is_tot = false, bool is_glob = false, size_t line = 0, size_t col = 0)
        : name(std::move(n)), type(std::move(t)), is_constant(is_const), is_closable(is_close), is_total(is_tot), is_global(is_glob), declaration_line(line), declaration_col(col) {}

    // Constructor for Type Aliases
    SymbolInfo(std::string n, std::shared_ptr<Type> t, bool is_glob, size_t line, size_t col)
        : name(std::move(n)), type(std::move(t)), is_type_alias(true), is_global(is_glob), declaration_line(line), declaration_col(col) {}

};

// Represents a single scope (e.g., global, function body, block)
class Scope {
public:
    enum class ScopeType { GLOBAL, FUNCTION, BLOCK, RECORD }; // Add more as needed

    Scope(Scope* parent_scope, ScopeType type);

    // Add a symbol to this specific scope
    // Returns false if symbol already exists in this scope
    bool add_symbol(const SymbolInfo& symbol);

    // Find a symbol in this scope only
    std::optional<SymbolInfo*> find_symbol_local(const std::string& name);

    // Find a symbol in this scope or any parent scope
    std::optional<SymbolInfo*> find_symbol(const std::string& name);

    Scope* get_parent() const { return parent; }
    ScopeType get_type() const { return scope_type; }
    const std::unordered_map<std::string, SymbolInfo>& get_symbols() const { return symbols; }


private:
    std::unordered_map<std::string, SymbolInfo> symbols;
    Scope* parent; // Null for global scope
    ScopeType scope_type;
};

// Manages the stack of scopes
class SymbolTableManager {
public:
    SymbolTableManager();

    void enter_scope(Scope::ScopeType type);
    void leave_scope();

    // Add symbol to the *current* scope
    bool add_symbol(const SymbolInfo& symbol);

    // Find symbol starting from the current scope and going up
    std::optional<SymbolInfo*> find_symbol(const std::string& name);

    Scope* get_current_scope() const;
    Scope* get_global_scope() const;

private:
    std::vector<std::unique_ptr<Scope>> scope_stack;
    // Keep track of all created scopes for ownership if needed,
    // although unique_ptr in stack handles it.
};

} // namespace teal::checker