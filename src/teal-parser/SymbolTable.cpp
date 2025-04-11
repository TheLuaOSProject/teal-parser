#include "SymbolTable.hpp"
#include <stdexcept>

using namespace teal::parser::typechecker;
// --- Scope ---

Scope::Scope(Scope *parent_scope, ScopeType type) : parent(parent_scope), scope_type(type) { }

bool Scope::add_symbol(const SymbolInfo &symbol)
{
    auto [it, inserted] = symbols.try_emplace(symbol.name, symbol);
    return inserted;
}

std::optional<SymbolInfo *> Scope::find_symbol_local(const std::string &name)
{
    auto it = symbols.find(name);
    if (it != symbols.end()) { return &it->second; }
    return std::nullopt;
}

std::optional<SymbolInfo *> Scope::find_symbol(const std::string &name)
{
    Scope *current = this;
    while (current != nullptr) {
        auto result = current->find_symbol_local(name);
        if (result) { return result; }
        current = current->parent;
    }
    return std::nullopt;
}

// --- SymbolTableManager ---

SymbolTableManager::SymbolTableManager()
{
    // Initialize with the global scope
    scope_stack.push_back(std::make_unique<Scope>(nullptr, Scope::ScopeType::GLOBAL));
}

void SymbolTableManager::enter_scope(Scope::ScopeType type)
{
    if (scope_stack.empty()) { throw std::runtime_error("Cannot enter scope: Scope stack is empty."); }
    Scope *parent = scope_stack.back().get();
    scope_stack.push_back(std::make_unique<Scope>(parent, type));
}

void SymbolTableManager::leave_scope()
{
    if (scope_stack.size() <= 1) { // Cannot leave the global scope
        throw std::runtime_error("Cannot leave global scope.");
    }
    scope_stack.pop_back();
}

bool SymbolTableManager::add_symbol(const SymbolInfo &symbol)
{
    if (scope_stack.empty()) { throw std::runtime_error("Cannot add symbol: Scope stack is empty."); }
    return scope_stack.back()->add_symbol(symbol);
}

std::optional<SymbolInfo *> SymbolTableManager::find_symbol(const std::string &name)
{
    if (scope_stack.empty()) { return std::nullopt; }
    return scope_stack.back()->find_symbol(name); // find_symbol walks up the chain
}

Scope *SymbolTableManager::get_current_scope() const
{
    if (scope_stack.empty()) { return nullptr; }
    return scope_stack.back().get();
}

Scope *SymbolTableManager::get_global_scope() const
{
    if (scope_stack.empty()) { return nullptr; }
    return scope_stack.front().get();
}
