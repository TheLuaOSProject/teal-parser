
#include "SymbolTable.hpp"

using namespace teal::parser::typecheck;

SymbolTable::SymbolTable() { _scopes.emplace_back(); }
void SymbolTable::push_scope() { _scopes.emplace_back(); }
void SymbolTable::pop_scope()
{
    if (_scopes.size() > 1) _scopes.pop_back();
}
bool SymbolTable::define(
    const std::string &name, const TypePtr &type, bool is_const, bool is_global, const std::optional<std::string> &attr
)
{
    if (is_global) {
        auto &globalScope = _scopes.front();
        if (globalScope.find(name) != globalScope.end()) return false;
        globalScope[name] = VariableInfo { type, attr, is_const, true };
        return true;
    } else {
        auto &current = _scopes.back();
        if (current.find(name) != current.end()) return false;
        current[name] = VariableInfo { type, attr, is_const, _scopes.size() == 1 };
        return true;
    }
}
VariableInfo *SymbolTable::lookup(const std::string &name)
{
    for (int i = _scopes.size() - 1; i >= 0; --i) {
        auto it = _scopes[i].find(name);
        if (it != _scopes[i].end()) return &it->second;
    }
    return nullptr;
}
VariableInfo *SymbolTable::lookup_local(const std::string &name)
{
    if (_scopes.empty()) return nullptr;
    auto &current = _scopes.back();
    auto it = current.find(name);
    if (it != current.end()) return &it->second;
    return nullptr;
}
VariableInfo *SymbolTable::lookup_global(const std::string &name)
{
    if (_scopes.empty()) return nullptr;
    auto &globalScope = _scopes.front();
    auto it = globalScope.find(name);
    if (it != globalScope.end()) return &it->second;
    return nullptr;
}
