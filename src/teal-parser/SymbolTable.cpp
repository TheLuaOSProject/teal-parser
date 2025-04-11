
#include "SymbolTable.hpp"

using namespace teal::parser::typecheck;

SymbolTable::SymbolTable() { scopes.emplace_back(); }
void SymbolTable::pushScope() { scopes.emplace_back(); }
void SymbolTable::popScope()
{
    if (scopes.size() > 1) scopes.pop_back();
}
bool SymbolTable::define(
    const std::string &name, const TypePtr &type, bool is_const, bool is_global, const std::optional<std::string> &attr
)
{
    if (is_global) {
        auto &globalScope = scopes.front();
        if (globalScope.find(name) != globalScope.end()) return false;
        globalScope[name] = VarInfo { type, attr, is_const, true };
        return true;
    } else {
        auto &current = scopes.back();
        if (current.find(name) != current.end()) return false;
        current[name] = VarInfo { type, attr, is_const, scopes.size() == 1 };
        return true;
    }
}
VarInfo *SymbolTable::lookup(const std::string &name)
{
    for (int i = scopes.size() - 1; i >= 0; --i) {
        auto it = scopes[i].find(name);
        if (it != scopes[i].end()) return &it->second;
    }
    return nullptr;
}
VarInfo *SymbolTable::lookupLocal(const std::string &name)
{
    if (scopes.empty()) return nullptr;
    auto &current = scopes.back();
    auto it = current.find(name);
    if (it != current.end()) return &it->second;
    return nullptr;
}
VarInfo *SymbolTable::lookupGlobal(const std::string &name)
{
    if (scopes.empty()) return nullptr;
    auto &globalScope = scopes.front();
    auto it = globalScope.find(name);
    if (it != globalScope.end()) return &it->second;
    return nullptr;
}
