#pragma once
#include <string>
#include <unordered_map>
#include <vector>
#include <optional>
#include <memory>

#include "Type.hpp"

namespace teal::parser::typecheck
{

struct VarInfo {
    TypePtr type;
    std::optional<std::string> attribute;
    bool is_const;
    bool is_global;
};

class SymbolTable {
public:
    SymbolTable();
    void pushScope();
    void popScope();
    bool define(const std::string &name, const TypePtr &type, bool is_const=false, bool is_global=false, const std::optional<std::string> &attr = std::nullopt);
    VarInfo *lookup(const std::string &name);
    VarInfo *lookupLocal(const std::string &name);
    VarInfo *lookupGlobal(const std::string &name);
private:
    std::vector<std::unordered_map<std::string, VarInfo>> scopes;
};

}
