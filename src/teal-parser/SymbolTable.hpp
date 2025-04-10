#pragma once
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "Type.hpp"

namespace teal::parser::typecheck
{

    struct VariableInfo {
        TypePtr type;
        std::optional<std::string> attribute;
        bool is_const;
        bool is_global;
    };

    class SymbolTable {
    public:
        SymbolTable();
        void push_scope();
        void pop_scope();
        bool define(const std::string &name, const TypePtr &type, bool is_const = false, bool is_global = false, const std::optional<std::string> &attr = std::nullopt);
        VariableInfo *lookup(const std::string &name);
        VariableInfo *lookup_local(const std::string &name);
        VariableInfo *lookup_global(const std::string &name);

    private:
        std::vector<std::unordered_map<std::string, VariableInfo>> _scopes;
    };

}
