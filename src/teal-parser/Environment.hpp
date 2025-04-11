#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <optional>
#include <variant>
#include "TypeSystem.hpp"
#include "TypeError.hpp"

namespace teal::parser::typechecker
{
    enum class SymbolKind {
        VARIABLE,
        FUNCTION,
        TYPE, // Record, Interface, Enum, Alias
        ENUM_MEMBER // Maybe treat like const variable?
        // PARAMETER ? (might just be VARIABLE in function scope)
    };

    struct SymbolInfo {
        SymbolKind kind;
        std::string name;
        std::shared_ptr<const Type> type;
        bool isMutable = true; // For const checks
        bool isDefined = true; // For forward declarations
        const parser::ast::ASTNode *declarationNode = nullptr; // Optional link back to AST
        // Add visibility if needed explicitly (e.g. GLOBAL vs LOCAL)

        SymbolInfo(SymbolKind k, std::string n, std::shared_ptr<const Type> t, bool mut = true, const parser::ast::ASTNode *node = nullptr)
            : kind(k), name(std::move(n)), type(std::move(t)), isMutable(mut), declarationNode(node) {}

        // Constructor for forward-declared types
         SymbolInfo(SymbolKind k, std::string n, const parser::ast::ASTNode *node = nullptr)
            : kind(k), name(std::move(n)), type(types::Unknown), isMutable(false), isDefined(false), declarationNode(node) {}

         // Update a forward declaration
         void define(std::shared_ptr<const Type> finalType) {
            if (kind != SymbolKind::TYPE) { /* error or assert */ }
            type = finalType;
            isDefined = true;
         }
    };

    // Using separate namespaces for types and values simplifies lookup
    // based on context (e.g., `x: T` looks for T in type namespace, `x = y` looks for y in value namespace)
    using ValueScope = std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>;
    using TypeScope = std::unordered_map<std::string, std::shared_ptr<SymbolInfo>>;

    class Environment {
    private:
        std::vector<ValueScope> valueScopes;
        std::vector<TypeScope> typeScopes;
        ErrorCollector &errors; // Reference to collect errors directly

         // Helper for recursive lookup
        std::shared_ptr<SymbolInfo> findSymbolRecursive(const std::vector<std::string>& nameParts, size_t partIndex, const TypeScope &currentScope) const;
        std::shared_ptr<SymbolInfo> findSymbolRecursive(const std::vector<std::string>& nameParts, size_t partIndex, const std::shared_ptr<const RecordType>& recordScope) const;
        std::shared_ptr<SymbolInfo> findSymbolRecursive(const std::vector<std::string>& nameParts, size_t partIndex, const std::shared_ptr<const InterfaceType>& ifaceScope) const;


    public:
        Environment(ErrorCollector &err);

        void pushScope();
        void popScope();

        // Define symbols in the *current *scope
        bool defineValue(const std::string &name, std::shared_ptr<const Type> type, bool isMutable = true, const parser::ast::ASTNode *node = nullptr);
        bool defineType(const std::string &name, std::shared_ptr<const Type> type, const parser::ast::ASTNode *node = nullptr);

        // Define a forward-declared type (placeholder)
        bool forwardDeclareType(const std::string &name, const parser::ast::ASTNode *node = nullptr);
        // Update a forward-declared type with its definition
        bool updateForwardDeclareType(const std::string &name, std::shared_ptr<const Type> type);

        // Lookup symbols, searching scopes from current outwards
        std::shared_ptr<SymbolInfo> lookupValue(const std::string &name) const;
        std::shared_ptr<SymbolInfo> lookupType(const std::string &name) const;
        std::shared_ptr<SymbolInfo> lookupType(const std::vector<std::string>& nameParts) const; // For qualified names like http.Response


        // Getters for predefined types (could be static or members)
        std::shared_ptr<const Type> getAnyType() const { return types::Any; }
        std::shared_ptr<const Type> getNilType() const { return types::Nil; }
        std::shared_ptr<const Type> getBooleanType() const { return types::Boolean; }
        std::shared_ptr<const Type> getIntegerType() const { return types::Integer; }
        std::shared_ptr<const Type> getNumberType() const { return types::Number; }
        std::shared_ptr<const Type> getStringType() const { return types::String; }
        std::shared_ptr<const Type> getThreadType() const { return types::Thread; }
        std::shared_ptr<const Type> getUnknownType() const { return types::Unknown; }

        // Store context about the current function being checked (for return statements)
        struct FunctionContext {
             std::vector<std::shared_ptr<const Type>> expectedReturnTypes;
             // bool allowsVarReturn; // TODO
        };
        std::vector<FunctionContext> functionStack;

        void enterFunction(const FunctionContext &context);
        void exitFunction();
        std::optional<FunctionContext> getCurrentFunctionContext() const;
    };

} // namespace teal::typechecker
