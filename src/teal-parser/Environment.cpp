#include "Environment.hpp"
#include "AST.hpp" // For node access

namespace teal::parser::typechecker
{
    Environment::Environment(ErrorCollector &err) : errors(err) {
        // Initialize with a global scope
        pushScope();
        // Builtins should be added here or in a separate setup function
    }

    void Environment::pushScope() {
        valueScopes.emplace_back();
        typeScopes.emplace_back();
    }

    void Environment::popScope() {
        if (valueScopes.size() > 1) { // Don't pop the global scope
            valueScopes.pop_back();
            typeScopes.pop_back();
        } else {
            // Log error or throw: Trying to pop global scope
             errors.addError("Internal error: Tried to pop global scope", 0, 0);
        }
    }

    bool Environment::defineValue(const std::string &name, std::shared_ptr<const Type> type, bool isMutable, const parser::ast::ASTNode *node) {
        if (valueScopes.empty()) {
             errors.addError("Internal error: No active scope to define value '" + name + "'", node ? node->line : 0, node ? node->column : 0);
            return false;
        }
        auto &currentScope = valueScopes.back();
        if (currentScope.count(name)) {
            errors.addError("Value '" + name + "' already defined in this scope", node ? node->line : 0, node ? node->column : 0);
            // Optionally point to previous definition: errors.addError("Previous definition was here", currentScope[name]->declarationNode);
            return false;
        }
        // Determine SymbolKind (simplistic for now)
        SymbolKind kind = (type and type->kind == Type::Kind::FUNCTION) ? SymbolKind::FUNCTION : SymbolKind::VARIABLE;
        currentScope[name] = std::make_shared<SymbolInfo>(kind, name, type ? type : types::Unknown, isMutable, node);
        return true;
    }

     bool Environment::defineType(const std::string &name, std::shared_ptr<const Type> type, const parser::ast::ASTNode *node) {
         if (typeScopes.empty()) {
             errors.addError("Internal error: No active scope to define type '" + name + "'", node ? node->line : 0, node ? node->column : 0);
             return false;
         }
         auto &currentScope = typeScopes.back();
         if (currentScope.count(name)) {
             // Allow redefinition only if it's updating a forward declaration
             auto existing = currentScope[name];
             if(existing and not existing->isDefined and existing->kind == SymbolKind::TYPE) {
                existing->define(type);
                 // Update the node if the definition provides a more specific one?
                 if(node) existing->declarationNode = node;
                return true;
             } else {
                errors.addError("Type '" + name + "' already defined in this scope", node ? node->line : 0, node ? node->column : 0);
                return false;
             }
         }
         currentScope[name] = std::make_shared<SymbolInfo>(SymbolKind::TYPE, name, type ? type : types::Unknown, false, node);
         return true;
     }

    bool Environment::forwardDeclareType(const std::string &name, const parser::ast::ASTNode *node) {
         if (typeScopes.empty()) {
             errors.addError("Internal error: No active scope to forward declare type '" + name + "'", node ? node->line : 0, node ? node->column : 0);
             return false;
         }
         auto &currentScope = typeScopes.back();
          if (currentScope.count(name)) {
             // Don't error if forward declaration already exists, just ignore.
             // Error if a full definition already exists.
             auto existing = currentScope[name];
             if(existing and existing->isDefined) {
                 errors.addError("Cannot forward declare type '" + name + "', it is already defined.", node ? node->line : 0, node ? node->column : 0);
                 return false;
             }
             return true; // Already forward declared is ok
          }
         currentScope[name] = std::make_shared<SymbolInfo>(SymbolKind::TYPE, name, node); // Creates an undefined SymbolInfo
         return true;
    }

     bool Environment::updateForwardDeclareType(const std::string &name, std::shared_ptr<const Type> type) {
          if (typeScopes.empty()) return false; // Should not happen

          // Search for the forward declaration starting from the current scope
          for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
            auto &scope = *it;
            auto found = scope.find(name);
            if (found != scope.end()) {
                if(not found->second->isDefined and found->second->kind == SymbolKind::TYPE) {
                    found->second->define(type);
                    return true;
                } else {
                    // Found, but it's already defined or not a type? Error?
                    // This case should ideally be caught by defineType.
                    errors.addError("Cannot update type '" + name + "'; it was not forward declared or already defined.", 0, 0); // Need node info
                    return false;
                }
            }
        }
        // Type not found even as forward declaration
        errors.addError("Cannot update type '" + name + "'; forward declaration not found.", 0, 0); // Need node info
        return false;
     }


    std::shared_ptr<SymbolInfo> Environment::lookupValue(const std::string &name) const {
        for (auto it = valueScopes.rbegin(); it != valueScopes.rend(); ++it) {
            const auto &scope = *it;
            auto found = scope.find(name);
            if (found != scope.end()) {
                return found->second;
            }
        }
        return nullptr; // Not found
    }

    std::shared_ptr<SymbolInfo> Environment::lookupType(const std::string &name) const {
         for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
            const auto &scope = *it;
            auto found = scope.find(name);
            if (found != scope.end()) {
                 // Ensure it's actually a type symbol
                 if (found->second->kind == SymbolKind::TYPE) {
                    // if (not found->second->isDefined) { // Handle lookup of forward-declared types?
                    //    errors.addError("Type '" + name + "' is used before its definition is complete.", 0,0); // Need node
                    //    return nullptr; // Or return the undefined symbol? Let's return it for now.
                    // }
                     return found->second;
                 } else {
                     // Found a symbol with this name, but it's not a type
                     errors.addError("Symbol '" + name + "' found, but it is not a type.", 0, 0); // Needs node info
                     return nullptr;
                 }
            }
        }
        return nullptr; // Not found
    }

    // Recursive helper for qualified type lookup (e.g., record.NestedType)
    std::shared_ptr<SymbolInfo> Environment::findSymbolRecursive(
        const std::vector<std::string>& nameParts,
        size_t partIndex,
        const TypeScope &currentScope) const
    {
         if (partIndex >= nameParts.size()) return nullptr; // Should not happen

         const std::string &currentPart = nameParts[partIndex];
         auto found = currentScope.find(currentPart);

         if (found == currentScope.end()) return nullptr; // Not found in this scope

         auto symbol = found->second;
         if (not symbol or symbol->kind != SymbolKind::TYPE or not symbol->isDefined) {
             return nullptr; // Not a defined type
         }

         if (partIndex == nameParts.size() - 1) {
             return symbol; // Found the final part
         }

         // Resolve the type and look inside it for the next part
         auto resolvedType = resolveAliases(symbol->type);
         if (not resolvedType) return nullptr;

         if (resolvedType->kind == Type::Kind::RECORD) {
             auto recordType = std::static_pointer_cast<const RecordType>(resolvedType);
             return findSymbolRecursive(nameParts, partIndex + 1, recordType);
         } else if (resolvedType->kind == Type::Kind::INTERFACE) {
              auto ifaceType = std::static_pointer_cast<const InterfaceType>(resolvedType);
             return findSymbolRecursive(nameParts, partIndex + 1, ifaceType);
         }
          // Add other container types if they can contain nested types (e.g., Modules)

         return nullptr; // Cannot look inside this type
    }

     std::shared_ptr<SymbolInfo> Environment::findSymbolRecursive(
        const std::vector<std::string>& nameParts,
        size_t partIndex,
        const std::shared_ptr<const RecordType>& recordScope) const
    {
        if (not recordScope or partIndex >= nameParts.size()) return nullptr;

        const std::string &fieldName = nameParts[partIndex];
        auto fieldInfoOpt = recordScope->findField(fieldName); // Includes inherited fields

        if (not fieldInfoOpt) return nullptr; // Field not found

        auto fieldType = fieldInfoOpt->type;
        if (not fieldType) return nullptr;

        // Can we treat fields as symbols directly? Maybe wrap FieldInfo in SymbolInfo?
        // For now, assume we want to find a *nested type *definition.
        // This requires records/interfaces to store nested type definitions, not just fields.
        // Let's assume for now qualified names only work via module system or pre-defined structures.
        // A simpler model: look for nested type definitions stored within the Record/Interface AST node?
        // This requires linking TypeSystem back to AST more strongly.

        // --- Simplified approach: Assume qualified lookup is for fields for now ---
        // If we are at the last part, return a temporary SymbolInfo for the field?
        if (partIndex == nameParts.size() - 1) {
            // Can't return a field directly as a SymbolInfo easily unless fields *are *SymbolInfos
            // Let's return nullptr for nested types within records for now, needs better structure.
            // To support `http.Response`, `http` needs to be a module-like record/table
            // containing a `Response` type symbol.
             return nullptr; // TODO: Implement proper nested type lookup
        }

        // Otherwise, try to recurse into the field's type if it's another record/interface
        auto resolvedType = resolveAliases(fieldType);
         if (not resolvedType) return nullptr;

         if (resolvedType->kind == Type::Kind::RECORD) {
             return findSymbolRecursive(nameParts, partIndex + 1, std::static_pointer_cast<const RecordType>(resolvedType));
         } else if (resolvedType->kind == Type::Kind::INTERFACE) {
             return findSymbolRecursive(nameParts, partIndex + 1, std::static_pointer_cast<const InterfaceType>(resolvedType));
         }

         return nullptr; // Cannot look inside this field type
    }
     std::shared_ptr<SymbolInfo> Environment::findSymbolRecursive(
        const std::vector<std::string>& nameParts,
        size_t partIndex,
        const std::shared_ptr<const InterfaceType>& ifaceScope) const
    {
         // Similar logic to RecordType lookup, searching fields/nested types within the interface
          if (not ifaceScope or partIndex >= nameParts.size()) return nullptr;
          // ... (implementation similar to record lookup) ...
          return nullptr; // TODO: Implement proper nested type lookup
    }


    std::shared_ptr<SymbolInfo> Environment::lookupType(const std::vector<std::string>& nameParts) const {
        if (nameParts.empty()) return nullptr;

        // Start search from the innermost scope
        for (auto it = typeScopes.rbegin(); it != typeScopes.rend(); ++it) {
             auto symbol = findSymbolRecursive(nameParts, 0, *it);
             if (symbol) return symbol;
        }

        return nullptr; // Not found
    }


    void Environment::enterFunction(const FunctionContext &context) {
        functionStack.push_back(context);
    }

    void Environment::exitFunction() {
        if (not functionStack.empty()) {
            functionStack.pop_back();
        } else {
             errors.addError("Internal error: Tried to exit function context when not inside one", 0, 0);
        }
    }

    std::optional<Environment::FunctionContext> Environment::getCurrentFunctionContext() const {
        if (not functionStack.empty()) {
            return functionStack.back();
        }
        return std::nullopt;
    }

} 
