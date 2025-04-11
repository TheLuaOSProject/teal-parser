#include "TypeSystem.hpp"
#include <sstream>
#include <algorithm>
#include <set> // For simplifying unions

namespace teal::parser::typechecker
{
    // --- toString Implementations ---

    std::string Type::toString() const {
        // Default implementation or make it pure virtual
        switch (kind) {
            case Kind::ANY: return "any";
            case Kind::NIL: return "nil";
            case Kind::BOOLEAN: return "boolean";
            case Kind::INTEGER: return "integer";
            case Kind::NUMBER: return "number";
            case Kind::STRING: return "string";
            case Kind::THREAD: return "thread";
            case Kind::ARRAY: return "array<?>"; // Should be specialized
            case Kind::TUPLE: return "tuple<?>"; // Should be specialized
            case Kind::MAP: return "map<?,?>"; // Should be specialized
            case Kind::FUNCTION: return "function(...)"; // Should be specialized
            case Kind::RECORD: return name.value_or("record<?>");
            case Kind::INTERFACE: return name.value_or("interface<?>");
            case Kind::ENUM: return name.value_or("enum<?>");
            case Kind::UNION: return "union<?>"; // Should be specialized
            case Kind::NOMINAL: return name.value_or("nominal<?>");
            case Kind::TYPE_ALIAS: return name.value_or("alias<?>");
            case Kind::MULTI_RETURN: return "(multiple values)"; // Should be specialized
            case Kind::UNKNOWN: return "<unknown>";
            default: return "<error_type>";
        }
    }

    std::string AnyType::toString() const { return "any"; }
    std::string NilType::toString() const { return "nil"; }
    std::string BooleanType::toString() const { return "boolean"; }
    std::string IntegerType::toString() const { return "integer"; }
    std::string NumberType::toString() const { return "number"; }
    std::string StringType::toString() const { return "string"; }
    std::string ThreadType::toString() const { return "thread"; }
    std::string UnknownType::toString() const { return "<unknown>"; }


    std::string ArrayType::toString() const {
        return "{" + (elementType ? elementType->toString() : "?") + "}";
    }

    std::string TupleType::toString() const {
        std::stringstream ss;
        ss << "{";
        for (size_t i = 0; i < elementTypes.size(); ++i) {
            ss << (elementTypes[i] ? elementTypes[i]->toString() : "?");
            if (i < elementTypes.size() - 1) ss << ", ";
        }
        ss << "}";
        return ss.str();
    }

    std::string MapType::toString() const {
        return "{" + (keyType ? keyType->toString() : "?") + ":" + (valueType ? valueType->toString() : "?") + "}";
    }

    std::string FunctionType::toString() const {
        std::stringstream ss;
        ss << "function";
        // TODO: Add generic params <...> later
        ss << "(";
        for (size_t i = 0; i < parameters.size(); ++i) {
            const auto &p = parameters[i];
            if (p.name) ss << *p.name;
            if (p.name and p.type) ss << ": ";
            if (p.type) ss << p.type->toString();
            else if (not p.name) ss << "?"; // Should not happen if type resolution worked
            if (p.isOptional) ss << "?";
            if (i < parameters.size() - 1) ss << ", ";
        }
        // TODO: Add varargs ... later
        ss << ")";
        if (not returnTypes.empty()) {
            ss << ": ";
            if (returnTypes.size() > 1) ss << "(";
            for (size_t i = 0; i < returnTypes.size(); ++i) {
                ss << (returnTypes[i] ? returnTypes[i]->toString() : "?");
                if (i < returnTypes.size() - 1) ss << ", ";
            }
             // TODO: Add vararg return ... later
            if (returnTypes.size() > 1) ss << ")";
        }
        return ss.str();
    }

     std::unordered_map<std::string, FieldInfo> InterfaceType::getAllFields() const {
        std::unordered_map<std::string, FieldInfo> allFields = fields; // Start with own fields
        for (const auto &base : baseInterfaces) {
            if (not base) continue; // Should not happen
            auto baseFields = base->getAllFields();
            // Merge, potentially checking for conflicts if needed (simple overwrite for now)
            allFields.insert(baseFields.begin(), baseFields.end());
        }
        return allFields;
    }

     std::optional<FieldInfo> InterfaceType::findField(const std::string &fieldName) const {
         auto it = fields.find(fieldName);
         if (it != fields.end()) {
             return it->second;
         }
         for (const auto &base : baseInterfaces) {
              if (not base) continue;
              auto found = base->findField(fieldName);
              if (found) return found;
         }
         return std::nullopt;
     }


    std::string InterfaceType::toString() const {
        return name.value_or("interface");
    }

    std::unordered_map<std::string, FieldInfo> RecordType::getAllFields() const {
        std::unordered_map<std::string, FieldInfo> allFields = fields; // Start with own fields
        for (const auto &iface : implementedInterfaces) {
            if (not iface) continue; // Should not happen
            auto ifaceFields = iface->getAllFields();
            // Merge, potentially checking for conflicts if needed (simple overwrite for now)
            allFields.insert(ifaceFields.begin(), ifaceFields.end());
        }
        return allFields;
    }

     std::optional<FieldInfo> RecordType::findField(const std::string &fieldName) const {
         auto it = fields.find(fieldName);
         if (it != fields.end()) {
             return it->second;
         }
         for (const auto &iface : implementedInterfaces) {
              if (not iface) continue;
              auto found = iface->findField(fieldName);
              if (found) return found;
         }
         return std::nullopt;
     }

    std::string RecordType::toString() const {
        return name.value_or("record");
    }

    bool EnumType::hasMember(const std::string &memberName) const {
        return std::find(members.begin(), members.end(), memberName) != members.end();
    }

    std::string EnumType::toString() const {
         return name.value_or("enum");
    }

    // Helper for comparing shared_ptr<const Type> for use in sets
    struct CompareTypePtr {
        bool operator()(const std::shared_ptr<const Type>& a, const std::shared_ptr<const Type>& b) const {
            // Simple comparison based on address or a more robust hash/comparison if needed
            // For canonicalization, we need a stable ordering. Kind + specific details?
            // Let's try a basic pointer comparison first, assuming unique type instances for canonical types.
             if (not a and not b) return false;
             if (not a) return true;
             if (not b) return false;
             // Comparing pointers only works if types are canonicalized (e.g. types::Number always points to the same object)
             // Otherwise need a deep comparison, which is complex.
             // For now, rely on pointer comparison + hope canonicalization works.
            if (a.get() < b.get()) return true;
            if (a.get() > b.get()) return false;
            // If pointers are same, they are equal.
            return false; // Equal pointers
        }
    };


    UnionType::UnionType(std::vector<std::shared_ptr<const Type>> opts) : Type(Kind::UNION) {
        // Simplify and flatten the union options
        std::set<std::shared_ptr<const Type>, CompareTypePtr> uniqueOptions;
        std::vector<std::shared_ptr<const Type>> toProcess = std::move(opts);
        std::vector<std::shared_ptr<const Type>> nextProcess;

        while (not toProcess.empty()) {
            for (const auto &opt : toProcess) {
                 if (not opt) continue;
                 auto realOpt = resolveAliases(opt); // Resolve aliases
                 if (not realOpt) continue;

                 if (realOpt->kind == Kind::UNION) {
                     // Flatten nested unions
                     const auto *nestedUnion = static_cast<const UnionType*>(realOpt.get());
                     nextProcess.insert(nextProcess.end(), nestedUnion->options.begin(), nestedUnion->options.end());
                 } else if (realOpt->kind != Kind::NIL and realOpt->kind != Kind::ANY) { // Don't add nil/any directly unless it's the *only *type
                     uniqueOptions.insert(realOpt);
                 } else if (realOpt->kind == Kind::ANY) {
                     // If 'any' is an option, the whole union becomes 'any'
                     options = { types::Any };
                     return;
                 } else if (realOpt->kind == Kind::NIL) {
                     // Handle nil separately? Or just let it be part of the set?
                     // Let's include it.
                      uniqueOptions.insert(realOpt);
                 }
            }
            toProcess = std::move(nextProcess);
            nextProcess.clear();
        }


        // Remove redundancies (e.g., number | integer -> number)
        bool hasNumber = false;
        for(const auto &opt : uniqueOptions) {
            if (opt->kind == Kind::NUMBER) hasNumber = true;
        }

        options.reserve(uniqueOptions.size());
        for(const auto &opt : uniqueOptions) {
            if (hasNumber and opt->kind == Kind::INTEGER) {
                continue; // Skip integer if number is present
            }
            options.push_back(opt);
        }

        // If only one option remains after simplification, it's not a union anymore
        // This should be handled by the caller creating the union, checking the size first.
        // Or return the single type directly? For now, allow single-element "unions".

        // Sort for canonical representation (optional but good)
        std::sort(options.begin(), options.end(), CompareTypePtr());

    }

     bool UnionType::contains(const std::shared_ptr<const Type>& type) const {
         if (not type) return false;
         auto realType = resolveAliases(type);
         if (not realType) return false;
         for (const auto &opt : options) {
             if (areEqual(opt, realType)) {
                 return true;
             }
         }
         return false;
     }


    std::string UnionType::toString() const {
        std::stringstream ss;
        for (size_t i = 0; i < options.size(); ++i) {
            ss << (options[i] ? options[i]->toString() : "?");
            if (i < options.size() - 1) ss << " | ";
        }
        return ss.str();
    }

     std::string NominalType::toString() const {
         std::stringstream ss;
         for(size_t i = 0; i < nameParts.size(); ++i) {
             ss << nameParts[i];
             if (i < nameParts.size() - 1) ss << ".";
         }
         // TODO: Add type arguments <...> later
         return ss.str();
     }

     std::string TypeAlias::toString() const {
         // Show the alias name, maybe the target type in verbose mode?
         return name.value_or("alias"); // Or maybe name.value_or("?") + " = " + aliasedType->toString();
     }

     std::string MultiReturnType::toString() const {
         std::stringstream ss;
         ss << "(";
          for (size_t i = 0; i < types.size(); ++i) {
            ss << (types[i] ? types[i]->toString() : "?");
            if (i < types.size() - 1) ss << ", ";
        }
        ss << ")";
        return ss.str();
     }

    // --- Type Comparison Implementations ---

    std::shared_ptr<const Type> resolveAliases(std::shared_ptr<const Type> type) {
        while (type and type->kind == Type::Kind::TYPE_ALIAS) {
             type = static_cast<const TypeAlias*>(type.get())->getUnderlyingType();
        }
        return type;
    }


    bool areEqual(const std::shared_ptr<const Type>& a, const std::shared_ptr<const Type>& b) {
        if (not a or not b) return not a and not b; // Both null or not

        auto realA = resolveAliases(a);
        auto realB = resolveAliases(b);

        if (not realA or not realB) return not realA and not realB; // Both null or not after resolving

        if (realA.get() == realB.get()) return true; // Same object instance (common for primitives/builtins)
        if (realA->kind != realB->kind) return false;

        // Kind-specific comparisons
        switch (realA->kind) {
            case Type::Kind::ANY:
            case Type::Kind::NIL:
            case Type::Kind::BOOLEAN:
            case Type::Kind::INTEGER:
            case Type::Kind::NUMBER:
            case Type::Kind::STRING:
            case Type::Kind::THREAD:
            case Type::Kind::UNKNOWN:
                return true; // Already checked kind match

            case Type::Kind::ARRAY: {
                const auto *arrA = static_cast<const ArrayType*>(realA.get());
                const auto *arrB = static_cast<const ArrayType*>(realB.get());
                return areEqual(arrA->elementType, arrB->elementType);
            }
            case Type::Kind::TUPLE: {
                const auto *tupA = static_cast<const TupleType*>(realA.get());
                const auto *tupB = static_cast<const TupleType*>(realB.get());
                if (tupA->elementTypes.size() != tupB->elementTypes.size()) return false;
                for (size_t i = 0; i < tupA->elementTypes.size(); ++i) {
                    if (not areEqual(tupA->elementTypes[i], tupB->elementTypes[i])) return false;
                }
                return true;
            }
            case Type::Kind::MAP: {
                const auto *mapA = static_cast<const MapType*>(realA.get());
                const auto *mapB = static_cast<const MapType*>(realB.get());
                return areEqual(mapA->keyType, mapB->keyType) and areEqual(mapA->valueType, mapB->valueType);
            }
            case Type::Kind::FUNCTION: {
                const auto *funcA = static_cast<const FunctionType*>(realA.get());
                const auto *funcB = static_cast<const FunctionType*>(realB.get());
                if (funcA->parameters.size() != funcB->parameters.size() or funcA->returnTypes.size() != funcB->returnTypes.size()) {
                    return false;
                }
                // TODO: Check varargs later
                for (size_t i = 0; i < funcA->parameters.size(); ++i) {
                    if (funcA->parameters[i].isOptional != funcB->parameters[i].isOptional or
                        not areEqual(funcA->parameters[i].type, funcB->parameters[i].type)) {
                        return false;
                    }
                }
                for (size_t i = 0; i < funcA->returnTypes.size(); ++i) {
                    if (not areEqual(funcA->returnTypes[i], funcB->returnTypes[i])) return false;
                }
                return true;
            }
            case Type::Kind::RECORD:
            case Type::Kind::INTERFACE:
            case Type::Kind::ENUM:
            case Type::Kind::NOMINAL: // Nominal types are equal only if they are the same definition
                return realA.get() == realB.get(); // Compare pointers (assuming canonical definitions)

            case Type::Kind::UNION: {
                 const auto *unionA = static_cast<const UnionType*>(realA.get());
                 const auto *unionB = static_cast<const UnionType*>(realB.get());
                 if (unionA->options.size() != unionB->options.size()) return false;
                 // Assumes options are canonicalized/sorted
                 for (size_t i = 0; i < unionA->options.size(); ++i) {
                     if (not areEqual(unionA->options[i], unionB->options[i])) return false;
                 }
                 return true;
            }

            case Type::Kind::TYPE_ALIAS: // Should have been resolved by resolveAliases
                 return false; // Indicate error or unexpected state

            case Type::Kind::MULTI_RETURN: {
                const auto *multiA = static_cast<const MultiReturnType*>(realA.get());
                const auto *multiB = static_cast<const MultiReturnType*>(realB.get());
                 if (multiA->types.size() != multiB->types.size()) return false;
                 for (size_t i = 0; i < multiA->types.size(); ++i) {
                     if (not areEqual(multiA->types[i], multiB->types[i])) return false;
                 }
                 return true;
            }

            default:
                return false;
        }
    }

    bool isSubtype(const std::shared_ptr<const Type>& sub, const std::shared_ptr<const Type>& super) {
        if (not sub or not super) return false;

        auto realSub = resolveAliases(sub);
        auto realSuper = resolveAliases(super);

         if (not realSub or not realSuper) return false;

        if (realSub.get() == realSuper.get()) return true; // Same type
        if (realSuper->isAny()) return true;           // Anything is subtype of 'any'
        // if (realSub->isNil()) return true;            // 'nil' is subtype of anything (for assignability, see isAssignable)


        // Handle specific subtype relationships
        if (realSub->isInteger() and realSuper->isNumber()) return true; // integer is subtype of number

        // Structural subtyping for tables (might deviate from Teal's nominal preference for records)
        // Let's stick to nominal for records/interfaces mostly.

        // Array: {T} <: {U} if T <: U
        if (realSub->kind == Type::Kind::ARRAY and realSuper->kind == Type::Kind::ARRAY) {
            const auto *arrSub = static_cast<const ArrayType*>(realSub.get());
            const auto *arrSuper = static_cast<const ArrayType*>(realSuper.get());
            return isSubtype(arrSub->elementType, arrSuper->elementType);
        }

        // Tuple: {T1, T2} <: {U1, U2} if T1<:U1 and T2<:U2
        if (realSub->kind == Type::Kind::TUPLE and realSuper->kind == Type::Kind::TUPLE) {
             const auto *tupSub = static_cast<const TupleType*>(realSub.get());
             const auto *tupSuper = static_cast<const TupleType*>(realSuper.get());
             if (tupSub->elementTypes.size() != tupSuper->elementTypes.size()) return false;
             for (size_t i = 0; i < tupSub->elementTypes.size(); ++i) {
                 if (not isSubtype(tupSub->elementTypes[i], tupSuper->elementTypes[i])) return false;
             }
             return true;
        }

        // Map: {K1:V1} <: {K2:V2} if K1==K2 and V1<:V2 (Invariant keys, Covariant values)
        if (realSub->kind == Type::Kind::MAP and realSuper->kind == Type::Kind::MAP) {
            const auto *mapSub = static_cast<const MapType*>(realSub.get());
            const auto *mapSuper = static_cast<const MapType*>(realSuper.get());
            return areEqual(mapSub->keyType, mapSuper->keyType) and isSubtype(mapSub->valueType, mapSuper->valueType);
        }

        // Function: function(P1..):R1 <: function(P2..):R2 if P2<:P1 (contravariant) and R1<:R2 (covariant)
         if (realSub->kind == Type::Kind::FUNCTION and realSuper->kind == Type::Kind::FUNCTION) {
             const auto *funcSub = static_cast<const FunctionType*>(realSub.get());
             const auto *funcSuper = static_cast<const FunctionType*>(realSuper.get());

             // Basic arity check (ignoring optional/varargs for now)
             if (funcSub->parameters.size() < funcSuper->parameters.size()) return false; // Subtype must accept at least as many required args
             // More precise check needed for optionals

             if (funcSub->returnTypes.size() != funcSuper->returnTypes.size()) return false; // Must return same number of values
             // TODO: Handle varargs correctly

             // Check parameters (contravariant) P2 <: P1 -> isSubtype(P2, P1)
             for (size_t i = 0; i < funcSuper->parameters.size(); ++i) {
                 // Ignore optionality difference for basic subtyping? Strict check:
                 //if (funcSub->parameters[i].isOptional != funcSuper->parameters[i].isOptional) return false;

                 // Contravariance check: super's param type must be subtype of sub's param type
                 if (not isSubtype(funcSuper->parameters[i].type, funcSub->parameters[i].type)) {
                     return false;
                 }
             }
             // Check return types (covariant) R1 <: R2 -> isSubtype(R1, R2)
             for (size_t i = 0; i < funcSub->returnTypes.size(); ++i) {
                 if (not isSubtype(funcSub->returnTypes[i], funcSuper->returnTypes[i])) {
                     return false;
                 }
             }
             return true;
         }

         // Record/Interface Subtyping (Nominal via `is`)
         if (realSub->kind == Type::Kind::RECORD and realSuper->kind == Type::Kind::INTERFACE) {
             const auto *recSub = static_cast<const RecordType*>(realSub.get());
             const auto *ifaceSuper = static_cast<const InterfaceType*>(realSuper.get());
             for(const auto &implemented : recSub->implementedInterfaces) {
                 if (not implemented) continue;
                 // Check direct implementation or recursive subtyping of interfaces
                 if (areEqual(implemented, realSuper) or isSubtype(implemented, realSuper)) {
                     return true;
                 }
             }
             // Check structural base type e.g. `record R is {R}`
             if(recSub->structuralBase and areEqual(recSub->structuralBase, realSuper)) {
                // This comparison is simplistic. `is {Node}` means it implements array-like access.
                // How to check if `realSuper` is exactly the required array type?
                // For now, only exact match of the structural base pointer.
                 return true;
             }

             (void)ifaceSuper;

         }
          if (realSub->kind == Type::Kind::INTERFACE and realSuper->kind == Type::Kind::INTERFACE) {
             const auto *ifaceSub = static_cast<const InterfaceType*>(realSub.get());
             const auto *ifaceSuper = static_cast<const InterfaceType*>(realSuper.get());
             for(const auto &base : ifaceSub->baseInterfaces) {
                  if (not base) continue;
                 if (areEqual(base, realSuper) or isSubtype(base, realSuper)) {
                     return true;
                 }
             }
             (void)ifaceSuper;
         }

         // Union Subtyping:
         // A <: B | C if A <: B or A <: C
         if (realSuper->kind == Type::Kind::UNION) {
             const auto *unionSuper = static_cast<const UnionType*>(realSuper.get());
             for (const auto &opt : unionSuper->options) {
                 if (isSubtype(realSub, opt)) {
                     return true;
                 }
             }
         }
         // A | B <: C if A <: C and B <: C
         if (realSub->kind == Type::Kind::UNION) {
             const auto *unionSub = static_cast<const UnionType*>(realSub.get());
             for (const auto &opt : unionSub->options) {
                 if (not isSubtype(opt, realSuper)) {
                     return false; // All options must be subtypes
                 }
             }
             return true; // All options were subtypes
         }


        // Default: No subtype relationship found
        return false;
    }

    bool isAssignable(const std::shared_ptr<const Type>& targetType, const std::shared_ptr<const Type>& valueType) {
        if (not targetType or not valueType) return false; // Cannot assign to/from invalid types

        auto realTarget = resolveAliases(targetType);
        auto realValue = resolveAliases(valueType);

        if (not realTarget or not realValue) return false;


        // Rule 1: Any value can be assigned to 'any'
        if (realTarget->isAny()) {
            return true;
        }
        // Rule 1b: 'any' can only be assigned to 'any' (or requires cast)
        if (realValue->isAny() and not realTarget->isAny()) {
            return false;
        }


        // Rule 2: Check subtyping relationship
        return isSubtype(realValue, realTarget);
    }


} // namespace teal::typechecker