#pragma once

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <unordered_map>
#include <optional>
#include <iostream> // For debugging output (optional)

#include "AST.hpp"   // Need AST nodes for function/record structures

namespace teal::parser::typechecker
{
    // Forward Declarations
    struct Type;
    class Environment; // Needed for resolving named types

    // --- Base Type Class ---
    struct Type
    {
        enum class Kind {
            ANY, NIL, BOOLEAN, INTEGER, NUMBER, STRING, THREAD,
            ARRAY, TUPLE, MAP, FUNCTION, RECORD, INTERFACE, ENUM, UNION,
            NOMINAL, // Represents a named type before full resolution
            TYPE_ALIAS,
            // --- Internal/Helper Kinds ---
            MULTI_RETURN, // Represents multiple return values
            UNKNOWN // Placeholder/Error type
        } kind;

        // Optional name for named types (Records, Enums, Interfaces, Aliases)
        std::optional<std::string> name;

        explicit Type(Kind k, std::optional<std::string> n = std::nullopt) : kind(k), name(std::move(n)) {}
        virtual ~Type() = default;

        // Basic type checks
        bool isAny() const { return kind == Kind::ANY; }
        bool isNil() const { return kind == Kind::NIL; }
        bool isBoolean() const { return kind == Kind::BOOLEAN; }
        bool isInteger() const { return kind == Kind::INTEGER; }
        bool isNumber() const { return kind == Kind::NUMBER || kind == Kind::INTEGER; } // Integer is a Number
        bool isString() const { return kind == Kind::STRING; }
        bool isThread() const { return kind == Kind::THREAD; }
        bool isPrimitive() const {
            return isBoolean() || isNumber() || isString() || isThread() || isNil();
        }
        bool isTableLike() const {
            return kind == Kind::ARRAY || kind == Kind::TUPLE || kind == Kind::MAP || kind == Kind::RECORD || kind == Kind::INTERFACE;
        }
        bool isNominal() const {
            return kind == Kind::RECORD || kind == Kind::INTERFACE || kind == Kind::ENUM || kind == Kind::NOMINAL || kind == Kind::TYPE_ALIAS;
        }


        // Virtual method for string representation (useful for errors/debugging)
        virtual std::string toString() const;

        // Get the underlying type if it's an alias
        virtual std::shared_ptr<const Type> getUnderlyingType() const { return nullptr; } // Overridden by TypeAlias
    };

    // --- Concrete Type Classes ---

    struct AnyType : Type { AnyType() : Type(Kind::ANY) {} std::string toString() const override; };
    struct NilType : Type { NilType() : Type(Kind::NIL) {} std::string toString() const override; };
    struct BooleanType : Type { BooleanType() : Type(Kind::BOOLEAN) {} std::string toString() const override; };
    struct IntegerType : Type { IntegerType() : Type(Kind::INTEGER) {} std::string toString() const override; };
    struct NumberType : Type { NumberType() : Type(Kind::NUMBER) {} std::string toString() const override; };
    struct StringType : Type { StringType() : Type(Kind::STRING) {} std::string toString() const override; };
    struct ThreadType : Type { ThreadType() : Type(Kind::THREAD) {} std::string toString() const override; };
    struct UnknownType : Type { UnknownType() : Type(Kind::UNKNOWN) {} std::string toString() const override; }; // Represents an error state

    struct ArrayType : Type {
        std::shared_ptr<const Type> elementType;
        ArrayType(std::shared_ptr<const Type> elem) : Type(Kind::ARRAY), elementType(std::move(elem)) {}
        std::string toString() const override;
    };

    struct TupleType : Type {
        std::vector<std::shared_ptr<const Type>> elementTypes;
        TupleType(std::vector<std::shared_ptr<const Type>> elems) : Type(Kind::TUPLE), elementTypes(std::move(elems)) {}
        std::string toString() const override;
    };

    struct MapType : Type {
        std::shared_ptr<const Type> keyType;
        std::shared_ptr<const Type> valueType;
        MapType(std::shared_ptr<const Type> key, std::shared_ptr<const Type> value)
            : Type(Kind::MAP), keyType(std::move(key)), valueType(std::move(value)) {}
        std::string toString() const override;
    };

    struct FunctionType : Type {
        struct Parameter {
            std::optional<std::string> name;
            std::shared_ptr<const Type> type;
            bool isOptional;
            // bool isVararg; // TODO: Add vararg support later
        };
        std::vector<Parameter> parameters;
        std::vector<std::shared_ptr<const Type>> returnTypes;
        // bool varargReturn; // TODO: Add vararg support later

        // Constructor without name (for anonymous functions or type nodes)
        FunctionType(std::vector<Parameter> params, std::vector<std::shared_ptr<const Type>> returns)
            : Type(Kind::FUNCTION), parameters(std::move(params)), returnTypes(std::move(returns)) {}
        // Constructor with name (for named function declarations)
         FunctionType(std::string funcName, std::vector<Parameter> params, std::vector<std::shared_ptr<const Type>> returns)
            : Type(Kind::FUNCTION, std::move(funcName)), parameters(std::move(params)), returnTypes(std::move(returns)) {}

        std::string toString() const override;
    };

    // Forward declare RecordType and InterfaceType for mutual dependency
    struct RecordType;
    struct InterfaceType;

    struct FieldInfo {
        std::shared_ptr<const Type> type;
        // Could add source location, etc.
    };

    struct InterfaceType : Type {
        // Interfaces this one inherits from
        std::vector<std::shared_ptr<const InterfaceType>> baseInterfaces;
        // Fields/methods declared directly in this interface
        std::unordered_map<std::string, FieldInfo> fields;
        // Optional: store the AST node for reference
        const parser::ast::RecordBody* definition = nullptr;


        InterfaceType(std::string name, const parser::ast::RecordBody* def = nullptr)
            : Type(Kind::INTERFACE, std::move(name)), definition(def) {}

        // Method to recursively get all fields (including inherited)
        std::unordered_map<std::string, FieldInfo> getAllFields() const;
         std::optional<FieldInfo> findField(const std::string& fieldName) const;

        std::string toString() const override;
    };

    struct RecordType : Type {
        // Interfaces this record implements
        std::vector<std::shared_ptr<const InterfaceType>> implementedInterfaces;
        // Optional: structural base type (e.g., array part) - Simple for now
        std::shared_ptr<const Type> structuralBase; // e.g., ArrayType for `is {Node}`
        // Fields declared directly in this record
        std::unordered_map<std::string, FieldInfo> fields;
        // Optional: store the AST node for reference
        const parser::ast::RecordBody* definition = nullptr;
        // Optional: where clause for discriminated unions
        const parser::ast::Expression* whereClause = nullptr; // Needs evaluation context later

        RecordType(std::string name, const parser::ast::RecordBody* def = nullptr)
            : Type(Kind::RECORD, std::move(name)), definition(def) {}

        // Method to recursively get all fields (declared + inherited from interfaces)
        std::unordered_map<std::string, FieldInfo> getAllFields() const;
        std::optional<FieldInfo> findField(const std::string& fieldName) const;

        std::string toString() const override;
    };

    struct EnumType : Type {
        std::vector<std::string> members;

        EnumType(std::string name, std::vector<std::string> mems)
            : Type(Kind::ENUM, std::move(name)), members(std::move(mems)) {}

        bool hasMember(const std::string& memberName) const;
        std::string toString() const override;
    };

    struct UnionType : Type {
        // Store options sorted/canonicalized? For now, just a vector.
        std::vector<std::shared_ptr<const Type>> options;

        UnionType(std::vector<std::shared_ptr<const Type>> opts); // Constructor handles simplification/flattening
        std::string toString() const override;

        // Helper to check if a type is contained within the union
        bool contains(const std::shared_ptr<const Type>& type) const;
    };

    // Represents a named type during resolution phases
    struct NominalType : Type {
        std::vector<std::string> nameParts; // e.g., {"http", "Response"}
        // std::vector<std::shared_ptr<const Type>> typeArguments; // TODO: Generics

        NominalType(std::vector<std::string> parts)
            : Type(Kind::NOMINAL, parts.back()), nameParts(std::move(parts)) {} // Use last part as temp name
         std::string toString() const override;
    };

     // Represents a type alias like `local type MyNumber = number`
    struct TypeAlias : Type {
        std::shared_ptr<const Type> aliasedType;
        // std::vector<GenericParam> genericParams; // TODO: Generics

        TypeAlias(std::string aliasName, std::shared_ptr<const Type> target)
            : Type(Kind::TYPE_ALIAS, std::move(aliasName)), aliasedType(std::move(target)) {}

        std::shared_ptr<const Type> getUnderlyingType() const override { return aliasedType; }
        std::string toString() const override;
    };

    // Helper type to represent multiple return values from functions or casts
    struct MultiReturnType : Type {
         std::vector<std::shared_ptr<const Type>> types;

         MultiReturnType(std::vector<std::shared_ptr<const Type>> t)
             : Type(Kind::MULTI_RETURN), types(std::move(t)) {}
         std::string toString() const override;
    };


    // --- Type Comparison and Utility Functions ---

    // Strict equality check
    bool areEqual(const std::shared_ptr<const Type>& a, const std::shared_ptr<const Type>& b);

    // Subtype check (is 'sub' a subtype of 'super'?)
    // Handles covariance/contravariance for functions, etc.
    bool isSubtype(const std::shared_ptr<const Type>& sub, const std::shared_ptr<const Type>& super);

    // Assignability check (can a value of type 'valueType' be assigned to a variable of type 'targetType'?)
    // Typically: isSubtype(valueType, targetType) || (valueType->isNil() && !targetType->isNil())
    // Nil can be assigned to any non-nil type.
    bool isAssignable(const std::shared_ptr<const Type>& targetType, const std::shared_ptr<const Type>& valueType);

    // Helper to get the "real" underlying type, resolving aliases
    std::shared_ptr<const Type> resolveAliases(std::shared_ptr<const Type> type);

    // --- Predefined Global Types ---
    namespace types {
        inline std::shared_ptr<const Type> Any = std::make_shared<AnyType>();
        inline std::shared_ptr<const Type> Nil = std::make_shared<NilType>();
        inline std::shared_ptr<const Type> Boolean = std::make_shared<BooleanType>();
        inline std::shared_ptr<const Type> Integer = std::make_shared<IntegerType>();
        inline std::shared_ptr<const Type> Number = std::make_shared<NumberType>();
        inline std::shared_ptr<const Type> String = std::make_shared<StringType>();
        inline std::shared_ptr<const Type> Thread = std::make_shared<ThreadType>();
        inline std::shared_ptr<const Type> Unknown = std::make_shared<UnknownType>(); // Error type

        // Helper to create simple function types
        inline std::shared_ptr<FunctionType> makeFunction(
            std::vector<std::shared_ptr<const Type>> params,
            std::vector<std::shared_ptr<const Type>> returns)
        {
            std::vector<FunctionType::Parameter> fParams;
            for(const auto& p : params) {
                fParams.push_back({std::nullopt, p, false});
            }
            return std::make_shared<FunctionType>(std::move(fParams), std::move(returns));
        }
         inline std::shared_ptr<FunctionType> makeFunction(
            std::vector<FunctionType::Parameter> params,
            std::vector<std::shared_ptr<const Type>> returns)
        {
             return std::make_shared<FunctionType>(std::move(params), std::move(returns));
        }
    } // namespace types

} // namespace teal::typechecker
