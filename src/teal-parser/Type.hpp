#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>
#include <unordered_map>
#include <cassert>

#include "AST.hpp"

namespace teal::parser::typecheck
{

    struct Type;
    struct TypeSymbol;
    struct TypeVarInfo;
    using TypePtr = std::shared_ptr<Type>;
    
    struct Type {
        enum class Kind {
            Nil, Boolean, Number, Integer, String,
            Any, Unknown, Union, Function, Record,
            Enum, Array, Map, Tuple, TypeVar
        };
        Kind kind;
        // Union type members
        std::vector<TypePtr> union_members;
        // Function type signature
        struct FunctionSig {
            std::vector<TypePtr> param_types;
            std::vector<bool> param_optional;
            bool param_varargs;
            std::vector<TypePtr> return_types;
            bool return_varargs;
            std::vector<TypePtr> type_params;
        };
        std::unique_ptr<FunctionSig> func;
        // Record or interface type
        std::shared_ptr<TypeSymbol> record;
        std::vector<TypePtr> type_args;
        // Enum type
        std::shared_ptr<TypeSymbol> enum_type;
        // Array type
        TypePtr element;
        // Map type
        TypePtr key;
        TypePtr value;
        // Tuple type
        std::vector<TypePtr> tuple_types;
        // Generic type variable
        std::shared_ptr<TypeVarInfo> type_var;
    
        Type(Kind k) : kind(k) {}
        static TypePtr makeNil();
        static TypePtr makeBoolean();
        static TypePtr makeNumber();
        static TypePtr makeInteger();
        static TypePtr makeString();
        static TypePtr makeAny();
        static TypePtr makeUnknown();
        static TypePtr makeUnion(const std::vector<TypePtr> &members);
        static TypePtr makeFunction(const std::vector<TypePtr> &params, const std::vector<bool> &opt, bool varargs,
                                    const std::vector<TypePtr> &rets, bool ret_varargs,
                                    const std::vector<TypePtr> &type_params = {});
        static TypePtr makeRecord(std::shared_ptr<TypeSymbol> symbol, const std::vector<TypePtr> &type_args = {});
        static TypePtr makeEnum(std::shared_ptr<TypeSymbol> symbol);
        static TypePtr makeArray(TypePtr element_type);
        static TypePtr makeMap(TypePtr key_type, TypePtr value_type);
        static TypePtr makeTuple(const std::vector<TypePtr> &element_types);
        static TypePtr makeTypeVar(const std::string &name, TypePtr constraint = nullptr);
    
        bool equals(const TypePtr &other) const;
        bool isAssignableTo(const TypePtr &target) const;
        std::string toString() const;
    };
    
    struct TypeSymbol {
        enum class Kind { Record, Interface, Enum, Alias, TypeVar };
        Kind kind;
        std::string name;
        bool is_interface;
        std::unordered_map<std::string, TypePtr> fields;
        TypePtr array_element_type;
        std::vector<TypePtr> interfaces;
        std::vector<TypePtr> type_params;
        std::vector<std::string> enum_values;
        TypePtr alias_target;
        std::shared_ptr<TypeSymbol> parent;
        std::unordered_map<std::string, std::shared_ptr<TypeSymbol>> nested_types;
        teal::parser::ast::RecordBody *ast_record_body = nullptr;
        teal::parser::ast::EnumBody *ast_enum_body = nullptr;
        teal::parser::ast::TypeAliasStatement *ast_type_alias = nullptr;
        TypePtr type;
    
        TypeSymbol(Kind k, std::string nm) : kind(k), name(std::move(nm)), is_interface(false), parent(nullptr) {}
    };
    
    struct TypeVarInfo {
        std::string name;
        TypePtr constraint;
        std::shared_ptr<TypeSymbol> ownerTypeSymbol;
        TypeVarInfo(std::string nm, TypePtr constr = nullptr, std::shared_ptr<TypeSymbol> owner=nullptr)
            : name(std::move(nm)), constraint(constr), ownerTypeSymbol(owner) {}
    };
    
}
