#pragma once
#include <cassert>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "AST.hpp"

namespace teal::parser::typecheck
{
    struct Type;
    struct TypeSymbol;
    struct TypeVariableInfo;
    using TypePtr = std::shared_ptr<Type>;

    struct Type {
        enum class Kind {
            NIL,
            BOOLEAN,
            NUMBER,
            INTEGER,
            STRING,
            ANY,
            UNKNOWN,

            UNION,
            FUNCTION,
            RECORD,
            INTERFACE,
            ENUM,
            ARRAY,
            MAP,
            TUPLE,
            TYPE_VARIABLE
        };
        Kind kind;
        // Union type members
        std::vector<TypePtr> union_members;
        // Function type signature
        struct FunctionSignature {
            std::vector<TypePtr> parameter_types;
            std::vector<bool> optional_parameters;
            bool is_varadict;
            std::vector<TypePtr> return_types;
            bool is_varadict_return;
            std::vector<TypePtr> type_parameters;

            ast::serialisation::Object serialise() const;
        };
        std::unique_ptr<FunctionSignature> function;
        // Record or interface type
        std::shared_ptr<TypeSymbol> record;
        std::vector<TypePtr> type_arguments;
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
        std::shared_ptr<TypeVariableInfo> type_variable;

        Type(Kind k) : kind(k) { }
        static TypePtr make_nil();
        static TypePtr make_boolean();
        static TypePtr make_number();
        static TypePtr make_integer();
        static TypePtr make_string();
        static TypePtr make_any();
        static TypePtr make_unknown();
        static TypePtr make_union(const std::vector<TypePtr> &members);
        static TypePtr make_function(const std::vector<TypePtr> &params, const std::vector<bool> &opt, bool varargs, const std::vector<TypePtr> &rets, bool ret_varargs, const std::vector<TypePtr> &type_params = {});
        static TypePtr make_record(std::shared_ptr<TypeSymbol> symbol, const std::vector<TypePtr> &type_args = {});
        static TypePtr make_enum(std::shared_ptr<TypeSymbol> symbol);
        static TypePtr make_array(TypePtr element_type);
        static TypePtr make_map(TypePtr key_type, TypePtr value_type);
        static TypePtr make_tuple(const std::vector<TypePtr> &element_types);
        static TypePtr make_type_variable(const std::string &name, TypePtr constraint = nullptr);

        bool equals(const TypePtr &other) const;
        bool is_assignable_to(const TypePtr &target) const;
        std::string to_string() const;

        ast::serialisation::Object serialise() const;
    };

    struct TypeSymbol {
        enum class Kind { RECORD, INTERFACE, ENUM, ALIAS, TYPE_VARIABLE };
        Kind kind;
        std::string name;
        bool is_interface;
        std::unordered_map<std::string, TypePtr> fields;
        TypePtr array_element_type;
        std::vector<TypePtr> interfaces;
        std::vector<TypePtr> type_parameters;
        std::vector<std::string> enum_values;
        TypePtr alias_target;
        std::shared_ptr<TypeSymbol> parent;
        std::unordered_map<std::string, std::shared_ptr<TypeSymbol>> nested_types;
        teal::parser::ast::RecordBody *ast_record_body = nullptr;
        teal::parser::ast::EnumBody *ast_enum_body = nullptr;
        teal::parser::ast::TypeAliasStatement *ast_type_alias = nullptr;
        TypePtr type;

        TypeSymbol(Kind k, std::string nm) : kind(k), name(std::move(nm)), is_interface(false), parent(nullptr) { }

        ast::serialisation::Object serialise() const;
    };

    struct TypeVariableInfo {
        std::string name;
        TypePtr constraint;
        std::shared_ptr<TypeSymbol> owner_type;
        TypeVariableInfo(std::string nm, TypePtr constr = nullptr, std::shared_ptr<TypeSymbol> owner = nullptr) : name(std::move(nm)), constraint(constr), owner_type(owner) { }

        ast::serialisation::Object serialise() const;
    };

}
