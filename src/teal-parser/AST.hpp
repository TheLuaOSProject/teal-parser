#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "Lexer.hpp"

namespace teal::parser::ast {

    template<typename T>
    // using Pointer = std::unique_ptr<T>;
    using Pointer = T *;
    
    //TODO: This needs to be an IndexPointer<T> so we can change the pointer size from 16 to 8
    // using Pointer = IndexPointer<T>;
    // using Pointer = ContainerPointer<T, std::vector>;

    namespace serialisation {
        template<typename T>
        using Pointer = std::unique_ptr<T>;

        template<typename T, typename ...TArgs>
        static inline constexpr Pointer<T> allocate(TArgs &&...args)
        { return std::make_unique<T>(std::forward<TArgs>(args)...); }

        struct Value;
        using Object = std::unordered_map<std::string_view, Pointer<Value>>;
        using Array = std::vector<Pointer<Value>>;
        using Number = std::variant<long long, double>;

        template <typename T>
        concept Serialisable = requires(T v) {
            { v.serialise() } -> std::same_as<Object>;
        };

        using ValueData = std::variant<std::string_view, bool, Number, Object, Array, std::monostate>;

        struct Value : public ValueData {

            using ValueData::variant;

            static inline Pointer<Value> from(Value val) { return allocate<Value>(std::move(val)); }

            template <Serialisable T>
            static inline Pointer<Value> from(const T &val)
            {
                return allocate<Value>(std::move(val.serialise()));
            }

            template <Serialisable T>
            static inline Pointer<Value> from(const Pointer<T> &val)
            {
                if (val == nullptr) return std::make_unique<Value>(std::monostate());
                else return Value::from(val->serialise());
            }

            template <Serialisable T>
            static inline Pointer<Value> from(const std::shared_ptr<T> &val)
            {
                if (val == nullptr) return allocate<Value>(std::monostate());
                else return Value::from(val->serialise());
            }

            template <typename T>
            static inline Pointer<Value> from(const Pointer<T> &val)
            {
                if (val == nullptr) return allocate<Value>(std::monostate());
                else return Value::from(std::move(*val.get()));
            }

            template <typename T>
            static inline Pointer<Value> from(const std::shared_ptr<T> &val)
            {
                if (val == nullptr) return allocate<Value>(std::monostate());
                else return Value::from(std::move(*val.get()));
            }

            template <Serialisable T>
            static Pointer<Value> from(const std::vector<T> &arr)
            {
                auto res = Array();
                res.reserve(arr.size());

                for (const auto &v : arr) { res.push_back(Value::from(v.serialise())); }

                return allocate<Value>(std::move(res));
            }

            template <typename T>
            static Pointer<Value> from(const std::vector<T> &arr)
            {
                auto res = Array();
                res.reserve(arr.size());

                for (const auto &v : arr) { res.push_back(Value::from(v)); }

                return allocate<Value>(std::move(res));
            }

            template <>
            Pointer<Value> from<bool>(const std::vector<bool> &arr)
            {
                auto res = Array();
                res.reserve(arr.size());

                for (auto v : arr) { res.push_back(allocate<Value>(bool(v))); }

                return allocate<Value>(std::move(res));
            }

            template <Serialisable T>
            static Pointer<Value> from(const std::vector<Pointer<T>> &arr)
            {
                auto res = Array();
                res.reserve(arr.size());

                for (const auto &v : arr) {
                    if (v == nullptr) {
                        res.push_back(Value::from(std::monostate()));
                    } else {
                        res.push_back(Value::from(v->serialise()));
                    }
                }

                return allocate<Value>(std::move(res));
            }

            template <typename T>
            static Pointer<Value> from(const std::vector<Pointer<T>> &arr)
            {
                auto res = Array();
                res.reserve(arr.size());

                for (const auto &v : arr) {
                    if (v == nullptr) {
                        res.push_back(allocate<Value>(std::monostate()));
                    } else {
                        res.push_back(Value::from(
                            std::move(*v.get()))); // I dont want to move here but I don't really have a choice
                    }
                }

                return allocate<Value>(std::move(res));
            }

            template <Serialisable T>
            static inline Pointer<Value> from(const std::optional<T> &t)
            {
                if (t.has_value()) {
                    return allocate<Value>(std::move(t.value().serialise()));
                } else {
                    return allocate<Value>(std::monostate());
                }
            }

            template <typename T>
            static inline Pointer<Value> from(const std::optional<T> &t)
            {
                if (t.has_value()) {
                    return allocate<Value>(std::move(t.value()));
                } else {
                    return allocate<Value>(std::monostate());
                }
            }

            std::string to_json() const;
            std::string to_lua_table() const;
        };
    }


// Base AST node classes
    struct ASTNode {};

#define $(n) struct n;

    struct ExpressionNode : ASTNode {};
#define $expression_nodes \
    $(NameExpression) $(NumberExpression) $(StringExpression) $(BooleanExpression) $(NilExpression) $(VarargExpression) \
    $(FunctionCallExpression) $(IndexExpression) $(FieldExpression) \
    $(BinaryOperationExpression) $(UnaryOperationExpression) $(FunctionDefinitionExpression) $(CastExpression) $(IsTypeExpression) \
    $(TableConstructorExpression)

    $expression_nodes;

    struct StatementNode : ASTNode {};
#define $statement_nodes \
    $(Block) $(ReturnStatement) $(BreakStatement) $(GotoStatement) $(LabelStatement) $(DoStatement) \
    $(IfStatement) $(WhileStatement) $(RepeatStatement) $(ForNumericStatement) $(ForInStatement) \
    $(FunctionDeclarationStatement) $(VariableDeclarationStatement) $(RecordDeclarationStatement) \
    $(EnumDeclarationStatement) $(TypeAliasStatement) $(AssignmentStatement) $(CallStatement)

    $statement_nodes;

    struct TypeNode : ASTNode {};
#define $type_nodes \
    $(BasicType) $(NominalType) $(TableType) $(FunctionType) $(UnionType) \
    $(RecordType) $(EnumType) $(RequireType)

    $type_nodes;
#undef $

    struct Unrepresentable {
        Unrepresentable() = delete;
        ~Unrepresentable() = delete;
    };

#define $(n) n,
    using Expression = UnionSlice<$expression_nodes Unrepresentable>;
    using PrefixExpression = UnionSlice<NameExpression, FunctionCallExpression, FieldExpression, IndexExpression>;
    using PrimaryExpression = UnionSliceOf_t<
        UnionSlice <NilExpression, BooleanExpression, NumberExpression, StringExpression, VarargExpression, FunctionDefinitionExpression, TableConstructorExpression>,
        PrefixExpression
    >;

    using Statement = UnionSlice<$statement_nodes Unrepresentable>;
    using Type = UnionSlice<$type_nodes Unrepresentable>;
#undef $
    struct FunctionBody;
    struct RecordBody;
    struct EnumBody;

    struct AST;

    // using ASTData = Union<Expression, Statement, Type, FunctionBody, RecordBody, EnumBody, std::monostate>;
    // struct AST : public ASTData {

    // };

    // template<typename T> //requires IsInUnion<T, AST>
    // using ASTPointer = UnionPointer<T, AST, Pointer>;

    //TODO: Replace std::vector with some datastructure in the block

// Expression AST nodes
    struct NameExpression : ExpressionNode {
        std::string_view name;
    };
    struct NumberExpression : ExpressionNode {
        std::string_view value;
    };
    struct StringExpression : ExpressionNode {
        std::string_view value;
    };
    struct BooleanExpression : ExpressionNode {
        bool value;
    };
    struct NilExpression : ExpressionNode {};
    struct VarargExpression : ExpressionNode {}; // represents "..."
    struct FunctionCallExpression : ExpressionNode {
        Expression base;
        std::string_view method_name; // method name if invoked with ':'
        std::vector<Expression> arguments;
    };
    struct IndexExpression : ExpressionNode {
        Expression table, index;
    };
    struct FieldExpression : ExpressionNode {
        Expression object;
        std::string_view field;
    };

    struct BinaryOperationExpression : ExpressionNode {
        Expression left, right;
    };
    struct UnaryOperationExpression : ExpressionNode {
        Expression operand;
    };
    

// Block shouldnt be a statement?
    struct Block : StatementNode {
        std::vector<Statement> statements;
    };

    struct GenericTypeParameter {
        std::string_view name;
        std::optional<std::string_view> is;
    };

// Function body for function definitions (parameters and return types)
    struct FunctionBody : ASTNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct Parameter {
            std::string_view name;
            bool is_varadict;
            bool is_optional;
            Type type;
        };
        std::vector<Parameter> parameters;
        std::vector<Type> return_types;
        bool varadict_return = false;
        UnionSlice<Block> body;
    };

    struct FunctionDefinitionExpression : ExpressionNode {
        UnionSlice<FunctionBody> body;
    };
    struct CastExpression : ExpressionNode {
        Expression expression;
        std::vector<Type> target_types;
    };
    struct IsTypeExpression : ExpressionNode {
        Expression expression;
        Type type;
    };

//{ "this", is = "a", [get_value()] = "table" }
    struct TableConstructorExpression : ExpressionNode {
        struct KeyValuePair {
            std::variant<std::string_view, Expression> key;
            Expression value;
            std::optional<Type> type; // can be null :)
        };

        using Field = std::variant<Expression, KeyValuePair>;
        std::vector<Field> fields;

    };

// Type AST nodes
    struct BasicType : TypeNode {
        std::string_view name;
    };
    struct NominalType : TypeNode {
        std::vector<std::string_view> name_parts;
        std::vector<Type> type_arguments;
    };
    struct TableType : TypeNode {
        std::vector<Type> element_types;
        Type key_type;
        bool is_map;
    };
    struct FunctionType : TypeNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct ParameterType {
            std::optional<std::string_view> name;
            bool is_optional;
            Type type;
        };
        std::vector<ParameterType> parameters;
        std::vector<Type> return_types;
        bool varadict_return;
    };
    struct UnionType : TypeNode {
        std::vector<Type> options;
    };

    struct RecordBody {
        std::vector<GenericTypeParameter> type_parameters;
        Type structural_ext;
        std::vector<Type> interface_ext;
        Expression where_clause;

    // todo: Turn this into `std::variant`
        // struct Entry {
        //     enum class Kind { FIELD, USERDATA, TYPE_ALIAS, RECORD, ENUM, INTERFACE } entry_kind;
        //     bool is_metamethod;
        //     std::optional<std::string_view> name;
        //     std::optional<std::string_view> key_literal;
        //     Type type;
        //     std::string_view type_name;
        //     Type type_value;
        //     std::string_view nested_name;
        //     Pointer<ASTNode> nested_body;
        // };

        struct Userdata {};
        struct TypeAlias {
            std::string_view name;
            Type type;  
        };
        struct Record {
            std::string_view name;
            UnionSlice<RecordBody> body;
        };
        struct Interface : Record {};
        struct Enum {
            std::string_view name;
            UnionSlice<EnumBody> body;
        };
        struct Field {
            bool is_metamethod;
            std::string_view name;
            Type type;
        };

        using Entry = Union<Field, Userdata, TypeAlias, Record, Enum, Interface>;
        std::vector<Entry> entries;
    };

    struct RecordType : TypeNode {
        UnionSlice<RecordBody> body;
    };
    struct EnumType : TypeNode {
        std::vector<std::string_view> elements;
    };
    struct RequireType : TypeNode {
        std::string_view module_name;
        std::vector<std::string_view> type_names;
    };

// Statement AST nodes
    struct ReturnStatement : StatementNode {
        std::vector<Expression> values;
    };
    struct BreakStatement : StatementNode {};
    struct GotoStatement : StatementNode {
        std::string_view label;
    };
    struct LabelStatement : StatementNode {
        std::string_view name;
    };
    struct DoStatement : StatementNode {
        Statement body;
    };
    struct IfStatement : StatementNode {
        struct IfBranch {
            Expression condition;
            Statement block;
        };
        std::vector<IfBranch> if_branches;
        UnionSlice<Block> else_block;
    };
    struct WhileStatement : StatementNode {
        Expression condition;
        UnionSlice<Block> body;
    };
    struct RepeatStatement : StatementNode {
        Expression condition;
        UnionSlice<Block> body;
    };
    struct ForNumericStatement : StatementNode {
        std::string_view variable_name;
        struct {
            Expression start, end, step;
        } expressions;
        UnionSlice<Block> body;
    };
    struct ForInStatement : StatementNode {
        std::vector<std::string_view> names;
        std::vector<Expression> expressions;
        UnionSlice<Block> body;
    };

    enum class Visibility { NONE, LOCAL, GLOBAL };

    constexpr inline std::string_view visibility_to_string(Visibility vis)
    {
        switch (vis) {
        case Visibility::NONE:
            return "none";
        case Visibility::LOCAL:
            return "local";
        case Visibility::GLOBAL:
            return "global";
        }
    }

    struct FunctionDeclarationStatement : StatementNode {
        Visibility visibility;
        std::vector<std::string_view> name_path;
        std::string_view method_name;
        bool is_method;
        bool is_macro;
        UnionSlice<FunctionBody> body;
    };
    struct VariableDeclarationStatement : StatementNode {
        Visibility visibility;
        struct Name { // name with optional attribute (for <attr>)
            std::string_view name;
            std::optional<std::string_view> attribute;            
        };
        std::vector<Name> names;
        std::vector<Type> types;
        std::vector<Expression> values;
    };

    struct RecordDeclarationStatement : StatementNode {
        bool is_interface;
        Visibility visibility;
        std::string_view name;
        UnionSlice<RecordBody> body;
    };
    struct EnumBody : ASTNode {
        std::vector<std::string_view> elements;
    };
    struct EnumDeclarationStatement : StatementNode {
        Visibility visibility;
        std::string_view name;
        UnionSlice<EnumBody> body;
    };
    struct TypeAliasStatement : StatementNode {
        Visibility visibility;
        std::string_view name;
        std::vector<GenericTypeParameter> type_parameters;
        Type type;
    };
    struct AssignmentStatement : StatementNode {
        std::vector<Expression> left, right;
    };

    struct CallStatement : StatementNode {
        Expression call;
    };

    using ASTData = UnionOf_t<
        Expression,
        Statement,
        Type,

        //not categorised
        Union <
            FunctionBody,
            RecordBody,
            EnumBody
        >
    >;
    struct AST : public ASTData {
        const Token &token;
        AST(const Token &tk, ASTData &&data) :
            ASTData(std::move(data)), token(tk)
        { }
    };
}
