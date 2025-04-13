#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "Lexer.hpp"

namespace teal::parser::ast {

    //Maybe change later? needs to be efficent
    template<typename T>
    using Pointer = std::unique_ptr<T>;
    // using Pointer = T *;
    
    template<typename T, typename ...TArgs>
    constexpr inline Pointer<T> allocate(TArgs ...args)
    { return std::make_unique<T>(std::forward<TArgs>(args)...); }

    namespace serialisation {
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
                if (val == nullptr) return allocate<Value>(std::monostate());
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
    struct ASTNode {
        size_t line, column;

        ASTNode(size_t line, size_t column) : line(line), column(column) { }

        ASTNode(const Token &tk) : line(tk.line), column(tk.col) { }

        ASTNode(const ASTNode &other) = delete;
        ASTNode(ASTNode &&other) = delete;

        virtual ~ASTNode() = default;

        virtual serialisation::Object serialise() const;

        template <typename T>
            requires std::is_base_of_v<ASTNode, T>
        constexpr inline bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }
    };
    struct Expression : ASTNode {
        Expression(const Token &tk) : ASTNode(tk) { }

        virtual ~Expression() = default;

        template <typename T>
            requires std::is_base_of_v<Expression, T>
        constexpr inline bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }
    };
    struct Statement : ASTNode {
        Statement(const Token &tk) : ASTNode(tk) { }

        virtual ~Statement() = default;

        template <typename T>
            requires std::is_base_of_v<Statement, T>
        constexpr inline bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }
    };

// Expression AST nodes
    struct NameExpression : Expression {
        std::string_view name;
        NameExpression(const Token &tk, std::string_view n) : Expression(tk), name(std::move(n)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct NumberExpression : Expression {
        std::string_view value;
        NumberExpression(const Token &tk, std::string_view v) : Expression(tk), value(std::move(v)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct StringExpression : Expression {
        std::string_view value;
        StringExpression(const Token &tk, std::string_view v) : Expression(tk), value(std::move(v)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct BooleanExpression : Expression {
        bool value;
        BooleanExpression(const Token &tk, bool v) : Expression(tk), value(v) { }

        virtual serialisation::Object serialise() const override;
    };
    struct NilExpression : Expression {
        NilExpression(const Token &tk) : Expression(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct VarargExpression : Expression {
        VarargExpression(const Token &tk) : Expression(tk) { }

        virtual serialisation::Object serialise() const override;
    }; // represents "..."
    struct FunctionCallExpression : Expression {
        Pointer<Expression> base;
        std::string_view method_name; // method name if invoked with ':'
        std::vector<Pointer<Expression>> arguments;
        FunctionCallExpression(const Token &tk, Pointer<Expression> &&base_expr, std::string_view method = "") :
            Expression(tk), base(std::move(base_expr)), method_name(std::move(method))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct IndexExpression : Expression {
        Pointer<Expression> table;
        Pointer<Expression> index;
        IndexExpression(const Token &tk, Pointer<Expression> &&tbl, Pointer<Expression> &&idx) :
            Expression(tk), table(std::move(tbl)), index(std::move(idx))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct FieldExpression : Expression {
        Pointer<Expression> object;
        std::string_view field;
        FieldExpression(const Token &tk, Pointer<Expression> &&obj, std::string_view fld) :
            Expression(tk), object(std::move(obj)), field(std::move(fld))
        {
        }

        virtual serialisation::Object serialise() const override;
    };

    struct OperationExpression : Expression {
        TokenType operation;

        OperationExpression(const Token &tk, TokenType op_t) : Expression(tk), operation(op_t) { }

        virtual ~OperationExpression() = default;

        virtual serialisation::Object serialise() const override;
    };

    struct BinaryOperationExpression : OperationExpression {
        Pointer<Expression> left, right;
        BinaryOperationExpression(const Token &tk, TokenType op_t, Pointer<Expression> &&l,
            Pointer<Expression> &&r) : OperationExpression(tk, op_t), left(std::move(l)), right(std::move(r))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct UnaryOperationExpression : OperationExpression {
        Pointer<Expression> operand;
        UnaryOperationExpression(const Token &tk, TokenType opType, Pointer<Expression> &&expr) :
            OperationExpression(tk, opType), operand(std::move(expr))
        {
        }

        virtual serialisation::Object serialise() const override;
    };

    struct TypeNode : ASTNode {
        TypeNode(const Token &tk) : ASTNode(tk) { }
        virtual ~TypeNode() = default;

        template <typename T>
            requires std::is_base_of_v<TypeNode, T>
        constexpr inline bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }
    };

// Block shouldnt be a statement?
    struct Block : Statement {
        std::vector<Pointer<Statement>> statements;
        Block(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };

    struct GenericTypeParameter {
        std::string_view name;
        std::optional<std::string_view> is;

        serialisation::Object serialise() const;
    };

// Function body for function definitions (parameters and return types)
    struct FunctionBody : ASTNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct Parameter {
            std::string_view name;
            bool is_varadict;
            bool is_optional;
            Pointer<TypeNode> type;

            serialisation::Object serialise() const;
        };
        std::vector<Parameter> parameters;
        std::vector<Pointer<TypeNode>> return_types;
        bool varadict_return = false;
        Pointer<Block> body;

        FunctionBody(const Token &tk) : ASTNode(tk) { }

        virtual serialisation::Object serialise() const override;
    };

    struct FunctionDefinitionExpression : Expression {
        Pointer<FunctionBody> body;
        FunctionDefinitionExpression(const Token &tk, Pointer<FunctionBody> &&b) :
            Expression(tk), body(std::move(b))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct CastExpression : Expression {
        Pointer<Expression> expression;
        std::vector<Pointer<TypeNode>> target_types;
        CastExpression(
            const Token &tk, Pointer<Expression> &&e, std::vector<Pointer<TypeNode>> &&types) :
            Expression(tk), expression(std::move(e)), target_types(std::move(types))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct IsTypeExpression : Expression {
        Pointer<Expression> expression;
        Pointer<TypeNode> type;
        IsTypeExpression(const Token &tk, Pointer<Expression> &&e, Pointer<TypeNode> &&t) :
            Expression(tk), expression(std::move(e)), type(std::move(t))
        {
        }

        virtual serialisation::Object serialise() const override;
    };

//{ "this", is = "a", [get_value()] = "table" }
    struct TableConstructorExpression : Expression {
        struct KeyValuePair {
            std::variant<std::string_view, Pointer<Expression>> key;
            Pointer<Expression> value;
            Pointer<TypeNode> type = nullptr; // can be null :)

            serialisation::Object serialise() const;
        };

        using Field = std::variant<Pointer<Expression>, KeyValuePair>;
        std::vector<Field> fields;

        TableConstructorExpression(const Token &tk) : Expression(tk) { }

        virtual serialisation::Object serialise() const override;
    };

// Type AST nodes
    struct BasicTypeNode : TypeNode {
        std::string_view name;
        BasicTypeNode(const Token &tk, std::string_view n) : TypeNode(tk), name(std::move(n)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct NominalTypeNode : TypeNode {
        std::vector<std::string_view> name_parts;
        std::vector<Pointer<TypeNode>> type_arguments;
        NominalTypeNode(
            const Token &tk, std::vector<std::string_view> &&parts, std::vector<Pointer<TypeNode>> &&args = {}) :
            TypeNode(tk), name_parts(std::move(parts)), type_arguments(std::move(args))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct TableTypeNode : TypeNode {
        std::vector<Pointer<TypeNode>> element_types;
        Pointer<TypeNode> key_type;
        bool is_map;
        TableTypeNode(const Token &tk) : TypeNode(tk), is_map(false) { }

        virtual serialisation::Object serialise() const override;
    };
    struct FunctionTypeNode : TypeNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct ParameterType {
            std::optional<std::string_view> name;
            bool is_optional;
            Pointer<TypeNode> type;

            serialisation::Object serialise() const;
        };
        std::vector<ParameterType> parameters;
        std::vector<Pointer<TypeNode>> return_types;
        bool varadict_return = false;

        FunctionTypeNode(const Token &tk) : TypeNode(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct UnionTypeNode : TypeNode {
        std::vector<Pointer<TypeNode>> options;
        UnionTypeNode(const Token &tk) : TypeNode(tk) { }

        virtual serialisation::Object serialise() const override;
    };

    struct RecordBody : ASTNode {
        std::vector<GenericTypeParameter> type_parameters;
        Pointer<TypeNode> structural_ext;
        std::vector<Pointer<TypeNode>> interface_ext;
        Pointer<Expression> where_clause;

    // todo: Turn this into `std::variant`
        struct Entry {
            enum class Kind { FIELD, USERDATA, TYPE_ALIAS, RECORD, ENUM, INTERFACE } entry_kind;
            bool is_metamethod;
            std::optional<std::string_view> name;
            std::optional<std::string_view> key_literal;
            Pointer<TypeNode> type;
            std::string_view type_name;
            Pointer<TypeNode> type_value;
            std::string_view nested_name;
            Pointer<ASTNode> nested_body;
        // Entry() : isMetamethod(false) {}

            serialisation::Object serialise() const;
        };

        std::vector<Entry> entries;

        RecordBody(const Token &tk) : ASTNode(tk) { }

        virtual serialisation::Object serialise() const override;
    };

    struct TypeRecordNode : TypeNode {
        Pointer<RecordBody> body;
        TypeRecordNode(const Token &tk, Pointer<RecordBody> &&b) : TypeNode(tk), body(std::move(b)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct TypeEnumNode : TypeNode {
        std::vector<std::string_view> elements;
        TypeEnumNode(const Token &tk, std::vector<std::string_view> &&elems) : TypeNode(tk), elements(std::move(elems)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct RequireTypeNode : TypeNode {
        std::string_view module_name;
        std::vector<std::string_view> type_names;
        RequireTypeNode(const Token &tk, std::string_view mod, std::vector<std::string_view> &&names) :
            TypeNode(tk), module_name(std::move(mod)), type_names(std::move(names))
        {
        }

        virtual serialisation::Object serialise() const override;
    };

// Statement AST nodes
    struct ReturnStatement : Statement {
        std::vector<Pointer<Expression>> values;
        ReturnStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct BreakStatement : Statement {
        BreakStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct GotoStatement : Statement {
        std::string_view label;
        GotoStatement(const Token &tk, std::string_view lbl) : Statement(tk), label(std::move(lbl)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct LabelStatement : Statement {
        std::string_view name;
        LabelStatement(const Token &tk, std::string_view n) : Statement(tk), name(std::move(n)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct DoStatement : Statement {
        Pointer<Block> body;
        DoStatement(const Token &tk, Pointer<Block> &&b) : Statement(tk), body(std::move(b)) { }

        virtual serialisation::Object serialise() const override;
    };
    struct IfStatement : Statement {
        struct IfBranch {
            Pointer<Expression> condition;
            Pointer<Block> block;

            serialisation::Object serialise() const;
        };
        std::vector<IfBranch> if_branches;
        Pointer<Block> else_block;

        IfStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct WhileStatement : Statement {
        Pointer<Expression> condition;
        Pointer<Block> body;

        WhileStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct RepeatStatement : Statement {
        Pointer<Block> body;
        Pointer<Expression> condition;

        RepeatStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct ForNumericStatement : Statement {
        std::string_view variable_name;
        struct {
            Pointer<Expression> start, end, step;
        } expressions;
        Pointer<Block> body;

        ForNumericStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct ForInStatement : Statement {
        std::vector<std::string_view> names;
        std::vector<Pointer<Expression>> exprs;
        Pointer<Block> body;

        ForInStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
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

    struct FunctionDeclarationStatement : Statement {
        Visibility visibility;
        std::vector<std::string_view> name_path;
        std::string_view method_name;
        bool is_method;
        bool is_macro;
        Pointer<FunctionBody> body;

        FunctionDeclarationStatement(const Token &tk, Visibility vis) : Statement(tk), visibility(vis), is_method(false)
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct VariableDeclarationStatement : Statement {
        Visibility visibility;
        struct Name { // name with optional attribute (for <attr>)
            std::string_view name;
            std::optional<std::string_view> attribute;

            serialisation::Object serialise() const;
        };
        std::vector<Name> names;
        std::vector<Pointer<TypeNode>> types;
        std::vector<Pointer<Expression>> values;

        VariableDeclarationStatement(const Token &tk, Visibility vis) : Statement(tk), visibility(vis) { }

        virtual serialisation::Object serialise() const override;
    };

    struct RecordDeclarationStatement : Statement {
        bool is_interface;
        Visibility visibility;
        std::string_view name;
        Pointer<RecordBody> body;

        RecordDeclarationStatement(
            const Token &tk, bool interface, Visibility vis, std::string_view n, Pointer<RecordBody> &&b) :
            Statement(tk), is_interface(interface), visibility(vis), name(std::move(n)), body(std::move(b))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct EnumBody : ASTNode {
        std::vector<std::string_view> elements;

        EnumBody(const Token &tk) : ASTNode(tk) { }

        virtual serialisation::Object serialise() const override;
    };
    struct EnumDeclarationStatement : Statement {
        Visibility visibility;
        std::string_view name;
        Pointer<EnumBody> body;

        EnumDeclarationStatement(const Token &tk, Visibility vis, std::string_view n, Pointer<EnumBody> &&b) :
            Statement(tk), visibility(vis), name(std::move(n)), body(std::move(b))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct TypeAliasStatement : Statement {
        Visibility visibility;
        std::string_view name;
        std::vector<GenericTypeParameter> type_parameters;
        Pointer<TypeNode> type;

        TypeAliasStatement(const Token &tk, Visibility vis, std::string_view n,
            std::vector<GenericTypeParameter> &&tparams, Pointer<TypeNode> &&val) :
            Statement(tk), visibility(vis), name(std::move(n)), type_parameters(std::move(tparams)),
            type(std::move(val))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
    struct AssignmentStatement : Statement {
        std::vector<Pointer<Expression>> left;
        std::vector<Pointer<Expression>> right;

        AssignmentStatement(const Token &tk) : Statement(tk) { }

        virtual serialisation::Object serialise() const override;
    };

    struct CallStatement : Statement {
        Pointer<FunctionCallExpression> call;
        CallStatement(const Token &tk, Pointer<FunctionCallExpression> &&c) : Statement(tk), call(std::move(c))
        {
        }

        virtual serialisation::Object serialise() const override;
    };
}
