#pragma once

#include <string>
#include <vector>
#include <memory>

#include "Lexer.hpp"

namespace teal::parser::ast
{
    // Base AST node classes
    struct ASTNode {
        size_t line, column;

        ASTNode(size_t line, size_t column):
            line(line),
            column(column)
        {}

        ASTNode(const Token &tk):
            line(tk.line),
            column(tk.col)
        {}

        ASTNode(const ASTNode &other) = delete;
        ASTNode(ASTNode &&other) = delete;

        virtual ~ASTNode() = default;
    };
    struct Expression : ASTNode {
        Expression(const Token &tk):
            ASTNode(tk)
        {}

        virtual ~Expression() = default;
    };
    struct Statement : ASTNode {
        Statement(const Token &tk):
            ASTNode(tk)
        {}

        virtual ~Statement() = default;
    };

    // Expression AST nodes
    struct NameExpression : Expression {
        std::string name;
        NameExpression(const Token &tk, std::string n): Expression(tk), name(std::move(n)) {}
    };
    struct NumberExpression : Expression {
        std::string value;
        NumberExpression(const Token &tk, std::string v): Expression(tk), value(std::move(v)) {}
    };
    struct StringExpression : Expression {
        std::string value;
        StringExpression(const Token &tk, std::string v): Expression(tk), value(std::move(v)) {}
    };
    struct BooleanExpression : Expression {
        bool value;
        BooleanExpression(const Token &tk, bool v): Expression(tk), value(v) {}
    };
    struct NilExpression : Expression {
        NilExpression(const Token &tk): Expression(tk) {}
    };
    struct VarargExpression : Expression {
        VarargExpression(const Token &tk): Expression(tk) {}
    };  // represents "..."
    struct FunctionCallExpression : Expression {
        std::unique_ptr<Expression> base;
        std::string method_name;                  // method name if invoked with ':'
        std::vector<std::unique_ptr<Expression>> arguments;
        FunctionCallExpression(const Token &tk, std::unique_ptr<Expression> base_expr, std::string method = "")
            : Expression(tk), base(std::move(base_expr)), method_name(std::move(method)) {}
    };
    struct IndexExpression : Expression {
        std::unique_ptr<Expression> table;
        std::unique_ptr<Expression> index;
        IndexExpression(const Token &tk, std::unique_ptr<Expression> tbl, std::unique_ptr<Expression> idx)
            : Expression(tk), table(std::move(tbl)), index(std::move(idx)) {}
    };
    struct FieldExpression : Expression {
        std::unique_ptr<Expression> object;
        std::string field;
        FieldExpression(const Token &tk, std::unique_ptr<Expression> obj, std::string fld)
            : Expression(tk), object(std::move(obj)), field(std::move(fld)) {}
    };
    struct OperationExpression : Expression {
        TokenType operation;

        OperationExpression(const Token &tk, TokenType op_t): Expression(tk), operation(op_t) {}

        virtual ~OperationExpression() = default;
    };

    struct BinaryOperationExpression : OperationExpression {
        std::unique_ptr<Expression> left, right;
        BinaryOperationExpression(const Token &tk, TokenType op_t, std::unique_ptr<Expression> l, std::unique_ptr<Expression> r)
            : OperationExpression(tk, op_t), left(std::move(l)), right(std::move(r)) {}
    };
    struct UnaryOperationExpression : OperationExpression {
        std::unique_ptr<Expression> operand;
        UnaryOperationExpression(const Token &tk, TokenType opType, std::unique_ptr<Expression> expr)
            : OperationExpression(tk, opType), operand(std::move(expr)) {}
    };

    struct TypeNode : ASTNode {
        TypeNode(const Token &tk): ASTNode(tk) {}
        virtual ~TypeNode() = default;
    };

    struct Block : ASTNode {
        std::vector<std::unique_ptr<Statement>> statements;
        Block(const Token &tk): ASTNode(tk) {}
    };

    struct GenericTypeParameter {
        std::string name;
        std::optional<std::string> is;
    };

    // Function body for function definitions (parameters and return types)
    struct FunctionBody : ASTNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct Parameter {
            std::string name;
            bool is_varadict;
            bool is_optional;
            std::unique_ptr<TypeNode> type;
        };
        std::vector<Parameter> parameters;
        std::vector<std::unique_ptr<TypeNode>> return_types;
        bool varadict_return = false;
        std::unique_ptr<Block> body;

        FunctionBody(const Token &tk): ASTNode(tk) {}
    };


    struct FunctionDefinitionExpression : Expression {
        std::unique_ptr<FunctionBody> body;
        FunctionDefinitionExpression(const Token &tk, std::unique_ptr<FunctionBody> b)
            : Expression(tk), body(std::move(b)) {}
    };
    struct CastExpression : Expression {
        std::unique_ptr<Expression> expression;
        std::vector<std::unique_ptr<TypeNode>> target_types;
        CastExpression(const Token &tk, std::unique_ptr<Expression> e, std::vector<std::unique_ptr<TypeNode>> types)
            : Expression(tk), expression(std::move(e)), target_types(std::move(types)) {}
    };
    struct IsTypeExpression : Expression {
        std::unique_ptr<Expression> expression;
        std::unique_ptr<TypeNode> type;
        IsTypeExpression(const Token &tk, std::unique_ptr<Expression> e, std::unique_ptr<TypeNode> t)
            : Expression(tk), expression(std::move(e)), type(std::move(t)) {}
    };

    //{ "this", is = "a", [get_value()] = "table" }
    struct TableConstructorExpression : Expression {
        struct KeyValuePair {
            std::variant<std::string, std::unique_ptr<Expression>> key;
            std::unique_ptr<Expression> value;
            std::unique_ptr<TypeNode> type = nullptr; //can be null :)
        };

        using Field = std::variant<std::unique_ptr<Expression>, KeyValuePair>;
        std::vector<Field> fields;

        TableConstructorExpression(const Token &tk): Expression(tk) {}
    };

    // Type AST nodes
    struct BasicTypeNode : TypeNode {
        std::string name;
        BasicTypeNode(const Token &tk, std::string n): TypeNode(tk), name(std::move(n)) {}
    };
    struct NominalTypeNode : TypeNode {
        std::vector<std::string> name_parts;
        std::vector<std::unique_ptr<TypeNode>> type_arguments;
        NominalTypeNode(const Token &tk, std::vector<std::string> parts, std::vector<std::unique_ptr<TypeNode>> &&args = {})
            : TypeNode(tk), name_parts(std::move(parts)), type_arguments(std::move(args)) {}
    };
    struct TableTypeNode : TypeNode {
        std::vector<std::unique_ptr<TypeNode>> element_types;
        std::unique_ptr<TypeNode> key_type;
        bool is_map;
        TableTypeNode(const Token &tk): TypeNode(tk), is_map(false) {}
    };
    struct FunctionTypeNode : TypeNode {
        std::vector<GenericTypeParameter> type_parameters;
        struct ParameterType {
            std::optional<std::string> name;
            bool is_optional;
            std::unique_ptr<TypeNode> type;
        };
        std::vector<ParameterType> parameters;
        std::vector<std::unique_ptr<TypeNode>> return_types;
        bool varadict_return = false;

        FunctionTypeNode(const Token &tk): TypeNode(tk) {}
    };
    struct UnionTypeNode : TypeNode {
        std::vector<std::unique_ptr<TypeNode>> options;
        UnionTypeNode(const Token &tk): TypeNode(tk) {}
    };
    struct TypeRecordNode : TypeNode {
        std::unique_ptr<ASTNode> body;  // points to a RecordBody AST node
        TypeRecordNode(const Token &tk, std::unique_ptr<ASTNode> b)
            : TypeNode(tk), body(std::move(b)) {}
    };
    struct TypeEnumNode : TypeNode {
        std::vector<std::string> elements;
        TypeEnumNode(const Token &tk, std::vector<std::string> elems)
            : TypeNode(tk), elements(std::move(elems)) {}
    };
    struct RequireTypeNode : TypeNode {
        std::string module_name;
        std::vector<std::string> type_names;
        RequireTypeNode(const Token &tk, std::string mod, std::vector<std::string> names)
            : TypeNode(tk), module_name(std::move(mod)), type_names(std::move(names)) {}
    };


    // Statement AST nodes
    struct ReturnStatement : Statement {
        std::vector<std::unique_ptr<Expression>> values;
        ReturnStatement(const Token &tk): Statement(tk) {}
    };
    struct BreakStatement : Statement {
        BreakStatement(const Token &tk): Statement(tk) {}
    };
    struct GotoStatement : Statement {
        std::string label;
        GotoStatement(const Token &tk, std::string lbl): Statement(tk), label(std::move(lbl)) {}
    };
    struct LabelStatement : Statement {
        std::string name;
        LabelStatement(const Token &tk, std::string n): Statement(tk), name(std::move(n)) {}
    };
    struct DoStatement : Statement {
        std::unique_ptr<Block> body;
        DoStatement(const Token &tk, std::unique_ptr<Block> b): Statement(tk), body(std::move(b)) {}
    };
    struct IfStatement : Statement {
        struct IfBranch {
            std::unique_ptr<Expression> condition;
            std::unique_ptr<Block> block;
        };
        std::vector<IfBranch> if_branches;
        std::unique_ptr<Block> else_block;

        IfStatement(const Token &tk): Statement(tk) {}
    };
    struct WhileStatement : Statement {
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Block> body;

        WhileStatement(const Token &tk): Statement(tk) {}
    };
    struct RepeatStatement : Statement {
        std::unique_ptr<Block> body;
        std::unique_ptr<Expression> condition;

        RepeatStatement(const Token &tk): Statement(tk) {}
    };
    struct ForNumericStatement : Statement {
        std::string variable_name;
        struct {
            std::unique_ptr<Expression> start, end, step;
        } expressions;
        std::unique_ptr<Block> body;

        ForNumericStatement(const Token &tk): Statement(tk) {}
    };
    struct ForInStatement : Statement {
        std::vector<std::string> names;
        std::vector<std::unique_ptr<Expression>> exprs;
        std::unique_ptr<Block> body;

        ForInStatement(const Token &tk): Statement(tk) {}
    };

    enum class Visibility {
        NONE,
        LOCAL,
        GLOBAL
    };

    struct FunctionDeclarationStatement : Statement {
        Visibility visibility;
        std::vector<std::string> name_path;
        std::string method_name;
        bool is_method;
        std::unique_ptr<FunctionBody> body;

        FunctionDeclarationStatement(const Token &tk, Visibility vis)
            : Statement(tk), visibility(vis), is_method(false) {}
    };
    struct VariableDeclarationStatement : Statement {
        Visibility visibility;
        struct Name {  // name with optional attribute (for <attr>)
            std::string name;
            std::optional<std::string> attribute;
        };
        std::vector<Name> names;
        std::vector<std::unique_ptr<TypeNode>> types;
        std::vector<std::unique_ptr<Expression>> values;

        VariableDeclarationStatement(const Token &tk, Visibility vis)
            : Statement(tk), visibility(vis) {}
    };
    struct RecordBody : ASTNode {
        std::vector<GenericTypeParameter> type_parameters;
        std::unique_ptr<TypeNode> structural_ext;
        std::vector<std::unique_ptr<TypeNode>> interface_ext;
        std::unique_ptr<Expression> where_clause;

        //todo: Turn this into `std::variant`
        struct Entry {
            enum class Kind { FIELD, USERDATA, TYPE_ALIAS, RECORD, ENUM, INTERFACE } kind;
            bool is_metamethod;
            std::optional<std::string> name;
            std::optional<std::string> key_literal;
            std::unique_ptr<TypeNode> type;
            std::string type_name;
            std::unique_ptr<TypeNode> type_value;
            std::string nested_name;
            std::unique_ptr<ASTNode> nested_body;
            // Entry() : isMetamethod(false) {}
        };
        std::vector<Entry> entries;

        RecordBody(const Token &tk): ASTNode(tk) {}
    };
    struct RecordDeclarationStatement : Statement {
        bool is_interface;
        Visibility visibility;
        std::string name;
        std::unique_ptr<RecordBody> body;

        RecordDeclarationStatement(const Token &tk, bool interface, Visibility vis, std::string n, std::unique_ptr<RecordBody> b)
            : Statement(tk), is_interface(interface), visibility(vis), name(std::move(n)), body(std::move(b)) {}
    };
    struct EnumBody : ASTNode {
        std::vector<std::string> elements;

        EnumBody(const Token &tk): ASTNode(tk) {}
    };
    struct EnumDeclarationStatement : Statement {
        Visibility visibility;
        std::string name;
        std::unique_ptr<EnumBody> body;

        EnumDeclarationStatement(const Token &tk, Visibility vis, std::string n, std::unique_ptr<EnumBody> b)
            : Statement(tk), visibility(vis), name(std::move(n)), body(std::move(b)) {}
    };
    struct TypeAliasStatement : Statement {
        Visibility visibility;
        std::string name;
        std::vector<GenericTypeParameter> type_parameters;
        std::unique_ptr<TypeNode> type;

        TypeAliasStatement(const Token &tk, Visibility vis, std::string n, std::vector<GenericTypeParameter> &&tparams, std::unique_ptr<TypeNode> val)
            : Statement(tk), visibility(vis), name(std::move(n)), type_parameters(std::move(tparams)), type(std::move(val)) {}
    };
    struct AssignmentStatement : Statement {
        std::vector<std::unique_ptr<Expression>> left;
        std::vector<std::unique_ptr<Expression>> right;

        AssignmentStatement(const Token &tk): Statement(tk) {}
    };

    struct CallStatement : Statement {
        std::unique_ptr<FunctionCallExpression> call;
        CallStatement(const Token &tk, std::unique_ptr<FunctionCallExpression> c)
            : Statement(tk), call(std::move(c)) {}
    };
}
