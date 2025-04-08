#pragma once

#include <string>
#include <vector>
#include <memory>

#include "Lexer.hpp"

namespace teal::parser::ast
{
    struct ASTNode {
        size_t line, column;


        ASTNode(Allocator, size_t line, size_t column):
            line(line),
            column(column)
        {}


        ASTNode(Allocator, const Token &tk):
            line(tk.line),
            column(tk.column)
        {}

        ASTNode(const ASTNode &other) = delete;
        ASTNode(ASTNode &&other) = delete;

        virtual ~ASTNode() = default;
    };
    struct Expression : ASTNode {

        Expression(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk)
        {}

        virtual ~Expression() = default;
    };
    struct Statement : ASTNode {

        Statement(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk)
        {}

        virtual ~Statement() = default;
    };

    // Expression AST nodes
    struct NameExpression : Expression {
        String name;

        NameExpression(Allocator allocator, const Token &tk, String &&n):
            Expression(allocator, tk), name(std::move(n), allocator) {}
    };
    struct NumberExpression : Expression {
        String value;

        NumberExpression(Allocator allocator, const Token &tk, String &&v):
            Expression(allocator, tk), value(std::move(v), allocator) {}
    };
    struct StringExpression : Expression {
        String value;

        StringExpression(Allocator allocator, const Token &tk, String &&v):
            Expression(allocator, tk), value(std::move(v), allocator) {}
    };
    struct BooleanExpression : Expression {
        bool value;

        BooleanExpression(Allocator allocator, const Token &tk, bool v):
            Expression(allocator, tk), value(v) {}
    };
    struct NilExpression : Expression {

        NilExpression(Allocator allocator, const Token &tk):
            Expression(allocator, tk) {}
    };
    struct VarargExpression : Expression {

        VarargExpression(Allocator allocator, const Token &tk):
            Expression(allocator, tk) {}
    };  // represents "..."
    struct FunctionCallExpression : Expression {
        Pointer<Expression> base;
        String method_name;                  // method name if invoked with ':'
        Vector<Pointer<Expression>> arguments;


        FunctionCallExpression(Allocator allocator, const Token &tk, Pointer<Expression> base_expr, std::string_view method = "")
            : Expression(allocator, tk), base(std::move(base_expr)), method_name(method, allocator), arguments(allocator) {}
    };
    struct IndexExpression : Expression {
        Pointer<Expression> table;
        Pointer<Expression> index;

        IndexExpression(Allocator allocator, const Token &tk, Pointer<Expression> tbl, Pointer<Expression> idx)
            : Expression(allocator, tk), table(std::move(tbl)), index(std::move(idx)) {}
    };
    struct FieldExpression : Expression {
        Pointer<Expression> object;
        String field;

        FieldExpression(Allocator allocator, const Token &tk, Pointer<Expression> obj, String &&fld)
            : Expression(allocator, tk), object(std::move(obj)), field(std::move(fld), allocator) {}
    };
    struct OperationExpression : Expression {
        TokenType operation;


        OperationExpression(Allocator allocator, const Token &tk, TokenType op_t):
            Expression(allocator, tk), operation(op_t) {}

        virtual ~OperationExpression() = default;
    };

    struct BinaryOperationExpression : OperationExpression {
        Pointer<Expression> left, right;

        BinaryOperationExpression(Allocator allocator, const Token &tk, TokenType op_t, Pointer<Expression> l, Pointer<Expression> r)
            : OperationExpression(allocator, tk, op_t), left(std::move(l)), right(std::move(r)) {}
    };
    struct UnaryOperationExpression : OperationExpression {
        Pointer<Expression> operand;

        UnaryOperationExpression(Allocator allocator, const Token &tk, TokenType opType, Pointer<Expression> expr)
            : OperationExpression(allocator, tk, opType), operand(std::move(expr)) {}
    };

    struct TypeNode : ASTNode {

        TypeNode(Allocator allocator, const Token &tk): ASTNode(allocator, tk) {}
        virtual ~TypeNode() = default;
    };

    struct Block : ASTNode {
        Vector<Pointer<Statement>> statements;

        Block(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk), statements(allocator) {}
    };


    struct GenericTypeParameter {
        String name;
        std::optional<String> is;

    };

    // Function body for function definitions (parameters and return types)
    struct FunctionBody : ASTNode {
        Vector<GenericTypeParameter> type_parameters;

        struct Parameter {
            String name;
            bool is_varadict;
            bool is_optional;
            Pointer<TypeNode> type;

        };
        Vector<Parameter> parameters;
        Vector<Pointer<TypeNode>> return_types;
        bool varadict_return = false;
        Pointer<Block> body;


        FunctionBody(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk),
            type_parameters(allocator),
            parameters(allocator),
            return_types(allocator)
            {}
    };


    struct FunctionDefinitionExpression : Expression {
        Pointer<FunctionBody> body;

        FunctionDefinitionExpression(Allocator allocator, const Token &tk, Pointer<FunctionBody> b)
            : Expression(allocator, tk), body(std::move(b)) {}
    };
    struct CastExpression : Expression {
        Pointer<Expression> expression;
        Vector<Pointer<TypeNode>> target_types;

        CastExpression(Allocator allocator, const Token &tk, Pointer<Expression> e, Vector<Pointer<TypeNode>> &&types)
            : Expression(allocator, tk), expression(std::move(e)), target_types(std::move(types), allocator) {}
    };
    struct IsTypeExpression : Expression {
        Pointer<Expression> expression;
        Pointer<TypeNode> type;

        IsTypeExpression(Allocator allocator, const Token &tk, Pointer<Expression> e, Pointer<TypeNode> t)
            : Expression(allocator, tk), expression(std::move(e)), type(std::move(t)) {}
    };

    //{ "this", is = "a", [get_value()] = "table" }
    struct TableConstructorExpression : Expression {

        struct KeyValuePair {
            std::variant<String, Pointer<Expression>> key;
            Pointer<Expression> value;
            Pointer<TypeNode> type = nullptr; //can be null :)

        };

        using Field = std::variant<Pointer<Expression>, KeyValuePair>;
        Vector<Field> fields;


        TableConstructorExpression(Allocator allocator, const Token &tk):
             Expression(allocator, tk), fields(allocator) {}
    };

    // Type AST nodes
    struct BasicTypeNode : TypeNode {
        String name;

        BasicTypeNode(Allocator allocator, const Token &tk, String &&n):
            TypeNode(allocator, tk), name(std::move(n), allocator) {}
    };
    struct NominalTypeNode : TypeNode {
        Vector<String> name_parts;
        Vector<Pointer<TypeNode>> type_arguments;


        NominalTypeNode(Allocator allocator, const Token &tk, Vector<String> &&parts, Vector<Pointer<TypeNode>> &&args)
            : TypeNode(allocator, tk), name_parts(std::move(parts), allocator), type_arguments(std::move(args), allocator) {}
    };
    struct TableTypeNode : TypeNode {
        Vector<Pointer<TypeNode>> element_types;
        Pointer<TypeNode> key_type;
        bool is_map;

        TableTypeNode(Allocator allocator, const Token &tk):
            TypeNode(allocator, tk), element_types(allocator), is_map(false) {}
    };
    struct FunctionTypeNode : TypeNode {
        Vector<GenericTypeParameter> type_parameters;

        struct ParameterType {
            std::optional<String> name;
            bool is_optional;
            Pointer<TypeNode> type;

        };
        Vector<ParameterType> parameters;
        Vector<Pointer<TypeNode>> return_types;
        bool varadict_return = false;


        FunctionTypeNode(Allocator allocator, const Token &tk):
            TypeNode(allocator, tk),
            type_parameters(allocator),
            parameters(allocator),
            return_types(allocator) {}
    };
    struct UnionTypeNode : TypeNode {
        Vector<Pointer<TypeNode>> options;

        UnionTypeNode(Allocator allocator, const Token &tk):
            TypeNode(allocator, tk), options(allocator) {}
    };
    struct TypeRecordNode : TypeNode {
        Pointer<ASTNode> body;  // points to a RecordBody AST node

        TypeRecordNode(Allocator allocator, const Token &tk, Pointer<ASTNode> b)
            : TypeNode(allocator, tk), body(std::move(b)) {}
    };
    struct TypeEnumNode : TypeNode {
        Vector<String> elements;

        TypeEnumNode(Allocator allocator, const Token &tk, Vector<String> &&elems)
            : TypeNode(allocator, tk), elements(std::move(elems), allocator) {}
    };
    struct RequireTypeNode : TypeNode {
        String module_name;
        Vector<String> type_names;

        RequireTypeNode(Allocator allocator, const Token &tk, String &&mod, Vector<String> &&names)
            : TypeNode(allocator, tk), module_name(std::move(mod), allocator), type_names(std::move(names), allocator) {}
    };


    // Statement AST nodes
    struct ReturnStatement : Statement {
        Vector<Pointer<Expression>> values;

        ReturnStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk), values(allocator) {}
    };
    struct BreakStatement : Statement {

        BreakStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk) {}
    };
    struct GotoStatement : Statement {
        String label;

        GotoStatement(Allocator allocator, const Token &tk, String &&lbl):
            Statement(allocator, tk), label(std::move(lbl), allocator) {}
    };
    struct LabelStatement : Statement {
        String name;

        LabelStatement(Allocator allocator, const Token &tk, String &&n):
            Statement(allocator, tk), name(std::move(n), allocator) {}
    };
    struct DoStatement : Statement {
        Pointer<Block> body;

        DoStatement(Allocator allocator, const Token &tk, Pointer<Block> b):
            Statement(allocator, tk), body(std::move(b)) {}
    };
    struct IfStatement : Statement {
        struct IfBranch {
            Pointer<Expression> condition;
            Pointer<Block> block;

        };
        Vector<IfBranch> if_branches;
        Pointer<Block> else_block;


        IfStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk), if_branches(allocator) {}
    };
    struct WhileStatement : Statement {
        Pointer<Expression> condition;
        Pointer<Block> body;


        WhileStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk) {}
    };
    struct RepeatStatement : Statement {
        Pointer<Block> body;
        Pointer<Expression> condition;


        RepeatStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk) {}
    };
    struct ForNumericStatement : Statement {
        String variable_name;
        struct {
            Pointer<Expression> start, end, step;
        } expressions;
        Pointer<Block> body;


        ForNumericStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk), variable_name(allocator) {}
    };
    struct ForInStatement : Statement {
        Vector<String> names;
        Vector<Pointer<Expression>> exprs;
        Pointer<Block> body;


        ForInStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk), names(allocator), exprs(allocator) {}
    };

    enum class Visibility {
        NONE,
        LOCAL,
        GLOBAL
    };

    struct FunctionDeclarationStatement : Statement {
        Visibility visibility;
        Vector<String> name_path;
        String method_name;
        bool is_method;
        Pointer<FunctionBody> body;


        FunctionDeclarationStatement(Allocator allocator, const Token &tk, Visibility vis)
            : Statement(allocator, tk), visibility(vis), name_path(allocator), method_name(allocator), is_method(false) {}
    };
    struct VariableDeclarationStatement : Statement {
        Visibility visibility;

        struct Name {  // name with optional attribute (for <attr>)
            String name;
            std::optional<String> attribute;

             Name(String &&n): name(std::move(n)) {}
        };
        Vector<Name> names;
        Vector<Pointer<TypeNode>> types;
        Vector<Pointer<Expression>> values;


        VariableDeclarationStatement(Allocator allocator, const Token &tk, Visibility vis)
            : Statement(allocator, tk), visibility(vis), names(allocator), types(allocator), values(allocator) {}
    };
    struct RecordBody : ASTNode {
        Vector<GenericTypeParameter> type_parameters;
        Pointer<TypeNode> structural_ext;
        Vector<Pointer<TypeNode>> interface_ext;
        Pointer<Expression> where_clause;


        struct Entry {
            enum class Kind { FIELD, USERDATA, TYPE_ALIAS, RECORD, ENUM, INTERFACE } kind;
            bool is_metamethod;
            std::optional<String> name;
            std::optional<String> key_literal;
            Pointer<TypeNode> type;
            String type_name;
            Pointer<TypeNode> type_value;
            String nested_name;
            Pointer<ASTNode> nested_body;

        };
        Vector<Entry> entries;


        RecordBody(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk),
            type_parameters(allocator),
            interface_ext(allocator),
            entries(allocator) {}
    };
    struct RecordDeclarationStatement : Statement {
        bool is_interface;
        Visibility visibility;
        String name;
        Pointer<RecordBody> body;


        RecordDeclarationStatement(Allocator allocator, const Token &tk, bool interface, Visibility vis, String &&n, Pointer<RecordBody> b):
            Statement(allocator, tk),
            is_interface(interface),
            visibility(vis),
            name(std::move(n), allocator),
            body(std::move(b)) {}
    };
    struct EnumBody : ASTNode {
        Vector<String> elements;


        EnumBody(Allocator allocator, const Token &tk):
            ASTNode(allocator, tk), elements(allocator) {}
    };
    struct EnumDeclarationStatement : Statement {
        Visibility visibility;
        String name;
        Pointer<EnumBody> body;


        EnumDeclarationStatement(Allocator allocator, const Token &tk, Visibility vis, String &&n, Pointer<EnumBody> b)
            : Statement(allocator, tk), visibility(vis), name(std::move(n), allocator), body(std::move(b)) {}
    };
    struct TypeAliasStatement : Statement {
        Visibility visibility;
        String name;
        Vector<GenericTypeParameter> type_parameters;
        Pointer<TypeNode> type;


        TypeAliasStatement(Allocator allocator, const Token &tk, Visibility vis, String &&n, Vector<GenericTypeParameter> &&tparams, Pointer<TypeNode> val)
            : Statement(allocator, tk), visibility(vis), name(std::move(n), allocator), type_parameters(std::move(tparams), allocator), type(std::move(val)) {}
    };
    struct AssignmentStatement : Statement {
        Vector<Pointer<Expression>> left;
        Vector<Pointer<Expression>> right;


        AssignmentStatement(Allocator allocator, const Token &tk):
            Statement(allocator, tk), left(allocator), right(allocator) {}
    };

    struct CallStatement : Statement {
        Pointer<FunctionCallExpression> call;

        CallStatement(Allocator allocator, const Token &tk, Pointer<FunctionCallExpression> c)
            : Statement(allocator, tk), call(std::move(c)) {}
    };
}
