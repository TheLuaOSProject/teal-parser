#pragma once

#include <string>
#include <vector>
#include <memory>

#include "Lexer.hpp"

namespace teal
{
    // Base AST node classes
    struct ASTNode { virtual ~ASTNode() = default; };
    struct Expression : ASTNode { virtual ~Expression() = default; };
    struct Statement : ASTNode { virtual ~Statement() = default; };

    // Expression AST nodes
    struct NameExpression : Expression {
        std::string name;
        NameExpression(std::string n) : name(std::move(n)) {}
    };
    struct NumberExpression : Expression {
        std::string value;
        NumberExpression(std::string v) : value(std::move(v)) {}
    };
    struct StringExpression : Expression {
        std::string value;
        StringExpression(std::string v) : value(std::move(v)) {}
    };
    struct BooleanExpression : Expression {
        bool value;
        BooleanExpression(bool v) : value(v) {}
    };
    struct NilExpression : Expression {};
    struct VarargExpression : Expression {};  // represents "..."
    struct FunctionCallExpression : Expression {
        std::unique_ptr<Expression> base;
        std::string method_name;                  // method name if invoked with ':'
        std::vector<std::unique_ptr<Expression>> arguments;
        FunctionCallExpression(std::unique_ptr<Expression> base_expr, std::string method = "")
            : base(std::move(base_expr)), method_name(std::move(method)) {}
    };
    struct IndexExpression : Expression {
        std::unique_ptr<Expression> table;
        std::unique_ptr<Expression> index;
        IndexExpression(std::unique_ptr<Expression> tbl, std::unique_ptr<Expression> idx)
            : table(std::move(tbl)), index(std::move(idx)) {}
    };
    struct FieldExpression : Expression {
        std::unique_ptr<Expression> object;
        std::string field;
        FieldExpression(std::unique_ptr<Expression> obj, std::string fld)
            : object(std::move(obj)), field(std::move(fld)) {}
    };
    struct OperationExpression : Expression {
        TokenType operation;

        OperationExpression(TokenType op_t) : operation(op_t) {}
    };

    struct BinaryOperationExpression : OperationExpression {
        std::unique_ptr<Expression> left, right;
        BinaryOperationExpression(TokenType op_t, std::unique_ptr<Expression> l, std::unique_ptr<Expression> r)
            : OperationExpression(op_t), left(std::move(l)), right(std::move(r)) {}
    };
    struct UnaryOperationExpression : OperationExpression {
        std::unique_ptr<Expression> operand;
        UnaryOperationExpression(TokenType opType, std::unique_ptr<Expression> expr)
            : OperationExpression(opType), operand(std::move(expr)) {}
    };

    struct TypeNode : ASTNode { virtual ~TypeNode() = default; };

    struct Block : ASTNode {
        std::vector<std::unique_ptr<Statement>> statements;
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
    };


    struct FunctionDefinitionExpression : Expression {
        std::unique_ptr<FunctionBody> body;
        FunctionDefinitionExpression(std::unique_ptr<FunctionBody> b) : body(std::move(b)) {}
    };
    struct CastExpression : Expression {
        std::unique_ptr<Expression> expression;
        std::vector<std::unique_ptr<TypeNode>> target_types;
        CastExpression(std::unique_ptr<Expression> e, std::vector<std::unique_ptr<TypeNode>> types)
            : expression(std::move(e)), target_types(std::move(types)) {}
    };
    struct IsTypeExpression : Expression {
        std::unique_ptr<Expression> expression;
        std::unique_ptr<TypeNode> type;
        IsTypeExpression(std::unique_ptr<Expression> e, std::unique_ptr<TypeNode> t)
            : expression(std::move(e)), type(std::move(t)) {}
    };

    //{ "this", is = "a", [get_value()] = "table" }
    struct TableConstructorExpression : Expression {
        // struct Field {
        //     //TODO: Change this into `std::variant<std::string, Expression>`
        //     std::optional<std::string> name_key;     // key if it's a Name field
        //     std::unique_ptr<Expression> key_expression;    // key expression if in [exp]
        //     std::unique_ptr<TypeNode> type;      // optional type annotation for Name field
        //     std::unique_ptr<Expression> value;      // field value expression
        // };
        // std::vector<Field> fields;
        struct KeyValuePair {
            std::variant<std::string, std::unique_ptr<Expression>> key;
            std::unique_ptr<Expression> value;
            std::unique_ptr<TypeNode> type = nullptr; //can be null :)
        };

        using Field = std::variant<std::unique_ptr<Expression>, KeyValuePair>;
        std::vector<Field> fields;
    };

    // Type AST nodes
    struct BasicTypeNode : TypeNode {
        std::string name;
        BasicTypeNode(std::string n) : name(std::move(n)) {}
    };
    struct NominalTypeNode : TypeNode {
        std::vector<std::string> name_parts;
        std::vector<std::unique_ptr<TypeNode>> type_arguments;
        NominalTypeNode(std::vector<std::string> parts, std::vector<std::unique_ptr<TypeNode>> &&args = {})
            : name_parts(std::move(parts)), type_arguments(std::move(args)) {}
    };
    struct TableTypeNode : TypeNode {
        std::vector<std::unique_ptr<TypeNode>> element_types;
        std::unique_ptr<TypeNode> key_type;
        bool is_map;
        TableTypeNode() : is_map(false) {}
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
    };
    struct UnionTypeNode : TypeNode {
        std::vector<std::unique_ptr<TypeNode>> options;
    };
    struct TypeRecordNode : TypeNode {
        std::unique_ptr<ASTNode> body;  // points to a RecordBody AST node
        TypeRecordNode(std::unique_ptr<ASTNode> b) : body(std::move(b)) {}
    };
    struct TypeEnumNode : TypeNode {
        std::vector<std::string> elements;
        TypeEnumNode(std::vector<std::string> elems) : elements(std::move(elems)) {}
    };
    struct RequireTypeNode : TypeNode {
        std::string module_name;
        std::vector<std::string> type_names;
        RequireTypeNode(std::string mod, std::vector<std::string> names)
            : module_name(std::move(mod)), type_names(std::move(names)) {}
    };


    // Statement AST nodes

    struct ReturnStatement : Statement {
        std::vector<std::unique_ptr<Expression>> values;
    };
    struct BreakStatement : Statement {};
    struct GotoStatement : Statement {
        std::string label;
        GotoStatement(std::string lbl) : label(std::move(lbl)) {}
    };
    struct LabelStatement : Statement {
        std::string name;
        LabelStatement(std::string n) : name(std::move(n)) {}
    };
    struct DoStatement : Statement {
        std::unique_ptr<Block> body;
        DoStatement(std::unique_ptr<Block> b) : body(std::move(b)) {}
    };
    struct IfStatement : Statement {
        struct IfBranch {
            std::unique_ptr<Expression> condition;
            std::unique_ptr<Block> block;
        };
        std::vector<IfBranch> if_branches;
        std::unique_ptr<Block> else_block;
    };
    struct WhileStatement : Statement {
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Block> body;
    };
    struct RepeatStatement : Statement {
        std::unique_ptr<Block> body;
        std::unique_ptr<Expression> condition;
    };
    struct ForNumericStatement : Statement {
        std::string variable_name;
        struct {
            std::unique_ptr<Expression> start, end, step;
        } expressions;
        std::unique_ptr<Block> body;
    };
    struct ForInStatement : Statement {
        std::vector<std::string> names;
        std::vector<std::unique_ptr<Expression>> exprs;
        std::unique_ptr<Block> body;
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

        FunctionDeclarationStatement(Visibility vis)
            : visibility(vis), is_method(false) {}
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
        VariableDeclarationStatement(Visibility vis) : visibility(vis) {}
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
    };
    struct RecordDeclarationStatement : Statement {
        bool is_interface;
        Visibility visibility;
        std::string name;
        std::unique_ptr<RecordBody> body;
        RecordDeclarationStatement(bool interface, Visibility vis, std::string n, std::unique_ptr<RecordBody> b)
            : is_interface(interface), visibility(vis), name(std::move(n)), body(std::move(b)) {}
    };
    struct EnumBody : ASTNode {
        std::vector<std::string> elements;
    };
    struct EnumDeclarationStatement : Statement {
        Visibility visibility;
        std::string name;
        std::unique_ptr<EnumBody> body;
        EnumDeclarationStatement(Visibility vis, std::string n, std::unique_ptr<EnumBody> b)
            : visibility(vis), name(std::move(n)), body(std::move(b)) {}
    };
    struct TypeAliasStatement : Statement {
        Visibility visibility;
        std::string name;
        std::vector<GenericTypeParameter> type_parameters;
        std::unique_ptr<TypeNode> type;
        TypeAliasStatement(Visibility vis, std::string n, std::vector<GenericTypeParameter> &&tparams, std::unique_ptr<TypeNode> val)
            : visibility(vis), name(std::move(n)), type_parameters(std::move(tparams)), type(std::move(val)) {}
    };
    struct AssignmentStatement : Statement {
        std::vector<std::unique_ptr<Expression>> left;
        std::vector<std::unique_ptr<Expression>> right;
    };

    struct CallStatement : Statement {
        std::unique_ptr<FunctionCallExpression> call;
        CallStatement(std::unique_ptr<FunctionCallExpression> c) : call(std::move(c)) {}
    };
}
