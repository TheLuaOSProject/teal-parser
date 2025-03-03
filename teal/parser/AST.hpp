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
        std::string methodName;                  // method name if invoked with ':'
        std::vector<std::unique_ptr<Expression>> args;
        FunctionCallExpression(std::unique_ptr<Expression> baseExpr, std::string method = "")
            : base(std::move(baseExpr)), methodName(std::move(method)) {}
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
        TokenType op;

        OperationExpression(TokenType opType) : op(opType) {}
    };

    struct BinaryOperationExpression : OperationExpression {
        std::unique_ptr<Expression> left;
        std::unique_ptr<Expression> right;
        BinaryOperationExpression(TokenType opType, std::unique_ptr<Expression> l, std::unique_ptr<Expression> r)
            : OperationExpression(opType), left(std::move(l)), right(std::move(r)) {}
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

    // Function body for function definitions (parameters and return types)
    struct FunctionBody : ASTNode {
        std::vector<std::string> typeParams;
        struct Param {
            std::string name;
            bool isVarArg;
            bool isOptional;
            std::unique_ptr<TypeNode> typeAnn;
        };
        std::vector<Param> params;
        std::vector<std::unique_ptr<TypeNode>> returnTypes;
        bool returnVarArg = false;
        std::unique_ptr<Block> body;
    };


    struct FunctionDefinitionExpression : Expression {
        std::unique_ptr<FunctionBody> body;
        FunctionDefinitionExpression(std::unique_ptr<FunctionBody> b) : body(std::move(b)) {}
    };
    struct CastExpression : Expression {
        std::unique_ptr<Expression> expr;
        std::vector<std::unique_ptr<TypeNode>> targetTypes;
        CastExpression(std::unique_ptr<Expression> e, std::vector<std::unique_ptr<TypeNode>> types)
            : expr(std::move(e)), targetTypes(std::move(types)) {}
    };
    struct IsTypeExpression : Expression {
        std::unique_ptr<Expression> expr;
        std::unique_ptr<TypeNode> type;
        IsTypeExpression(std::unique_ptr<Expression> e, std::unique_ptr<TypeNode> t)
            : expr(std::move(e)), type(std::move(t)) {}
    };
    struct TableConstructorExpression : Expression {
        struct Field {
            std::optional<std::string> nameKey;     // key if it's a Name field
            std::unique_ptr<Expression> keyExpr;    // key expression if in [exp]
            std::unique_ptr<TypeNode> typeAnn;      // optional type annotation for Name field
            std::unique_ptr<Expression> value;      // field value expression
        };
        std::vector<Field> fields;
    };

    // Type AST nodes
    struct BasicTypeNode : TypeNode {
        std::string name;
        BasicTypeNode(std::string n) : name(std::move(n)) {}
    };
    struct NominalTypeNode : TypeNode {
        std::vector<std::string> nameParts;
        std::vector<std::string> typeArgs;
        NominalTypeNode(std::vector<std::string> parts, std::vector<std::string> args = {})
            : nameParts(std::move(parts)), typeArgs(std::move(args)) {}
    };
    struct TableTypeNode : TypeNode {
        std::vector<std::unique_ptr<TypeNode>> elementTypes;
        std::unique_ptr<TypeNode> keyType;
        bool isMap;
        TableTypeNode() : isMap(false) {}
    };
    struct FunctionTypeNode : TypeNode {
        std::vector<std::string> typeParams;
        struct ParamType {
            std::optional<std::string> name;
            bool isOptional;
            std::unique_ptr<TypeNode> type;
        };
        std::vector<ParamType> params;
        std::vector<std::unique_ptr<TypeNode>> returnTypes;
        bool returnVarArg = false;
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
        std::string moduleName;
        std::vector<std::string> typeNames;
        RequireTypeNode(std::string mod, std::vector<std::string> names)
            : moduleName(std::move(mod)), typeNames(std::move(names)) {}
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
        std::vector<IfBranch> ifBranches;
        std::unique_ptr<Block> elseBlock;
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
        std::string varName;
        std::unique_ptr<Expression> startExp;
        std::unique_ptr<Expression> endExp;
        std::unique_ptr<Expression> stepExp;
        std::unique_ptr<Block> body;
    };
    struct ForInStatement : Statement {
        std::vector<std::string> names;
        std::vector<std::unique_ptr<Expression>> exprs;
        std::unique_ptr<Block> body;
    };
    struct FunctionDeclarationStatement : Statement {
        bool isLocal;
        bool isGlobal;
        std::vector<std::string> namePath;
        std::string methodName;
        bool isMethod;
        std::unique_ptr<FunctionBody> body;
        FunctionDeclarationStatement(bool local, bool global)
            : isLocal(local), isGlobal(global), isMethod(false) {}
    };
    struct VariableDeclarationStatement : Statement {
        bool isLocal;
        bool isGlobal;
        struct NameAttrib {  // name with optional attribute (for <attr>)
            std::string name;
            std::optional<std::string> attrib;
        };
        std::vector<NameAttrib> names;
        std::vector<std::unique_ptr<TypeNode>> types;
        std::vector<std::unique_ptr<Expression>> values;
        VariableDeclarationStatement(bool local, bool global) : isLocal(local), isGlobal(global) {}
    };
    struct RecordBody : ASTNode {
        std::vector<std::string> typeParams;
        std::unique_ptr<TypeNode> structuralExt;
        std::vector<std::unique_ptr<TypeNode>> interfaceExt;
        std::unique_ptr<Expression> whereClause;
        struct Entry {
            enum class Kind { Field, Userdata, TypeAlias, Record, Enum, Interface } kind;
            bool isMetamethod;
            std::optional<std::string> fieldName;
            std::optional<std::string> fieldKeyLiteral;
            std::unique_ptr<TypeNode> fieldType;
            std::string typeName;
            std::unique_ptr<TypeNode> typeValue;
            std::string nestedName;
            std::unique_ptr<ASTNode> nestedBody;
            Entry() : isMetamethod(false) {}
        };
        std::vector<Entry> entries;
    };
    struct RecordDeclarationStatement : Statement {
        bool isInterface;
        bool isLocal;
        bool isGlobal;
        std::string name;
        std::unique_ptr<RecordBody> body;
        RecordDeclarationStatement(bool interface, bool local, bool global, std::string n, std::unique_ptr<RecordBody> b)
            : isInterface(interface), isLocal(local), isGlobal(global), name(std::move(n)), body(std::move(b)) {}
    };
    struct EnumBody : ASTNode {
        std::vector<std::string> elements;
    };
    struct EnumDeclarationStatement : Statement {
        bool isLocal;
        bool isGlobal;
        std::string name;
        std::unique_ptr<EnumBody> body;
        EnumDeclarationStatement(bool local, bool global, std::string n, std::unique_ptr<EnumBody> b)
            : isLocal(local), isGlobal(global), name(std::move(n)), body(std::move(b)) {}
    };
    struct TypeAliasStatement : Statement {
        bool isLocal;
        bool isGlobal;
        std::string name;
        std::unique_ptr<TypeNode> typeValue;
        TypeAliasStatement(bool local, bool global, std::string n, std::unique_ptr<TypeNode> val)
            : isLocal(local), isGlobal(global), name(std::move(n)), typeValue(std::move(val)) {}
    };
    struct AssignmentStatement : Statement {
        std::vector<std::unique_ptr<Expression>> lhs;
        std::vector<std::unique_ptr<Expression>> rhs;
    };
    struct CallStatement : Statement {
        std::unique_ptr<FunctionCallExpression> call;
        CallStatement(std::unique_ptr<FunctionCallExpression> c) : call(std::move(c)) {}
    };
}
