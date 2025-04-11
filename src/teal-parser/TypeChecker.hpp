#pragma once
#include "AST.hpp"
#include "ErrorReporter.hpp"
#include "SymbolTable.hpp"
#include "Type.hpp"

namespace teal::parser::typecheck
{

    class TypeChecker {
    public:
        TypeChecker();
        void check(teal::parser::ast::Block *ast);
        const ErrorReporter &getErrorReporter() const { return errors; }

    private:
        SymbolTable symbols;
        ErrorReporter errors;
        void declareType(
            teal::parser::ast::RecordDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr
        );
        void declareType(
            teal::parser::ast::EnumDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr
        );
        void declareType(teal::parser::ast::TypeAliasStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr);
        void defineType(std::shared_ptr<TypeSymbol> typeSymbol);
        TypePtr checkExpression(teal::parser::ast::Expression *expr);
        TypePtr checkExpression(teal::parser::ast::Expression *expr, const TypePtr &expectedType);
        void checkStatement(teal::parser::ast::Statement *stmt);
        void checkIf(teal::parser::ast::IfStatement *node);
        void checkWhile(teal::parser::ast::WhileStatement *node);
        void checkRepeat(teal::parser::ast::RepeatStatement *node);
        void checkForNumeric(teal::parser::ast::ForNumericStatement *node);
        void checkForIn(teal::parser::ast::ForInStatement *node);
        void checkFunctionDecl(teal::parser::ast::FunctionDeclarationStatement *node);
        void checkVarDecl(teal::parser::ast::VariableDeclarationStatement *node);
        void checkAssignment(teal::parser::ast::AssignmentStatement *node);
        TypePtr resolveTypeNode(
            teal::parser::ast::TypeNode *typeNode, std::shared_ptr<TypeSymbol> currentTypeSym = nullptr
        );
        std::shared_ptr<TypeSymbol> findTypeSymbol(const std::vector<std::string> &nameParts);
        struct NarrowInfo {
            TypePtr thenType;
            TypePtr elseType;
        };
        std::unordered_map<std::string, NarrowInfo> analyzeCondition(teal::parser::ast::Expression *cond);
        int loopDepth;
    };

};
