#pragma once

#include <memory>
#include <vector>
#include <optional>

#include "AST.hpp"
#include "Environment.hpp"
#include "TypeSystem.hpp"
#include "TypeError.hpp"

namespace teal::parser::typechecker
{
    // Forward declare AST node types to avoid including full AST here if possible
    // (Though it's likely needed for detailed checks) - Included for now.

    class TypeChecker {
    // private:
    public: //TODO: make private
        Environment env;
        ErrorCollector &errors; // Use the reference passed in constructor

        // --- Type Resolution ---
        std::shared_ptr<const Type> resolveTypeNode(const ast::TypeNode *node);
        std::shared_ptr<const Type> resolveBasicType(const ast::BasicTypeNode *node);
        std::shared_ptr<const Type> resolveNominalType(const ast::NominalTypeNode *node);
        std::shared_ptr<const Type> resolveTableType(const ast::TableTypeNode *node);
        std::shared_ptr<const Type> resolveFunctionType(const ast::FunctionTypeNode *node);
        std::shared_ptr<const Type> resolveUnionType(const ast::UnionTypeNode *node);
        std::shared_ptr<const Type> resolveRecordTypeNode(const ast::TypeRecordNode *node); // For anonymous records in types
        std::shared_ptr<const Type> resolveEnumTypeNode(const ast::TypeEnumNode *node); // For anonymous enums in types
        std::shared_ptr<const Type> resolveRequireType(const ast::RequireTypeNode *node);

        // --- Expression Type Checking ---
        // Returns the inferred type of the expression, or types::Unknown on error.

        std::shared_ptr<const Type> checkExpressionInternal(
            const ast::Expression *expr,
            const std::shared_ptr<const Type>& expectedTypeHint = nullptr // <-- Added hint
        );
    
        // Change checkTableConstructorExpression signature
        std::shared_ptr<const Type> checkTableConstructorExpression(
            const ast::TableConstructorExpression *expr,
            const std::shared_ptr<const Type>& expectedTypeHint // <-- Added hint
        );

        std::shared_ptr<const Type> checkExpression(const ast::Expression *expr);
        std::shared_ptr<const Type> checkNameExpression(const ast::NameExpression *expr);
        std::shared_ptr<const Type> checkNumberExpression(const ast::NumberExpression *expr);
        std::shared_ptr<const Type> checkStringExpression(const ast::StringExpression *expr);
        std::shared_ptr<const Type> checkBooleanExpression(const ast::BooleanExpression *expr);
        std::shared_ptr<const Type> checkNilExpression(const ast::NilExpression *expr);
        std::shared_ptr<const Type> checkVarargExpression(const ast::VarargExpression *expr); // TODO
        std::shared_ptr<const Type> checkFunctionCallExpression(const ast::FunctionCallExpression *expr);
        std::shared_ptr<const Type> checkIndexExpression(const ast::IndexExpression *expr);
        std::shared_ptr<const Type> checkFieldExpression(const ast::FieldExpression *expr);
        std::shared_ptr<const Type> checkBinaryOperationExpression(const ast::BinaryOperationExpression *expr);
        std::shared_ptr<const Type> checkUnaryOperationExpression(const ast::UnaryOperationExpression *expr);
        std::shared_ptr<const Type> checkFunctionDefinitionExpression(const ast::FunctionDefinitionExpression *expr);
        std::shared_ptr<const Type> checkCastExpression(const ast::CastExpression *expr);
        std::shared_ptr<const Type> checkIsTypeExpression(const ast::IsTypeExpression *expr);

         // Helper for checking assignability in various contexts
        void checkAssignmentCompat(const std::shared_ptr<const Type>& target, const std::shared_ptr<const Type>& value, const ast::ASTNode &errorNode);

        // Helper for checking function call arguments
        std::vector<std::shared_ptr<const Type>> checkFunctionArguments(
            const ast::FunctionCallExpression &callNode,
            const FunctionType &funcType,
            const std::shared_ptr<const Type>& selfTypeOpt = nullptr // For method calls
        );


        // --- Statement Type Checking ---
        void checkStatement(const ast::Statement *stmt);
        void checkBlock(const ast::Block *block, bool createNewScope = true);
        void checkReturnStatement(const ast::ReturnStatement *stmt);
        void checkBreakStatement(const ast::BreakStatement *stmt); // No type check needed
        void checkGotoStatement(const ast::GotoStatement *stmt);   // No type check needed
        void checkLabelStatement(const ast::LabelStatement *stmt); // No type check needed
        void checkDoStatement(const ast::DoStatement *stmt);
        void checkIfStatement(const ast::IfStatement *stmt);
        void checkWhileStatement(const ast::WhileStatement *stmt);
        void checkRepeatStatement(const ast::RepeatStatement *stmt);
        void checkForNumericStatement(const ast::ForNumericStatement *stmt);
        void checkForInStatement(const ast::ForInStatement *stmt);
        void checkFunctionDeclarationStatement(ast::FunctionDeclarationStatement *stmt); // Non-const to potentially update definition
        void checkVariableDeclarationStatement(const ast::VariableDeclarationStatement *stmt);
        void checkRecordDeclarationStatement(ast::RecordDeclarationStatement *stmt); // Non-const to update definition
        void checkEnumDeclarationStatement(const ast::EnumDeclarationStatement *stmt);
        void checkTypeAliasStatement(const ast::TypeAliasStatement *stmt);
        void checkAssignmentStatement(const ast::AssignmentStatement *stmt);
        void checkCallStatement(const ast::CallStatement *stmt);

         // Helper for building FunctionType from FunctionBody/FunctionTypeNode
         std::shared_ptr<const FunctionType> buildFunctionType(const ast::FunctionBody &body, const std::string &funcName = "");
         std::shared_ptr<const FunctionType> buildFunctionType(const ast::FunctionTypeNode &typeNode);

         // Helper for processing record/interface bodies (used by declarations and type nodes)
          void processRecordBody(const ast::RecordBody &body, RecordType *record);
          void processRecordBody(const ast::RecordBody &body, InterfaceType *iface);

           // Helper to check if an expression is a valid L-Value (can be assigned to)
          bool isLValue(const ast::Expression *expr);

    public:
        TypeChecker(ErrorCollector &err);

        // Entry point for checking a whole AST (assuming it's a Block or top-level statements)
        void check(const ast::ASTNode *root);
    };

} // namespace teal::typechecker

