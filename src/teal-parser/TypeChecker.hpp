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
        const ErrorReporter &get_error_reporter() const { return _errors; }
        const SymbolTable &get_symbols() const { return _symbols; }
        SymbolTable &get_symbols() { return _symbols; }

    private:
        SymbolTable _symbols;
        ErrorReporter _errors;
        std::shared_ptr<TypeSymbol> declare_type(teal::parser::ast::RecordDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr);
        std::shared_ptr<TypeSymbol> declare_type(teal::parser::ast::EnumDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr);
        std::shared_ptr<TypeSymbol> declare_type(teal::parser::ast::TypeAliasStatement *node, std::shared_ptr<TypeSymbol> parent = nullptr);
        std::shared_ptr<TypeSymbol> define_type(std::shared_ptr<TypeSymbol> typeSymbol);
        TypePtr check_expression(teal::parser::ast::Expression *expr);
        TypePtr check_expression(teal::parser::ast::Expression *expr, const TypePtr &expe);
        void check_statement(teal::parser::ast::Statement *stmt);
        void check_if(teal::parser::ast::IfStatement *node);
        void check_while(teal::parser::ast::WhileStatement *node);
        void check_repeat(teal::parser::ast::RepeatStatement *node);
        void check_numeric_for(teal::parser::ast::ForNumericStatement *node);
        void check_for_in(teal::parser::ast::ForInStatement *node);
        void check_function_declaration(teal::parser::ast::FunctionDeclarationStatement *node);
        void check_variable_declaration(teal::parser::ast::VariableDeclarationStatement *node);
        void check_assignment(teal::parser::ast::AssignmentStatement *node);
        TypePtr resolve_type_node(teal::parser::ast::TypeNode *ty_node, std::shared_ptr<TypeSymbol> current_ty = nullptr);
        std::shared_ptr<TypeSymbol> find_type_symbol(const std::vector<std::string> &name);
        struct NarrowInfo {
            TypePtr then_type;
            TypePtr else_type;
        };
        std::unordered_map<std::string, NarrowInfo> analyze_condition(teal::parser::ast::Expression *cond);
        int _loop_depth;
    };

};
