#pragma once

#include <memory>
#include <optional>
#include <stack>
#include <string>
#include <vector>

#include "AST.hpp"
#include "Common.hpp" // For Error definition base
#include "SymbolTable.hpp"
#include "Types.hpp"

namespace teal::parser::typechecker
{

// Specific error kinds for the type checker
    struct TypeErrorDetail {
        std::string message;
    // Add more fields if needed, e.g., related types
        TypeErrorDetail(std::string msg) : message(std::move(msg)) { }
     // Default to_string
        virtual std::string to_string() const { return message; }
    };

    struct TypeMismatchError : TypeErrorDetail {
        std::shared_ptr<Type> expected;
        std::shared_ptr<Type> actual;
        TypeMismatchError(std::shared_ptr<Type> exp, std::shared_ptr<Type> act) :
            TypeErrorDetail(""), expected(std::move(exp)), actual(std::move(act))
        {
            message = "Type mismatch: expected '" + (expected ? expected->to_string() : "unknown") + "', got '"
                + (actual ? actual->to_string() : "unknown") + "'";
        }
        std::string to_string() const override { return message; }
    };
    struct UndefinedVariableError : TypeErrorDetail {
        std::string var_name;
        UndefinedVariableError(std::string name) : TypeErrorDetail("Undefined variable: '" + name + "'"), var_name(std::move(name)) { }
        std::string to_string() const override { return message; }
    };
    struct InvalidOperationError : TypeErrorDetail {
        std::string operation;
        std::vector<std::shared_ptr<Type>> operand_types;
        InvalidOperationError(std::string op, std::vector<std::shared_ptr<Type>> operands) :
            TypeErrorDetail("Invalid operation '" + op + "' for operand type(s)"), operation(std::move(op)), operand_types(std::move(operands))
        {
            message += ": ";
            for (size_t i = 0; i < operand_types.size(); ++i) {
                message += (operand_types[i] ? operand_types[i]->to_string() : "<unknown>");
                if (i < operand_types.size() - 1) message += ", ";
            }
        }
        std::string to_string() const override { return message; }
    };
    struct ArityMismatchError : TypeErrorDetail {
        int expected_min;
        std::optional<int> expected_max; // nullopt if variadic
        int actual;
        ArityMismatchError(int exp_min, std::optional<int> exp_max, int act) :
            TypeErrorDetail(""), expected_min(exp_min), expected_max(exp_max), actual(act)
        {
            message = "Function call arity mismatch: expected ";
            if (expected_max.has_value()) {
                if (expected_min == *expected_max) message += std::to_string(expected_min);
                else message += std::to_string(expected_min) + " to " + std::to_string(*expected_max);
            } else {
                message += "at least " + std::to_string(expected_min);
            }
            message += " arguments, got " + std::to_string(actual);
        }
        std::string to_string() const override { return message; }
    };
    struct FieldNotFoundError : TypeErrorDetail {
        std::string field_name;
        std::shared_ptr<Type> record_type;
        FieldNotFoundError(std::string name, std::shared_ptr<Type> rec) :
            TypeErrorDetail("Field '" + name + "' not found in type '" + (rec ? rec->to_string() : "<unknown>") + "'"), field_name(std::move(name)),
            record_type(std::move(rec))
        {
        }
        std::string to_string() const override { return message; }
    };
    struct NotIndexableError : TypeErrorDetail {
        std::shared_ptr<Type> base_type;
        std::shared_ptr<Type> index_type;
        NotIndexableError(std::shared_ptr<Type> base, std::shared_ptr<Type> index) :
            TypeErrorDetail(
                "Type '" + (base ? base->to_string() : "<unknown>") + "' cannot be indexed with type '" + (index ? index->to_string() : "<unknown>")
                + "'"
            ),
            base_type(std::move(base)), index_type(std::move(index))
        {
        }
        std::string to_string() const override { return message; }
    };
    struct NotCallableError : TypeErrorDetail {
        std::shared_ptr<Type> type;
        NotCallableError(std::shared_ptr<Type> t) :
            TypeErrorDetail("Type '" + (t ? t->to_string() : "<unknown>") + "' is not callable"), type(std::move(t))
        {
        }
        std::string to_string() const override { return message; }
    };

    struct NotAllCodePathsReturn { };

    struct NotImplementedError {
        const std::type_info &type;

        std::string to_string() const
        {
            return std::format("Type '{}' not implemented yet!", type.name());
        }
    };

// Type alias for the specific error variant used by TypeChecker
    using CheckerError = teal::parser::Error<
        TypeErrorDetail, TypeMismatchError, UndefinedVariableError, InvalidOperationError, ArityMismatchError, FieldNotFoundError, NotIndexableError,
        NotAllCodePathsReturn, NotCallableError>;

// Forward declare Visitor classes if they are complex
    class ASTVisitor {
    public:
        virtual ~ASTVisitor() = default;
    // Define visit methods for each AST node type
    // Returning shared_ptr<Type> to represent the type of the expression/node
        virtual std::shared_ptr<Type> visit(ast::ASTNode *node) = 0; // Dispatcher
        virtual std::shared_ptr<Type> visit(ast::Expression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::Statement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::NameExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::NumberExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::StringExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::BooleanExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::NilExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::VarargExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::FunctionCallExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::IndexExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::FieldExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::BinaryOperationExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::UnaryOperationExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::FunctionDefinitionExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::CastExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::IsTypeExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::TableConstructorExpression *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::Block *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::ReturnStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::BreakStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::GotoStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::LabelStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::DoStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::IfStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::WhileStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::RepeatStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::ForNumericStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::ForInStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::FunctionDeclarationStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::VariableDeclarationStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::RecordDeclarationStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::EnumDeclarationStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::TypeAliasStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::AssignmentStatement *node) = 0;
        virtual std::shared_ptr<Type> visit(ast::CallStatement *node) = 0;
    // No visit for TypeNode, use resolve_type instead
    };

    class TypeChecker : public ASTVisitor {
    public:
        TypeChecker();
        std::vector<CheckerError> check(ast::Block *root);

    // --- AST Visitor Methods ---
    // #define $undefined override { throw std::runtime_error(std::format("Node '{}' not implemented yet", typeid(*node).name())); }
        std::shared_ptr<Type> visit(ast::ASTNode *node) override; // Dispatcher
        std::shared_ptr<Type> visit(ast::Expression *node) override;
        std::shared_ptr<Type> visit(ast::Statement *node) override;
        std::shared_ptr<Type> visit(ast::NameExpression *node) override;
        std::shared_ptr<Type> visit(ast::NumberExpression *node) override;
        std::shared_ptr<Type> visit(ast::StringExpression *node) override;
        std::shared_ptr<Type> visit(ast::BooleanExpression *node) override;
        std::shared_ptr<Type> visit(ast::NilExpression *node) override;
        std::shared_ptr<Type> visit(ast::VarargExpression *node) override;
        std::shared_ptr<Type> visit(ast::FunctionCallExpression *node) override;
        std::shared_ptr<Type> visit(ast::IndexExpression *node) override;
        std::shared_ptr<Type> visit(ast::FieldExpression *node) override;
        std::shared_ptr<Type> visit(ast::BinaryOperationExpression *node) override;
        std::shared_ptr<Type> visit(ast::UnaryOperationExpression *node) override;
        std::shared_ptr<Type> visit(ast::FunctionDefinitionExpression *node) override;
        std::shared_ptr<Type> visit(ast::CastExpression *node) override;
        std::shared_ptr<Type> visit(ast::IsTypeExpression *node) override;
        std::shared_ptr<Type> visit(ast::TableConstructorExpression *node) override;
        std::shared_ptr<Type> visit(ast::Block *node) override;
        std::shared_ptr<Type> visit(ast::ReturnStatement *node) override;
        std::shared_ptr<Type> visit(ast::BreakStatement *node) override;
        std::shared_ptr<Type> visit(ast::GotoStatement *node) override;
        std::shared_ptr<Type> visit(ast::LabelStatement *node) override;
        std::shared_ptr<Type> visit(ast::DoStatement *node) override;
        std::shared_ptr<Type> visit(ast::IfStatement *node) override;
        std::shared_ptr<Type> visit(ast::WhileStatement *node) override;
        std::shared_ptr<Type> visit(ast::RepeatStatement *node) override;
        std::shared_ptr<Type> visit(ast::ForNumericStatement *node) override;
        std::shared_ptr<Type> visit(ast::ForInStatement *node) override;
        std::shared_ptr<Type> visit(ast::FunctionDeclarationStatement *node) override;
        std::shared_ptr<Type> visit(ast::VariableDeclarationStatement *node) override;
        std::shared_ptr<Type> visit(ast::RecordDeclarationStatement *node) override;
        std::shared_ptr<Type> visit(ast::EnumDeclarationStatement *node) override;
        std::shared_ptr<Type> visit(ast::TypeAliasStatement *node) override;
        std::shared_ptr<Type> visit(ast::AssignmentStatement *node) override;
        std::shared_ptr<Type> visit(ast::CallStatement *node) override;

    private:
        SymbolTableManager symbol_table;
        std::vector<CheckerError> errors;
        std::stack<std::shared_ptr<FunctionType>> current_function_type; // For return checks
    // Track current loop depth for break/continue validity?
    // Type cache if needed

    // --- Helper Methods ---
        void add_error(const CheckerError::Kind_t &detail, const ast::ASTNode &location_node);
        void add_error(const CheckerError::Kind_t &detail, size_t line, size_t col);

    // Resolves an AST TypeNode to an internal Type representation
        std::shared_ptr<Type> resolve_type_node(ast::TypeNode *type_node);
        std::shared_ptr<TupleType> resolve_type_list(
            const std::vector<std::unique_ptr<ast::TypeNode>> &type_nodes, const ast::ASTNode &fallback_node
        );

     // Checks if source type can be assigned to target type
        bool check_assignability(const std::shared_ptr<Type> &target, const std::shared_ptr<Type> &source, const ast::ASTNode &error_node);

    // Converts FunctionBody AST to FunctionType internal representation
        std::shared_ptr<FunctionType> resolve_function_body(ast::FunctionBody *body);
    // Converts RecordBody AST to RecordType internal representation
        void populate_record_type(RecordType *record_type, ast::RecordBody *body);
     // Converts EnumBody AST to EnumType internal representation
        void populate_enum_type(EnumType *enum_type, ast::EnumBody *body);

    // Load standard library types into the global scope
        void load_stdlib();

     // Type inference for table constructor
        std::shared_ptr<Type> infer_table_constructor_type(ast::TableConstructorExpression *node);

    // Handle type narrowing in conditional blocks
        void apply_narrowing(const ast::Expression *condition, bool is_true_branch);
        void restore_narrowed_types(Scope *scope_to_restore); // Needs careful management

    // Check operation validity
        std::shared_ptr<Type> check_binary_operation(ast::BinaryOperationExpression *node, std::shared_ptr<Type> left, std::shared_ptr<Type> right);
        std::shared_ptr<Type> check_unary_operation(ast::UnaryOperationExpression *node, std::shared_ptr<Type> operand);

    // Handle function calls including method calls
        std::shared_ptr<TupleType> check_function_call(
            const ast::FunctionCallExpression *call_node,
            std::shared_ptr<Type> base_type, // Type of the thing being called
            const std::vector<std::shared_ptr<Type>> &arg_types, bool is_method_call, const std::string &method_name = ""
        );

        std::shared_ptr<Type> check_metamethod(
            const std::string &op_name, const ast::ASTNode &error_node, std::shared_ptr<Type> t1, std::shared_ptr<Type> t2
        );

        std::shared_ptr<Type> check_metamethod(
            const std::string &op_name, const ast::ASTNode &error_node, std::shared_ptr<Type> operand
        );
    };

} // namespace teal::checker
