#pragma once

#include <string>
#include <vector>
#include <stdexcept>
#include "AST.hpp" // Include AST to get location info

namespace teal::parser::typechecker
{
    struct TypeError
    {
        std::string message;
        size_t line;
        size_t column;

        TypeError(std::string msg, size_t l, size_t c)
            : message(std::move(msg)), line(l), column(c) {}

        TypeError(std::string msg, const parser::ast::ASTNode& node)
            : message(std::move(msg)), line(node.line), column(node.column) {}

        std::string toString() const {
            return "Error (" + std::to_string(line) + ":" + std::to_string(column) + "): " + message;
        }
    };

    class ErrorCollector
    {
        std::vector<TypeError> errors;

    public:
        void addError(std::string msg, size_t line, size_t col) {
            errors.emplace_back(std::move(msg), line, col);
        }

        void addError(std::string msg, const parser::ast::ASTNode& node) {
             errors.emplace_back(std::move(msg), node);
        }

        bool hasErrors() const {
            return !errors.empty();
        }

        const std::vector<TypeError>& getErrors() const {
            return errors;
        }

        void clear() {
            errors.clear();
        }
    };

    // Custom exception for fatal errors during checking (e.g., internal logic error)
    class TypeCheckError : public std::runtime_error {
    public:
        TypeCheckError(const std::string& message) : std::runtime_error(message) {}
    };

} // namespace teal::typechecker
