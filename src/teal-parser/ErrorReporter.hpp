#pragma once
#include <string>
#include <vector>

#include <format>

namespace teal::parser::typecheck
{

    struct Error {
        size_t line;
        size_t column;
        std::string message;

        std::string to_string() const { return std::format("Error at {}:{}: {}", line, column, message); }
    };

    class ErrorReporter {
    public:
        void addError(size_t line, size_t column, const std::string &message);
        bool empty() const;
        const std::vector<Error> &getErrors() const;
        std::string formatErrors() const;

    private:
        std::vector<Error> errors;
    };

}
