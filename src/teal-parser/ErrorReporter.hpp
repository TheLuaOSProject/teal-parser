#pragma once
#include <string>
#include <vector>
#include <source_location>
#include <format>

namespace teal::parser::typecheck
{
    struct Error {
        size_t line;
        size_t column;
        std::string message;
        std::source_location occured_at;

        std::string to_string() const { return std::format("Error at {}:{} ({}:{}): {}", line, column, occured_at.file_name(), occured_at.line(), message); }
    };

    class ErrorReporter {
    public:
        void add_error(size_t line, size_t column, const std::string &message, std::source_location loc = std::source_location::current());
        bool empty() const;
        const std::vector<Error> &get_errors() const;
        std::string format_errors() const;

    private:
        std::vector<Error> errors;
    };

}
