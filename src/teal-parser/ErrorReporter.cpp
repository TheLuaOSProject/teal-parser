#include "ErrorReporter.hpp"
#include <sstream>

using namespace teal::parser::typecheck;

void ErrorReporter::add_error(size_t line, size_t column, const std::string &message, std::source_location loc)
{
    errors.push_back({ line, column, message, loc });
}
bool ErrorReporter::empty() const { return errors.empty(); }
const std::vector<Error> &ErrorReporter::get_errors() const { return errors; }
std::string ErrorReporter::format_errors() const
{
    std::ostringstream oss;
    for (auto &err : errors) {
        oss << "Error";
        if (err.line > 0) { oss << " at line " << err.line << ", col " << err.column; }
        oss << ": " << err.message << "\n";
    }
    return oss.str();
}
