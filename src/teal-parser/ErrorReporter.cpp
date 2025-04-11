#include "ErrorReporter.hpp"
#include <sstream>

using namespace teal::parser::typecheck;

void ErrorReporter::add_error(size_t line, size_t column, const std::string &message)
{
    _errors.push_back({ line, column, message });
}
bool ErrorReporter::empty() const { return _errors.empty(); }
const std::vector<Error> &ErrorReporter::get_errors() const { return _errors; }
std::string ErrorReporter::format_errors() const
{
    std::ostringstream oss;
    for (auto &err : _errors) {
        oss << "Error";
        if (err.line > 0) { oss << " at line " << err.line << ", col " << err.column; }
        oss << ": " << err.message << "\n";
    }
    return oss.str();
}
