#include "ErrorReporter.hpp"
#include <sstream>

using namespace teal::parser::typecheck;

void ErrorReporter::addError(size_t line, size_t column, const std::string &message)
{
    errors.push_back({ line, column, message });
}
bool ErrorReporter::empty() const { return errors.empty(); }
const std::vector<Error> &ErrorReporter::getErrors() const { return errors; }
std::string ErrorReporter::formatErrors() const
{
    std::ostringstream oss;
    for (auto &err : errors) {
        oss << "Error";
        if (err.line > 0) { oss << " at line " << err.line << ", col " << err.column; }
        oss << ": " << err.message << "\n";
    }
    return oss.str();
}
