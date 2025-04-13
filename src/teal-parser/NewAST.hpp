// Copyright (C) 2025 Amrit Bhogal
// 
// teal-parser-test is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// teal-parser-test is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with teal-parser-test. If not, see <https://www.gnu.org/licenses/>.

#pragma once

#include <variant>
#include <vector>
#include <memory>
#include <string>

#include "Lexer.hpp"

namespace teal::parser::ast::v2
{
    struct ASTNode {};
    struct Expression : ASTNode {};

    struct NameExpression : Expression {
        std::string_view value;
    };

    struct NumberExpression : Expression {
        std::string_view value;
    };

    struct StringExpression : Expression {
        std::string_view value;
    };

    struct BooleanExpression : Expression {
        bool value;
    };

    struct NilExpression : Expression {};
    struct VarargExpression : Expression {};

    struct FunctionCallExpression : Expression {
        Expression *base;
        std::string method_name;
        std::vector<Expression *> arguments;
    };

    struct IndexExpression : Expression {
        Expression *table, *index;
    };

    struct FieldExpression : Expression {
        Expression *object;
        std::string field;
    };

    struct OperationExpression : Expression {
        
    };
}
