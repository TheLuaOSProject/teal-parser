// Copyright (C) 2025 Amrit Bhogal
//
// This file is part of teal-parser-test.
//
// teal-parser-test is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// teal-parser-test is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with teal-parser-test.  If not, see <https://www.gnu.org/licenses/>.

#pragma once

#include <string>
#include <vector>
#include <variant>
#include <expected>

namespace teal
{
    namespace tokens
    {
        struct EndOfFile {};
        struct Identifier {
            std::string value;
        };
        struct Number {
            long double value;
        };
        struct String {
            std::string value;
        };
        struct Keyword {
            enum class Type {
                NIL = 4, TRUE, FALSE,
                FUNCTION, END, DO,
                IF, THEN, ELSE, ELSEIF,
                WHILE, REPEAT, UNTIL, FOR, IN,
                BREAK, GOTO, RETURN,
                LOCAL, GLOBAL, RECORD, INTERFACE, ENUM, TYPE,
                WHERE,
                AND, OR, NOT, AS, IS,
            } type;
        };
        struct Operator {
            enum class Type {
                ASSIGNMENT = static_cast<int>(Keyword::Type::IS) + 1,
                EQUALS, NOT_EQUALS, LESS, LESS_EQUALS, GREATER, GREATER_EQUALS,
                CONCAT, ADD, SUBTRACT, MULTIPLY, DIVIDE, FLOOR_DIVIDE, MODULUS, POWER,
                BITWISE_AND, BITWISE_OR, BITWISE_XOR, BITWISE_LEFT_SHIFT, BITWSIE_RIGHT_SHIFT,
                LENGTH,
                LEFT_PARENTHESIS, RIGHT_PARENTHESIS, LEFT_BRACE, RIGHT_BRACE, LEFT_BRACKET, RIGHT_BRACKET,
                COMMA, SEMICOLON, COLON, DOUBLE_COLON, DOT,
                VARARG, QUESTION
            } type;
        };
    }

    using TokenUnion = std::variant <
        tokens::EndOfFile,
        tokens::String,
        tokens::Number,
        tokens::Identifier,
        tokens::Keyword,
        tokens::Operator
    >;

    struct Token: public TokenUnion {
        // std::string text;
        size_t line, column;

        constexpr Token(/*std::string text,*/ size_t line, size_t col, const TokenUnion &token)
            : TokenUnion(token), /*text(text),*/ line(line), column(col) {}

        constexpr Token(const Token &) = delete;
        constexpr Token(Token &&) = delete;

        template<typename T>
        constexpr T &get() const
        { return std::get<T>(*this); }

        template<typename T>
        constexpr bool is() const
        { return std::holds_alternative<T>(*this); }
    };

    struct LexError {
        std::string message;
        size_t line, column;
    };

    std::expected<std::vector<Token>, std::vector<LexError>> tokenise(const std::string_view &src);
}
