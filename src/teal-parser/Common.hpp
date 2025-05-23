// Copyright (C) 2025 Amrit Bhogal
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#pragma once

#include <format>
#include <source_location>
#include <variant>

namespace teal::parser
{
    // concept Stringable = requires(const std::string &s) {
    //     { s.to_string() } -> std::convertible_to<std::string>;
    // };

    // For std::visit overloads
    template <typename... T>
    struct Overload : T... {
        using T::operator()...;
    };

    template <typename... T>
    Overload(T...) -> Overload<T...>;

    template <typename... T> // requires Stringable<T>
    struct Error {
        using Kind_t = std::variant<T...>;
        Kind_t kind;
        size_t line, column;
        std::source_location location;

        Error(Kind_t kind, size_t line, size_t column, std::source_location location = std::source_location::current()) :
            kind(kind), line(line), column(column), location(location)
        {
        }

        constexpr inline std::string to_string() const
        {
            return std::visit(
                Overload {
                    [this](const auto &e) {
                        if constexpr (requires { e.to_string(); }) {
                            return e.to_string();
                        } else {
                            return std::format("[{}:{}] Error at {}:{} - {}", location.file_name(), location.line(), line, column, e);
                        }
                    },
                },
                kind
            );
        }
    };
}

template <typename TExpected, typename... T>
static constexpr bool operator->*(const std::variant<T...> &variant, TExpected *expected)
{
    try {
        *expected = std::get<TExpected>(variant);
        return true;
    } catch (std::bad_variant_access) {
        return false;
    }
}
