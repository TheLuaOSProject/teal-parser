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

#include "NewLexer.hpp"

using namespace teal;

std::expected<std::vector<Token>, std::vector<LexError>> tokenise(const std::string_view &)
{ return std::unexpected(std::vector<LexError>()); }

