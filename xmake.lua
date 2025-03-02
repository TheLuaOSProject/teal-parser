-- Copyright (C) 2025 Amrit Bhogal
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

add_rules("mode.debug", "mode.release")

set_languages("gnuxx26")

target("teal-parser")
    add_files("teal/**.cpp")
    add_files("main.cpp")

    add_cxxflags("-fexperimental-library")
    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions"
        -- { force = true }
    )
