#include <cstdlib>
#include <string_view>
#include <format>
#include <iostream>
#include <chrono>
#include <ranges>
#include <functional>
#include <cxxabi.h>

#include "teal/parser/Parser.hpp"

template<typename TClock = std::chrono::steady_clock>
struct Stopwatch {
    TClock::time_point start;

    auto elapsed() const -> decltype(auto)
    { return TClock::now() - start; }

    TClock::time_point reset()
    { return start = TClock::now(); }
};

constexpr const char BIG[] = {
#   embed "tl.lua"
    , 0
};

constexpr const char BIGGER[] = {
#   embed "tl.tl"
    , 0
};

constexpr std::string_view TEST_CASES[] = {
    R"(
local function filename_to_module_name(filename)
   local path = os.getenv("TL_PATH") or package.path
   for entry in path:gmatch("[^;]+") do
      entry = entry:gsub("%.", "%%.")
      local lua_pat = "^" .. entry:gsub("%?", ".+") .. "$"
      local d_tl_pat = lua_pat:gsub("%%.lua%$", "%%.d%%.tl$")
      local tl_pat = lua_pat:gsub("%%.lua%$", "%%.tl$")

      for _, pat in ipairs({ tl_pat, d_tl_pat, lua_pat }) do
         local cap = filename:match(pat)
         if cap then
            return (cap:gsub("[/\\]", "."))
         end
      end
   end

   return (filename:gsub("%.lua$", ""):gsub("%.d%.tl$", ""):gsub("%.tl$", ""):gsub("[/\\]", "."))
end
    )",

    R"(
local enum State
   "open"
   "closed"
end

local record Point
   x: number
   y: number
end

local interface Character
   sprite: Image
   position: Point
   kind: string
end

local record Spaceship
   is Character
   where self.kind == "spaceship"

   weapon: Weapons
end

local record TreeNode<T>
   is {TreeNode<T>}

   item: T
end

local record File
   is userdata

   status: function(): State
   close: function(f: File): boolean, string
end
)",

    R"(
local function keys<K,V>(xs: {K:V}):{K}
   local ks = {}
   for k, v in pairs(xs) do
      table.insert(ks, k)
   end
   return ks
end

local s = keys({ a = 1, b = 2 })
    )",

    R"(
local record Tree<X> is {Tree<X>}
   item: X
end

local t: Tree<number> = {
   item = 1,
   { item = 2 },
   { item = 3, { item = 4 } },
}
    )",

    R"(
--test string

local my_str = [[
hello
world
]]

    )",

    R"(
fcall "test"
    )",

    R"(
    local a = require("test")
    )",

    R"(
package.cpath = package.cpath..";"..package.cpath:gsub("%.so", ".dylib")

local ltreesitter = require("ltreesitter")
local teal_parser = ltreesitter.require("teal")

print(teal_parser)
    )",

    R"(
if type "4" as string is number then print "ok" end
    )",

    R"(
local a: function()
    )",

    R"(
        local type a = function(integer, integer): integer
    )",

    R"(
local a: function(...: any): any...
    )",

    R"(
local record R
    a: integer
end
    )",

    R"(
local record R
    type: Type
end
    )",

    R"(
local record MyRec
    interface I
    end

    enum E
        "one"
        "two"
        "three"
        "ikuyo!"
    end
end
    )",

    R"(
(node.args[1].argtype as SelfType).display_type = def
    )",

    R"(
funcall {
    key = "value",
    [key] = "value",
    "value",
    self:get_value(),
    key: type = "value",
}
    )",

    R"(
local record Rec
   cbs: {K:V<A, B, C>}
end
    )",

    BIG,
    BIGGER
};

int main() {
    std::cout << "Started...\n";
    teal::Lexer::Tests().run_all();

    int retval = EXIT_SUCCESS;
    int i = 0;
    auto stopwatch = Stopwatch<>();
    for (const auto &test : TEST_CASES | std::views::drop(19)) {
        std::cout << std::format("Test case {}\n", i++);
        stopwatch.reset();
        auto [tokens, lexErrors] = teal::Lexer(std::string(test)).tokenize();
        std::cout << std::format("  Lexing took {}\n", std::chrono::duration_cast<std::chrono::milliseconds>(stopwatch.elapsed()));
        stopwatch.reset();

        if (not lexErrors.empty()) {
            std::cerr << "  Errors encountered during lexing:\n";
            for (auto &err : lexErrors) {
                std::cerr << std::format("    [tl.tl:{}:{}] {}", err.line, err.column, err.to_string()) << std::endl;
            }

            continue;
        }

        // Syntax parsing
        auto parser = teal::Parser(tokens);
        std::unique_ptr<teal::Block> ast = parser.parse();
        std::cout << std::format("  Parsing took {}\n", std::chrono::duration_cast<std::chrono::milliseconds>(stopwatch.elapsed()));

        if (ast == nullptr or not parser.errors().empty()) {
            retval = EXIT_FAILURE;
            std::cerr << "  Errors encountered during parsing:\n";
            for (auto &err : parser.errors()) {
                std::cerr << std::format("    [tl.tl:{}:{}] {}", err.line, err.column, err.message) << std::endl;
                // std::cerr << "Line " << err.line << ", Col " << err.col
                //           << ": " << err.message << std::endl;
            }

            return retval;
        } else {
            std::cout << "  Parsing completed successfully.\n";
            std::cout << std::format("  {} statements found", ast->statements.size()) << std::endl;
            auto ptr = ast->statements.at(0).get();
            std::cout << std::format("    {} is the top decl", typeid(*ptr).name()) << std::endl;

            // Traverse AST: list top-level declarations
            // for (auto &stmt : ast->statements) {
            //     auto ptr = stmt.get();
            //     // std::cout << std::format("  {} found\n", demangle(typeid(*ptr).name()).value());
            //     if (auto funcDecl = dynamic_cast<teal::FunctionDeclarationStatement *>(ptr)) {
            //         std::string fullName;
            //         if (!funcDecl->namePath.empty()) {
            //             fullName = funcDecl->namePath[0];
            //             for (size_t i = 1; i < funcDecl->namePath.size(); ++i) {
            //                 fullName += "." + funcDecl->namePath[i];
            //             }
            //             if (funcDecl->isMethod) {
            //                 fullName += ":" + funcDecl->methodName;
            //             }
            //         } else {
            //             fullName = funcDecl->methodName;
            //         }
            //         std::cout << "    " << (funcDecl->isLocal ? "Local" : "Global")
            //                   << " Function: " << fullName << std::endl;
            //     } else if (auto varDecl = dynamic_cast<teal::VariableDeclarationStatement *>(stmt.get())) {
            //         std::cout << "    " << (varDecl->isLocal ? "Local" : "Global")
            //                   << " Variable Declaration: ";
            //         for (size_t i = 0; i < varDecl->names.size(); ++i) {
            //             std::cout << "      " << varDecl->names[i].name;
            //             if (i < varDecl->names.size() - 1) std::cout << ", ";
            //         }
            //         std::cout << std::endl;
            //     }
            // }
        }
    }
    return retval;
}

