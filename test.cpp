#include <teal-parser/Parser.hpp>

#include <chrono>
#include <print>
#include <unistd.h>

static const char TL_SRC[] = {
#embed  "tl.tl"

, 0
};

_LIBCPP_BEGIN_NAMESPACE_STD

bool __is_posix_terminal(FILE *f)
{ return isatty(fileno(f)); }

_LIBCPP_END_NAMESPACE_STD

int main(int argc, char *argv[])
{   std::string src = TL_SRC;
    if (argc > 1) {
        auto f = std::fopen(argv[1], "r");
        if (f == nullptr) {
            std::println(stderr, "Failed to open `{}`", argv[1]);
            return 1;
        }

        std::fseek(f, 0, SEEK_END);

        auto len = std::ftell(f);
        std::fseek(f, 0, SEEK_SET);
        src.resize(len);
        std::fread(src.data(), 1, len, f);
        std::fclose(f);
        std::println("Read {} bytes from {}", len, argv[1]);
    }


    auto lexer = teal::parser::Lexer(src);

    auto start = std::chrono::steady_clock::now();
    auto [tks, errs] = lexer.tokenize();
    auto end = std::chrono::steady_clock::now();

    constexpr auto to_ms = [](std::chrono::steady_clock::duration d) constexpr
    { return std::chrono::duration_cast<std::chrono::milliseconds>(d); };

    auto len = to_ms(end - start);
    std::println("Lexing took {}", len);


    if (errs.size() > 0) {
        std::println(stderr, "Lexing errors:");
        for (auto err : errs) {
            std::println("    - {} (tl.tl:{}:{})", err.to_string(), err.line, err.column);
        }
        return 1;
    }

    // auto parser = teal::parser::Parser(tks, alloc);
    auto parser = teal::parser::Parser(tks);
    start = std::chrono::steady_clock::now();
    auto [root, perrs] = parser.parse();
    end = std::chrono::steady_clock::now();

    len = to_ms(end - start);
    std::println("Parsing took {}", len);

    if (perrs.size() > 0) {
        std::println(stderr, "Parser errors:");
        for (auto err : perrs) {
            std::println("    - {} (tl.tl:{}:{})", err.to_string(), err.line, err.column);
        }
        return 1;
    }

    std::println("{} blocks", root->statements.size());

    start = std::chrono::steady_clock::now();
    auto obj = teal::parser::ast::serialisation::Value::from(root->serialise());
    end = std::chrono::steady_clock::now();
    len = to_ms(end - start);

    std::println("Serialisation took {}", len);

    start = std::chrono::steady_clock::now();
    auto json = obj->to_json();
    end = std::chrono::steady_clock::now();
    len = to_ms(end - start);
    std::println("JSON took {}", len);

    auto f = std::fopen("AST.json", "w+b");
    if (f == nullptr) {
        std::println(stderr, "Failed to open AST.json");
        return 0;;
    }
    std::fwrite(json.data(), 1, json.size(), f);
    std::fclose(f);


    start = std::chrono::steady_clock::now();
    auto lua = obj->to_lua_table();
    end = std::chrono::steady_clock::now();
    len = to_ms(end - start);
    std::println("Lua took {}", len);

    f = std::fopen("AST.lua", "w+b");
    if (f == nullptr) {
        std::println(stderr, "Failed to open AST.lua");
        return 0;;
    }

    std::print(f, "return {}", lua);
    std::fclose(f);
}
