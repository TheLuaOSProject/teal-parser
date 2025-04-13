#include <teal-parser/Parser.hpp>
// #include <teal-parser/TypeChecker.hpp>

#include <chrono>
#include <print>
#include <unistd.h>

static const char TL_SRC[] = {
#embed "tl.tl"

    , 0
};

_LIBCPP_BEGIN_NAMESPACE_STD

bool __is_posix_terminal(FILE *f) { return isatty(fileno(f)); }

_LIBCPP_END_NAMESPACE_STD

int main(int argc, char *argv[])
{
    std::string src = TL_SRC;
    std::string_view filename = "tl.tl";
    if (argc > 1) {
        auto f = std::fopen(argv[1], "r");
        if (f == nullptr) {
            std::println(stderr, "Failed to open `{}`", argv[1]);
            return 1;
        }
        filename = argv[1];

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

    constexpr auto to_ms = [](std::chrono::steady_clock::duration d) constexpr { return std::chrono::duration_cast<std::chrono::milliseconds>(d); };

    auto len = to_ms(end - start);
    std::println("Lexing took {}", len);
    std::println("{} tokens", tks.size());

    if (errs.size() > 0) {
        std::println(stderr, "Lexing errors:");
        for (auto err : errs) {
        #ifdef NDEBUG
            std::println("    - {} ({}:{}:{})", err.to_string(), filename, err.line, err.column);
        #else
            std::println("    - [{}:{}] {} ({}:{}:{})", err.location.file_name(), err.location.line(), err.to_string(), filename, err.line, err.column);
        #endif
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
        #ifdef NDEBUG
            std::println("    - {} ({}:{}:{})", err.to_string(), filename, err.line, err.column);
        #else
            std::println("    - [{}:{}] {} ({}:{}:{})", err.location.file_name(), err.location.line(), err.to_string(), filename, err.line, err.column);
        #endif
        }
        return 1;
    }

    std::println("{} blocks", root->statements.size());

    {return 0;}

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
        return 0;
    }
    std::fwrite(json.data(), 1, json.size(), f);
    std::fclose(f);

    // auto tycheck = teal::parser::typecheck::TypeChecker();
    // start = std::chrono::steady_clock::now();
    // tycheck.check(root.get());
    // end = std::chrono::steady_clock::now();
    // len = to_ms(end - start);
    // std::println("Type checking took {}", len);

    // auto terrs = tycheck.get_error_reporter().get_errors();
    // if (terrs.size() > 0) {
    //     std::println(stderr, "Type checking errors:");
    //     for (auto err : terrs) { std::println("    - {} ({}:{}:{} - {}:{})", err.message, filename, err.line, err.column, err.occured_at.file_name(), err.occured_at.line()); }
    //     return 1;
    // }
    // std::println("No type errors");

    // start = std::chrono::steady_clock::now();
    // auto lua = obj->to_lua_table();
    // end = std::chrono::steady_clock::now();
    // len = to_ms(end - start);
    // std::println("Lua took {}", len);

    // f = std::fopen("AST.lua", "w+b");
    // if (f == nullptr) {
    //     std::println(stderr, "Failed to open AST.lua");
    //     return 0;;
    // }

    // std::print(f, "return {}", lua);
    // std::fclose(f);
}
