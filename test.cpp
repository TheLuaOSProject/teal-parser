#include <teal-parser/Parser.hpp>

#include <chrono>
#include <print>

constexpr size_t DEFAULT_SIZE = 1024 * 1024;

static const char TL_SRC[] = {
#embed  "tl.tl"

, 0
};

int main()
{
    auto mem = std::pmr::monotonic_buffer_resource(DEFAULT_SIZE);

    auto alloc = std::pmr::polymorphic_allocator(&mem);
    auto lexer = teal::parser::Lexer(TL_SRC, alloc);

    auto start = std::chrono::steady_clock::now();
    auto [tks, errs] = lexer.tokenize();
    auto end = std::chrono::steady_clock::now();

    auto len = end - start;
    std::println("Lexing took {}ms", len);


    if (errs.size() > 0) {
        std::println(stderr, "Lexing errors:");
        for (auto err : errs) {
            std::println("    - {} (tl.tl:{}:{})", err.to_string(), err.line, err.column);
        }
        return 1;
    }

    auto parser = teal::parser::Parser(tks, alloc);
    start = std::chrono::steady_clock::now();
    auto [root, perrs] = parser.parse();
    end = std::chrono::steady_clock::now();

    len = end - start;
    std::println("Parsing took {}ms", len);

    if (perrs.size() > 0) {
        std::println(stderr, "Parser errors:");
        for (auto err : perrs) {
            std::println("    - {}", err.to_string());
        }
        return 1;
    }

    std::println("{} blocks", root->statements.size());
}
