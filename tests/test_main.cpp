#include <teal-parser/Lexer.hpp>
#include <teal-parser/Parser.hpp>
#include <cassert>
#include <print>
#include <vector>
#include <string_view>
#include <algorithm>

using namespace teal::parser;
using namespace teal::parser::ast;

void test_lexer_simple();
void test_parser_simple();
void test_parser_complex();

int main() {
    test_lexer_simple();
    test_parser_simple();
    test_parser_complex();
    std::println("All tests passed");
    return 0;
}

void test_lexer_simple() {
    std::string_view src = R"(
local record Point
    x: integer
    y: integer
end
)";
    Lexer lexer(src);
    auto [tks, errs] = lexer.tokenize();
    assert(errs.empty());
    std::vector<TokenType> expected = {
        TokenType::LOCAL, TokenType::RECORD, TokenType::NAME,
        TokenType::NAME, TokenType::COLON, TokenType::NAME,
        TokenType::NAME, TokenType::COLON, TokenType::NAME,
        TokenType::END, TokenType::END_OF_FILE
    };
    assert(tks.size() >= expected.size());
    for (size_t i = 0; i < expected.size(); ++i) {
        assert(tks[i].type == expected[i]);
    }
}

void test_parser_simple() {
    std::string_view src = R"(
local record Point
    x: integer
    y: integer
end
)";
    Lexer lexer(src);
    auto [tks, lex_errs] = lexer.tokenize();
    assert(lex_errs.empty());
    Parser parser(tks);
    auto [ast_opt, parse_errs] = parser.parse();
    assert(parse_errs.empty());
    assert(ast_opt.has_value());
    const Block &blk = ast_opt.value().get();
    assert(!blk.statements.empty());
}

void test_parser_complex() {
    std::string_view src = R"(
local record Container<T> is { Point }, Serializable where 1
    record Sub
        value: integer
    end
    enum Colors
        "RED"
    end
    interface I
        value: integer
    end
    userdata
    type Alias = integer
    value: T
end
)";
    Lexer lex(src);
    auto [tks, lex_errs] = lex.tokenize();
    assert(lex_errs.empty());
    Parser parser(tks);
    auto [ast_opt, parse_errs] = parser.parse();
    assert(parse_errs.empty());
    assert(ast_opt.has_value());
    const Block &blk = ast_opt.value().get();
    assert(!blk.statements.empty());
}
