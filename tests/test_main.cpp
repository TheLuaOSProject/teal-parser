#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest/doctest.h"
#include <teal-parser/Lexer.hpp>
#include <teal-parser/Parser.hpp>
#include <vector>
#include <string_view>
using namespace teal::parser;
using namespace teal::parser::ast;

TEST_CASE("lexer simple record") {
    std::string_view src = R"(-- comment
local x = 123
local y = 0xFF
local s = "hi"
local l = [[long]]
)";
    Lexer lexer(src);
    auto [tks, errs] = lexer.tokenize();
    REQUIRE(errs.empty());
    std::vector<TokenType> expected = {
        TokenType::LOCAL, TokenType::RECORD, TokenType::NAME,
        TokenType::NAME, TokenType::COLON, TokenType::NAME,
        TokenType::NAME, TokenType::COLON, TokenType::NAME,
        TokenType::END, TokenType::END_OF_FILE};
    REQUIRE(tks.size() >= expected.size());
    for(size_t i = 0; i < expected.size(); ++i) {
        CHECK(tks[i].type == expected[i]);
    }
}

TEST_CASE("lexer numbers strings and comments") {
    std::string_view src = R"(-- comment
local x = 123
local y = 0xFF
local s = "hi"
local l = [[long]]
)";
    Lexer lex(src);
    auto [tks, errs] = lex.tokenize();
    REQUIRE(errs.empty());
    std::vector<TokenType> expected = {
        TokenType::LOCAL, TokenType::NAME, TokenType::ASSIGN, TokenType::NUMBER,
        TokenType::LOCAL, TokenType::NAME, TokenType::ASSIGN, TokenType::NUMBER,
        TokenType::LOCAL, TokenType::NAME, TokenType::ASSIGN, TokenType::STRING,
        TokenType::LOCAL, TokenType::NAME, TokenType::ASSIGN, TokenType::STRING,
        TokenType::END_OF_FILE};
    REQUIRE(tks.size() >= expected.size());
    for(size_t i = 0; i < expected.size(); ++i) {
        CHECK(tks[i].type == expected[i]);
    }
}

TEST_CASE("parser simple record") {
    std::string_view src = R"(
local record Point
    x: integer
    y: integer
end
)";
    Lexer lexer(src);
    auto [tks, lex_errs] = lexer.tokenize();
    REQUIRE(lex_errs.empty());
    Parser parser(tks);
    auto [ast_opt, parse_errs] = parser.parse();
    REQUIRE(parse_errs.empty());
    REQUIRE(ast_opt.has_value());
    const Block &blk = ast_opt.value().get();
    REQUIRE(blk.statements.size() == 1);
    const auto &rec = blk.statements[0].get<RecordDeclarationStatement>();
    CHECK(rec.name == "Point");
    const auto &body = rec.body->entries;
    REQUIRE(body.size() == 2);
    CHECK(body[0].template holds_alternative<RecordBody::Field>());
    CHECK(body[1].template holds_alternative<RecordBody::Field>());
}

TEST_CASE("parser complex record") {
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
    REQUIRE(lex_errs.empty());
    Parser parser(tks);
    auto [ast_opt, parse_errs] = parser.parse();
    REQUIRE(parse_errs.empty());
    REQUIRE(ast_opt.has_value());
    const Block &blk = ast_opt.value().get();
    REQUIRE(blk.statements.size() == 1);
    const auto &rec = blk.statements[0].get<RecordDeclarationStatement>();
    CHECK(rec.name == "Container");
    const auto &entries = rec.body->entries;
    REQUIRE(entries.size() == 5);
    CHECK(entries[0].template holds_alternative<RecordBody::Record>());
    CHECK(entries[1].template holds_alternative<RecordBody::Enum>());
    CHECK(entries[2].template holds_alternative<RecordBody::Interface>());
    CHECK(entries[3].template holds_alternative<RecordBody::Userdata>());
    bool last_ok = entries[4].template holds_alternative<RecordBody::TypeAlias>() ||
                   entries[4].template holds_alternative<RecordBody::Field>();
    CHECK(last_ok);
}

TEST_CASE("parser function with control structures") {
    std::string_view src = R"(
local function foo(a: integer, b: integer): integer
    if a > b then
        return a
    else
        return b
    end
end
)";
    Lexer lex(src);
    auto [tks, lex_errs] = lex.tokenize();
    REQUIRE(lex_errs.empty());
    Parser parser(tks);
    auto [ast_opt, parse_errs] = parser.parse();
    REQUIRE(parse_errs.empty());
    REQUIRE(ast_opt.has_value());
    const Block &blk = ast_opt.value().get();
    REQUIRE(blk.statements.size() == 1);
    const auto &func = blk.statements[0].get<FunctionDeclarationStatement>();
    CHECK(func.name_path.size() == 1);
    CHECK(func.name_path[0] == "foo");
    const auto &params = func.body->parameters;
    REQUIRE(params.size() == 2);
    CHECK(params[0].name == "a");
    CHECK(params[1].name == "b");
    const auto &ret = func.body->return_types;
    REQUIRE(ret.size() == 1);
}
