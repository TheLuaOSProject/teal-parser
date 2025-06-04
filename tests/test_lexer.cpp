#include <doctest/doctest.h>
#include <teal-parser/Lexer.hpp>

using namespace teal::parser;

TEST_CASE("lex keywords") {
    Lexer lex("nil");
    auto [toks, errs] = lex.tokenize();
    REQUIRE(errs.empty());
    REQUIRE(toks.size() >= 2);
    REQUIRE(toks[0].type == TokenType::NIL);
    REQUIRE(toks.back().type == TokenType::END_OF_FILE);
}

TEST_CASE("lex numbers") {
    Lexer lex("123 456.789 0x1aF");
    auto [toks, errs] = lex.tokenize();
    REQUIRE(errs.empty());
    REQUIRE(toks[0].type == TokenType::NUMBER);
    REQUIRE(toks[0].text == "123");
    REQUIRE(toks[1].type == TokenType::NUMBER);
    REQUIRE(toks[1].text == "456.789");
    REQUIRE(toks[2].type == TokenType::NUMBER);
    REQUIRE(toks[2].text == "0x1aF");
}

TEST_CASE("lex strings") {
    Lexer lex("'hello' \"world\"");
    auto [toks, errs] = lex.tokenize();
    REQUIRE(errs.empty());
    REQUIRE(toks[0].type == TokenType::STRING);
    REQUIRE(toks[0].text == "hello");
    REQUIRE(toks[1].type == TokenType::STRING);
    REQUIRE(toks[1].text == "world");
}

TEST_CASE("lex unterminated string") {
    Lexer lex("\"unterminated");
    auto [toks, errs] = lex.tokenize();
    REQUIRE(!errs.empty());
    bool found = false;
    for (auto &e : errs) {
        if (std::holds_alternative<Lexer::UnterminatedStringLiteral>(e.kind)) {
            found = true;
            break;
        }
    }
    REQUIRE(found);
}
