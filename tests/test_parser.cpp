#include <doctest/doctest.h>
#include <teal-parser/Lexer.hpp>
#include <teal-parser/Parser.hpp>

using namespace teal::parser;

static std::vector<Token> lex_source(const char* src) {
    Lexer lex(src);
    auto [toks, errs] = lex.tokenize();
    REQUIRE(errs.empty());
    return toks;
}

TEST_CASE("parse return statement") {
    Parser p(lex_source("return 123"));
    auto [block, errs] = p.parse();
    REQUIRE(errs.empty());
    REQUIRE(block != nullptr);
    REQUIRE(block->statements.size() == 1);
    auto* ret = dynamic_cast<ast::ReturnStatement*>(block->statements[0].get());
    REQUIRE(ret != nullptr);
    REQUIRE(ret->values.size() == 1);
}

TEST_CASE("parse local variable") {
    Parser p(lex_source("local x = 1"));
    auto [block, errs] = p.parse();
    REQUIRE(errs.empty());
    REQUIRE(block != nullptr);
    REQUIRE(block->statements.size() == 1);
    auto* var = dynamic_cast<ast::VariableDeclarationStatement*>(block->statements[0].get());
    REQUIRE(var != nullptr);
    REQUIRE(var->names.size() == 1);
    REQUIRE(var->names[0].name == "x");
}
