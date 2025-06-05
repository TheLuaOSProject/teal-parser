#include <doctest/doctest.h>
#include <teal-parser/Lexer.hpp>
#include <teal-parser/Parser.hpp>
#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <vector>
#include <filesystem> // For std::filesystem

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

TEST_CASE("Parse tl.tl") {
    std::ifstream t_file("/app/tl.tl"); // Use the confirmed absolute path
    REQUIRE_MESSAGE(t_file.is_open(), "Failed to open /app/tl.tl. Check if file exists at the absolute path.");

    std::stringstream buffer;
    buffer << t_file.rdbuf();
    std::string tl_content = buffer.str();
    REQUIRE_FALSE(tl_content.empty());

    Lexer lex(tl_content.c_str());
    auto [tokens, lex_errs] = lex.tokenize();

    for (const auto& err : lex_errs) {
        std::cout << "Lexer Error: " << err.to_string() << std::endl;
    }
    REQUIRE(lex_errs.empty());

    Parser p(tokens);
    auto [block, parse_errs] = p.parse();

    if (!parse_errs.empty()) {
        std::cout << "Parser Errors found in tl.tl:" << std::endl;
        for (const auto& err : parse_errs) {
            std::cout << err.to_string() << std::endl;
        }
    }
    // CHECK instead of REQUIRE to see all errors if any
    CHECK(parse_errs.empty());
    // We might not get a full block if parsing fails early,
    // but if there are no errors, we should have a block.
    if (parse_errs.empty()) {
        REQUIRE(block != nullptr);
    }
}
