
#include "Lexer.hpp"
#include <cassert>
#include <iostream>

using namespace teal::parser;

const Token Token::NULL_TOKEN = Token { TokenType::END_OF_FILE, "", -1, -1 };

static Vector<String> split(Allocator alloc, std::string_view i, std::string_view d)
{
    auto ts = Vector<String>(alloc);
    int s = 0;
    size_t e = i.find(d);
    while (e != String::npos)
    {
        ts.push_back(String(i.substr(s, e - s), alloc));
        s = e + 1;
        e = i.find(d, s);
    }
    ts.push_back(String(i.substr(s), alloc));
    return ts;
}

std::expected<Token, Lexer::Error> Lexer::lex()
{
    skip_whitespace();
    if (pos >= length) return std::unexpected(make_error(Overflow {}));
    skip_whitespace();
    if (pos >= length) return std::unexpected(make_error(Overflow {}));
    char c = peek();
    if (pos >= length) return std::unexpected(make_error(Overflow {}));
    int tok_line = line;
    int tok_col = col;
    if (c == '\"' or c == '\'' or (c == '[' and (peek(1) == '[' or peek(1) == '='))) {
        return read_string();
    }
    if (std::isdigit(c)) {
        return read_number();
    }
    if (std::isalpha(c) or c == '_') {
        return read_name();
    }
    switch (c) {
    case '.':
        consume();
        if (peek() == '.') {
            consume();
            if (peek() == '.') {
                consume();
                return Token { TokenType::VAR_ARG, "...", tok_line, tok_col };
            } else {
                return Token { TokenType::CONCAT, "..", tok_line, tok_col };
            }
        } else {
            return Token { TokenType::DOT, ".", tok_line, tok_col };
        }
    case '=':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::EQUALS, "==", tok_line, tok_col };
        } else {
            return Token { TokenType::ASSIGN, "=", tok_line, tok_col };
        }
    case '~':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::NOT_EQ, "~=", tok_line, tok_col };
        } else {
            return Token { TokenType::BIT_XOR, "~", tok_line, tok_col };
        }
    case '<':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::LESS_EQ, "<=", tok_line, tok_col };
        } else if (peek() == '<') {
            consume();
            return Token { TokenType::SHIFT_L, "<<", tok_line, tok_col };
        } else {
            return Token { TokenType::LESS, "<", tok_line, tok_col };
        }
    case '>':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::GREATER_EQ, ">=", tok_line, tok_col };
        } else if (peek() == '>') {
            consume();
            return Token { TokenType::SHIFT_R, ">>", tok_line, tok_col };
        } else {
            return Token { TokenType::GREATER, ">", tok_line, tok_col };
        }
    case ':':
        consume();
        if (peek() == ':') {
            consume();
            return Token { TokenType::DOUBLE_COLON, "::", tok_line, tok_col };
        } else {
            return Token { TokenType::COLON, ":", tok_line, tok_col };
        }
    case '(':
        consume();
        return Token { TokenType::L_PAREN, "(", tok_line, tok_col };
    case ')':
        consume();
        return Token { TokenType::R_PAREN, ")", tok_line, tok_col };
    case '[':
        consume();
        return Token { TokenType::L_BRACKET, "[", tok_line, tok_col };
    case ']':
        consume();
        return Token { TokenType::R_BRACKET, "]", tok_line, tok_col };
    case '{':
        consume();
        return Token { TokenType::L_BRACE, "{", tok_line, tok_col };
    case '}':
        consume();
        return Token { TokenType::R_BRACE, "}", tok_line, tok_col };
    case ',':
        consume();
        return Token { TokenType::COMMA, ",", tok_line, tok_col };
    case ';':
        consume();
        return Token { TokenType::SEMICOLON, ";", tok_line, tok_col };
    case '+':
        consume();
        return Token { TokenType::ADD, "+", tok_line, tok_col };
    case '-':
        if (skip_comment()) {
            return lex();
        } else {
            consume();
            return Token { TokenType::SUB, "-", tok_line, tok_col };
        }
    case '*':
        consume();
        return Token { TokenType::MUL, "*", tok_line, tok_col };
    case '/':
        consume();
        if (peek() == '/') {
            consume();
            return Token { TokenType::FLOOR_DIV, "//", tok_line, tok_col };
        } else {
            return Token { TokenType::DIV, "/", tok_line, tok_col };
        }
    case '%':
        consume();
        return Token { TokenType::MOD, "%", tok_line, tok_col };
    case '^':
        consume();
        return Token { TokenType::POW, "^", tok_line, tok_col };
    case '#':
        consume();
        return Token { TokenType::LENGTH, "#", tok_line, tok_col };
    case '?':
        consume();
        return Token { TokenType::QUESTION, "?", tok_line, tok_col };
    case '|':
        consume();
        return Token { TokenType::BIT_OR, "|", tok_line, tok_col };
    default:
        char invalidTk = c;
        consume();
        return std::unexpected(make_error(InvalidCharacter { invalidTk }));
    }
}

std::pair<Vector<Token>, Vector<Lexer::Error>> Lexer::tokenize() {
    auto lines = split(allocator, src, "\n");
    while (true) {
        if (std::expected<Token, Error> val = lex()) {
            tokens.push_back(*val);
        } else {
            if (errors.size() >= max_errors) {
                errors.push_back(make_error(TooManyErrors { errors.size() }));
                break;
            }
            auto e = val.error();
            if (std::holds_alternative<Overflow>(e.kind)) {
                tokens.push_back(Token { TokenType::END_OF_FILE, "<EOF>", line, col });
                break;
            } else errors.push_back(e);
        }
    }
    return { tokens, errors };
}

void Lexer::Tests::basic_keyword() {
    std::string input = "nil";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens.size() >= 2);
    assert(tokens[0].type == TokenType::NIL);
    assert(tokens[0].text == "nil");
    std::cout << "testBasicKeyword passed.\n";
}

void Lexer::Tests::numbers() {
    std::string input = "123 456.789 0x1aF";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == TokenType::NUMBER);
    assert(tokens[0].text == "123");
    assert(tokens[1].type == TokenType::NUMBER);
    assert(tokens[1].text == "456.789");
    assert(tokens[2].type == TokenType::NUMBER);
    assert(tokens[2].text == "0x1aF");
    std::cout << "testNumbers passed.\n";
}

void Lexer::Tests::strings() {
    std::string input = "'hello' \"world\"";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == TokenType::STRING);
    assert(tokens[0].text == "hello");
    assert(tokens[1].type == TokenType::STRING);
    assert(tokens[1].text == "world");
    std::cout << "testStrings passed.\n";
}

void Lexer::Tests::long_string() {
    std::string input = "[[Hello\nWorld]]";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == TokenType::STRING);
    assert(tokens[0].text == "Hello\nWorld");
    std::cout << "testLongString passed.\n";
}

void Lexer::Tests::long_comment() {
    std::string input = "--[==[This is a long comment]==]\n123";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(not tokens.empty());
    assert(tokens[0].type == TokenType::NUMBER);
    assert(tokens[0].text == "123");
    std::cout << "testLongComment passed.\n";
}

void Lexer::Tests::mixed_tokens() {
    std::string input = "if x == 10 then return x else return 0 end";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == TokenType::IF);
    assert(tokens[1].type == TokenType::NAME and tokens[1].text == "x");
    assert(tokens[2].type == TokenType::EQUALS);
    assert(tokens[3].type == TokenType::NUMBER and tokens[3].text == "10");
    std::cout << "testMixedTokens passed.\n";
}

void Lexer::Tests::unterminated_string() {

#if not defined(NDEBUG)
    std::string input = "\"unterminated string";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(not errors.empty());
    bool found_error = false;
    for (const auto &err : errors) {
        if (std::holds_alternative<UnterminatedStringLiteral>(err.kind)) {
            found_error = true;
            break;
        }
    }
    (void)found_error;
    assert(found_error);
    std::cout << "testUnterminatedString passed.\n";
#endif
}

void Lexer::Tests::invalid_long_string_delimiter() {
#if not defined(NDEBUG)
    std::string input = "[=[Invalid long string";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(not errors.empty());
    bool found_error = false;
    for (const auto &err : errors) {
        if (std::holds_alternative<UnterminatedLongStringLiteral>(err.kind)) {
            found_error = true;
            break;
        }
    }
    assert(found_error);
    std::cout << "testInvalidLongStringDelimiter passed.\n";
#endif
}

void Lexer::Tests::run_all()
{
    basic_keyword();
    numbers();
    strings();
    long_string();
    long_comment();
    mixed_tokens();
    unterminated_string();
    invalid_long_string_delimiter();
}
