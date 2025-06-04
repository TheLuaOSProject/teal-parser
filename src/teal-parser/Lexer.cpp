
#include "Lexer.hpp"
#include <cassert>
#include <iostream>

using namespace teal::parser;

const Token Token::NULL_TOKEN = Token { TokenType::END_OF_FILE, "", -1, -1 };

static std::vector<std::string> split(const std::string &i, const std::string_view &d)
{
    std::vector<std::string> ts;
    int s = 0;
    size_t e = i.find(d);
    while (e != std::string::npos) {
        ts.push_back(i.substr(s, e - s));
        s = e + 1;
        e = i.find(d, s);
    }
    ts.push_back(i.substr(s));
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
    if (c == '\"' or c == '\'' or (c == '[' and (peek(1) == '[' or peek(1) == '='))) return read_string();

    if (std::isdigit(c)) return read_number();
    if (std::isalpha(c) or c == '_') return read_name();
    switch (c) {
    case '.':
        consume();
        if (peek() == '.') {
            consume();
            if (peek() == '.') {
                consume();
                return Token { TokenType::VAR_ARG, "...", tok_line, tok_col };
            } else return Token { TokenType::CONCAT, "..", tok_line, tok_col };
        } else return Token { TokenType::DOT, ".", tok_line, tok_col };

    case '=':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::EQUALS, "==", tok_line, tok_col };
        } else return Token { TokenType::ASSIGN, "=", tok_line, tok_col };

    case '~':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::NOT_EQ, "~=", tok_line, tok_col };
        } else return Token { TokenType::BIT_XOR, "~", tok_line, tok_col };

    case '<':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::LESS_EQ, "<=", tok_line, tok_col };
        } else if (peek() == '<') {
            consume();
            return Token { TokenType::SHIFT_L, "<<", tok_line, tok_col };
        } else return Token { TokenType::LESS, "<", tok_line, tok_col };

    case '>':
        consume();
        if (peek() == '=') {
            consume();
            return Token { TokenType::GREATER_EQ, ">=", tok_line, tok_col };
        } else if (peek() == '>') {
            consume();
            return Token { TokenType::SHIFT_R, ">>", tok_line, tok_col };
        } else return Token { TokenType::GREATER, ">", tok_line, tok_col };

    case ':':
        consume();
        if (peek() == ':') {
            consume();
            return Token { TokenType::DOUBLE_COLON, "::", tok_line, tok_col };
        } else return Token { TokenType::COLON, ":", tok_line, tok_col };

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
        if (skip_comment()) return lex();
        else {
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
        } else return Token { TokenType::DIV, "/", tok_line, tok_col };

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
        char invalid = c;
        consume();
        return std::unexpected(make_error(InvalidCharacter { invalid }));
    }
}

std::pair<std::vector<Token>, std::vector<Lexer::Error>> Lexer::tokenize()
{
    auto lines = split(src, "\n");
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

