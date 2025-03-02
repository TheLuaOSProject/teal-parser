#include "Lexer.hpp"

#include <cassert>
#include <iostream>

using namespace teal;

const Token Token::NULLTOKEN = Token(TokenType::EndOfFile, "", -1, -1);


[[gnu::used]]
static std::vector<std::string> split(const std::string &i, const std::string_view &d)
{
    std::vector<std::string> ts;

    int s = 0;
    size_t e = i.find(d);

    while (e != std::string::npos)
    {
        ts.push_back(i.substr(s, e - s));
        s = e + 1;
        e = i.find(d, s);
    }

    ts.push_back(i.substr(s));
    return ts;
}

std::expected<Token, Lexer::Error> Lexer::lex()
{
    skipWhitespace();
    if (pos >= length) return std::unexpected(makeError(Overflow()));
    skipComment();
    skipWhitespace();
    if (pos >= length) return std::unexpected(makeError(Overflow()));

    char c = peekChar();
    if (pos >= length) return std::unexpected(makeError(Overflow()));
    int tokLine = line;
    int tokCol = col;


    if (c == '\"' or c == '\'' or (c == '[' and (peekChar(1) == '[' or peekChar(1) == '='))) {
        return readString();
        //
    }
    if (std::isdigit(c)) {
        return readNumber();
        //
    }
    if (std::isalpha(c) or c == '_') {
        return readName();
        //
    }
    // Punctuators and operators
    switch (c) {
        case '.':
            getChar();
            if (peekChar() == '.') {
                getChar();
                if (peekChar() == '.') {
                    getChar();
                    return Token {TokenType::Op_VarArg, "...", tokLine, tokCol};
                } else {
                    return Token {TokenType::Op_Concat, "..", tokLine, tokCol};
                }
            } else {
                return Token {TokenType::Op_Dot, ".", tokLine, tokCol};
            }

        case '=':
            getChar();
            if (peekChar() == '=') {
                getChar();
                return Token {TokenType::Op_Equals, "==", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_Assign, "=", tokLine, tokCol};
            }

        case '~':
            getChar();
            if (peekChar() == '=') {
                getChar();
                return Token {TokenType::Op_NotEq, "~=", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_BitXor, "~", tokLine, tokCol};
            }

        case '<':
            getChar();
            if (peekChar() == '=') {
                getChar();
                return Token {TokenType::Op_LessEq, "<=", tokLine, tokCol};
            } else if (peekChar() == '<') {
                getChar();
                return Token {TokenType::Op_ShiftL, "<<", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_Less, "<", tokLine, tokCol};
            }

        case '>':
            getChar();
            if (peekChar() == '=') {
                getChar();
                return Token {TokenType::Op_GreaterEq, ">=", tokLine, tokCol};
            } else if (peekChar() == '>') {
                getChar();
                return Token {TokenType::Op_ShiftR, ">>", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_Greater, ">", tokLine, tokCol};
            }

        case ':':
            getChar();
            if (peekChar() == ':') {
                getChar();
                return Token {TokenType::Op_DoubleColon, "::", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_Colon, ":", tokLine, tokCol};
            }

        case '(':
            getChar();
            return Token {TokenType::Op_LParen, "(", tokLine, tokCol};

        case ')':
            getChar();
            return Token {TokenType::Op_RParen, ")", tokLine, tokCol};

        case '[':
            getChar();
            return Token {TokenType::Op_LBracket, "[", tokLine, tokCol};

        case ']':
            getChar();
            return Token {TokenType::Op_RBracket, "]", tokLine, tokCol};

        case '{':
            getChar();
            return Token {TokenType::Op_LBrace, "{", tokLine, tokCol};

        case '}':
            getChar();
            return Token {TokenType::Op_RBrace, "}", tokLine, tokCol};

        case ',':
            getChar();
            return Token {TokenType::Op_Comma, ",", tokLine, tokCol};

        case ';':
            getChar();
            return Token {TokenType::Op_Semicolon, ";", tokLine, tokCol};

        case '+':
            getChar();
            return Token {TokenType::Op_Add, "+", tokLine, tokCol};

        case '-':
            getChar();
            return Token {TokenType::Op_Sub, "-", tokLine, tokCol};

        case '*':
            getChar();
            return Token {TokenType::Op_Mul, "*", tokLine, tokCol};

        case '/':
            getChar();
            if (peekChar() == '/') {
                getChar();
                return Token {TokenType::Op_FloorDiv, "//", tokLine, tokCol};
            } else {
                return Token {TokenType::Op_Div, "/", tokLine, tokCol};
            }

        case '%':
            getChar();
            return Token {TokenType::Op_Mod, "%", tokLine, tokCol};

        case '^':
            getChar();
            return Token {TokenType::Op_Pow, "^", tokLine, tokCol};

        case '#':
            getChar();
            return Token {TokenType::Op_Len, "#", tokLine, tokCol};

        case '?':
            getChar();
            return Token {TokenType::Op_Question, "?", tokLine, tokCol};

        default: {
            // Unknown character
            // std::string msg = std::string("Unexpected character '") + c + "' (" + std::to_string((int)c) + ")";
            // errors.push_back({msg, tokLine, tokCol});
            return std::unexpected(makeError(InvalidCharacter(c)));
            getChar();
        }
    }

}

std::pair<std::vector<Token>, std::vector<Lexer::Error>> Lexer::tokenize() {
    auto lines = split(src, "\n");
    while (true) {
        if (std::expected<Token, Error> val = lex()) {
            tokens.push_back(*val);
        } else {
            auto e = val.error();
            if (std::holds_alternative<Overflow>(e.kind)) {
                tokens.push_back(Token { TokenType::EndOfFile, "<EOF>", line, col });
                break;
            } else errors.push_back(e);
        }
    }
    // return {TokenType::EOF_, "<eof>", line, col};
    return {tokens, errors};
}

// Helper function to print tokens for debugging (if needed)
void printTokens(const std::vector<teal::Token>& tokens) {
    for (const auto& token : tokens) {
        std::cout << "Line " << token.line << ", Col " << token.col
                  << ": " << static_cast<int>(token.type) << " -> " << token.text << "\n";
    }
}

void Lexer::Tests::basicKeyword() {
    std::string input = "nil";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    // Assuming the lexer emits an EOF token at the end.
    assert(tokens.size() >= 2);
    assert(tokens[0].type == teal::TokenType::K_nil);
    assert(tokens[0].text == "nil");
    std::cout << "testBasicKeyword passed.\n";
}

void Lexer::Tests::numbers() {
    std::string input = "123 456.789 0x1aF";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    // Expect three number tokens followed by an EOF token.
    assert(tokens[0].type == teal::TokenType::Number);
    assert(tokens[0].text == "123");
    assert(tokens[1].type == teal::TokenType::Number);
    assert(tokens[1].text == "456.789");
    assert(tokens[2].type == teal::TokenType::Number);
    assert(tokens[2].text == "0x1aF");
    std::cout << "testNumbers passed.\n";
}

void Lexer::Tests::strings() {
    std::string input = "'hello' \"world\"";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    // Two string tokens: one for 'hello' and one for "world"
    assert(tokens[0].type == teal::TokenType::String);
    assert(tokens[0].text == "hello");
    assert(tokens[1].type == teal::TokenType::String);
    assert(tokens[1].text == "world");
    std::cout << "testStrings passed.\n";
}

void Lexer::Tests::longString() {
    // A long string literal should capture newlines correctly.
    std::string input = "[[Hello\nWorld]]";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == teal::TokenType::String);
    // The string token should contain "Hello\nWorld" exactly.
    assert(tokens[0].text == "Hello\nWorld");
    std::cout << "testLongString passed.\n";
}

void Lexer::Tests::longComment() {
    // A long comment should be skipped.
    std::string input = "--[==[This is a long comment]==]\n123";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    // Since the comment is skipped, the first token should be the number "123"
    assert(!tokens.empty());
    assert(tokens[0].type == teal::TokenType::Number);
    assert(tokens[0].text == "123");
    std::cout << "testLongComment passed.\n";
}

void Lexer::Tests::mixedTokens() {
    // A simple control flow snippet to check keywords, names, operators, and numbers.
    std::string input = "if x == 10 then return x else return 0 end";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    // Expected token sequence:
    // [K_if, Name("x"), Op_Equals, Number("10"), K_then, K_return,
    //  Name("x"), K_else, K_return, Number("0"), K_end, EOF]
    assert(tokens[0].type == teal::TokenType::K_if);
    assert(tokens[1].type == teal::TokenType::Name && tokens[1].text == "x");
    assert(tokens[2].type == teal::TokenType::Op_Equals);
    assert(tokens[3].type == teal::TokenType::Number && tokens[3].text == "10");
    // Further checks can be added similarly.
    std::cout << "testMixedTokens passed.\n";
}

void Lexer::Tests::unterminatedString() {
    // This input should trigger an error because the string is not terminated.
    std::string input = "\"unterminated string";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    // We expect at least one parse error.
    assert(!errors.empty());
    bool foundError = false;
    for (const auto &err : errors) {
        // if (err.message.find("Unterminated string literal") != std::string::npos) {
        if (std::holds_alternative<UnterminatedStringLiteral>(err.kind)) {
            foundError = true;
            break;
        }
    }
    assert(foundError);
    std::cout << "testUnterminatedString passed.\n";
}

void Lexer::Tests::invalidLongStringDelimiter() {
    // An invalid long string delimiter should produce an error.
    std::string input = "[=[Invalid long string";
    teal::Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    // We expect a parse error indicating an invalid long string delimiter.
    assert(!errors.empty());
    bool foundError = false;
    for (const auto& err : errors) {
        if (std::holds_alternative<UnterminatedLongStringLiteral>(err.kind)) {
            foundError = true;
            break;
        }
    }
    assert(foundError);
    std::cout << "testInvalidLongStringDelimiter passed.\n";
}

void Lexer::Tests::runAll()
{
    basicKeyword();
    numbers();
    strings();
    longString();
    longComment();
    mixedTokens();
    unterminatedString();
    invalidLongStringDelimiter();
}
