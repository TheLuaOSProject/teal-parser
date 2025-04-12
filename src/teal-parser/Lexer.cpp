
#include "Lexer.hpp"
#include <cassert>
#include <iostream>
#include <source_location>

using namespace teal::parser;

const Token Token::NULL_TOKEN = Token { TokenType::END_OF_FILE, "", -1, -1 };

static std::vector<std::string_view> split(std::string_view i, std::string_view d)
{
    std::vector<std::string_view> ts;
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

bool Lexer::skip_comment()
{
    if (not(peek() == '-' and peek(1) == '-')) return false;
    consume();
    consume();
    if (peek() == '[') {
        consume();
        int eq_count = 0;
        while (peek() == '=') {
            consume();
            eq_count++;
        }
        if (peek() == '[') {
            consume();
            std::string end_marker = "]";
            end_marker.append(eq_count, '=');
            end_marker.push_back(']');
            size_t end_index = _src.find(end_marker, _pos);
            if (end_index == std::string::npos) {
                push_error(UnterminatedLongComment());
                _pos = _len;
            } else {
                for (size_t i = _pos; i < end_index; ++i) {
                    if (_src[i] == '\r' or _src[i] == '\n') {
                        if (_src[i] == '\r' and i + 1 < end_index and _src[i + 1] == '\n') i++;
                        _line++;
                        _col = 1;
                    } else {
                        _col++;
                    }
                }
                _pos = end_index + end_marker.size();
                _col += end_marker.size();
            }
            return true;
        }
    }
    int cur_ln = _line;
    while (cur_ln == _line) { consume(); }
    return true;
}


std::expected<Token, Lexer::Error> Lexer::read_string()
{
    int start_ln = _line, start_col = _col;

    char delim = consume();
    size_t start = _pos;
    if (delim == '[' and (peek() == '[' or peek() == '=')) {
        int eq_c = 0;
        while (peek() == '=') {
            consume();
            eq_c++;
        }

        if (peek() != '[') {
            push_error(InvalidLongStringDelimiter());
            return Token { TokenType::STRING, std::string_view(), start_ln, start_col };
        }
        consume();

        start = _pos;
        while (true) {
            if (not peek()) return std::unexpected(Error(Lexer::UnterminatedLongStringLiteral(), start_ln, start_col, std::source_location::current()));
            if (peek() == ']' and peek(eq_c+1) == ']') {
                consume();
                auto done = std::ranges::all_of(
                    // src | std::views::drop(pos) | std::views::take(eq_c),
                    _src.substr(_pos, eq_c),
                    [](char c) { return c == '='; }
                );
                if (done) break;
            }
            consume();
        }
        auto s = _src | std::views::drop(start) | std::views::take(_pos - start);
        _pos += eq_c + 1;
        return Token { TokenType::STRING, s, start_ln, start_col };
    }

    // bool closed = false;
    // while (_pos < _len) {
    //     char c = consume();
    //     if (not c or c == '\n' or c == '\r') break;
    //     if (c == delim and (peek(-2) != '\\' and peek(-3) != '\\')) {
    //         closed = true;
    //         break;
    //     }
    // }
    bool closed = false;
    while (_pos < _len) {
        char c = peek();

        if (c == '\0' or c == '\n' or c == '\r')
            break;

        if (c == delim) {
            size_t backslash_count = 0;
            for (int i = _pos - 1; i >= 0 and _src[i] == '\\'; --i)
                backslash_count++;

            if (backslash_count % 2 == 0) {
                consume();
                closed = true;
                break;
            }
        }

        consume();
    }

    if (not closed) return std::unexpected(make_error(Lexer::UnterminatedStringLiteral()));

    return Token { TokenType::STRING, _src.substr(start, _pos), start_ln, start_col };
}

Token Lexer::read_number() {
    int start_line = _line;
    int start_col = _col;
    size_t start_pos = _pos; // Record the start position in the source
    bool is_hex = false;
    
    if (peek() == '0') {
        consume(); // Skip the '0'
        if (peek() == 'x' || peek() == 'X') {
            is_hex = true;
            consume(); // Skip the 'x' or 'X'
        }
    }
    
    bool seen_decimal = false;
    while (_pos < _len) {
        char c = peek();
        if (is_hex) {
            if (std::isxdigit(c) || c == '.') {
                if (c == '.' && seen_decimal)
                    break;
                if (c == '.')
                    seen_decimal = true;
                consume();
            } else if (c == 'p' || c == 'P') {
                consume();
                if (peek() == '+' || peek() == '-')
                    consume();
            } else {
                break;
            }
        } else {
            if (std::isdigit(c) || c == '.') {
                if (c == '.' && seen_decimal)
                    break;
                if (c == '.')
                    seen_decimal = true;
                consume();
            } else if (c == 'e' || c == 'E') {
                consume();
                if (peek() == '+' || peek() == '-')
                    consume();
            } else {
                break;
            }
        }
    }
    
    // Create a string_view that refers directly to the source substring
    // std::string_view num_view{_source.data() + start_pos, _pos - start_pos};
    
    return { TokenType::NUMBER, _src.substr(start_pos, _pos - start_pos), start_line, start_col };
}

Token Lexer::read_name()
{
    int start_line = _line;
    int start_col = _col;
    // std::string_view name;
    // name.push_back(consume());
    // while (std::isalnum(peek()) or peek() == '_') { name.push_back(consume()); }
    size_t start = _pos;
    while (std::isalnum(peek()) or peek() == '_')
        consume();
    auto nm = _src.substr(start, _pos - start);
    static const std::unordered_map<std::string_view, TokenType> keywords = {
        { "nil",       TokenType::NIL       },
        { "true",      TokenType::TRUE      },
        { "false",     TokenType::FALSE     },
        { "function",  TokenType::FUNCTION  },
        { "end",       TokenType::END       },
        { "do",        TokenType::DO        },
        { "if",        TokenType::IF        },
        { "then",      TokenType::THEN      },
        { "else",      TokenType::ELSE      },
        { "elseif",    TokenType::ELSEIF    },
        { "while",     TokenType::WHILE     },
        { "repeat",    TokenType::REPEAT    },
        { "until",     TokenType::UNTIL     },
        { "for",       TokenType::FOR       },
        { "in",        TokenType::IN        },
        { "break",     TokenType::BREAK     },
        { "goto",      TokenType::GOTO      },
        { "return",    TokenType::RETURN    },
        { "local",     TokenType::LOCAL     },
        { "global",    TokenType::GLOBAL    },
        { "record",    TokenType::RECORD    },
        { "interface", TokenType::INTERFACE },
        { "enum",      TokenType::ENUM      },
        { "type",      TokenType::TYPE      },
        { "where",     TokenType::WHERE     },
        { "and",       TokenType::AND       },
        { "or",        TokenType::OR        },
        { "not",       TokenType::NOT       },
        { "as",        TokenType::AS        },
        { "is",        TokenType::IS        },
        { "macroexp",  TokenType::MACROEXP  }
    };
    auto it = keywords.find(nm);
    if (it != keywords.end()) return Token { it->second, nm, start_line, start_col };
    return Token { TokenType::NAME, nm, start_line, start_col };
}

std::expected<Token, Lexer::Error> Lexer::lex()
{
    skip_whitespace();
    if (_pos >= _len) return std::unexpected(make_error(Overflow {}));
    skip_whitespace();
    if (_pos >= _len) return std::unexpected(make_error(Overflow {}));
    char c = peek();
    if (_pos >= _len) return std::unexpected(make_error(Overflow {}));
    int tok_line = _line;
    int tok_col = _col;
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
    auto lines = split(_src, "\n");
    while (true) {
        if (std::expected<Token, Error> val = lex()) {
            _tokens.push_back(*val);
        } else {
            if (_errors.size() >= max_errors) {
                _errors.push_back(make_error(TooManyErrors { _errors.size() }));
                break;
            }
            auto e = val.error();
            if (std::holds_alternative<Overflow>(e.kind)) {
                _tokens.push_back(Token { TokenType::END_OF_FILE, "<EOF>", _line, _col });
                break;
            } else _errors.push_back(e);
        }
    }
    return { _tokens, _errors };
}

void Lexer::Tests::basic_keyword()
{
    std::string input = "nil";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens.size() >= 2);
    assert(tokens[0].type == TokenType::NIL);
    assert(tokens[0].text == "nil");
    std::cout << "testBasicKeyword passed.\n";
}

void Lexer::Tests::numbers()
{
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

void Lexer::Tests::strings()
{
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

void Lexer::Tests::long_string()
{
    std::string input = "[[Hello\nWorld]]";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(tokens[0].type == TokenType::STRING);
    assert(tokens[0].text == "Hello\nWorld");
    std::cout << "testLongString passed.\n";
}

void Lexer::Tests::long_comment()
{
    std::string input = "--[==[This is a long comment]==]\n123";
    Lexer lexer(input);
    auto [tokens, errors] = lexer.tokenize();
    assert(errors.empty());
    assert(not tokens.empty());
    assert(tokens[0].type == TokenType::NUMBER);
    assert(tokens[0].text == "123");
    std::cout << "testLongComment passed.\n";
}

void Lexer::Tests::mixed_tokens()
{
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

void Lexer::Tests::unterminated_string()
{

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

void Lexer::Tests::invalid_long_string_delimiter()
{
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
