
#include "Lexer.hpp"
#include <cassert>
#include <iostream>
#include <source_location>
#include <algorithm>
#include <array>

using namespace std::string_view_literals;
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
            char c = peek();
            if (not c) return std::unexpected(Error(Lexer::UnterminatedLongStringLiteral(), start_ln, start_col $on_debug(, std::source_location::current())));
            if (c == ']' and peek(eq_c+1) == ']') {
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
        if (peek() == 'x' or peek() == 'X') {
            is_hex = true;
            consume(); // Skip the 'x' or 'X'
        }
    }
    
    bool seen_decimal = false;
    while (char c = peek()) {
        if (is_hex) {
            if (std::isxdigit(c) or c == '.') {
                if (c == '.' and seen_decimal)
                    break;
                if (c == '.')
                    seen_decimal = true;
                consume();
            } else if (c == 'p' or c == 'P') {
                consume();
                if (peek() == '+' or peek() == '-')
                    consume();
            } else {
                break;
            }
        } else {
            if (std::isdigit(c) or c == '.') {
                if (c == '.' and seen_decimal)
                    break;
                if (c == '.')
                    seen_decimal = true;
                consume();
            } else if (c == 'e' or c == 'E') {
                consume();
                if (peek() == '+' or peek() == '-')
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
    size_t start = _pos;
    while (is_identifier_character(consume()));
    _pos--;
    auto nm = _src.substr(start, _pos - start);
    _col += nm.size();

    [[likely]]
    if (nm.size() <= 9) {
        switch (bit_str(nm)) {
            case bit_str("nil"):        return Token { TokenType::NIL, nm, start_line, start_col };
            case bit_str("true"):       return Token { TokenType::TRUE, nm, start_line, start_col };
            case bit_str("false"):      return Token { TokenType::FALSE, nm, start_line, start_col };
            case bit_str("end"):        return Token { TokenType::END, nm, start_line, start_col };
            case bit_str("do"):         return Token { TokenType::DO, nm, start_line, start_col };
            case bit_str("if"):         return Token { TokenType::IF, nm, start_line, start_col };
            case bit_str("then"):       return Token { TokenType::THEN, nm, start_line, start_col };
            case bit_str("else"):       return Token { TokenType::ELSE, nm, start_line, start_col };
            case bit_str("elseif"):     return Token { TokenType::ELSEIF, nm, start_line, start_col };
            case bit_str("while"):      return Token { TokenType::WHILE, nm, start_line, start_col };
            case bit_str("repeat"):     return Token { TokenType::REPEAT, nm, start_line, start_col };
            case bit_str("until"):      return Token { TokenType::UNTIL, nm, start_line, start_col };
            case bit_str("for"):        return Token { TokenType::FOR, nm, start_line, start_col };
            case bit_str("in"):         return Token { TokenType::IN, nm, start_line, start_col };
            case bit_str("break"):      return Token { TokenType::BREAK, nm, start_line, start_col };
            case bit_str("goto"):       return Token { TokenType::GOTO, nm, start_line, start_col };
            case bit_str("return"):     return Token { TokenType::RETURN, nm, start_line, start_col };
            case bit_str("local"):      return Token { TokenType::LOCAL, nm, start_line, start_col };
            case bit_str("global"):     return Token { TokenType::GLOBAL, nm, start_line, start_col };
            case bit_str("record"):     return Token { TokenType::RECORD, nm, start_line, start_col };
            case bit_str("enum"):       return Token { TokenType::ENUM, nm, start_line, start_col };
            case bit_str("type"):       return Token { TokenType::TYPE, nm, start_line, start_col };
            case bit_str("where"):      return Token { TokenType::WHERE, nm, start_line, start_col };
            case bit_str("and"):        return Token { TokenType::AND, nm, start_line, start_col };
            case bit_str("or"):         return Token { TokenType::OR, nm, start_line, start_col };
            case bit_str("not"):        return Token { TokenType::NOT, nm, start_line, start_col };
            case bit_str("as"):         return Token { TokenType::AS, nm, start_line, start_col };
            case bit_str("is"):         return Token { TokenType::IS, nm, start_line, start_col };

            //bitstr can only contain 8 chars, so anything after it would make it into an ident
            case bit_str("function"):
                [[unlikely]]
                if (nm.size() > 8)
                    goto name;
                return Token { TokenType::FUNCTION, nm, start_line, start_col };
            case bit_str("macroexp"):
                [[unlikely]]
                if (nm.size() > 8)
                    goto name;
                return Token { TokenType::MACROEXP, nm, start_line, start_col };
            case bit_str("interface"):
                [[unlikely]]
                if (nm.size() > 9)
                    goto name;
                return Token { TokenType::INTERFACE, nm, start_line, start_col };

            name:
            default:                    return Token { TokenType::NAME, nm, start_line, start_col };
        }
    } else {
        //TODO: check for `interfac`
        return Token { TokenType::NAME, nm, start_line, start_col };
    }

    // int start_line = _line;
    // int start_col = _col;
    // size_t start = _pos;
    // while (is_identifier_character(consume()));
    // _pos--;
    // auto nm =_src.substr(start, _pos - start);
    // static const std::unordered_map<std::string_view, TokenType> keywords = {
    //     { "nil",       TokenType::NIL       },
    //     { "true",      TokenType::TRUE      },
    //     { "false",     TokenType::FALSE     },
    //     { "function",  TokenType::FUNCTION  },
    //     { "end",       TokenType::END       },
    //     { "do",        TokenType::DO        },
    //     { "if",        TokenType::IF        },
    //     { "then",      TokenType::THEN      },
    //     { "else",      TokenType::ELSE      },
    //     { "elseif",    TokenType::ELSEIF    },
    //     { "while",     TokenType::WHILE     },
    //     { "repeat",    TokenType::REPEAT    },
    //     { "until",     TokenType::UNTIL     },
    //     { "for",       TokenType::FOR       },
    //     { "in",        TokenType::IN        },
    //     { "break",     TokenType::BREAK     },
    //     { "goto",      TokenType::GOTO      },
    //     { "return",    TokenType::RETURN    },
    //     { "local",     TokenType::LOCAL     },
    //     { "global",    TokenType::GLOBAL    },
    //     { "record",    TokenType::RECORD    },
    //     { "interface", TokenType::INTERFACE },
    //     { "enum",      TokenType::ENUM      },
    //     { "type",      TokenType::TYPE      },
    //     { "where",     TokenType::WHERE     },
    //     { "and",       TokenType::AND       },
    //     { "or",        TokenType::OR        },
    //     { "not",       TokenType::NOT       },
    //     { "as",        TokenType::AS        },
    //     { "is",        TokenType::IS        },
    //     { "macroexp",  TokenType::MACROEXP  }
    // };
    // auto it = keywords.find(nm);
    // if (it != keywords.end()) { return { it->second, nm, start_line, start_col }; }
    // return { TokenType::NAME, nm, start_line, start_col };
}


std::expected<Token, Lexer::Error> Lexer::lex()
{
    skip_whitespace();
    if (_pos >= _len) return std::unexpected(make_error(EndOfFile {}));
    char c = peek();

    int tok_line =_line;
    int tok_col = _col;
    if (c == '\"' or c == '\'' or (c == '[' and (peek(1) == '[' or peek(1) == '=')))
        return read_string();

    if (std::isdigit(c)) return read_number();
    if (is_identifier_character(c))
        return read_name();
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

std::pair<const std::vector<Token> &, const std::vector<Lexer::Error> &> Lexer::tokenize()
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
            if (std::holds_alternative<EndOfFile>(e.kind)) {
                _tokens.push_back(Token { TokenType::END_OF_FILE, "<EOF>", _line, _col });
                break;
            } else _errors.push_back(e);
        }
    }
    return { _tokens, _errors };
}
