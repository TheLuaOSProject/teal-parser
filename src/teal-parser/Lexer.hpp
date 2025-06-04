#pragma once

#include "Common.hpp"
#include <cctype>
#include <expected>
#include <functional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace teal::parser
{
    enum class TokenType {
        END_OF_FILE,
        NAME,
        NUMBER,
        STRING,
        NIL,
        TRUE,
        FALSE,
        FUNCTION,
        END,
        DO,
        IF,
        THEN,
        ELSE,
        ELSEIF,
        WHILE,
        REPEAT,
        UNTIL,
        FOR,
        IN,
        BREAK,
        GOTO,
        RETURN,
        LOCAL,

    // TEAL KEYWORDS//
        GLOBAL,
        RECORD,
        INTERFACE,
        ENUM,
        TYPE,
        WHERE,
        AS,
        IS,
        MACROEXP,
    //---//

        AND,
        OR,
        NOT,
        ASSIGN,
        EQUALS,
        NOT_EQ,
        LESS,
        LESS_EQ,
        GREATER,
        GREATER_EQ,
        CONCAT,
        ADD,
        SUB,
        MUL,
        DIV,
        FLOOR_DIV,
        MOD,
        POW,
        BIT_AND,
        BIT_OR,
        BIT_XOR,
        SHIFT_L,
        SHIFT_R,
        LENGTH,
        L_PAREN,
        R_PAREN,
        L_BRACE,
        R_BRACE,
        L_BRACKET,
        R_BRACKET,
        COMMA,
        SEMICOLON,
        COLON,
        DOUBLE_COLON,
        DOT,
        VAR_ARG,
        QUESTION
    };

    struct Token {
        TokenType type;
        std::string text;
        int line;
        int col;

        static const Token NULL_TOKEN;

        constexpr bool is_null_token() const { return this == &NULL_TOKEN; }

        static constexpr inline bool type_is_teal_keyword(TokenType type)
        {
            switch (type) {
            case TokenType::GLOBAL... TokenType::MACROEXP:
                return true;
            default:
                return false;
            }
        }

        constexpr inline bool is_teal_keyword() const { return type_is_teal_keyword(type); }

        std::optional<Token> teal_to_name() const
        {
            const auto mkname = [this](const std::string_view &id) { return Token { TokenType::NAME, std::string(id), line, col }; };
            switch (type) {
            case TokenType::TYPE:
                return mkname("type");
            case TokenType::AS:
                return mkname("as");
            case TokenType::IS:
                return mkname("is");
            case TokenType::WHERE:
                return mkname("where");
            case TokenType::GLOBAL:
                return mkname("global");
            case TokenType::RECORD:
                return mkname("record");
            case TokenType::INTERFACE:
                return mkname("interface");
            case TokenType::ENUM:
                return mkname("enum");
            case TokenType::MACROEXP:
                return mkname("macroexp");
            default:
                return std::nullopt;
            }
        }

        static constexpr std::string type_to_string(TokenType type)
        {
            switch (type) {
            case TokenType::END_OF_FILE:
                return "EOF";
            case TokenType::NAME:
                return "Name";
            case TokenType::NUMBER:
                return "Number";
            case TokenType::STRING:
                return "String";
            case TokenType::NIL:
                return "nil";
            case TokenType::TRUE:
                return "true";
            case TokenType::FALSE:
                return "false";
            case TokenType::FUNCTION:
                return "function";
            case TokenType::END:
                return "end";
            case TokenType::DO:
                return "do";
            case TokenType::IF:
                return "if";
            case TokenType::THEN:
                return "then";
            case TokenType::ELSE:
                return "else";
            case TokenType::ELSEIF:
                return "elseif";
            case TokenType::WHILE:
                return "while";
            case TokenType::REPEAT:
                return "repeat";
            case TokenType::UNTIL:
                return "until";
            case TokenType::FOR:
                return "for";
            case TokenType::IN:
                return "in";
            case TokenType::BREAK:
                return "break";
            case TokenType::GOTO:
                return "goto";
            case TokenType::RETURN:
                return "return";
            case TokenType::LOCAL:
                return "local";
            case TokenType::GLOBAL:
                return "global";
            case TokenType::RECORD:
                return "record";
            case TokenType::INTERFACE:
                return "interface";
            case TokenType::ENUM:
                return "enum";
            case TokenType::TYPE:
                return "type";
            case TokenType::WHERE:
                return "where";
            case TokenType::AS:
                return "as";
            case TokenType::IS:
                return "is";
            case TokenType::MACROEXP:
                return "macroexp";
            case TokenType::AND:
                return "and";
            case TokenType::OR:
                return "or";
            case TokenType::NOT:
                return "not";
            case TokenType::ASSIGN:
                return "=";
            case TokenType::EQUALS:
                return "==";
            case TokenType::NOT_EQ:
                return "~=";
            case TokenType::LESS:
                return "<";
            case TokenType::LESS_EQ:
                return "<=";
            case TokenType::GREATER:
                return ">";
            case TokenType::GREATER_EQ:
                return ">=";
            case TokenType::CONCAT:
                return "..";
            case TokenType::ADD:
                return "+";
            case TokenType::SUB:
                return "-";
            case TokenType::MUL:
                return "*";
            case TokenType::DIV:
                return "/";
            case TokenType::FLOOR_DIV:
                return "//";
            case TokenType::MOD:
                return "%";
            case TokenType::POW:
                return "^";
            case TokenType::BIT_AND:
                return "&";
            case TokenType::BIT_OR:
                return "|";
            case TokenType::BIT_XOR:
                return "~";
            case TokenType::SHIFT_L:
                return "<<";
            case TokenType::SHIFT_R:
                return ">>";
            case TokenType::LENGTH:
                return "#";
            case TokenType::L_PAREN:
                return "(";
            case TokenType::R_PAREN:
                return ")";
            case TokenType::L_BRACE:
                return "{";
            case TokenType::R_BRACE:
                return "}";
            case TokenType::L_BRACKET:
                return "[";
            case TokenType::R_BRACKET:
                return "]";
            case TokenType::COMMA:
                return ",";
            case TokenType::SEMICOLON:
                return ";";
            case TokenType::COLON:
                return ":";
            case TokenType::DOUBLE_COLON:
                return "::";
            case TokenType::DOT:
                return ".";
            case TokenType::VAR_ARG:
                return "...";
            case TokenType::QUESTION:
                return "?";
            default:
                return "Unknown";
            }
        }

        constexpr std::string to_string() const
        {
            switch (type) {
            case TokenType::NAME:
                return "Name(" + text + ")";
            case TokenType::NUMBER:
                return "Number(" + text + ")";
            case TokenType::STRING:
                return "String(\"" + text + "\")";
            default:
                return type_to_string(type);
            }
        }
    };

    class Lexer {
    public:
        struct InvalidCharacter {
            char character;

            constexpr inline std::string to_string() const { return std::format("Invalid character: '{}'", character); }
        };

        struct UnterminatedLongComment {
            constexpr inline std::string to_string() const { return "Unterminated long comment"; }
        };

        struct UnterminatedStringLiteral {
            constexpr inline std::string to_string() const { return "Unterminated string literal"; }
        };

        struct UnterminatedLongStringLiteral : UnterminatedStringLiteral {
            constexpr inline std::string to_string() const { return "Unterminated long string literal"; }
        };

        struct InvalidLongStringDelimiter {
            char delimiter;

            constexpr inline std::string to_string() const { return std::format("Invalid long string delimiter: '{}'", delimiter); }
        };

        struct Overflow {
            constexpr inline std::string to_string() const { return "Lexer overflow"; }
        };

        struct TooManyErrors {
            size_t error_count;

            constexpr inline std::string to_string() const { return std::format("Too many errors: {}", error_count); }
        };

        using Error = teal::parser::Error<
            InvalidCharacter, InvalidLongStringDelimiter, UnterminatedLongComment, UnterminatedStringLiteral, UnterminatedLongStringLiteral, Overflow,
            TooManyErrors>;

        Lexer(const std::string &source) : max_errors(10), src(source), length(source.size()), pos(0), line(1), col(1), tokens(), errors() { }
        std::pair<std::vector<Token>, std::vector<Error>> tokenize();
        std::expected<Token, Error> lex();

        const size_t max_errors;

    private:
        std::string src;
        size_t length, pos;
        int line, col;
        std::vector<Token> tokens;
        std::vector<Error> errors;

        [[gnu::const]]
        constexpr inline Error make_error(Error::Kind_t err, std::source_location loc = std::source_location::current()) const
        {
            return Error(err, line, col, loc);
        }

        constexpr inline void push_error(Error::Kind_t err) { errors.push_back(make_error(err)); }

        [[gnu::pure]]
        inline char peek(int look_ahead = 0) const
        {
            return (pos + look_ahead < length and pos + look_ahead >= 0 ? src[pos + look_ahead] : '\0');
        }

        [[gnu::pure]]
        inline std::optional<std::reference_wrapper<Token>> previous_token(int look_behind = 1)
        {
            int idx = tokens.size() - look_behind;
            if (idx < 0 or size_t(idx) > tokens.size()) return std::nullopt;
            return tokens[idx];
        }

        char consume()
        {
            if (pos >= length) return '\0';
            char c = src[pos++];
            if (c == '\r' or c == '\n') {
                if (c == '\r' and pos < length and src[pos] == '\n') pos++;
                line++;
                col = 1;
            } else {
                col++;
            }
            return c;
        }

        void skip_whitespace()
        {
            while (true) {
                char c = peek();
                if (c == ' ' or c == '\t' or c == '\r' or c == '\n') {
                    consume();
                } else {
                    break;
                }
            }
        }

        bool skip_comment()
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
                    size_t end_index = src.find(end_marker, pos);
                    if (end_index == std::string::npos) {
                        push_error(UnterminatedLongComment());
                        pos = length;
                    } else {
                        for (size_t i = pos; i < end_index; ++i) {
                            if (src[i] == '\r' or src[i] == '\n') {
                                if (src[i] == '\r' and i + 1 < end_index and src[i + 1] == '\n') i++;
                                line++;
                                col = 1;
                            } else {
                                col++;
                            }
                        }
                        pos = end_index + end_marker.size();
                        col += end_marker.size();
                    }
                    return true;
                }
            }
            int cur_ln = line;
            while (cur_ln == line) { consume(); }
            return true;
        }

        Token read_string()
        {
            int start_line = line;
            int start_col = col;
            std::string str;
            char start_char = consume();
            if (start_char == '[' and (peek() == '[' or peek() == '=')) {
                int eq_count = 0;
                while (peek() == '=') {
                    consume();
                    eq_count++;
                }
                if (peek() != '[') {
                    push_error(InvalidLongStringDelimiter {});
                    return Token { TokenType::STRING, "", start_line, start_col };
                }
                consume();
                std::string end_marker = "]";
                end_marker.append(eq_count, '=');
                end_marker.push_back(']');
                size_t end_index = src.find(end_marker, pos);
                if (end_index == std::string::npos) {
                    push_error(UnterminatedLongStringLiteral());
                    pos = length;
                    return { TokenType::STRING, str, start_line, start_col };
                } else {
                    for (size_t i = pos; i < end_index; ++i) {
                        char c = src[i];
                        str.push_back(c);
                        if (c == '\r' or c == '\n') {
                            if (c == '\r' and i + 1 < end_index and src[i + 1] == '\n') { i++; }
                            line++;
                            col = 1;
                        } else {
                            col++;
                        }
                    }
                    pos = end_index + end_marker.size();
                    col += end_marker.size();
                    return { TokenType::STRING, str, start_line, start_col };
                }
            }
            char delim = start_char;
            bool closed = false;
            while (pos < length) {
                char c = consume();
                if (c == '\0') break;
                if (c == delim) {
                    closed = true;
                    break;
                }
                if (c == '\\') {
                    if (pos < length) {
                        char next = consume();
                        switch (next) {
                        case 'n':
                            str.push_back('\n');
                            break;
                        case 'r':
                            str.push_back('\r');
                            break;
                        case 't':
                            str.push_back('\t');
                            break;
                        case '\\':
                            str.push_back('\\');
                            break;
                        case '\"':
                            str.push_back('\"');
                            break;
                        case '\'':
                            str.push_back('\'');
                            break;
                        default:
                            str.push_back(next);
                            break;
                        }
                    } else {
                        str.push_back('\\');
                    }
                } else {
                    str.push_back(c);
                }
            }
            if (not closed) { push_error(UnterminatedStringLiteral()); }
            return { TokenType::STRING, str, start_line, start_col };
        }

        Token read_number()
        {
            int start_line = line;
            int start_col = col;
            std::string num_str;
            bool is_hex = false;
            if (peek() == '0') {
                num_str.push_back(consume());
                if (peek() == 'x' or peek() == 'X') {
                    is_hex = true;
                    num_str.push_back(consume());
                }
            }
            bool seen_decimal = false;
            while (pos < length) {
                char c = peek();
                if (is_hex) {
                    if (std::isxdigit(c) or c == '.') {
                        if (c == '.' and seen_decimal) break;
                        if (c == '.') seen_decimal = true;
                        num_str.push_back(consume());
                    } else if (c == 'p' or c == 'P') {
                        num_str.push_back(consume());
                        if (peek() == '+' or peek() == '-') { num_str.push_back(consume()); }
                    } else {
                        break;
                    }
                } else {
                    if (std::isdigit(c) or c == '.') {
                        if (c == '.' and seen_decimal) break;
                        if (c == '.') seen_decimal = true;
                        num_str.push_back(consume());
                    } else if (c == 'e' or c == 'E') {
                        num_str.push_back(consume());
                        if (peek() == '+' or peek() == '-') { num_str.push_back(consume()); }
                    } else {
                        break;
                    }
                }
            }
            return { TokenType::NUMBER, num_str, start_line, start_col };
        }

        bool match_end_marker(const std::string &end_marker)
        {
            if (pos + end_marker.size() > length) { return false; }
            for (size_t i = 0; i < end_marker.size(); i++) {
                if (src[pos + i] != end_marker[i]) { return false; }
            }
            for (size_t i = 0; i < end_marker.size(); i++) { consume(); }
            return true;
        }

        Token read_name()
        {
            int start_line = line;
            int start_col = col;
            std::string name;
            name.push_back(consume());
            while (std::isalnum(peek()) or peek() == '_') { name.push_back(consume()); }
            static const std::unordered_map<std::string, TokenType> keywords = {
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
            auto it = keywords.find(name);
            if (it != keywords.end()) { return { it->second, name, start_line, start_col }; }
            return { TokenType::NAME, name, start_line, start_col };
        }

    };

} // namespace teal::parser
