#pragma once

#include "Common.hpp"
#include <algorithm>
#include <cctype>
#include <expected>
#include <functional>
#include <string>
#include <unordered_map>
#include <ranges>
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
        std::string_view text;
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
                return std::format("Name({})", text);
            case TokenType::NUMBER:
                return std::format("Number({})", text);
            case TokenType::STRING:
                return std::format("String(\"{}\")", text);
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

        struct UnexpectedEOF {};

        struct TooManyErrors {
            size_t error_count;

            constexpr inline std::string to_string() const { return std::format("Too many errors: {}", error_count); }
        };

        using Error = teal::parser::Error<
            InvalidCharacter, InvalidLongStringDelimiter, UnterminatedLongComment, UnterminatedStringLiteral, UnterminatedLongStringLiteral, Overflow,
            TooManyErrors, UnexpectedEOF>;

        constexpr Lexer(std::string_view source) : max_errors(10), _src(source), _len(source.size()), _pos(0), _line(1), _col(1), _tokens(), _errors()
        {
            _tokens.reserve(_len);
        }
        std::pair<std::vector<Token>, std::vector<Error>> tokenize();
        std::expected<Token, Error> lex();

        const size_t max_errors;

    private:
        std::string_view _src;
        const size_t _len;
        size_t _pos;
        int _line, _col;
        std::vector<Token> _tokens;
        std::vector<Error> _errors;

        [[gnu::const]]
        constexpr inline Error make_error(Error::Kind_t err, std::source_location loc = std::source_location::current()) const
        {
            return Error(err, _line, _col, loc);
        }

        constexpr inline void push_error(Error::Kind_t err) { _errors.push_back(make_error(err)); }

        [[gnu::pure]]
        constexpr inline char peek(int look_ahead = 0) const
        {
            [[likely]]
            if (_pos + look_ahead < _len and _pos + look_ahead >= 0)
                return _src[_pos + look_ahead];
            else
                return '\0';
            // return (pos + look_ahead < length and pos + look_ahead >= 0 ? src[pos + look_ahead] : '\0');
        }

        [[gnu::pure]]
        inline std::optional<std::reference_wrapper<Token>> previous_token(int look_behind = 1)
        {
            int idx = _tokens.size() - look_behind;
            [[unlikely]]
            if (idx < 0 or size_t(idx) > _tokens.size()) return std::nullopt;
            return _tokens[idx];
        }

        char consume()
        {
            [[unlikely]]
            if (_pos >= _len) return '\0';

            char c = _src[_pos++];
            if (c == '\r' or c == '\n') {
                if (c == '\r' and _pos < _len and _src[_pos] == '\n') _pos++;
                _line++;
                _col = 1;
            } else {
                _col++;
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

        bool skip_comment();

        std::expected<Token, Error> read_string();

        Token read_number();

        bool match_end_marker(std::string_view end_marker)
        {
            if (_pos + end_marker.size() > _len) { return false; }
            for (size_t i = 0; i < end_marker.size(); i++) {
                if (_src[_pos + i] != end_marker[i]) { return false; }
            }
            for (size_t i = 0; i < end_marker.size(); i++) { consume(); }
            return true;
        }

        Token read_name();

    public:
        class Tests {
        public:
            void basic_keyword();
            void numbers();
            void strings();
            void long_string();
            void long_comment();
            void mixed_tokens();
            void unterminated_string();
            void invalid_long_string_delimiter();

            void run_all();
        };
    };

} // namespace teal::parser
