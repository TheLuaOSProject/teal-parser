#pragma once

#include <expected>
#include <vector>
#include <string>
#include <cctype>
#include <utility>
#include <unordered_map>

#include "Common.hpp"

namespace teal
{
    enum class TokenType {
        EndOfFile, Name, Number, String,
        K_nil, K_true, K_false,
        K_function, K_end, K_do, K_if, K_then, K_else, K_elseif,
        K_while, K_repeat, K_until, K_for, K_in,
        K_break, K_goto, K_return,
        K_local, /*K_global, K_record, K_interface, K_enum, K_type,*/
        // K_require,
        // K_where,
        K_and, K_or, K_not, /*K_as, K_is,*/
        Op_Assign, Op_Equals, Op_NotEq, Op_Less, Op_LessEq, Op_Greater, Op_GreaterEq,
        Op_Concat, Op_Add, Op_Sub, Op_Mul, Op_Div, Op_FloorDiv, Op_Mod, Op_Pow,
        Op_BitAnd, Op_BitOr, Op_BitXor, Op_ShiftL, Op_ShiftR,
        Op_Len,
        Op_LParen, Op_RParen, Op_LBrace, Op_RBrace, Op_LBracket, Op_RBracket,
        Op_Comma, Op_Semicolon, Op_Colon, Op_DoubleColon, Op_Dot,
        Op_VarArg, Op_Question,

        //Teal keywords, these can be used as identifiers
        K_global, K_record, K_interface, K_enum, K_type, K_where, K_as, K_is
    };

    struct Token {
        TokenType type;
        std::string text;
        int line;
        int col;

        static const Token NULLTOKEN;

        constexpr bool isNullToken() const
        { return this == &NULLTOKEN; }

        static constexpr inline bool typeIsTealKeyword(TokenType type)
        {
            switch (type) {
                case TokenType::K_global...TokenType::K_is: return true;
                default: return false;
            }
        }

        constexpr inline bool isTealKeyword() const
        { return typeIsTealKeyword(type); }

        std::optional<Token> tealToName() const
        {
            const auto mkname = [this](const std::string_view &id) {
                return Token { TokenType::Name, std::string(id), line, col };
            };
            switch (type) {
            #define $check(n) case TokenType::K_##n: return mkname(#n); break
                $check(type);
                $check(as);
                $check(is);
                $check(where);
                $check(global);
                $check(record);
                $check(interface);
                $check(enum);
            #undef $check
            default:
                return std::nullopt;
            }
        }

        constexpr std::string toString() const
        {
            switch (type) {
                case TokenType::EndOfFile: return "EOF";
                case TokenType::Name: return "Name("+text+")";
                case TokenType::Number: return "Number("+text+")";
                case TokenType::String: return "String(\"" + text + "\")";
                case TokenType::K_nil: return "nil";
                case TokenType::K_true: return "true";
                case TokenType::K_false: return "false";
                case TokenType::K_function: return "function";
                case TokenType::K_end: return "end";
                case TokenType::K_do: return "do";
                case TokenType::K_if: return "if";
                case TokenType::K_then: return "then";
                case TokenType::K_else: return "else";
                case TokenType::K_elseif: return "elseif";
                case TokenType::K_while: return "while";
                case TokenType::K_repeat: return "repeat";
                case TokenType::K_until: return "until";
                case TokenType::K_for: return "for";
                case TokenType::K_in: return "in";
                case TokenType::K_break: return "break";
                case TokenType::K_goto: return "goto";
                case TokenType::K_return: return "return";
                case TokenType::K_local: return "local";
                case TokenType::K_global: return "global";
                case TokenType::K_record: return "record";
                case TokenType::K_interface: return "interface";
                case TokenType::K_enum: return "enum";
                case TokenType::K_type: return "type";
                case TokenType::K_where: return "where";
                case TokenType::K_and: return "and";
                case TokenType::K_or: return "or";
                case TokenType::K_not: return "not";
                case TokenType::K_as: return "as";
                case TokenType::K_is: return "is";
                case TokenType::Op_Assign: return "=";
                case TokenType::Op_Equals: return "==";
                case TokenType::Op_NotEq: return "~=";
                case TokenType::Op_Less: return "<";
                case TokenType::Op_LessEq: return "<=";
                case TokenType::Op_Greater: return ">";
                case TokenType::Op_GreaterEq: return ">=";
                case TokenType::Op_Concat: return "..";
                case TokenType::Op_Add: return "+";
                case TokenType::Op_Sub: return "-";
                case TokenType::Op_Mul: return "*";
                case TokenType::Op_Div: return "/";
                case TokenType::Op_FloorDiv: return "//";
                case TokenType::Op_Mod: return "%";
                case TokenType::Op_Pow: return "^";
                case TokenType::Op_BitAnd: return "&";
                case TokenType::Op_BitOr: return "|";
                case TokenType::Op_BitXor: return "~";
                case TokenType::Op_ShiftL: return "<<";
                case TokenType::Op_ShiftR: return ">>";
                case TokenType::Op_Len: return "#";
                case TokenType::Op_LParen: return "(";
                case TokenType::Op_RParen: return ")";
                case TokenType::Op_LBrace: return "{";
                case TokenType::Op_RBrace: return "}";
                case TokenType::Op_LBracket: return "[";
                case TokenType::Op_RBracket: return "]";
                case TokenType::Op_Comma: return ",";
                case TokenType::Op_Semicolon: return ";";
                case TokenType::Op_Colon: return ":";
                case TokenType::Op_DoubleColon: return "::";
                case TokenType::Op_Dot: return ".";
                case TokenType::Op_VarArg: return "...";
                case TokenType::Op_Question: return "?";
                // default: return "Unknown";
            }
        }
    };

    class Lexer {
    public:
        struct InvalidCharacter {
            char character;
        };

        struct UnterminatedLongComment {

        };

        struct UnterminatedStringLiteral {

        };

        struct UnterminatedLongStringLiteral : UnterminatedStringLiteral {

        };

        struct InvalidLongStringDelimiter {
            char delimiter;
        };

        struct Overflow {

        };

        struct TooManyErrors {
            size_t errorCount;
        };

        struct Error : public teal::Error <
            InvalidCharacter,
            InvalidLongStringDelimiter,
            UnterminatedLongComment,
            UnterminatedStringLiteral,
            UnterminatedLongStringLiteral,
            Overflow,
            TooManyErrors
        > {
            int line, column;
        
            constexpr inline std::string toString() const {
                switch (kind.index()) {
                    case 0: return std::format("Invalid character '{}'", std::get<InvalidCharacter>(kind).character);
                    case 1: return "Invalid long string delimiter";
                    case 2: return "Unterminated long ocmment";
                    case 3: return "Unterminated string literal";
                    case 4: return "Unterminated long string literal";
                    case 5: return "Overflow";
                    case 6: return std::format("Too many errors ({})", std::get<TooManyErrors>(kind).errorCount);
                    default: return "Unknown";
                };
            };
        };

        Lexer(const std::string &source)
            : maxErrors(10), _src(source), _length(source.size()), _pos(0), _line(1), _col(1), _tokens(), _errors() {}
        std::pair<std::vector<Token>, std::vector<Error>> tokenize();
        std::expected<Token, Error> lex();

        const size_t maxErrors;

    private:
        std::string _src;
        size_t _length, _pos;
        int _line, _col;
        std::vector<Token> _tokens;
        std::vector<Error> _errors;

        [[gnu::const]]
        constexpr inline Error makeError(Error::Kind_t err) const
        { return { {err}, _line, _col }; }

        constexpr inline void pushError(Error::Kind_t err)
        { _errors.push_back(makeError(err)); }

        [[gnu::pure]]
        inline char peek(int lookAhead = 0) const {
            return (_pos+lookAhead < _length and _pos+lookAhead >= 0 ? _src[_pos+lookAhead] : '\0');
        }

        [[gnu::pure]]
        inline std::optional<std::reference_wrapper<Token>> previousToken(int lookBehind = 1) {
            int idx = _tokens.size() - lookBehind;
            if (idx < 0 or size_t(idx) > _tokens.size()) return std::nullopt;
            return _tokens[idx];
        }
        char consume() {
            if (_pos >= _length) return '\0';
            char c = _src[_pos++];
            if (c == '\r' or c == '\n') {  // handle newlines uniformly
                if (c == '\r' and _pos < _length and _src[_pos] == '\n') _pos++;
                _line++;
                _col = 1;
            } else {
                _col++;
            }
            return c;
        }
        void skipWhitespace() {
            while (true) {
                char c = peek();
                if (c == ' ' or c == '\t' or c == '\r' or c == '\n') {
                    consume();
                } else {
                    break;
                }
            }
        }
        bool skipComment() {
            if (not (peek() == '-' and peek(1) == '-'))
                return false;

            consume(), consume();
            // Long comment check
            if (peek() == '[') {
                consume();
                int eqCount = 0;
                while (peek() == '=') { consume(); eqCount++; }
                if (peek() == '[') {
                    consume();
                    std::string endMarker = "]";
                    endMarker.append(eqCount, '=');
                    endMarker.push_back(']');
                    size_t endIndex = _src.find(endMarker, _pos);
                    if (endIndex == std::string::npos) {
                        pushError(UnterminatedLongComment());
                        _pos = _length;
                    } else {
                        // Skip everything up to end of long comment
                        for (size_t i = _pos; i < endIndex; ++i) {
                            if (_src[i] == '\r' or _src[i] == '\n') {
                                if (_src[i] == '\r' and i+1 < endIndex and _src[i+1] == '\n') i++;
                                _line++;
                                _col = 1;
                            } else {
                                _col++;
                            }
                        }
                        _pos = endIndex + endMarker.size();
                        _col += endMarker.size();
                    }
                    return true;
                }
                // If not a valid "[==[", treat as normal comment after "--["
            }
            // Single-line comment (consume until newline)
            int curLn = _line;
            while (curLn == _line) {
                consume();
            }
            return true;
        }

        Token readString() {
            int startLine = _line;
            int startCol = _col;
            std::string str;
            char startChar = consume();  // read opening delimiter

            // Check if this is a long string literal (multiline)
            if (startChar == '[' and (peek() == '[' or peek() == '=')) {
                int eqCount = 0;
                while (peek() == '=') {
                    consume();
                    eqCount++;
                }
                if (peek() != '[') {
                    // Not a valid long string delimiter, so report an error.
                    pushError(InvalidLongStringDelimiter {});
                    return Token {TokenType::String, "", startLine, startCol};
                }
                // Consume the final '[' that starts the long string content.
                consume();

                // Build the closing delimiter: a ']' followed by the same number of '=' and a final ']'
                std::string endMarker = "]";
                endMarker.append(eqCount, '=');
                endMarker.push_back(']');

                // Look for the end marker in the source
                size_t endIndex = _src.find(endMarker, _pos);
                if (endIndex == std::string::npos) {
                    pushError(UnterminatedLongStringLiteral());
                    _pos = _length;
                    return {TokenType::String, str, startLine, startCol};
                } else {
                    // Consume the content until the end marker, updating line and col counts.
                    for (size_t i = _pos; i < endIndex; ++i) {
                        char c = _src[i];
                        str.push_back(c);
                        if (c == '\r' or c == '\n') {
                            if (c == '\r' and i+1 < endIndex and _src[i+1] == '\n') {
                                i++;  // handle CRLF as a single newline
                            }
                            _line++;
                            _col = 1;
                        } else {
                            _col++;
                        }
                    }
                    _pos = endIndex + endMarker.size();
                    _col += endMarker.size();
                    return {TokenType::String, str, startLine, startCol};
                }
            }

            // Otherwise, treat as a regular (short) string literal (using ' or " as delimiters)
            char delim = startChar;
            bool closed = false;
            while (_pos < _length) {
                char c = consume();
                if (c == '\0') break;
                if (c == delim) {
                    closed = true;
                    break;
                }
                if (c == '\\') {  // handle escape sequences
                    if (_pos < _length) {
                        char next = consume();
                        switch (next) {
                            case 'n':  str.push_back('\n'); break;
                            case 'r':  str.push_back('\r'); break;
                            case 't':  str.push_back('\t'); break;
                            case '\\': str.push_back('\\'); break;
                            case '\"': str.push_back('\"'); break;
                            case '\'': str.push_back('\''); break;
                            default:   str.push_back(next); break;
                        }
                    } else {
                        str.push_back('\\');
                    }
                } else {
                    str.push_back(c);
                }
            }
            if (not closed) {
                // errors.push_back({"Unterminated string literal", startLine, startCol});
                pushError(UnterminatedStringLiteral());
            }
            return {TokenType::String, str, startLine, startCol};
        }


        Token readNumber() {
            int startLine = _line;
            int startCol = _col;
            std::string numStr;
            bool isHex = false;
            if (peek() == '0') {
                numStr.push_back(consume());
                if (peek() == 'x' or peek() == 'X') {
                    isHex = true;
                    numStr.push_back(consume());
                }
            }
            bool seenDecimal = false;
            while (_pos < _length) {
                char c = peek();
                if (isHex) {
                    if (std::isxdigit(c) or c == '.') {
                        if (c == '.' and seenDecimal) break;
                        if (c == '.') seenDecimal = true;
                        numStr.push_back(consume());
                    } else if (c == 'p' or c == 'P') {  // exponent in hex
                        numStr.push_back(consume());
                        if (peek() == '+' or peek() == '-') {
                            numStr.push_back(consume());
                        }
                    } else {
                        break;
                    }
                } else {
                    if (std::isdigit(c) or c == '.') {
                        if (c == '.' and seenDecimal) break;
                        if (c == '.') seenDecimal = true;
                        numStr.push_back(consume());
                    } else if (c == 'e' or c == 'E') {  // decimal exponent
                        numStr.push_back(consume());
                        if (peek() == '+' or peek() == '-') {
                            numStr.push_back(consume());
                        }
                    } else {
                        break;
                    }
                }
            }
            return {TokenType::Number, numStr, startLine, startCol};
        }

        // Utility function to check whether the next characters match the entire endMarker
        bool matchEndMarker(const std::string &endMarker) {
            // Compare substring from src[pos..pos + endMarker.size()-1]
            // without consuming if it doesn't match
            if (_pos + endMarker.size() > _length) {
                return false;
            }
            for (size_t i = 0; i < endMarker.size(); i++) {
                if (_src[_pos + i] != endMarker[i]) {
                    return false;
                }
            }
            // If matched, consume them
            for (size_t i = 0; i < endMarker.size(); i++) {
                consume(); // this updates line/col properly
            }
            return true;
        }
        Token  readName() {
            int startLine = _line;
            int startCol = _col;
            std::string name;
            name.push_back(consume());
            while (std::isalnum(peek()) or peek() == '_') {
                name.push_back(consume());
            }
            static const std::unordered_map<std::string, TokenType> keywords = {
                {"nil", TokenType::K_nil}, {"true", TokenType::K_true}, {"false", TokenType::K_false},
                {"function", TokenType::K_function}, {"end", TokenType::K_end}, {"do", TokenType::K_do}, {"if", TokenType::K_if},
                {"then", TokenType::K_then}, {"else", TokenType::K_else}, {"elseif", TokenType::K_elseif}, {"while", TokenType::K_while},
                {"repeat", TokenType::K_repeat}, {"until", TokenType::K_until}, {"for", TokenType::K_for}, {"in", TokenType::K_in},
                {"break", TokenType::K_break}, {"goto", TokenType::K_goto}, {"return", TokenType::K_return},

                {"local", TokenType::K_local}, {"global", TokenType::K_global}, {"record", TokenType::K_record},
                {"interface", TokenType::K_interface}, {"enum", TokenType::K_enum}, {"type", TokenType::K_type},
                // {"require", TokenType::K_require},
                {"where", TokenType::K_where},
                {"and", TokenType::K_and}, {"or", TokenType::K_or}, {"not", TokenType::K_not},
                {"as", TokenType::K_as}, {"is", TokenType::K_is}
            };
            auto it = keywords.find(name);
            if (it != keywords.end()) {
                //the teal keywords need special handling because they can also be `Name`s
                auto tk = it->second;

                if (Token::typeIsTealKeyword(tk)) {
                    switch (tk) {
                    // case TokenType::K_global...TokenType::K_is:

                    //No clue how I am gonna properly lex this,
                    //`global a = 4` is valid, `global` should be a `TokenType::K_global`
                    //`a.global = 4` is also valid but `global` should be `TokenType::Name`
                    case TokenType::K_global:
                        if (auto prevTk = previousToken()) {

                        } /* else, its the global kw */
                        break;
                    default:
                        __builtin_unreachable();
                    };
                }

                return {it->second, name, startLine, startCol};
            }
            return {TokenType::Name, name, startLine, startCol};
        }

    public:
        class Tests {
        public:
            void basicKeyword();
            void numbers();
            void strings();
            void longString();
            void longComment();
            void mixedTokens();
            void unterminatedString();
            void invalidLongStringDelimiter();

            void runAll();
        };
    };
}
