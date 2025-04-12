#pragma once

#include "AST.hpp"
#include <source_location>

namespace teal::parser
{

    class Parser {
    public:
        struct Expected {};
        struct ExpectedToken : Expected {
            TokenType expected_type;
            Token got;

            explicit ExpectedToken(TokenType expected_type, const Token &got):
                expected_type(expected_type), got(got)
            { }

            constexpr inline std::string to_string() const
            { return std::format("Expected {}, got {}", Token::type_to_string(expected_type), got.to_string()); }
        };

        struct ExpectedExpression : Expected {
            Token after;

            explicit ExpectedExpression(const Token &after):
                after(after)
            { }

            constexpr inline std::string to_string() const
            { return std::format("Expected expression after {}", after.to_string()); }
        };

        struct ExpectedArguments : Expected {
            constexpr inline std::string to_string() const
            { return "Expected arguments after method call"; }
        };

        struct Unexpected {};

        struct UnexpectedToken : Unexpected {
            Token got;

            explicit UnexpectedToken(const Token &got):
                got(got)
            { }

            constexpr inline std::string to_string() const
            { return std::format("Unexpected token: {}", got.to_string()); }
        };

        struct UnexpectedExpression : Unexpected {
            Token got;

            explicit UnexpectedExpression(const Token &got):
                got(got)
            { }

            constexpr inline std::string to_string() const
            { return std::format("Unexpected expression: {}", got.to_string()); }
        };

        struct UnexpectedFunctionCall : Unexpected {
            constexpr inline std::string to_string() const
            { return "Unexpected function call"; }
        };

        // struct Expect

        struct CannotAssignToFunctionCall {
            constexpr inline std::string to_string() const
            { return "Cannot assign to function call"; }
        };

        struct TooManyErrors {
            size_t error_count;

            constexpr inline std::string to_string() const
            { return std::format("Too many errors: {}", error_count); }
        };

        struct SemanticError {};

        struct MacroexpMember : SemanticError {
            constexpr inline std::string to_string() const
            { return "Macroexps cabbot be members"; }
        };

        struct GlobalVariableMustBeTyped : SemanticError {
            std::string_view global;

            explicit GlobalVariableMustBeTyped(std::string_view glname):
                global(glname)
            { }

            constexpr inline std::string to_string() const
            { return std::format("Global variable '{}' must be typed or have initial value", global); }
        };

        using Error = teal::parser::Error<
            ExpectedToken,
            ExpectedExpression,
            ExpectedArguments,
            UnexpectedExpression,
            UnexpectedToken,
            UnexpectedFunctionCall,
            CannotAssignToFunctionCall,
            TooManyErrors,
            MacroexpMember,
            GlobalVariableMustBeTyped
        >;

        Parser(std::vector<Token> toks) : max_errors(10), _tokens(std::move(toks)), _pos(0) { }
        std::tuple<std::unique_ptr<ast::Block>, std::vector<Error>> parse()
        {
            try {
                return { parse_chunk(), _errors };
            } catch (const StopParsingException &) {
                push_error(TooManyErrors { _errors.size() }, true);
                return { nullptr, _errors };
            }
        }

        const size_t max_errors;

    private:
        class StopParsingException : public std::exception { };
        class UnexpectedEOFException : public StopParsingException { };

        std::vector<Token> _tokens;
        size_t _pos;
        std::vector<Error> _errors;

        template <typename T>
            requires std::is_base_of_v<ast::ASTNode, T>
        constexpr inline std::unique_ptr<T> make_node(const Token &tk, auto &&...args)
        {
            return std::make_unique<T>(tk, std::forward<decltype(args)>(args)...);
        }

        template <typename T>
            requires std::is_base_of_v<ast::ASTNode, T>
        constexpr inline std::unique_ptr<T> make_node(auto &&...args)
        {
            return std::make_unique<T>(peek_token(), std::forward<decltype(args)>(args)...);
        }

        const Token &peek_token(int forward = 0) const
        {
            [[unlikely]]
            if (_pos + forward > _tokens.size())
                throw UnexpectedEOFException();
            return _tokens[_pos + forward];
        }
        bool is_at_end() const { return peek_token().type == TokenType::END_OF_FILE; }
        bool check(TokenType t) const
        {
            return (t == TokenType::NAME
                    and (Token::type_is_teal_keyword(peek_token().type) or peek_token().text == "macroexp"))
                or peek_token().type == t;
        }
        bool match(TokenType t)
        {
            if (check(t)) {
                _pos++;
                return true;
            }
            return false;
        }
        bool match_any(std::initializer_list<TokenType> types)
        {
            if (check(TokenType::END_OF_FILE)) return false;
            for (TokenType t : types) {
                if (check(t)) {
                    _pos++;
                    return true;
                }
            }
            return false;
        }
        constexpr inline void push_error(const Error::Kind_t &err, bool nothrow = false, std::source_location loc = std::source_location::current())
        {
            _errors.push_back(Error(err, peek_token().line, peek_token().col, loc));
            if (_errors.size() >= max_errors and not nothrow) throw StopParsingException {};
        }

        std::optional<Token> consume(TokenType t, const Error::Kind_t &err_msg, bool nothrow = false, std::source_location loc = std::source_location::current())
        {
            Token tok = peek_token();
            if (check(t)) {
                _pos++;
                return tok;
            } else {
                push_error(err_msg, nothrow, loc);
                return std::nullopt;
            }
        }

        std::optional<Token> consume(TokenType t, bool nothrow = false, std::source_location loc = std::source_location::current())
        {
            Token tok = peek_token();
            if (check(t)) {
                _pos++;
                return tok;
            } else {
                push_error(ExpectedToken(t, tok), nothrow, loc);
                return std::nullopt;
            }
        }
        
        
        void skip_to_next_statement()
        {
            while (not is_at_end()) {
                TokenType t = peek_token().type;
                if (t == TokenType::SEMICOLON or t == TokenType::RETURN or t == TokenType::BREAK
                    or t == TokenType::GLOBAL or t == TokenType::LOCAL or t == TokenType::IF or t == TokenType::WHILE
                    or t == TokenType::FOR or t == TokenType::FUNCTION or t == TokenType::REPEAT or t == TokenType::END
                    or t == TokenType::UNTIL or t == TokenType::ELSE or t == TokenType::ELSEIF)
                    return;
                _pos++;
            }
        }

        std::unique_ptr<ast::Block> parse_chunk();
        std::unique_ptr<ast::Statement> parse_stat();
        std::unique_ptr<ast::Statement> parse_assignment_or_call();
        std::unique_ptr<ast::Statement> parse_label();
        std::unique_ptr<ast::Statement> parse_if();
        std::unique_ptr<ast::Statement> parse_while();
        std::unique_ptr<ast::Statement> parse_repeat();
        std::unique_ptr<ast::Statement> parse_for();
        std::unique_ptr<ast::Statement> parse_do();
        std::unique_ptr<ast::Statement> parse_function_decl(ast::Visibility vis, bool is_macroexp = false);
        std::unique_ptr<ast::Statement> parse_var_decl(ast::Visibility vis);
        std::unique_ptr<ast::Statement> parse_record_decl(ast::Visibility vis, bool is_interface);
        std::unique_ptr<ast::Statement> parse_enum_decl(ast::Visibility vis);
        std::unique_ptr<ast::Statement> parse_type_alias_decl(ast::Visibility vis);
        std::vector<ast::VariableDeclarationStatement::Name> parse_att_name_list();
        std::vector<std::string_view> parse_name_list();
        std::unique_ptr<ast::Expression> parse_expression();
        std::vector<std::unique_ptr<ast::Expression>> parse_expression_list();
        std::unique_ptr<ast::Expression> parse_prefix_expression();
        std::unique_ptr<ast::Expression> parse_var_expression();
        std::unique_ptr<ast::Expression> parse_primary_expression();
        std::unique_ptr<ast::Expression> parse_exp_rec(int min_prec);
        int get_binary_precedence(TokenType op);
        bool is_right_associative(TokenType op);
        std::unique_ptr<ast::Expression> parse_unary_expression();
        std::unique_ptr<ast::Expression> parse_function_def_expression();
        std::unique_ptr<ast::Expression> parse_table_constructor();
        std::unique_ptr<ast::TypeNode> parse_type();
        std::unique_ptr<ast::TypeNode> parse_base_type();
        std::unique_ptr<ast::TypeNode> parse_nominal_type();
        std::unique_ptr<ast::TypeNode> parse_function_type();
        std::vector<std::unique_ptr<ast::TypeNode>> parse_type_list();
        std::vector<ast::FunctionTypeNode::ParameterType> parse_param_type_list();
        std::vector<std::unique_ptr<ast::TypeNode>> parse_return_type_list(bool *var_arg);
        std::unique_ptr<ast::RecordBody> parse_record_body();
        std::unique_ptr<ast::EnumBody> parse_enum_body();
        void parse_interface_list(ast::RecordBody *rb);
        bool parse_generic_list(std::vector<ast::GenericTypeParameter> *t_params);
        bool parse_typeargs(std::vector<std::unique_ptr<ast::TypeNode>> *types);
    };

} // namespace teal::parser
