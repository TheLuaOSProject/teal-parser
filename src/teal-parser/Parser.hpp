#pragma once

#include <source_location>
#include "AST.hpp"

namespace teal::parser
{

class Parser {
public:
    struct Error {
        std::string message;
        int line, column;
    };

    Parser(std::vector<Token> toks) : max_errors(10), _tokens(std::move(toks)), _pos(0) {}
    const std::vector<Error>& errors() const { return _errors; }
    std::unique_ptr<Block> parse() {
        try {
            return parse_chunk();
        } catch (const StopParsingException&) {
            push_error(std::format("Too many parsing errors ({})", _errors.size()), true);
            return nullptr;
        }
    }

    const size_t max_errors;
private:
    class StopParsingException : public std::exception {};
    class UnexpectedEOFException : public StopParsingException {};

    std::vector<Token> _tokens;
    size_t _pos;
    std::vector<Error> _errors;

    const Token &peek_token(int forward = 0) const
    {
        [[unlikely]]
        if (_pos + forward > _tokens.size())
            throw UnexpectedEOFException();
        return _tokens[_pos + forward];
    }
    bool is_at_end() const { return peek_token().type == TokenType::END_OF_FILE; }
    bool check(TokenType t) const {
        return (t == TokenType::NAME and (Token::type_is_teal_keyword(peek_token().type) or peek_token().text == "macroexp"))
            or peek_token().type == t;
    }
    bool match(TokenType t) { if (check(t)) { _pos++; return true; } return false; }
    bool match_any(std::initializer_list<TokenType> types) {
        if (check(TokenType::END_OF_FILE)) return false;
        for (TokenType t : types) {
            if (check(t)) { _pos++; return true; }
        }
        return false;
    }
    constexpr inline void push_error(const std::string_view& msg, bool nothrow = false) {
        _errors.push_back({ std::string(msg), peek_token().line, peek_token().col });
        if (_errors.size() >= max_errors and not nothrow)
            throw StopParsingException{};
    }
#define $push_error(msg, ...) push_error(std::format("<{}:{}> {}", std::source_location::current().file_name(), std::source_location::current().line(), msg) __VA_OPT__(,) __VA_ARGS__)
    std::optional<Token> consume(TokenType t, const std::string_view& err_msg) {
        if (check(t)) {
            Token tok = peek_token();
            _pos++;
            return tok;
        } else {
            push_error(err_msg);
            return std::nullopt;
        }
    }
#define $consume(tk, msg) consume(tk, std::format("<{}:{}> {}", std::source_location::current().file_name(), std::source_location::current().line(), msg))
    void skip_to_next_statement() {
        while (not is_at_end()) {
            TokenType t = peek_token().type;
            if (t == TokenType::SEMICOLON or t == TokenType::RETURN or t == TokenType::BREAK or
                t == TokenType::GLOBAL or t == TokenType::LOCAL or t == TokenType::IF or t == TokenType::WHILE or
                t == TokenType::FOR or t == TokenType::FUNCTION or t == TokenType::REPEAT or
                t == TokenType::END or t == TokenType::UNTIL or t == TokenType::ELSE or t == TokenType::ELSEIF)
                return;
            _pos++;
        }
    }

    std::unique_ptr<Block> parse_chunk();
    std::unique_ptr<Statement> parse_stat();
    std::unique_ptr<Statement> parse_assignment_or_call();
    std::unique_ptr<Statement> parse_label();
    std::unique_ptr<Statement> parse_if();
    std::unique_ptr<Statement> parse_while();
    std::unique_ptr<Statement> parse_repeat();
    std::unique_ptr<Statement> parse_for();
    std::unique_ptr<Statement> parse_do();
    std::unique_ptr<Statement> parse_function_decl(Visibility vis, bool is_macroexp = false);
    std::unique_ptr<Statement> parse_var_decl(Visibility vis);
    std::unique_ptr<Statement> parse_record_decl(Visibility vis, bool is_interface);
    std::unique_ptr<Statement> parse_enum_decl(Visibility vis);
    std::unique_ptr<Statement> parse_type_alias_decl(Visibility vis);
    std::vector<VariableDeclarationStatement::Name> parse_att_name_list();
    std::vector<std::string> parse_name_list();
    std::unique_ptr<Expression> parse_expression();
    std::vector<std::unique_ptr<Expression>> parse_expression_list();
    std::unique_ptr<Expression> parse_prefix_expression();
    std::unique_ptr<Expression> parse_var_expression();
    std::unique_ptr<Expression> parse_primary_expression();
    std::unique_ptr<Expression> parse_exp_rec(int min_prec);
    int get_binary_precedence(TokenType op);
    bool is_right_associative(TokenType op);
    std::unique_ptr<Expression> parse_unary_expression();
    std::unique_ptr<Expression> parse_function_def_expression();
    std::unique_ptr<Expression> parse_table_constructor();
    std::unique_ptr<TypeNode> parse_type();
    std::unique_ptr<TypeNode> parse_base_type();
    std::unique_ptr<TypeNode> parse_nominal_type();
    std::unique_ptr<TypeNode> parse_function_type();
    std::vector<std::unique_ptr<TypeNode>> parse_type_list();
    std::vector<FunctionTypeNode::ParameterType> parse_param_type_list();
    std::vector<std::unique_ptr<TypeNode>> parse_return_type_list(bool *var_arg);
    std::unique_ptr<RecordBody> parse_record_body();
    std::unique_ptr<EnumBody> parse_enum_body();
    void parse_interface_list(RecordBody *rb);
    bool parse_generic_list(std::vector<GenericTypeParameter> *t_params);
    bool parse_typeargs(std::vector<std::unique_ptr<TypeNode>> *types);
};

}
