#pragma once

#include <source_location>
#include "AST.hpp"

namespace teal {

class Parser {
public:
    struct Error {
        std::string message;
        int line, col;
    };

    Parser(std::vector<Token> toks) : max_errors(10), tokens_(std::move(toks)), pos_(0) {}
    const std::vector<Error>& errors() const { return errors_; }
    std::unique_ptr<Block> parse() {
        try {
            return parse_chunk();
        } catch (const StopParsingException&) {
            push_error(std::format("Too many parsing errors ({})", errors_.size()), true);
            return nullptr;
        }
    }

    const size_t max_errors;
private:
    class StopParsingException : public std::exception {};

    std::vector<Token> tokens_;
    size_t pos_;
    std::vector<Error> errors_;

    const Token& peek_token(int forward = 0) const { return tokens_[pos_ + forward]; }
    bool is_at_end() const { return peek_token().type == TokenType::END_OF_FILE; }
    bool check(TokenType t) const {
        return (t == TokenType::NAME and (Token::type_is_teal_keyword(peek_token().type) or peek_token().text == "macroexp"))
            or peek_token().type == t;
    }
    bool match(TokenType t) { if (check(t)) { pos_++; return true; } return false; }
    bool match_any(std::initializer_list<TokenType> types) {
        if (check(TokenType::END_OF_FILE)) return false;
        for (TokenType t : types) {
            if (check(t)) { pos_++; return true; }
        }
        return false;
    }
    constexpr inline void push_error(const std::string_view& msg, bool nothrow = false) {
        errors_.push_back({ std::string(msg), peek_token().line, peek_token().col });
        if (errors_.size() >= max_errors and not nothrow)
            throw StopParsingException{};
    }
#define push_error_macro(msg, ...) push_error(std::format("<{}:{}> {}", std::source_location::current().file_name(), std::source_location::current().line(), msg) __VA_OPT__(,) __VA_ARGS__)
    std::optional<Token> consume(TokenType t, const std::string_view& err_msg) {
        if (check(t)) {
            Token tok = peek_token();
            pos_++;
            return tok;
        } else {
            push_error(err_msg);
            return std::nullopt;
        }
    }
#define consume_macro(tk, msg) consume(tk, std::format("<{}:{}> {}", std::source_location::current().file_name(), std::source_location::current().line(), msg))
    void skip_to_next_statement() {
        while (not is_at_end()) {
            TokenType t = peek_token().type;
            if (t == TokenType::OP_SEMICOLON or t == TokenType::K_RETURN or t == TokenType::K_BREAK or
                t == TokenType::K_GLOBAL or t == TokenType::K_LOCAL or t == TokenType::K_IF or t == TokenType::K_WHILE or
                t == TokenType::K_FOR or t == TokenType::K_FUNCTION or t == TokenType::K_REPEAT or
                t == TokenType::K_END or t == TokenType::K_UNTIL or t == TokenType::K_ELSE or t == TokenType::K_ELSEIF)
                return;
            pos_++;
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
    std::unique_ptr<Statement> parse_function_decl(bool is_local, bool is_global);
    std::unique_ptr<Statement> parse_var_decl(bool is_local, bool is_global);
    std::unique_ptr<Statement> parse_record_decl(bool is_local, bool is_global, bool is_interface);
    std::unique_ptr<Statement> parse_enum_decl(bool is_local, bool is_global);
    std::unique_ptr<Statement> parse_type_alias_decl(bool is_local, bool is_global);
    std::vector<VariableDeclarationStatement::NameAttrib> parse_att_name_list();
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
    std::vector<FunctionTypeNode::ParamType> parse_param_type_list();
    std::vector<std::unique_ptr<TypeNode>> parse_return_type_list(bool &var_arg);
    std::unique_ptr<RecordBody> parse_record_body();
    std::unique_ptr<EnumBody> parse_enum_body();
    void parse_interface_list(RecordBody &rb);
    bool parse_generic_list(std::vector<GenericTypeParameter> &t_params);
};

}
