#pragma once

#include <source_location>
#include "AST.hpp"

namespace teal::parser
{

class Parser {
public:
    struct Error {
        String message;
        int line, column;

        constexpr inline std::string to_string() const
        { return message.data(); }
    };

    Parser(Vector<Token> toks, Allocator alloc = Allocator(std::pmr::get_default_resource())):
        allocator(alloc),
        max_errors(10),
        _tokens(std::move(toks), alloc),
        _pos(0)
    {}
    std::tuple<Pointer<ast::Block>, Vector<Error>> parse() {
        try {
            return {parse_chunk(), _errors};
        } catch (const StopParsingException&) {
            push_error(std::format("Too many parsing errors ({})", _errors.size()), true);
            return {nullptr, _errors};
        }
    }

    const Allocator allocator;
    const size_t max_errors;
private:
    class StopParsingException : public std::exception {};
    class UnexpectedEOFException : public StopParsingException {};

    Vector<Token> _tokens;
    size_t _pos;
    Vector<Error> _errors;

    constexpr inline String string(std::string_view s)
    { return String(s, allocator); }

    constexpr inline String string()
    { return String(allocator); }
    template<typename T, typename ...TArgs> requires std::is_base_of_v<ast::ASTNode, T>
    constexpr inline Pointer<T> make_node(const Token &tk, TArgs &&...args)
    { return allocate<T>(allocator, allocator, tk, std::forward<TArgs>(args)...); }

    template<typename T, typename ...TArgs> requires std::is_base_of_v<ast::ASTNode, T>
    constexpr inline Pointer<T> make_node(TArgs &&...args)
    { return allocate<T>(allocator, allocator, peek_token(), std::forward<TArgs>(args)...); }

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
    constexpr inline void push_error(std::string_view msg, bool nothrow = false) {
        _errors.push_back({ string(msg), peek_token().line, peek_token().column });
        if (_errors.size() >= max_errors and not nothrow)
            throw StopParsingException{};
    }
#define $push_error(msg, ...) push_error(std::format("<{}:{}> {}", std::source_location::current().file_name(), std::source_location::current().line(), msg) __VA_OPT__(,) __VA_ARGS__)
    std::optional<Token> consume(TokenType t, std::string_view err_msg) {
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

    Pointer<ast::Block> parse_chunk();
    Pointer<ast::Statement> parse_stat();
    Pointer<ast::Statement> parse_assignment_or_call();
    Pointer<ast::Statement> parse_label();
    Pointer<ast::Statement> parse_if();
    Pointer<ast::Statement> parse_while();
    Pointer<ast::Statement> parse_repeat();
    Pointer<ast::Statement> parse_for();
    Pointer<ast::Statement> parse_do();
    Pointer<ast::Statement> parse_function_decl(ast::Visibility vis, bool is_macroexp = false);
    Pointer<ast::Statement> parse_var_decl(ast::Visibility vis);
    Pointer<ast::Statement> parse_record_decl(ast::Visibility vis, bool is_interface);
    Pointer<ast::Statement> parse_enum_decl(ast::Visibility vis);
    Pointer<ast::Statement> parse_type_alias_decl(ast::Visibility vis);
    Vector<ast::VariableDeclarationStatement::Name> parse_att_name_list();
    Vector<String> parse_name_list();
    Pointer<ast::Expression> parse_expression();
    Vector<Pointer<ast::Expression>> parse_expression_list();
    Pointer<ast::Expression> parse_prefix_expression();
    Pointer<ast::Expression> parse_var_expression();
    Pointer<ast::Expression> parse_primary_expression();
    Pointer<ast::Expression> parse_exp_rec(int min_prec);
    int get_binary_precedence(TokenType op);
    bool is_right_associative(TokenType op);
    Pointer<ast::Expression> parse_unary_expression();
    Pointer<ast::Expression> parse_function_def_expression();
    Pointer<ast::Expression> parse_table_constructor();
    Pointer<ast::TypeNode> parse_type();
    Pointer<ast::TypeNode> parse_base_type();
    Pointer<ast::TypeNode> parse_nominal_type();
    Pointer<ast::TypeNode> parse_function_type();
    Vector<Pointer<ast::TypeNode>> parse_type_list();
    Vector<ast::FunctionTypeNode::ParameterType> parse_param_type_list();
    Vector<Pointer<ast::TypeNode>> parse_return_type_list(bool *var_arg);
    Pointer<ast::RecordBody> parse_record_body();
    Pointer<ast::EnumBody> parse_enum_body();
    void parse_interface_list(ast::RecordBody *rb);
    bool parse_generic_list(Vector<ast::GenericTypeParameter> *t_params);
    bool parse_typeargs(Vector<Pointer<ast::TypeNode>> *types);
};

}
