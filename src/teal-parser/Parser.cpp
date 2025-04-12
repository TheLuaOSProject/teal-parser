#include "Parser.hpp"
#include "AST.hpp"
#include "Lexer.hpp"
#include <cassert>
#include <format>

using namespace teal::parser;
using namespace teal::parser::ast;

template <>
struct std::formatter<Token> {
    constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
    constexpr auto format(const Token &tk, std::format_context &ctx) const
    {
        return std::format_to(ctx.out(), "Token {{.kind=\"{}\", .at={}:{}}}", tk.to_string(), tk.line, tk.col);
    }
};

template <>
struct std::formatter<TokenType> {
    constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
    constexpr auto format(const TokenType &tk, std::format_context &ctx) const
    {
        return std::format_to(ctx.out(), "{}", Token::type_to_string(tk));
    }
};

std::unique_ptr<Block> Parser::parse_chunk()
{
    auto block = make_node<Block>();
    while (not is_at_end()) {
        if (check(TokenType::END_OF_FILE)) break;
        auto stmt = parse_stat();
        if (stmt) block->statements.push_back(std::move(stmt));
    }
    return block;
}

std::unique_ptr<Statement> Parser::parse_stat()
{
    if (match(TokenType::SEMICOLON)) { return nullptr; }
    TokenType t = peek_token().type;
    Visibility vis;
    switch (t) {
    case TokenType::IF:
        return parse_if();
    case TokenType::WHILE:
        return parse_while();
    case TokenType::REPEAT:
        return parse_repeat();
    case TokenType::FOR:
        return parse_for();
    case TokenType::DO:
        return parse_do();
    case TokenType::FUNCTION:
        _pos++;
        return parse_function_decl(Visibility::NONE);

    case TokenType::LOCAL:
        vis = Visibility::LOCAL;
        goto globloc;
    case TokenType::GLOBAL:
        vis = Visibility::GLOBAL;
        goto globloc;
    globloc: {
        _pos++;
        if (match(TokenType::FUNCTION) or match(TokenType::MACROEXP)) {
            return parse_function_decl(vis, true);
        } else if (match(TokenType::RECORD)) {
            return parse_record_decl(vis, false);
        } else if (match(TokenType::INTERFACE)) {
            return parse_record_decl(vis, true);
        } else if (match(TokenType::ENUM)) {
            return parse_enum_decl(vis);
        } else if (match(TokenType::TYPE)) {
            return parse_type_alias_decl(vis);
        } else {
            return parse_var_decl(vis);
        }
        break;
    }

    case TokenType::RETURN: {
        _pos++;
        auto ret_stmt = make_node<ReturnStatement>();
        if (not check(TokenType::SEMICOLON) and not check(TokenType::END) and not check(TokenType::ELSE)
            and not check(TokenType::ELSEIF) and not check(TokenType::UNTIL) and not check(TokenType::END_OF_FILE)) {
            ret_stmt->values = parse_expression_list();
        }
        match(TokenType::SEMICOLON);
        return ret_stmt;
    }
    case TokenType::BREAK:
        _pos++;
        return make_node<BreakStatement>();
    case TokenType::GOTO: {
        _pos++;
        if (not check(TokenType::NAME)) {
            // $push_error("expected label name after 'goto'");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            return nullptr;
        }
        std::string label_name = peek_token().text;
        _pos++;
        return make_node<GotoStatement>(std::move(label_name));
    }
    case TokenType::DOUBLE_COLON:
        return parse_label();
    default:
        return parse_assignment_or_call();
    }
}

std::unique_ptr<Statement> Parser::parse_assignment_or_call()
{
    const Token &prefix_tk = peek_token();
    auto prefix = parse_prefix_expression();
    if (not prefix) {
        skip_to_next_statement();
        return nullptr;
    }
    if (check(TokenType::ASSIGN) or check(TokenType::COMMA)) {
        if (dynamic_cast<FunctionCallExpression *>(prefix.get())) {
            // $push_error("cannot assign to function call");
            push_error(CannotAssignToFunctionCall());
            prefix = make_node<NameExpression>("_error_");
        }
        auto assign = make_node<AssignmentStatement>();
        assign->left.push_back(std::move(prefix));
        while (match(TokenType::COMMA)) {
            auto var = parse_var_expression();
            if (not var) {
                // $push_error("expected variable");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::COMMA)) {
                    _pos++;
                    continue;
                }
                break;
            }
            assign->left.push_back(std::move(var));
        }
        const Token &tk = peek_token();
        consume(TokenType::ASSIGN);
        assign->right = parse_expression_list();
        if (assign->right.empty())
            push_error(ExpectedExpression(tk));
        return assign;
    } else {
        auto call_expression = dynamic_cast<FunctionCallExpression *>(prefix.get());
        if (not call_expression) {
            push_error(UnexpectedExpression(prefix_tk));
            skip_to_next_statement();
            return nullptr;
        }
        std::unique_ptr<FunctionCallExpression> call_node(static_cast<FunctionCallExpression *>(prefix.release()));
        return make_node<CallStatement>(std::move(call_node));
    }
}

std::unique_ptr<Statement> Parser::parse_label()
{
    // consume(TokenType::DOUBLE_COLON);
    consume(TokenType::DOUBLE_COLON);
    if (not check(TokenType::NAME)) {
        // $push_error("expected label name after '::'");
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullptr;
    }
    std::string name = peek_token().text;
    _pos++;
    consume(TokenType::DOUBLE_COLON);
    return make_node<LabelStatement>(std::move(name));
}

std::unique_ptr<Statement> Parser::parse_if()
{
    const Token &iftk = peek_token();
    consume(TokenType::IF);
    auto if_stmt = make_node<IfStatement>();
    auto cond = parse_expression();
    if (not cond) { push_error(ExpectedExpression(iftk)); }
    consume(TokenType::THEN);
    auto then_block = make_node<Block>();
    while (not check(TokenType::END) and not check(TokenType::ELSE) and not check(TokenType::ELSEIF) and not is_at_end()
    ) {
        auto st = parse_stat();
        if (st) then_block->statements.push_back(std::move(st));
        if (check(TokenType::END) or check(TokenType::ELSE) or check(TokenType::ELSEIF) or check(TokenType::UNTIL)
            or check(TokenType::END_OF_FILE))
            break;
    }
    if_stmt->if_branches.push_back({ std::move(cond), std::move(then_block) });
    while (match(TokenType::ELSEIF)) {
        const Token &elseif_tk = peek_token();
        auto elseif_cond = parse_expression();
        if (not elseif_cond) push_error(ExpectedExpression(elseif_tk));
        consume(TokenType::THEN);
        auto elseif_block = make_node<Block>();
        while (not check(TokenType::END) and not check(TokenType::ELSE) and not check(TokenType::ELSEIF)
               and not is_at_end()) {
            auto st = parse_stat();
            if (st) elseif_block->statements.push_back(std::move(st));
            if (check(TokenType::END) or check(TokenType::ELSE) or check(TokenType::ELSEIF) or check(TokenType::UNTIL)
                or check(TokenType::END_OF_FILE))
                break;
        }
        if_stmt->if_branches.push_back({ std::move(elseif_cond), std::move(elseif_block) });
    }
    if (match(TokenType::ELSE)) {
        auto else_block = make_node<Block>();
        while (not check(TokenType::END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) else_block->statements.push_back(std::move(st));
            if (check(TokenType::END) or check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
        }
        if_stmt->else_block = std::move(else_block);
    }
    consume(TokenType::END);
    return if_stmt;
}

std::unique_ptr<Statement> Parser::parse_while()
{
    consume(TokenType::WHILE);
    auto while_stmt = make_node<WhileStatement>();
    const Token &while_tk = peek_token();
    while_stmt->condition = parse_expression();
    if (not while_stmt->condition) push_error(ExpectedExpression(while_tk));
    consume(TokenType::DO);
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::END) or check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    while_stmt->body = std::move(body_block);
    return while_stmt;
}

std::unique_ptr<Statement> Parser::parse_repeat()
{
    consume(TokenType::REPEAT);
    auto repeat_stmt = make_node<RepeatStatement>();
    auto body_block = make_node<Block>();
    while (not check(TokenType::UNTIL) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::UNTIL);
    repeat_stmt->body = std::move(body_block);
    const Token &until_tk = peek_token();
    repeat_stmt->condition = parse_expression();
    if (not repeat_stmt->condition) push_error(ExpectedExpression(until_tk));
    return repeat_stmt;
}

std::unique_ptr<Statement> Parser::parse_for()
{
    consume(TokenType::FOR);
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        skip_to_next_statement();
        return nullptr;
    }
    std::string var_name = peek_token().text;
    _pos++;
    if (match(TokenType::ASSIGN)) {
        auto for_num = make_node<ForNumericStatement>();
        for_num->variable_name = var_name;
        for_num->expressions.start = parse_expression();
        consume(TokenType::COMMA);
        for_num->expressions.end = parse_expression();
        if (match(TokenType::COMMA)) { for_num->expressions.step = parse_expression(); }
        consume(TokenType::DO);
        auto body_block = make_node<Block>();
        while (not check(TokenType::END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) body_block->statements.push_back(std::move(st));
            if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
        }
        consume(TokenType::END);
        for_num->body = std::move(body_block);
        return for_num;
    } else {
        std::vector<std::string> name_list;
        name_list.push_back(var_name);
        while (match(TokenType::COMMA)) {
            if (not check(TokenType::NAME)) {
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                break;
            }
            name_list.push_back(peek_token().text);
            _pos++;
        }
        consume(TokenType::IN);
        auto exprs = parse_expression_list();
        consume(TokenType::DO);
        auto body_block = make_node<Block>();
        while (not check(TokenType::END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) body_block->statements.push_back(std::move(st));
            if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
        }
        consume(TokenType::END);
        auto for_in = make_node<ForInStatement>();
        for_in->names = std::move(name_list);
        for_in->exprs = std::move(exprs);
        for_in->body = std::move(body_block);
        return for_in;
    }
}

std::unique_ptr<Statement> Parser::parse_do()
{
    consume(TokenType::DO);
    auto block_node = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) block_node->statements.push_back(std::move(st));
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    return make_node<DoStatement>(std::move(block_node));
}

std::unique_ptr<Statement> Parser::parse_function_decl(Visibility vis, bool is_macroexp)
{
    if (not check(TokenType::NAME)) push_error(ExpectedToken(TokenType::NAME, peek_token()));
    std::vector<std::string> name_path;
    std::string method_name;
    bool is_method = false;
    if (check(TokenType::NAME)) {
        name_path.push_back(peek_token().text);
        _pos++;
    }
    if (not is_macroexp) {
        while (true) {
            if (match(TokenType::DOT)) {
                if (not check(TokenType::NAME)) {
                    // $push_error("expected name after '.' in function name");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    break;
                }
                name_path.push_back(peek_token().text);
                _pos++;
            } else if (match(TokenType::COLON)) {
                is_method = true;
                if (not check(TokenType::NAME)) {
                    // $push_error("expected name after ':' in function name");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                } else {
                    method_name = peek_token().text;
                    _pos++;
                }
                break;
            } else {
                break;
            }
        }
    } else {
        if (check(TokenType::DOT) or check(TokenType::COLON)) {
            // $push_error("macroexps are not allowed to be members");
            push_error(MacroexpMember());
            while (match(TokenType::DOT) or match(TokenType::COLON));
            // return nullptr;
        }
    }
    auto func_body = make_node<FunctionBody>();
    if (match(TokenType::LESS)) { parse_generic_list(&func_body->type_parameters); }
    consume(TokenType::L_PAREN);
    if (not check(TokenType::R_PAREN)) {
        while (true) {
            if (check(TokenType::VAR_ARG)) {
                _pos++;
                std::unique_ptr<TypeNode> var_type;
                if (match(TokenType::COLON)) { var_type = parse_type(); }
                func_body->parameters.push_back({ "...", true, false, std::move(var_type) });
                break;
            }
            if (not check(TokenType::NAME)) {
                // $push_error("expected parameter name or '...'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::R_PAREN)) break;
                _pos++;
            } else {
                std::string param_name = peek_token().text;
                _pos++;
                bool opt_flag = false;
                if (match(TokenType::QUESTION)) opt_flag = true;
                std::unique_ptr<TypeNode> type_ann;
                if (match(TokenType::COLON)) { type_ann = parse_type(); }
                func_body->parameters.push_back({ param_name, false, opt_flag, std::move(type_ann) });
            }
            if (not match(TokenType::COMMA)) break;
        }
    }
    consume(TokenType::R_PAREN);
    if (match(TokenType::COLON)) {
        bool ret_var_arg = false;
        func_body->return_types = parse_return_type_list(&ret_var_arg);
        func_body->varadict_return = ret_var_arg;
    }
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    func_body->body = std::move(body_block);
    auto func_decl = make_node<FunctionDeclarationStatement>(vis);
    func_decl->name_path = std::move(name_path);
    func_decl->method_name = method_name;
    func_decl->is_method = is_method;
    func_decl->is_macro = is_macroexp;
    func_decl->body = std::move(func_body);
    return func_decl;
}

std::unique_ptr<Statement> Parser::parse_var_decl(Visibility vis)
{
    auto var_stmt = make_node<VariableDeclarationStatement>(vis);
    auto names = parse_att_name_list();
    if (names.empty()) push_error(ExpectedToken(TokenType::NAME, peek_token()));
    var_stmt->names = std::move(names);
    if (match(TokenType::COLON)) var_stmt->types = parse_type_list();
    if (match(TokenType::ASSIGN)) var_stmt->values = parse_expression_list();
    if (vis == Visibility::NONE) {
        if (var_stmt->types.empty() and var_stmt->values.empty()) {
            // $push_error("global variable must have type or initial value");
            push_error(GlobalVariableMustBeTyped(var_stmt->names[0].name));
        }
    }
    return var_stmt;
}

std::unique_ptr<Statement> Parser::parse_record_decl(Visibility vis, bool is_interface)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullptr;
    }
    std::string name = peek_token().text;
    _pos++;
    auto body = parse_record_body();
    return make_node<RecordDeclarationStatement>(is_interface, vis, std::move(name), std::move(body));
}

std::unique_ptr<Statement> Parser::parse_enum_decl(Visibility vis)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullptr;
    }
    std::string name = peek_token().text;
    _pos++;
    auto body = parse_enum_body();
    return make_node<EnumDeclarationStatement>(vis, std::move(name), std::move(body));
}

std::unique_ptr<Statement> Parser::parse_type_alias_decl(Visibility vis)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullptr;
    }
    std::string name = peek_token().text;
    _pos++;
    std::unique_ptr<TypeNode> type_value;
    std::vector<GenericTypeParameter> type_args;
    if (match(TokenType::LESS)) { parse_generic_list(&type_args); }

    if (match(TokenType::ASSIGN)) {
        if (check(TokenType::RECORD)) {
            _pos++;
            type_value = make_node<TypeRecordNode>(parse_record_body());
        } else if (check(TokenType::ENUM)) {
            _pos++;
            type_value = make_node<TypeEnumNode>(std::move(parse_enum_body()->elements));
        } else {
            type_value = parse_type();
        }
    } else {
        if (vis == Visibility::LOCAL)
            push_error(ExpectedToken(TokenType::ASSIGN, peek_token()));
    }
    return make_node<TypeAliasStatement>(vis, std::move(name), std::move(type_args), std::move(type_value));
}

std::vector<VariableDeclarationStatement::Name> Parser::parse_att_name_list()
{
    std::vector<VariableDeclarationStatement::Name> list;
    if (not check(TokenType::NAME)) { return list; }
    do {
        VariableDeclarationStatement::Name na;
        na.name = peek_token().text;
        _pos++;
        if (match(TokenType::LESS)) {
            if (not check(TokenType::NAME)) {
                // $push_error("expected attribute name in '< >'")
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                na.attribute = peek_token().text;
                _pos++;
            }
            consume(TokenType::GREATER);
        }
        list.push_back(std::move(na));
    } while (match(TokenType::COMMA));
    return list;
}

std::vector<std::string> Parser::parse_name_list()
{
    std::vector<std::string> list;
    if (not check(TokenType::NAME)) { return list; }
    list.push_back(peek_token().text);
    _pos++;
    while (match(TokenType::COMMA)) {
        if (not check(TokenType::NAME)) {
            // $push_error("expected name after ','");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            break;
        }
        list.push_back(peek_token().text);
        _pos++;
    }
    return list;
}

std::unique_ptr<Expression> Parser::parse_expression() { return parse_exp_rec(1); }

std::vector<std::unique_ptr<Expression>> Parser::parse_expression_list()
{
    std::vector<std::unique_ptr<Expression>> exprs;
    auto first = parse_expression();
    if (first) exprs.push_back(std::move(first));
    while (match(TokenType::COMMA)) {
        auto e = parse_expression();
        if (e) {
            exprs.push_back(std::move(e));
        } else {
            // $push_error("expected expression after ','");
            push_error(ExpectedExpression(peek_token()));
            if (not check(TokenType::COMMA)) break;
        }
    }
    return exprs;
}

std::unique_ptr<Expression> Parser::parse_prefix_expression()
{
    std::unique_ptr<Expression> base;
    if (match(TokenType::L_PAREN)) {
        base = parse_expression();
        consume(TokenType::R_PAREN);
    } else if (check(TokenType::NAME)) {
        auto s = peek_token().text;
        base = make_node<NameExpression>(std::move(s)
        ); // specialisation of std::move for string makes it const qualified. Why? because fuck you
        _pos++;
    } else {
        // $push_error(std::format("expected '(' or Name, got {}", peek_token()));
        push_error(ExpectedToken(TokenType::L_PAREN, peek_token()));
        return nullptr;
    }
    while (true) {
        if (match(TokenType::COLON)) {
            if (not check(TokenType::NAME)) {
                // $push_error("expected method name after ':'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                break;
            }
            std::string method = peek_token().text;
            _pos++;
            std::vector<std::unique_ptr<Expression>> args;
            if (match(TokenType::L_PAREN)) {
                if (not check(TokenType::R_PAREN)) { args = parse_expression_list(); }
                consume(TokenType::R_PAREN);
            } else if (check(TokenType::L_BRACE)) {
                args.push_back(parse_table_constructor());
            } else if (check(TokenType::STRING)) {
                auto x = peek_token().text;
                args.push_back(make_node<StringExpression>(std::move(x)));
                _pos++;
            } else {
                push_error(ExpectedArguments());
                // $push_error("expected arguments after method call");
            }
            auto call = make_node<FunctionCallExpression>(std::move(base), std::move(method));
            for (auto &arg : args) call->arguments.push_back(std::move(arg));
            base = std::move(call);
        } else if (match(TokenType::L_PAREN)) {
            std::vector<std::unique_ptr<Expression>> args;
            if (not check(TokenType::R_PAREN)) { args = parse_expression_list(); }
            consume(TokenType::R_PAREN);
            auto call = make_node<FunctionCallExpression>(std::move(base), "");
            for (auto &arg : args) call->arguments.push_back(std::move(arg));
            base = std::move(call);
        } else if (check(TokenType::L_BRACE)) {
            auto table_arg = parse_table_constructor();
            auto call = make_node<FunctionCallExpression>(std::move(base), "");
            call->arguments.push_back(std::move(table_arg));
            base = std::move(call);
        } else if (check(TokenType::STRING)) {
            std::string lit = peek_token().text;
            _pos++;
            auto call = make_node<FunctionCallExpression>(std::move(base), "");
            call->arguments.push_back(make_node<StringExpression>(std::move(lit)));
            base = std::move(call);
        } else if (match(TokenType::DOT)) {
            if (not check(TokenType::NAME)) {
                bool ok = false;
                const auto mkname = [this](const std::string_view &id) {
                    return Token { TokenType::NAME, std::string(id), peek_token().line, peek_token().col };
                };
                switch (peek_token().type) {
                case TokenType::TYPE:
                    _tokens[_pos] = mkname("type");
                    ok = true;
                    break;
                case TokenType::RECORD:
                    _tokens[_pos] = mkname("record");
                    ok = true;
                    break;
                case TokenType::ENUM:
                    _tokens[_pos] = mkname("enum");
                    ok = true;
                    break;
                case TokenType::INTERFACE:
                    _tokens[_pos] = mkname("interface");
                    ok = true;
                    break;
                default:
                    ok = false;
                    break;
                }
                if (not ok) {
                    // $push_error(std::format("expected field name after '.', got `{}`", peek_token().type));
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    break;
                }
            }
            std::string field = peek_token().text;
            _pos++;
            base = make_node<FieldExpression>(std::move(base), std::move(field));
        } else if (match(TokenType::L_BRACKET)) {
            auto index_expression = parse_expression();
            consume(TokenType::R_BRACKET);
            base = make_node<IndexExpression>(std::move(base), std::move(index_expression));
        } else {
            break;
        }
    }
    return base;
}

std::unique_ptr<Expression> Parser::parse_var_expression()
{
    auto expr = parse_prefix_expression();
    if (not expr) return nullptr;
    if (dynamic_cast<FunctionCallExpression *>(expr.get())) {
        // $push_error("unexpected function call in assignment");
        push_error(UnexpectedFunctionCall());
        return make_node<NameExpression>("_error_");
    }
    return expr;
}

std::unique_ptr<Expression> Parser::parse_primary_expression()
{
    TokenType t = peek_token().type;
    if (t == TokenType::NIL) {
        _pos++;
        return make_node<NilExpression>();
    }
    if (t == TokenType::TRUE) {
        _pos++;
        return make_node<BooleanExpression>(true);
    }
    if (t == TokenType::FALSE) {
        _pos++;
        return make_node<BooleanExpression>(false);
    }
    if (t == TokenType::NUMBER) {
        std::string num = peek_token().text;
        _pos++;
        return make_node<NumberExpression>(std::move(num));
    }
    if (t == TokenType::STRING) {
        std::string str = peek_token().text;
        _pos++;
        return make_node<StringExpression>(std::move(str));
    }
    if (t == TokenType::VAR_ARG) {
        _pos++;
        return make_node<VarargExpression>();
    }
    if (t == TokenType::FUNCTION) { return parse_function_def_expression(); }
    if (t == TokenType::L_BRACE) { return parse_table_constructor(); }
    if (t == TokenType::NAME or t == TokenType::L_PAREN or Token::type_is_teal_keyword(t)) {
        return parse_prefix_expression();
    }
    // $push_error(std::format("unexpected token `{}` (type: {}) in expression", peek_token().text, peek_token().type));
    push_error(UnexpectedToken(peek_token()));
    return nullptr;
}

std::unique_ptr<Expression> Parser::parse_exp_rec(int min_prec)
{
    auto left = parse_unary_expression();
    while (true) {
        TokenType type = peek_token().type;
        int prec = get_binary_precedence(type);
        if (prec < min_prec) break;
        if (type == TokenType::AS) {
            _pos++;
            std::vector<std::unique_ptr<TypeNode>> cast_types;
            if (match(TokenType::L_PAREN)) {
                if (not check(TokenType::R_PAREN)) {
                    cast_types = parse_type_list();
                    if (match(TokenType::VAR_ARG)) { }
                }
                consume(TokenType::R_PAREN);
            } else {
                auto type_node = parse_type();
                if (type_node) cast_types.push_back(std::move(type_node));
            }
            left = make_node<CastExpression>(std::move(left), std::move(cast_types));
            continue;
        } else if (type == TokenType::IS) {
            _pos++;
            auto type_node = parse_type();
            left = make_node<IsTypeExpression>(std::move(left), std::move(type_node));
            continue;
        }
        bool right_assoc = is_right_associative(type);
        int next_min_prec = (right_assoc ? prec : prec + 1);
        _pos++;
        auto right = parse_exp_rec(next_min_prec);
        left = make_node<BinaryOperationExpression>(type, std::move(left), std::move(right));
    }
    return left;
}

int Parser::get_binary_precedence(TokenType op)
{
    switch (op) {
    case TokenType::OR:
        return 1;
    case TokenType::AND:
        return 2;
    case TokenType::IS:
        return 3;
    case TokenType::LESS:
    case TokenType::LESS_EQ:
    case TokenType::GREATER:
    case TokenType::GREATER_EQ:
    case TokenType::EQUALS:
    case TokenType::NOT_EQ:
        return 4;
    case TokenType::BIT_OR:
        return 5;
    case TokenType::BIT_XOR:
        return 6;
    case TokenType::BIT_AND:
        return 7;
    case TokenType::SHIFT_L:
    case TokenType::SHIFT_R:
        return 8;
    case TokenType::CONCAT:
        return 9;
    case TokenType::ADD:
    case TokenType::SUB:
        return 10;
    case TokenType::MUL:
    case TokenType::DIV:
    case TokenType::FLOOR_DIV:
    case TokenType::MOD:
        return 11;
    case TokenType::POW:
        return 13;
    case TokenType::AS:
        return 14;
    default:
        return -1;
    }
}

bool Parser::is_right_associative(TokenType op) { return (op == TokenType::CONCAT or op == TokenType::POW); }

std::unique_ptr<Expression> Parser::parse_unary_expression()
{
    if (check(TokenType::NOT) or check(TokenType::SUB) or check(TokenType::LENGTH) or check(TokenType::BIT_XOR)) {
        TokenType token = peek_token().type;
        _pos++;
        auto operand = parse_unary_expression();
        return make_node<UnaryOperationExpression>(token, std::move(operand));
    }
    return parse_primary_expression();
}

bool Parser::parse_generic_list(std::vector<GenericTypeParameter> *t_params)
{
    if (not check(TokenType::NAME)) {
        // $push_error("expected type parameter name");
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return false;
    }
    while (check(TokenType::NAME)) {
        GenericTypeParameter param = { peek_token().text, std::nullopt };
        _pos++;
        if (match(TokenType::IS) and match(TokenType::NAME)) param.is = peek_token().text;
        t_params->push_back(param);
        if (not match(TokenType::COMMA)) break;
    }
    return (bool)consume(TokenType::GREATER);
}

bool Parser::parse_typeargs(std::vector<std::unique_ptr<TypeNode>> *types)
{
    // if (not check(TokenType::NAME) and not check(TokenType::NIL)) {
    //     $push_error("expected type name in type arguments");
    // }
    // while (check(TokenType::NAME) or check(TokenType::NIL)) {
    //     type_args.push_back(peek_token().text);
    //     _pos++;
    //     if (not match(TokenType::COMMA)) break;
    // }

    while (peek_token().type != TokenType::GREATER) {
        auto ty = parse_type();
        if (not ty) return false;
        types->push_back(std::move(ty));
        if (not match(TokenType::COMMA)) break;
    }

    consume(TokenType::GREATER);
    return true;
}

std::unique_ptr<Expression> Parser::parse_function_def_expression()
{
    consume(TokenType::FUNCTION);
    auto func_body = make_node<FunctionBody>();
    if (match(TokenType::LESS)) { parse_generic_list(&func_body->type_parameters); }
    consume(TokenType::L_PAREN);
    if (not check(TokenType::R_PAREN)) {
        while (true) {
            if (check(TokenType::VAR_ARG)) {
                _pos++;
                std::unique_ptr<TypeNode> var_type;
                if (match(TokenType::COLON)) { var_type = parse_type(); }
                func_body->parameters.push_back({ "...", true, false, std::move(var_type) });
                break;
            }
            if (not check(TokenType::NAME)) {
                // $push_error("expected parameter name or '...'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::R_PAREN)) break;
                _pos++;
            } else {
                std::string param_name = peek_token().text;
                _pos++;
                bool opt_flag = false;
                if (match(TokenType::QUESTION)) opt_flag = true;
                std::unique_ptr<TypeNode> type_ann;
                if (match(TokenType::COLON)) { type_ann = parse_type(); }
                func_body->parameters.push_back({ param_name, false, opt_flag, std::move(type_ann) });
            }
            if (not match(TokenType::COMMA)) break;
        }
    }
    consume(TokenType::R_PAREN);
    if (match(TokenType::COLON)) {
        bool ret_var_arg = false;
        func_body->return_types = parse_return_type_list(&ret_var_arg);
        func_body->varadict_return = ret_var_arg;
    }
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    func_body->body = std::move(body_block);
    return make_node<FunctionDefinitionExpression>(std::move(func_body));
}

std::unique_ptr<Expression> Parser::parse_table_constructor()
{
    consume(TokenType::L_BRACE);
    auto table = make_node<TableConstructorExpression>();
    if (not check(TokenType::R_BRACE)) {
        while (true) {
            TableConstructorExpression::Field field;
            //[...]
            if (match(TokenType::L_BRACKET)) {
                auto key_expr = parse_expression();
                consume(TokenType::R_BRACKET);
                consume(TokenType::ASSIGN);
                field = TableConstructorExpression::KeyValuePair(std::move(key_expr), parse_expression());
            } else if (check(TokenType::NAME) and (
                    (peek_token(1).type == TokenType::COLON and peek_token(3).type != TokenType::L_PAREN)
                    or peek_token(1).type == TokenType::ASSIGN
                )) {
                TableConstructorExpression::KeyValuePair kvp;
                kvp.key = peek_token().text;

                //{ k: string = "test", self:get_value() } are both valid
                if (_pos + 1 < _tokens.size()
                    and (peek_token(1).type == TokenType::COLON or peek_token(1).type == TokenType::ASSIGN)) {
                    _pos++;
                    if (match(TokenType::COLON)) { kvp.type = parse_type(); }
                    consume(TokenType::ASSIGN);
                }
                kvp.value = parse_expression();
                field = std::move(kvp);
            } else { // expression, array part of the table
                std::unique_ptr<Expression> expr_ptr = parse_expression();
                field = std::move(expr_ptr);
            }

            table->fields.push_back(std::move(field));
            if (match(TokenType::COMMA) or match(TokenType::SEMICOLON)) {
                if (check(TokenType::R_BRACE)) break;
                else continue;
            } else break;
        }
    }
    consume(TokenType::R_BRACE);
    return table;
}

std::unique_ptr<TypeNode> Parser::parse_type()
{
    std::unique_ptr<TypeNode> first_type;
    if (match(TokenType::L_PAREN)) {
        first_type = parse_type();
        consume(TokenType::R_PAREN);
    } else {
        first_type = parse_base_type();
    }
    if (not first_type) { first_type = make_node<BasicTypeNode>("nil"); }
    if (check(TokenType::BIT_OR)) {
        auto union_node = make_node<UnionTypeNode>();
        union_node->options.push_back(std::move(first_type));
        while (match(TokenType::BIT_OR)) {
            auto next_type = parse_base_type();
            if (not next_type) next_type = make_node<BasicTypeNode>("nil");
            union_node->options.push_back(std::move(next_type));
        }
        return union_node;
    }
    return first_type;
}

constexpr inline bool is_primitive(const std::string_view &name)
{
    return name == "string" or name == "integer" or name == "number" or name == "boolean" or name == "nil";
}

std::unique_ptr<TypeNode> Parser::parse_base_type()
{
    if (check(TokenType::NAME) or check(TokenType::NIL)) {
        std::string name = peek_token().text;
        if (is_primitive(name)) {
            _pos++;
            return make_node<BasicTypeNode>(std::move(name));
        } else return parse_nominal_type();
    }
    if (check(TokenType::L_BRACE)) {
        _pos++;
        auto first_type = parse_type();
        if (not first_type) first_type = make_node<BasicTypeNode>("nil");
        if (match(TokenType::COLON)) {
            auto second_type = parse_type();
            if (not second_type) second_type = make_node<BasicTypeNode>("nil");
            consume(TokenType::R_BRACE);
            auto map_node = make_node<TableTypeNode>();
            map_node->is_map = true;
            map_node->key_type = std::move(first_type);
            map_node->element_types.push_back(std::move(second_type));
            return map_node;
        } else {
            std::vector<std::unique_ptr<TypeNode>> types;
            types.push_back(std::move(first_type));
            while (match(TokenType::COMMA)) {
                auto t = parse_type();
                if (not t) t = make_node<BasicTypeNode>("nil");
                types.push_back(std::move(t));
            }
            consume(TokenType::R_BRACE);
            if (types.size() == 1) {
                auto arr_node = make_node<TableTypeNode>();
                arr_node->is_map = false;
                arr_node->element_types.push_back(std::move(types[0]));
                return arr_node;
            } else {
                auto tuple_node = make_node<TableTypeNode>();
                tuple_node->is_map = false;
                for (auto &tt : types) tuple_node->element_types.push_back(std::move(tt));
                return tuple_node;
            }
        }
    }
    if (check(TokenType::FUNCTION)) { return parse_function_type(); }
    // $push_error(std::format("expected type, got {}", peek_token()));
    push_error(ExpectedToken(TokenType::NAME, peek_token()));
    return nullptr;
}

std::unique_ptr<TypeNode> Parser::parse_nominal_type()
{
    std::vector<std::string> name_parts;
    if (not check(TokenType::NAME)) { return nullptr; }
    name_parts.push_back(peek_token().text);
    _pos++;
    while (match(TokenType::DOT)) {
        if (not check(TokenType::NAME)) {
            // $push_error("expected name after '.' in type name");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            break;
        }
        name_parts.push_back(peek_token().text);
        _pos++;
    }
    std::vector<std::unique_ptr<TypeNode>> type_args;
    if (match(TokenType::LESS)) {
        if (not parse_typeargs(&type_args)) return nullptr;
    }
    return make_node<NominalTypeNode>(std::move(name_parts), std::move(type_args));
}

std::unique_ptr<TypeNode> Parser::parse_function_type()
{
    consume(TokenType::FUNCTION);
    auto node = make_node<FunctionTypeNode>();
    if (match(TokenType::LESS)) { parse_generic_list(&node->type_parameters); }
    consume(TokenType::L_PAREN);
    if (not check(TokenType::R_PAREN)) {
        while (true) {
            FunctionTypeNode::ParameterType param;
            if (check(TokenType::NAME) or check(TokenType::VAR_ARG)) {
                if (peek_token(1).type == TokenType::QUESTION or peek_token(1).type == TokenType::COLON) {
                    param.name = peek_token().text;
                    _pos++;
                    param.is_optional = false;
                    if (match(TokenType::QUESTION)) param.is_optional = true;
                    consume(TokenType::COLON);
                } else {
                    param.name.reset();
                    param.is_optional = false;
                }
                param.type = parse_type();
            } else if (match(TokenType::QUESTION)) {
                param.name.reset();
                param.is_optional = true;
                param.type = parse_type();
            } else {
                param.name.reset();
                param.is_optional = false;
                param.type = parse_type();
            }
            node->parameters.push_back(std::move(param));
            if (not match(TokenType::COMMA)) break;
        }
    }
    consume(TokenType::R_PAREN);
    if (match(TokenType::COLON)) {
        bool var_arg = false;
        node->return_types = parse_return_type_list(&var_arg);
        node->varadict_return = var_arg;
    }
    return node;
}

std::vector<std::unique_ptr<TypeNode>> Parser::parse_type_list()
{
    std::vector<std::unique_ptr<TypeNode>> types;
    auto first = parse_type();
    if (first) types.push_back(std::move(first));
    else types.push_back(make_node<BasicTypeNode>("nil"));
    while (match(TokenType::COMMA)) {
        auto t = parse_type();
        if (t) types.push_back(std::move(t));
        else types.push_back(make_node<BasicTypeNode>("nil"));
    }
    return types;
}

std::vector<FunctionTypeNode::ParameterType> Parser::parse_param_type_list() { return {}; }

std::vector<std::unique_ptr<TypeNode>> Parser::parse_return_type_list(bool *var_arg)
{
    std::vector<std::unique_ptr<TypeNode>> types;
    *var_arg = false;
    if (match(TokenType::L_PAREN)) {
        if (not check(TokenType::R_PAREN)) {
            types = parse_type_list();
            if (match(TokenType::VAR_ARG)) *var_arg = true;
        }
        consume(TokenType::R_PAREN);
    } else {
        types = parse_type_list();
        if (match(TokenType::VAR_ARG)) *var_arg = true;
    }
    return types;
}

std::unique_ptr<RecordBody> Parser::parse_record_body()
{
    auto rb = make_node<RecordBody>();
    if (match(TokenType::LESS)) {
        parse_generic_list(&rb->type_parameters);
        // if (not check(TokenType::NAME)) {
        //     $push_error("expected type parameter name");
        // }
        // while (check(TokenType::NAME)) {
        //     rb->type_parameters.push_back(peek_token().text);
        //     _pos++;
        //     if (not match(TokenType::COMMA)) break;
        // }
        // consume(TokenType::GREATER);
    }
    if (match(TokenType::IS)) { parse_interface_list(rb.get()); }
    if (match(TokenType::WHERE)) {
        const Token &tk = peek_token();
        rb->where_clause = parse_expression();
        // if (not rb->where_clause) { $push_error("expected expression after 'where'"); }
        if (not rb->where_clause) push_error(ExpectedExpression(tk));
    }
    while (not check(TokenType::END) and not is_at_end()) {
        RecordBody::Entry entry;
        if (check(TokenType::NAME) and peek_token().text == "userdata") {
            _pos++;
            entry.entry_kind = RecordBody::Entry::Kind::USERDATA;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (check(TokenType::TYPE) and peek_token(1).type != TokenType::COLON) {
            consume(TokenType::TYPE);
            if (not check(TokenType::NAME)) {
                // $push_error("expected name after 'type'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                entry.type_name = peek_token().text;
                _pos++;
            }
            consume(TokenType::ASSIGN);
            entry.type_value = parse_type();
            entry.entry_kind = RecordBody::Entry::Kind::TYPE_ALIAS;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::RECORD)) {
            if (not check(TokenType::NAME)) {
                // $push_error(std::format("expected name after 'record', got {}", peek_token()));
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                entry.nested_name = peek_token().text;
                _pos++;
            }
            entry.nested_body = parse_record_body();
            entry.entry_kind = RecordBody::Entry::Kind::RECORD;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::ENUM)) {
            if (not check(TokenType::NAME)) {
                // $push_error("expected name after 'enum'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                entry.nested_name = peek_token().text;
                _pos++;
            }
            entry.nested_body = parse_enum_body();
            entry.entry_kind = RecordBody::Entry::Kind::ENUM;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::INTERFACE)) {
            if (not check(TokenType::NAME)) {
                // $push_error(std::format("expected name after 'interface', got {}", peek_token()));
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                entry.nested_name = peek_token().text;
                _pos++;
            }
            entry.nested_body = parse_record_body();
            entry.entry_kind = RecordBody::Entry::Kind::INTERFACE;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        bool is_meta = false;
        if (check(TokenType::NAME) and peek_token().text == "metamethod") {
            _pos++;
            is_meta = true;
        }
        if (check(TokenType::NAME)) {
            entry.name = peek_token().text;
            _pos++;
        } else if (match(TokenType::L_BRACKET)) {
            if (not check(TokenType::STRING)) {
                // $push_error("expected literal string key in record field");
                push_error(ExpectedToken(TokenType::STRING, peek_token()));
            } else {
                entry.key_literal = peek_token().text;
                _pos++;
            }
            consume(TokenType::R_BRACKET);
        } else {
            if (check(TokenType::END) or is_at_end()) break;
            // $push_error(std::format("unexpected token `{}` in record body", peek_token()));
            push_error(UnexpectedToken(peek_token()));
            _pos++;
            continue;
        }
        consume(TokenType::COLON);
        entry.type = parse_type();
        entry.is_metamethod = is_meta;
        entry.entry_kind = RecordBody::Entry::Kind::FIELD;
        rb->entries.push_back(std::move(entry));
    }
    consume(TokenType::END);
    return rb;
}

std::unique_ptr<EnumBody> Parser::parse_enum_body()
{
    auto body = make_node<EnumBody>();
    while (not check(TokenType::END) and not is_at_end()) {
        if (check(TokenType::STRING)) {
            body->elements.push_back(peek_token().text);
            _pos++;
            if (match(TokenType::COMMA) or match(TokenType::SEMICOLON)) { continue; }
        } else if (check(TokenType::END)) {
            break;
        } else {
            // $push_error("expected string in enum");
            push_error(ExpectedToken(TokenType::STRING, peek_token()));
            _pos++;
        }
    }
    consume(TokenType::END);
    return body;
}

void Parser::parse_interface_list(RecordBody *rb)
{
    if (match(TokenType::L_BRACE)) {
        rb->structural_ext = parse_type();
        consume(TokenType::R_BRACE);
        if (match(TokenType::COMMA)) {
            do {
                auto nom = parse_nominal_type();
                if (nom) rb->interface_ext.push_back(std::move(nom));
                else {
                    // $push_error("expected interface name");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    break;
                }
            } while (match(TokenType::COMMA));
        }
    } else {
        auto nom = parse_nominal_type();
        if (nom) rb->interface_ext.push_back(std::move(nom));
        else push_error(ExpectedToken(TokenType::NAME, peek_token()));
        while (match(TokenType::COMMA)) {
            auto nom2 = parse_nominal_type();
            if (nom2) rb->interface_ext.push_back(std::move(nom2));
            else {
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                break;
            }
        }
    }
}
