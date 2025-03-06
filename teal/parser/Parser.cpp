#include "Parser.hpp"
#include "AST.hpp"
#include "Lexer.hpp"
#include <format>
#include <cassert>

using namespace teal;

template<>
struct std::formatter<Token> {
    constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
    constexpr auto format(const Token &tk, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "Token {{.kind=\"{}\", .at={}:{}}}", tk.toString(), tk.line, tk.col);
    }
};

template<>
struct std::formatter<TokenType> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
    constexpr auto format(const TokenType &tk, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{}", Token::typeToString(tk));
    }
};

std::unique_ptr<Block> Parser::parse_chunk() {
    auto block = std::make_unique<Block>();
    while (not is_at_end()) {
        if (check(TokenType::END_OF_FILE)) break;
        auto stmt = parse_stat();
        if (stmt) block->statements.push_back(std::move(stmt));
    }
    return block;
}

std::unique_ptr<Statement> Parser::parse_stat() {
    if (match(TokenType::OP_SEMICOLON)) {
        return nullptr;
    }
    TokenType t = peek_token().type;
    switch (t) {
    case TokenType::K_IF: return parse_if();
    case TokenType::K_WHILE: return parse_while();
    case TokenType::K_REPEAT: return parse_repeat();
    case TokenType::K_FOR: return parse_for();
    case TokenType::K_DO: return parse_do();
    case TokenType::K_FUNCTION:
        pos_++;
        return parse_function_decl(false, false);
    case TokenType::K_LOCAL:
        pos_++;
        if (match(TokenType::K_FUNCTION)) {
            return parse_function_decl(true, false);
        } else if (match(TokenType::K_RECORD)) {
            return parse_record_decl(true, false, false);
        } else if (match(TokenType::K_INTERFACE)) {
            return parse_record_decl(true, false, true);
        } else if (match(TokenType::K_ENUM)) {
            return parse_enum_decl(true, false);
        } else if (match(TokenType::K_TYPE)) {
            return parse_type_alias_decl(true, false);
        } else {
            return parse_var_decl(true, false);
        }
    case TokenType::K_GLOBAL:
        pos_++;
        if (match(TokenType::K_FUNCTION)) {
            return parse_function_decl(false, true);
        } else if (match(TokenType::K_RECORD)) {
            return parse_record_decl(false, true, false);
        } else if (match(TokenType::K_INTERFACE)) {
            return parse_record_decl(false, true, true);
        } else if (match(TokenType::K_ENUM)) {
            return parse_enum_decl(false, true);
        } else if (match(TokenType::K_TYPE)) {
            return parse_type_alias_decl(false, true);
        } else {
            return parse_var_decl(false, true);
        }
    case TokenType::K_RETURN: {
        pos_++;
        auto ret_stmt = std::make_unique<ReturnStatement>();
        if (not check(TokenType::OP_SEMICOLON) and not check(TokenType::K_END) and not check(TokenType::K_ELSE) and not check(TokenType::K_ELSEIF) and not check(TokenType::K_UNTIL) and not check(TokenType::END_OF_FILE)) {
            ret_stmt->values = parse_expression_list();
        }
        match(TokenType::OP_SEMICOLON);
        return ret_stmt;
    }
    case TokenType::K_BREAK:
        pos_++;
        return std::make_unique<BreakStatement>();
    case TokenType::K_GOTO: {
        pos_++;
        if (not check(TokenType::NAME)) {
            push_error("expected label name after 'goto'");
            return nullptr;
        }
        std::string label_name = peek_token().text;
        pos_++;
        return std::make_unique<GotoStatement>(label_name);
    }
    case TokenType::OP_DOUBLE_COLON:
        return parse_label();
    default:
        return parse_assignment_or_call();
    }
}

std::unique_ptr<Statement> Parser::parse_assignment_or_call() {
    auto prefix = parse_prefix_expression();
    if (not prefix) {
        skip_to_next_statement();
        return nullptr;
    }
    if (check(TokenType::OP_ASSIGN) or check(TokenType::OP_COMMA)) {
        if (dynamic_cast<FunctionCallExpression*>(prefix.get())) {
            push_error("cannot assign to function call");
            prefix = std::make_unique<NameExpression>("_error_");
        }
        auto assign = std::make_unique<AssignmentStatement>();
        assign->lhs.push_back(std::move(prefix));
        while (match(TokenType::OP_COMMA)) {
            auto var = parse_var_expression();
            if (not var) {
                push_error("expected variable");
                if (check(TokenType::OP_COMMA)) { pos_++; continue; }
                break;
            }
            assign->lhs.push_back(std::move(var));
        }
        consume(TokenType::OP_ASSIGN, "expected '=' in assignment");
        assign->rhs = parse_expression_list();
        if (assign->rhs.empty()) {
            push_error("expected expression after '='");
        }
        return assign;
    } else {
        auto call_expression = dynamic_cast<FunctionCallExpression*>(prefix.get());
        if (not call_expression) {
            push_error("unexpected expression statement");
            skip_to_next_statement();
            return nullptr;
        }
        std::unique_ptr<FunctionCallExpression> call_node(static_cast<FunctionCallExpression*>(prefix.release()));
        return std::make_unique<CallStatement>(std::move(call_node));
    }
}

std::unique_ptr<Statement> Parser::parse_label() {
    consume(TokenType::OP_DOUBLE_COLON, "internal error: '::' expected for label");
    if (not check(TokenType::NAME)) {
        push_error("expected label name after '::'");
        return nullptr;
    }
    std::string name = peek_token().text;
    pos_++;
    consume(TokenType::OP_DOUBLE_COLON, "expected '::' after label name");
    return std::make_unique<LabelStatement>(name);
}

std::unique_ptr<Statement> Parser::parse_if() {
    consume(TokenType::K_IF, "internal error: 'if' expected");
    auto if_stmt = std::make_unique<IfStatement>();
    auto cond = parse_expression();
    if (not cond) {
        push_error("expected condition after 'if'");
    }
    consume(TokenType::K_THEN, "expected 'then' after condition");
    auto then_block = std::make_unique<Block>();
    while (not check(TokenType::K_END) and not check(TokenType::K_ELSE) and not check(TokenType::K_ELSEIF) and not is_at_end()) {
        auto st = parse_stat();
        if (st) then_block->statements.push_back(std::move(st));
        if (check(TokenType::K_END) or check(TokenType::K_ELSE) or check(TokenType::K_ELSEIF) or check(TokenType::K_UNTIL) or check(TokenType::END_OF_FILE))
            break;
    }
    if_stmt->if_branches.push_back({ std::move(cond), std::move(then_block) });
    while (match(TokenType::K_ELSEIF)) {
        auto elseif_cond = parse_expression();
        if (not elseif_cond) {
            push_error("expected condition after 'elseif'");
        }
        consume(TokenType::K_THEN, "expected 'then' after condition");
        auto elseif_block = std::make_unique<Block>();
        while (not check(TokenType::K_END) and not check(TokenType::K_ELSE) and not check(TokenType::K_ELSEIF) and not is_at_end()) {
            auto st = parse_stat();
            if (st) elseif_block->statements.push_back(std::move(st));
            if (check(TokenType::K_END) or check(TokenType::K_ELSE) or check(TokenType::K_ELSEIF) or check(TokenType::K_UNTIL) or check(TokenType::END_OF_FILE))
                break;
        }
        if_stmt->if_branches.push_back({ std::move(elseif_cond), std::move(elseif_block) });
    }
    if (match(TokenType::K_ELSE)) {
        auto else_block = std::make_unique<Block>();
        while (not check(TokenType::K_END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) else_block->statements.push_back(std::move(st));
            if (check(TokenType::K_END) or check(TokenType::K_UNTIL) or check(TokenType::END_OF_FILE))
                break;
        }
        if_stmt->else_block = std::move(else_block);
    }
    consume(TokenType::K_END, "expected 'end' to close 'if'");
    return if_stmt;
}

std::unique_ptr<Statement> Parser::parse_while() {
    consume(TokenType::K_WHILE, "internal error: 'while' expected");
    auto while_stmt = std::make_unique<WhileStatement>();
    while_stmt->condition = parse_expression();
    if (not while_stmt->condition) {
        push_error("expected condition after 'while'");
    }
    consume(TokenType::K_DO, "expected 'do' after condition");
    auto body_block = std::make_unique<Block>();
    while (not check(TokenType::K_END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::K_END) or check(TokenType::K_UNTIL) or check(TokenType::END_OF_FILE))
            break;
    }
    consume(TokenType::K_END, "expected 'end' to close 'while'");
    while_stmt->body = std::move(body_block);
    return while_stmt;
}

std::unique_ptr<Statement> Parser::parse_repeat() {
    consume(TokenType::K_REPEAT, "internal error: 'repeat' expected");
    auto repeat_stmt = std::make_unique<RepeatStatement>();
    auto body_block = std::make_unique<Block>();
    while (not check(TokenType::K_UNTIL) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::K_UNTIL) or check(TokenType::END_OF_FILE))
            break;
    }
    consume(TokenType::K_UNTIL, "expected 'until' after 'repeat' block");
    repeat_stmt->body = std::move(body_block);
    repeat_stmt->condition = parse_expression();
    if (not repeat_stmt->condition) {
        push_error("expected condition after 'until'");
    }
    return repeat_stmt;
}

std::unique_ptr<Statement> Parser::parse_for() {
    consume(TokenType::K_FOR, "internal error: 'for' expected");
    if (not check(TokenType::NAME)) {
        push_error("expected identifier after 'for'");
        skip_to_next_statement();
        return nullptr;
    }
    std::string var_name = peek_token().text;
    pos_++;
    if (match(TokenType::OP_ASSIGN)) {
        auto for_num = std::make_unique<ForNumericStatement>();
        for_num->var_name = var_name;
        for_num->start_exp = parse_expression();
        consume(TokenType::OP_COMMA, "expected ',' after start value");
        for_num->end_exp = parse_expression();
        if (match(TokenType::OP_COMMA)) {
            for_num->step_exp = parse_expression();
        }
        consume(TokenType::K_DO, "expected 'do' in numeric for");
        auto body_block = std::make_unique<Block>();
        while (not check(TokenType::K_END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) body_block->statements.push_back(std::move(st));
            if (check(TokenType::K_END) or check(TokenType::END_OF_FILE))
                break;
        }
        consume(TokenType::K_END, "expected 'end' to close 'for'");
        for_num->body = std::move(body_block);
        return for_num;
    } else {
        std::vector<std::string> name_list;
        name_list.push_back(var_name);
        while (match(TokenType::OP_COMMA)) {
            if (not check(TokenType::NAME)) {
                push_error("expected name in for-in loop");
                break;
            }
            name_list.push_back(peek_token().text);
            pos_++;
        }
        consume(TokenType::K_IN, "expected 'in' in for loop");
        auto exprs = parse_expression_list();
        consume(TokenType::K_DO, "expected 'do' in for loop");
        auto body_block = std::make_unique<Block>();
        while (not check(TokenType::K_END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) body_block->statements.push_back(std::move(st));
            if (check(TokenType::K_END) or check(TokenType::END_OF_FILE))
                break;
        }
        consume(TokenType::K_END, "expected 'end' to close 'for'");
        auto for_in = std::make_unique<ForInStatement>();
        for_in->names = std::move(name_list);
        for_in->exprs = std::move(exprs);
        for_in->body = std::move(body_block);
        return for_in;
    }
}

std::unique_ptr<Statement> Parser::parse_do() {
    consume(TokenType::K_DO, "internal error: 'do' expected");
    auto block_node = std::make_unique<Block>();
    while (not check(TokenType::K_END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) block_node->statements.push_back(std::move(st));
        if (check(TokenType::K_END) or check(TokenType::END_OF_FILE))
            break;
    }
    consume(TokenType::K_END, "expected 'end' to close 'do' block");
    return std::make_unique<DoStatement>(std::move(block_node));
}

std::unique_ptr<Statement> Parser::parse_function_decl(bool is_local, bool is_global) {
    if (not check(TokenType::NAME)) {
        push_error("expected function name after 'function'");
    }
    std::vector<std::string> name_path;
    std::string method_name;
    bool is_method = false;
    if (check(TokenType::NAME)) {
        name_path.push_back(peek_token().text);
        pos_++;
    }
    while (true) {
        if (match(TokenType::OP_DOT)) {
            if (not check(TokenType::NAME)) {
                push_error("expected name after '.' in function name");
                break;
            }
            name_path.push_back(peek_token().text);
            pos_++;
        } else if (match(TokenType::OP_COLON)) {
            is_method = true;
            if (not check(TokenType::NAME)) {
                push_error("expected name after ':' in function name");
            } else {
                method_name = peek_token().text;
                pos_++;
            }
            break;
        } else {
            break;
        }
    }
    auto func_body = std::make_unique<FunctionBody>();
    if (match(TokenType::OP_LESS)) {
        parse_generic_list(func_body->type_params);
    }
    consume(TokenType::OP_LPAREN, "expected '(' after function name");
    if (not check(TokenType::OP_RPAREN)) {
        while (true) {
            if (check(TokenType::OP_VARARG)) {
                pos_++;
                std::unique_ptr<TypeNode> var_type;
                if (match(TokenType::OP_COLON)) {
                    var_type = parse_type();
                }
                func_body->params.push_back({ "...", true, false, std::move(var_type) });
                break;
            }
            if (not check(TokenType::NAME)) {
                push_error("expected parameter name or '...'");
                if (check(TokenType::OP_RPAREN)) break;
                pos_++;
            } else {
                std::string param_name = peek_token().text;
                pos_++;
                bool opt_flag = false;
                if (match(TokenType::OP_QUESTION)) opt_flag = true;
                std::unique_ptr<TypeNode> type_ann;
                if (match(TokenType::OP_COLON)) {
                    type_ann = parse_type();
                }
                func_body->params.push_back({ param_name, false, opt_flag, std::move(type_ann) });
            }
            if (not match(TokenType::OP_COMMA)) break;
        }
    }
    consume(TokenType::OP_RPAREN, "expected ')' after parameters");
    if (match(TokenType::OP_COLON)) {
        bool ret_var_arg = false;
        func_body->return_types = parse_return_type_list(ret_var_arg);
        func_body->return_var_arg = ret_var_arg;
    }
    auto body_block = std::make_unique<Block>();
    while (not check(TokenType::K_END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::K_END) or check(TokenType::END_OF_FILE))
            break;
    }
    consume(TokenType::K_END, "expected 'end' to close function");
    func_body->body = std::move(body_block);
    auto func_decl = std::make_unique<FunctionDeclarationStatement>(is_local, is_global);
    func_decl->name_path = std::move(name_path);
    func_decl->method_name = method_name;
    func_decl->is_method = is_method;
    func_decl->body = std::move(func_body);
    return func_decl;
}

std::unique_ptr<Statement> Parser::parse_var_decl(bool is_local, bool is_global) {
    auto var_stmt = std::make_unique<VariableDeclarationStatement>(is_local, is_global);
    auto names = parse_att_name_list();
    if (names.empty()) {
        push_error("expected variable name");
    }
    var_stmt->names = std::move(names);
    if (match(TokenType::OP_COLON)) {
        var_stmt->types = parse_type_list();
    }
    if (match(TokenType::OP_ASSIGN)) {
        var_stmt->values = parse_expression_list();
    }
    if (not is_local and is_global) {
        if (var_stmt->types.empty() and var_stmt->values.empty()) {
            push_error("global variable must have type or initial value");
        }
    }
    return var_stmt;
}

std::unique_ptr<Statement> Parser::parse_record_decl(bool is_local, bool is_global, bool is_interface) {
    if (not check(TokenType::NAME)) {
        push_error(std::format("expected name after `{}` got {}", is_interface ? "interface" : "record", peek_token()));
        return nullptr;
    }
    std::string name = peek_token().text;
    pos_++;
    auto body = parse_record_body();
    return std::make_unique<RecordDeclarationStatement>(is_interface, is_local, is_global, name, std::move(body));
}

std::unique_ptr<Statement> Parser::parse_enum_decl(bool is_local, bool is_global) {
    if (not check(TokenType::NAME)) {
        push_error("expected name after 'enum'");
        return nullptr;
    }
    std::string name = peek_token().text;
    pos_++;
    auto body = parse_enum_body();
    return std::make_unique<EnumDeclarationStatement>(is_local, is_global, name, std::move(body));
}

std::unique_ptr<Statement> Parser::parse_type_alias_decl(bool is_local, bool is_global) {
    if (not check(TokenType::NAME)) {
        push_error("expected name after 'type'");
        return nullptr;
    }
    std::string name = peek_token().text;
    pos_++;
    std::unique_ptr<TypeNode> type_value;
    if (match(TokenType::OP_ASSIGN)) {
        if (check(TokenType::K_RECORD)) {
            pos_++;
            type_value = std::make_unique<TypeRecordNode>(parse_record_body());
        } else if (check(TokenType::K_ENUM)) {
            pos_++;
            type_value = std::make_unique<TypeEnumNode>(parse_enum_body()->elements);
        } else {
            type_value = parse_type();
        }
    } else {
        if (is_local) {
            push_error("expected '=' in local type alias");
        }
    }
    return std::make_unique<TypeAliasStatement>(is_local, is_global, name, std::move(type_value));
}

std::vector<VariableDeclarationStatement::NameAttrib> Parser::parse_att_name_list() {
    std::vector<VariableDeclarationStatement::NameAttrib> list;
    if (not check(TokenType::NAME)) {
        return list;
    }
    do {
        VariableDeclarationStatement::NameAttrib na;
        na.name = peek_token().text;
        pos_++;
        if (match(TokenType::OP_LESS)) {
            if (not check(TokenType::NAME)) {
                push_error("expected attribute name in '< >'");
            } else {
                na.attrib = peek_token().text;
                pos_++;
            }
            consume(TokenType::OP_GREATER, "expected '>' after attribute");
        }
        list.push_back(std::move(na));
    } while (match(TokenType::OP_COMMA));
    return list;
}

std::vector<std::string> Parser::parse_name_list() {
    std::vector<std::string> list;
    if (not check(TokenType::NAME)) {
        return list;
    }
    list.push_back(peek_token().text);
    pos_++;
    while (match(TokenType::OP_COMMA)) {
        if (not check(TokenType::NAME)) {
            push_error("expected name after ','");
            break;
        }
        list.push_back(peek_token().text);
        pos_++;
    }
    return list;
}

std::unique_ptr<Expression> Parser::parse_expression() {
    return parse_exp_rec(1);
}

std::vector<std::unique_ptr<Expression>> Parser::parse_expression_list() {
    std::vector<std::unique_ptr<Expression>> exprs;
    auto first = parse_expression();
    if (first) exprs.push_back(std::move(first));
    while (match(TokenType::OP_COMMA)) {
        auto e = parse_expression();
        if (e) {
            exprs.push_back(std::move(e));
        } else {
            push_error("expected expression after ','");
            if (not check(TokenType::OP_COMMA)) break;
        }
    }
    return exprs;
}

std::unique_ptr<Expression> Parser::parse_prefix_expression() {
    std::unique_ptr<Expression> base;
    if (match(TokenType::OP_LPAREN)) {
        base = parse_expression();
        consume(TokenType::OP_RPAREN, "expected ')'");
    } else if (check(TokenType::NAME)) {
        base = std::make_unique<NameExpression>(peek_token().text);
        pos_++;
    } else {
        push_error(std::format("expected '(' or Name, got {}", peek_token()));
        return nullptr;
    }
    while (true) {
        if (match(TokenType::OP_COLON)) {
            if (not check(TokenType::NAME)) {
                push_error("expected method name after ':'");
                break;
            }
            std::string method = peek_token().text;
            pos_++;
            std::vector<std::unique_ptr<Expression>> args;
            if (match(TokenType::OP_LPAREN)) {
                if (not check(TokenType::OP_RPAREN)) {
                    args = parse_expression_list();
                }
                consume(TokenType::OP_RPAREN, "expected ')' after arguments");
            } else if (check(TokenType::OP_LBRACE)) {
                args.push_back(parse_table_constructor());
            } else if (check(TokenType::STRING)) {
                args.push_back(std::make_unique<StringExpression>(peek_token().text));
                pos_++;
            } else {
                push_error("expected arguments after method call");
            }
            auto call = std::make_unique<FunctionCallExpression>(std::move(base), method);
            for (auto &arg : args) call->args.push_back(std::move(arg));
            base = std::move(call);
        } else if (match(TokenType::OP_LPAREN)) {
            std::vector<std::unique_ptr<Expression>> args;
            if (not check(TokenType::OP_RPAREN)) {
                args = parse_expression_list();
            }
            consume(TokenType::OP_RPAREN, "expected ')'");
            auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
            for (auto &arg : args) call->args.push_back(std::move(arg));
            base = std::move(call);
        } else if (check(TokenType::OP_LBRACE)) {
            auto table_arg = parse_table_constructor();
            auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
            call->args.push_back(std::move(table_arg));
            base = std::move(call);
        } else if (check(TokenType::STRING)) {
            std::string lit = peek_token().text;
            pos_++;
            auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
            call->args.push_back(std::make_unique<StringExpression>(lit));
            base = std::move(call);
        } else if (match(TokenType::OP_DOT)) {
            if (not check(TokenType::NAME)) {
                bool ok = false;
                const auto mkname = [this](const std::string_view &id) {
                    return Token { TokenType::NAME, std::string(id), peek_token().line, peek_token().col };
                };
                switch (peek_token().type) {
                case TokenType::K_TYPE: _tokens[pos_] = mkname("type"); ok = true; break;
                case TokenType::K_RECORD: _tokens[pos_] = mkname("record"); ok = true; break;
                case TokenType::K_ENUM: _tokens[pos_] = mkname("enum"); ok = true; break;
                case TokenType::K_INTERFACE: _tokens[pos_] = mkname("interface"); ok = true; break;
                default: ok = false; break;
                }
                if (not ok) {
                    push_error(std::format("expected field name after '.', got `{}`", peek_token().type));
                    break;
                }
            }
            std::string field = peek_token().text;
            pos_++;
            base = std::make_unique<FieldExpression>(std::move(base), field);
        } else if (match(TokenType::OP_LBRACKET)) {
            auto index_expression = parse_expression();
            consume(TokenType::OP_RBRACKET, "expected ']'");
            base = std::make_unique<IndexExpression>(std::move(base), std::move(index_expression));
        } else {
            break;
        }
    }
    return base;
}

std::unique_ptr<Expression> Parser::parse_var_expression() {
    auto expr = parse_prefix_expression();
    if (not expr) return nullptr;
    if (dynamic_cast<FunctionCallExpression*>(expr.get())) {
        push_error("unexpected function call in assignment");
        return std::make_unique<NameExpression>("_error_");
    }
    return expr;
}

std::unique_ptr<Expression> Parser::parse_primary_expression() {
    TokenType t = peek_token().type;
    if (t == TokenType::K_NIL) { pos_++; return std::make_unique<NilExpression>(); }
    if (t == TokenType::K_TRUE) { pos_++; return std::make_unique<BooleanExpression>(true); }
    if (t == TokenType::K_FALSE) { pos_++; return std::make_unique<BooleanExpression>(false); }
    if (t == TokenType::NUMBER) { std::string num = peek_token().text; pos_++; return std::make_unique<NumberExpression>(num); }
    if (t == TokenType::STRING) { std::string str = peek_token().text; pos_++; return std::make_unique<StringExpression>(str); }
    if (t == TokenType::OP_VARARG) { pos_++; return std::make_unique<VarargExpression>(); }
    if (t == TokenType::K_FUNCTION) {
        return parse_function_def_expression();
    }
    if (t == TokenType::OP_LBRACE) {
        return parse_table_constructor();
    }
    if (t == TokenType::NAME or t == TokenType::OP_LPAREN or Token::type_is_teal_keyword(t)) {
        return parse_prefix_expression();
    }
    push_error(std::format("unexpected token `{}` (type: {}) in expression", peek_token().text, peek_token().type));
    return nullptr;
}

std::unique_ptr<Expression> Parser::parse_exp_rec(int min_prec) {
    auto left = parse_unary_expression();
    while (true) {
        TokenType op_type = peek_token().type;
        int prec = get_binary_precedence(op_type);
        if (prec < min_prec) break;
        if (op_type == TokenType::K_AS) {
            pos_++;
            std::vector<std::unique_ptr<TypeNode>> cast_types;
            if (match(TokenType::OP_LPAREN)) {
                if (not check(TokenType::OP_RPAREN)) {
                    cast_types = parse_type_list();
                    if (match(TokenType::OP_VARARG)) {
                    }
                }
                consume(TokenType::OP_RPAREN, "expected ')'");
            } else {
                auto type_node = parse_type();
                if (type_node) cast_types.push_back(std::move(type_node));
            }
            left = std::make_unique<CastExpression>(std::move(left), std::move(cast_types));
            continue;
        } else if (op_type == TokenType::K_IS) {
            pos_++;
            auto type_node = parse_type();
            left = std::make_unique<IsTypeExpression>(std::move(left), std::move(type_node));
            continue;
        }
        bool right_assoc = is_right_associative(op_type);
        int next_min_prec = (right_assoc ? prec : prec + 1);
        pos_++;
        auto right = parse_exp_rec(next_min_prec);
        left = std::make_unique<BinaryOperationExpression>(op_type, std::move(left), std::move(right));
    }
    return left;
}

int Parser::get_binary_precedence(TokenType op) {
    switch (op) {
    case TokenType::K_OR: return 1;
    case TokenType::K_AND: return 2;
    case TokenType::K_IS: return 3;
    case TokenType::OP_LESS: case TokenType::OP_LESS_EQ:
    case TokenType::OP_GREATER: case TokenType::OP_GREATER_EQ:
    case TokenType::OP_EQUALS: case TokenType::OP_NOT_EQ: return 4;
    case TokenType::OP_BIT_OR: return 5;
    case TokenType::OP_BIT_XOR: return 6;
    case TokenType::OP_BIT_AND: return 7;
    case TokenType::OP_SHIFT_L: case TokenType::OP_SHIFT_R: return 8;
    case TokenType::OP_CONCAT: return 9;
    case TokenType::OP_ADD: case TokenType::OP_SUB: return 10;
    case TokenType::OP_MUL: case TokenType::OP_DIV:
    case TokenType::OP_FLOOR_DIV: case TokenType::OP_MOD: return 11;
    case TokenType::OP_POW: return 13;
    case TokenType::K_AS: return 14;
    default: return -1;
    }
}

bool Parser::is_right_associative(TokenType op) {
    return (op == TokenType::OP_CONCAT or op == TokenType::OP_POW);
}

std::unique_ptr<Expression> Parser::parse_unary_expression() {
    if (check(TokenType::K_NOT) or check(TokenType::OP_SUB) or check(TokenType::OP_LEN) or check(TokenType::OP_BIT_XOR)) {
        TokenType op_token = peek_token().type;
        pos_++;
        auto operand = parse_unary_expression();
        return std::make_unique<UnaryOperationExpression>(op_token, std::move(operand));
    }
    return parse_primary_expression();
}

bool Parser::parse_generic_list(std::vector<GenericTypeParameter>& t_params) {
    if (not check(TokenType::NAME)) {
        push_error("expected type parameter name");
        return false;
    }
    while (check(TokenType::NAME)) {
        GenericTypeParameter param = { peek_token().text, std::nullopt };
        pos_++;
        if (match(TokenType::K_IS) and match(TokenType::NAME))
            param.is = peek_token().text;
        t_params.push_back(param);
        if (not match(TokenType::OP_COMMA)) break;
    }
    return (bool)consume(TokenType::OP_GREATER, "expected '>' after type parameters");
}

std::unique_ptr<Expression> Parser::parse_function_def_expression() {
    consume(TokenType::K_FUNCTION, "internal error: 'function' expected");
    auto func_body = std::make_unique<FunctionBody>();
    if (match(TokenType::OP_LESS)) {
        parse_generic_list(func_body->type_params);
    }
    consume(TokenType::OP_LPAREN, "expected '(' in function literal");
    if (not check(TokenType::OP_RPAREN)) {
        while (true) {
            if (check(TokenType::OP_VARARG)) {
                pos_++;
                std::unique_ptr<TypeNode> var_type;
                if (match(TokenType::OP_COLON)) {
                    var_type = parse_type();
                }
                func_body->params.push_back({ "...", true, false, std::move(var_type) });
                break;
            }
            if (not check(TokenType::NAME)) {
                push_error("expected parameter name or '...'");
                if (check(TokenType::OP_RPAREN)) break;
                pos_++;
            } else {
                std::string param_name = peek_token().text;
                pos_++;
                bool opt_flag = false;
                if (match(TokenType::OP_QUESTION)) opt_flag = true;
                std::unique_ptr<TypeNode> type_ann;
                if (match(TokenType::OP_COLON)) {
                    type_ann = parse_type();
                }
                func_body->params.push_back({ param_name, false, opt_flag, std::move(type_ann) });
            }
            if (not match(TokenType::OP_COMMA)) break;
        }
    }
    consume(TokenType::OP_RPAREN, "expected ')' in function literal");
    if (match(TokenType::OP_COLON)) {
        bool ret_var_arg = false;
        func_body->return_types = parse_return_type_list(ret_var_arg);
        func_body->return_var_arg = ret_var_arg;
    }
    auto body_block = std::make_unique<Block>();
    while (not check(TokenType::K_END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(std::move(st));
        if (check(TokenType::K_END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::K_END, "expected 'end' to close function");
    func_body->body = std::move(body_block);
    return std::make_unique<FunctionDefinitionExpression>(std::move(func_body));
}

std::unique_ptr<Expression> Parser::parse_table_constructor() {
    consume(TokenType::OP_LBRACE, "internal error: '{' expected");
    auto table = std::make_unique<TableConstructorExpression>();
    if (not check(TokenType::OP_RBRACE)) {
        while (true) {
            TableConstructorExpression::Field field;
            if (match(TokenType::OP_LBRACKET)) {
                auto key_expression = parse_expression();
                consume(TokenType::OP_RBRACKET, "expected ']'");
                consume(TokenType::OP_ASSIGN, "expected '=' after key");
                field.key_expr = std::move(key_expression);
                field.value = parse_expression();
            } else if (check(TokenType::NAME)) {
                std::string name = peek_token().text;
                if (pos_ + 1 < tokens_.size() and (tokens_[pos_ + 1].type == TokenType::OP_COLON or tokens_[pos_ + 1].type == TokenType::OP_ASSIGN)) {
                    pos_++;
                    field.name_key = name;
                    if (match(TokenType::OP_COLON)) {
                        field.type_ann = parse_type();
                    }
                    consume(TokenType::OP_ASSIGN, "expected '=' after field name");
                    field.value = parse_expression();
                } else {
                    field.value = parse_expression();
                }
            } else {
                field.value = parse_expression();
            }
            table->fields.push_back(std::move(field));
            if (match(TokenType::OP_COMMA) or match(TokenType::OP_SEMICOLON)) {
                if (check(TokenType::OP_RBRACE)) break;
                continue;
            } else {
                break;
            }
        }
    }
    consume(TokenType::OP_RBRACE, "expected '}'");
    return table;
}

std::unique_ptr<TypeNode> Parser::parse_type() {
    std::unique_ptr<TypeNode> first_type;
    if (match(TokenType::OP_LPAREN)) {
        first_type = parse_type();
        consume(TokenType::OP_RPAREN, "expected ')'");
    } else {
        first_type = parse_base_type();
    }
    if (not first_type) {
        first_type = std::make_unique<BasicTypeNode>("nil");
    }
    if (check(TokenType::OP_BIT_OR)) {
        auto union_node = std::make_unique<UnionTypeNode>();
        union_node->options.push_back(std::move(first_type));
        while (match(TokenType::OP_BIT_OR)) {
            auto next_type = parse_base_type();
            if (not next_type) next_type = std::make_unique<BasicTypeNode>("nil");
            union_node->options.push_back(std::move(next_type));
        }
        return union_node;
    }
    return first_type;
}

constexpr inline bool is_primitive(const std::string_view& name) {
    return name == "string" or name == "integer" or name == "number" or name == "boolean" or name == "nil";
}

std::unique_ptr<TypeNode> Parser::parse_base_type() {
    if (check(TokenType::NAME)) {
        std::string name = peek_token().text;
        if (is_primitive(name)) {
            pos_++;
            return std::make_unique<BasicTypeNode>(name);
        } else return parse_nominal_type();
    }
    if (check(TokenType::OP_LBRACE)) {
        pos_++;
        auto first_type = parse_type();
        if (not first_type) first_type = std::make_unique<BasicTypeNode>("nil");
        if (match(TokenType::OP_COLON)) {
            auto second_type = parse_type();
            if (not second_type) second_type = std::make_unique<BasicTypeNode>("nil");
            consume(TokenType::OP_RBRACE, "expected '}'");
            auto map_node = std::make_unique<TableTypeNode>();
            map_node->is_map = true;
            map_node->key_type = std::move(first_type);
            map_node->element_types.push_back(std::move(second_type));
            return map_node;
        } else {
            std::vector<std::unique_ptr<TypeNode>> types;
            types.push_back(std::move(first_type));
            while (match(TokenType::OP_COMMA)) {
                auto t = parse_type();
                if (not t) t = std::make_unique<BasicTypeNode>("nil");
                types.push_back(std::move(t));
            }
            consume(TokenType::OP_RBRACE, "expected '}'");
            if (types.size() == 1) {
                auto arr_node = std::make_unique<TableTypeNode>();
                arr_node->is_map = false;
                arr_node->element_types.push_back(std::move(types[0]));
                return arr_node;
            } else {
                auto tuple_node = std::make_unique<TableTypeNode>();
                tuple_node->is_map = false;
                for (auto &tt : types) tuple_node->element_types.push_back(std::move(tt));
                return tuple_node;
            }
        }
    }
    if (check(TokenType::K_FUNCTION)) {
        return parse_function_type();
    }
    push_error(std::format("expected type, got {}", peek_token()));
    return nullptr;
}

std::unique_ptr<TypeNode> Parser::parse_nominal_type() {
    std::vector<std::string> name_parts;
    if (not check(TokenType::NAME)) {
        return nullptr;
    }
    name_parts.push_back(peek_token().text);
    pos_++;
    while (match(TokenType::OP_DOT)) {
        if (not check(TokenType::NAME)) {
            push_error("expected name after '.' in type name");
            break;
        }
        name_parts.push_back(peek_token().text);
        pos_++;
    }
    std::vector<std::string> type_args;
    if (match(TokenType::OP_LESS)) {
        if (not check(TokenType::NAME)) {
            push_error("expected type name in type arguments");
        }
        while (check(TokenType::NAME)) {
            type_args.push_back(peek_token().text);
            pos_++;
            if (not match(TokenType::OP_COMMA)) break;
        }
        consume(TokenType::OP_GREATER, "expected '>' after type arguments");
    }
    return std::make_unique<NominalTypeNode>(name_parts, type_args);
}

std::unique_ptr<TypeNode> Parser::parse_function_type() {
    consume(TokenType::K_FUNCTION, "internal error: 'function' expected");
    auto node = std::make_unique<FunctionTypeNode>();
    if (match(TokenType::OP_LESS)) {
        parse_generic_list(node->type_params);
    }
    consume(TokenType::OP_LPAREN, "expected '(' in function type");
    if (not check(TokenType::OP_RPAREN)) {
        while (true) {
            FunctionTypeNode::ParamType param;
            if (check(TokenType::NAME) or check(TokenType::OP_VARARG)) {
                if (peek_token(1).type == TokenType::OP_QUESTION or peek_token(1).type == TokenType::OP_COLON) {
                    param.name = peek_token().text;
                    pos_++;
                    param.is_optional = false;
                    if (match(TokenType::OP_QUESTION)) param.is_optional = true;
                    consume(TokenType::OP_COLON, "expected ':' after parameter name");
                } else {
                    param.name.reset();
                    param.is_optional = false;
                }
                param.type = parse_type();
            } else if (match(TokenType::OP_QUESTION)) {
                param.name.reset();
                param.is_optional = true;
                param.type = parse_type();
            } else {
                param.name.reset();
                param.is_optional = false;
                param.type = parse_type();
            }
            node->params.push_back(std::move(param));
            if (not match(TokenType::OP_COMMA)) break;
        }
    }
    consume(TokenType::OP_RPAREN, "expected ')'");
    if (match(TokenType::OP_COLON)) {
        bool var_arg = false;
        node->return_types = parse_return_type_list(var_arg);
        node->return_var_arg = var_arg;
    }
    return node;
}

std::vector<std::unique_ptr<TypeNode>> Parser::parse_type_list() {
    std::vector<std::unique_ptr<TypeNode>> types;
    auto first = parse_type();
    if (first) types.push_back(std::move(first));
    else types.push_back(std::make_unique<BasicTypeNode>("nil"));
    while (match(TokenType::OP_COMMA)) {
        auto t = parse_type();
        if (t) types.push_back(std::move(t));
        else types.push_back(std::make_unique<BasicTypeNode>("nil"));
    }
    return types;
}

std::vector<FunctionTypeNode::ParamType> Parser::parse_param_type_list() {
    return {};
}

std::vector<std::unique_ptr<TypeNode>> Parser::parse_return_type_list(bool &var_arg) {
    std::vector<std::unique_ptr<TypeNode>> types;
    var_arg = false;
    if (match(TokenType::OP_LPAREN)) {
        if (not check(TokenType::OP_RPAREN)) {
            types = parse_type_list();
            if (match(TokenType::OP_VARARG)) var_arg = true;
        }
        consume(TokenType::OP_RPAREN, "expected ')'");
    } else {
        types = parse_type_list();
        if (match(TokenType::OP_VARARG)) var_arg = true;
    }
    return types;
}

std::unique_ptr<RecordBody> Parser::parse_record_body() {
    auto rb = std::make_unique<RecordBody>();
    if (match(TokenType::OP_LESS)) {
        if (not check(TokenType::NAME)) {
            push_error("expected type parameter name");
        }
        while (check(TokenType::NAME)) {
            rb->type_params.push_back(peek_token().text);
            pos_++;
            if (not match(TokenType::OP_COMMA)) break;
        }
        consume(TokenType::OP_GREATER, "expected '>' after type parameters");
    }
    if (match(TokenType::K_IS)) {
        parse_interface_list(*rb);
    }
    if (match(TokenType::K_WHERE)) {
        rb->where_clause = parse_expression();
        if (not rb->where_clause) {
            push_error("expected expression after 'where'");
        }
    }
    while (not check(TokenType::K_END) and not is_at_end()) {
        RecordBody::Entry entry;
        if (check(TokenType::NAME) and peek_token().text == "userdata") {
            pos_++;
            entry.kind = RecordBody::Entry::Kind::USERDATA;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (check(TokenType::K_TYPE) and peek_token(1).type != TokenType::OP_COLON) {
            consume(TokenType::K_TYPE, "internal error");
            if (not check(TokenType::NAME)) {
                push_error("expected name after 'type'");
            } else {
                entry.type_name = peek_token().text;
                pos_++;
            }
            consume(TokenType::OP_ASSIGN, "expected '=' after type name");
            entry.type_value = parse_type();
            entry.kind = RecordBody::Entry::Kind::TYPE_ALIAS;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::K_RECORD)) {
            if (not check(TokenType::NAME)) {
                push_error(std::format("expected name after 'record', got {}", peek_token()));
            } else {
                entry.nested_name = peek_token().text;
                pos_++;
            }
            entry.nested_body = parse_record_body();
            entry.kind = RecordBody::Entry::Kind::RECORD;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::K_ENUM)) {
            if (not check(TokenType::NAME)) {
                push_error("expected name after 'enum'");
            } else {
                entry.nested_name = peek_token().text;
                pos_++;
            }
            entry.nested_body = parse_enum_body();
            entry.kind = RecordBody::Entry::Kind::ENUM;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::K_INTERFACE)) {
            if (not check(TokenType::NAME)) {
                push_error(std::format("expected name after 'interface', got {}", peek_token()));
            } else {
                entry.nested_name = peek_token().text;
                pos_++;
            }
            entry.nested_body = parse_record_body();
            entry.kind = RecordBody::Entry::Kind::INTERFACE;
            rb->entries.push_back(std::move(entry));
            continue;
        }
        bool is_meta = false;
        if (check(TokenType::NAME) and peek_token().text == "metamethod") {
            pos_++;
            is_meta = true;
        }
        if (check(TokenType::NAME)) {
            entry.field_name = peek_token().text;
            pos_++;
        } else if (match(TokenType::OP_LBRACKET)) {
            if (not check(TokenType::STRING)) {
                push_error("expected literal string key in record field");
            } else {
                entry.field_key_literal = peek_token().text;
                pos_++;
            }
            consume(TokenType::OP_RBRACKET, "expected ']'");
        } else {
            if (check(TokenType::K_END) or is_at_end()) break;
            push_error(std::format("unexpected token `{}` in record body", peek_token()));
            pos_++;
            continue;
        }
        consume(TokenType::OP_COLON, std::format("expected ':' after record key"));
        entry.field_type = parse_type();
        entry.is_metamethod = is_meta;
        entry.kind = RecordBody::Entry::Kind::FIELD;
        rb->entries.push_back(std::move(entry));
    }
    consume(TokenType::K_END, "expected 'end' to close record");
    return rb;
}

std::unique_ptr<EnumBody> Parser::parse_enum_body() {
    auto body = std::make_unique<EnumBody>();
    while (not check(TokenType::K_END) and not is_at_end()) {
        if (check(TokenType::STRING)) {
            body->elements.push_back(peek_token().text);
            pos_++;
            if (match(TokenType::OP_COMMA) or match(TokenType::OP_SEMICOLON)) {
                continue;
            }
        } else if (check(TokenType::K_END)) {
            break;
        } else {
            push_error("expected string in enum");
            pos_++;
        }
    }
    consume(TokenType::K_END, "expected 'end' to close enum");
    return body;
}

void Parser::parse_interface_list(RecordBody &rb) {
    if (match(TokenType::OP_LBRACE)) {
        rb.structural_ext = parse_type();
        consume(TokenType::OP_RBRACE, "expected '}' in interface list");
        if (match(TokenType::OP_COMMA)) {
            do {
                auto nom = parse_nominal_type();
                if (nom) rb.interface_ext.push_back(std::move(nom));
                else {
                    push_error("expected interface name");
                    break;
                }
            } while (match(TokenType::OP_COMMA));
        }
    } else {
        auto nom = parse_nominal_type();
        if (nom) rb.interface_ext.push_back(std::move(nom));
        else push_error("expected interface name");
        while (match(TokenType::OP_COMMA)) {
            auto nom2 = parse_nominal_type();
            if (nom2) rb.interface_ext.push_back(std::move(nom2));
            else {
                push_error("expected interface name");
                break;
            }
        }
    }
}

