#include "Parser.hpp"
#include "AST.hpp"
#include "Lexer.hpp"
#include <cassert>
#include <format>

using namespace teal::parser;
using namespace teal::parser::ast;

using std::nullopt;

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

UnionSlice<ast::Block> Parser::parse_chunk()
{
    auto stats = std::vector<ast::Statement>();
    while (not is_at_end()) {
        if (check(TokenType::END_OF_FILE)) break;
        auto stmt = parse_stat();
        if (stmt) stats.push_back(stmt.value());
    }

    return make_node(Block { .statements = stats });
}

std::optional<ast::Statement> Parser::parse_stat()
{
    if (match(TokenType::SEMICOLON)) return nullopt;
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
        std::vector<ast::Expression> vals;;
        if (not check(TokenType::SEMICOLON) and not check(TokenType::END) and not check(TokenType::ELSE)
            and not check(TokenType::ELSEIF) and not check(TokenType::UNTIL) and not check(TokenType::END_OF_FILE)) {
            vals = parse_expression_list();
        }
        match(TokenType::SEMICOLON);
        return make_node(ReturnStatement { .values = vals });
    }
    case TokenType::BREAK:
        _pos++;
        return make_node(BreakStatement {});
    case TokenType::GOTO: {
        _pos++;
        if (not check(TokenType::NAME)) {
            // $push_error("expected label name after 'goto'");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            return nullopt;
        }
        auto label_name = peek_token().text;
        _pos++;
        return make_node(GotoStatement { .label = label_name });
    }
    case TokenType::DOUBLE_COLON:
        return parse_label();
    default:
        return parse_assignment_or_call();
    }
}

std::optional<UnionSlice<ast::AssignmentStatement, ast::CallStatement>> Parser::parse_assignment_or_call()
{
    const Token &prefix_tk = peek_token();
    bool called = false;
    auto prefix = parse_prefix_expression(&called);
    if (not prefix) {
        skip_to_next_statement();
        return nullopt;
    }
    auto val = prefix.value();
    if (check(TokenType::ASSIGN) or check(TokenType::COMMA)) {
        if (called) {
            push_error(UnexpectedFunctionCall());
            skip_to_next_statement();
            prefix = make_node(NameExpression { .name = "ERROR" });
        }
        
        auto assign = make_node(AssignmentStatement {});
        assign->left.push_back(val);
        while (match(TokenType::COMMA)) {
            auto var = parse_var_expression();
            if (not var) {
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::COMMA)) {
                    _pos++;
                    continue;
                }
                break;
            }
            assign->left.push_back(var.value());
        }
        const Token &tk = peek_token();
        consume(TokenType::ASSIGN);
        assign->right = parse_expression_list();
        if (assign->right.empty())
            push_error(ExpectedExpression(tk));
        return assign;
    } else {
        if (!called) {
            push_error(UnexpectedExpression(prefix_tk));
            skip_to_next_statement();
            return nullopt;
        }

        auto call_expr = val.get_unsafe<ast::FunctionCallExpression>();
        return make_node(CallStatement { .call = call_expr });
    }

}

std::optional<UnionSlice<ast::LabelStatement>> Parser::parse_label()
{
    // consume(TokenType::DOUBLE_COLON);
    consume(TokenType::DOUBLE_COLON);
    if (not check(TokenType::NAME)) {
        // $push_error("expected label name after '::'");
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullopt;
    }
    auto name = peek_token().text;
    _pos++;
    consume(TokenType::DOUBLE_COLON);
    return make_node(LabelStatement { .name = name });
}

std::optional<UnionSlice<ast::IfStatement>> Parser::parse_if()
{
    const Token &iftk = peek_token();
    if (not consume(TokenType::IF)) return nullopt;
    auto if_stmt = make_node(IfStatement {
        .if_branches = {},
        .else_block = nullopt,
    });
    auto cond = parse_expression();
    if (not cond) {
        push_error(ExpectedExpression(iftk));
        return nullopt;
    }
    if (not consume(TokenType::THEN))
        return nullopt;
    
    auto then_block = make_node<Block>();
    while (not check(TokenType::END) and not check(TokenType::ELSE) and not check(TokenType::ELSEIF) and not is_at_end()) {
        auto st = parse_stat();
        if (st) then_block->statements.push_back(st.value());
        if (check(TokenType::END) or check(TokenType::ELSE) or check(TokenType::ELSEIF) or check(TokenType::UNTIL)
            or check(TokenType::END_OF_FILE))
            break;
    }
    if_stmt->if_branches.push_back({ cond.value(), std::move(then_block) });
    while (match(TokenType::ELSEIF)) {
        const Token &elseif_tk = peek_token();
        auto elseif_cond = parse_expression();
        if (not elseif_cond) push_error(ExpectedExpression(elseif_tk));
        consume(TokenType::THEN);
        auto elseif_block = make_node<Block>();
        while (not check(TokenType::END) and not check(TokenType::ELSE) and not check(TokenType::ELSEIF)
               and not is_at_end()) {
            auto st = parse_stat();
            if (st) elseif_block->statements.push_back(st.value());
            if (check(TokenType::END) or check(TokenType::ELSE) or check(TokenType::ELSEIF) or check(TokenType::UNTIL)
                or check(TokenType::END_OF_FILE))
                break;
        }
        if_stmt->if_branches.push_back({ elseif_cond.value(), std::move(elseif_block) });
    }
    if (match(TokenType::ELSE)) {
        auto else_block = make_node<Block>();
        while (not check(TokenType::END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) else_block->statements.push_back(st.value());
            if (check(TokenType::END) or check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
        }
        if_stmt->else_block = std::move(else_block);
    }
    consume(TokenType::END);
    return if_stmt;
}

std::optional<UnionSlice<ast::WhileStatement>> Parser::parse_while()
{
    if (not consume(TokenType::WHILE)) return nullopt;
    // auto while_stmt = make_node(WhileStatement {
    //     .
    // });
    const Token &while_tk = peek_token();
    auto condition = parse_expression();
    if (not condition) push_error(ExpectedExpression(while_tk));
    consume(TokenType::DO);
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(st.value());
        if (check(TokenType::END) or check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    return make_node(WhileStatement {
        .condition = condition.value(),
        .body = body_block,
    });
}

std::optional<UnionSlice<ast::RepeatStatement>> Parser::parse_repeat()
{
    if (not consume(TokenType::REPEAT)) return nullopt;
    // auto repeat_stmt = make_node<RepeatStatement>();
    auto body_block = make_node<Block>();
    while (not check(TokenType::UNTIL) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(st.value());
        if (check(TokenType::UNTIL) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::UNTIL);
    const Token &until_tk = peek_token();
    auto cond = parse_expression();
    if (not cond) push_error(ExpectedExpression(until_tk));
    return make_node(RepeatStatement {
        .condition = cond.value(),
        .body = body_block,
    });
}

std::optional<UnionSlice<ast::ForInStatement, ast::ForNumericStatement>> Parser::parse_for()
{
    if (not consume(TokenType::FOR)) return nullopt;
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        skip_to_next_statement();
        return nullopt;
    }
    auto var_name = peek_token().text;
    _pos++;
    if (match(TokenType::ASSIGN)) {
        // auto for_num = make_node<ForNumericStatement>();
        // for_num->variable_name = var_name;
        // for_num->expressions.start = parse_expression();
        // auto for_num = make_node(ForNumericStatement {
        //     .variable_name = var_name,
        //     .expressions = {},
        // });
        consume(TokenType::COMMA);
        auto startexpr = parse_expression();
        if (not startexpr) {
            push_error(ExpectedExpression(peek_token()));
            return nullopt;
        }
        auto endexpr = parse_expression();
        if (not endexpr) {
            push_error(ExpectedExpression(peek_token()));
            return nullopt;
        }
        std::optional<ast::Expression> stepexpr = nullopt;
        if (match(TokenType::COMMA)) { stepexpr = parse_expression(); }
        consume(TokenType::DO);
        auto body_block = make_node<Block>();
        while (not check(TokenType::END) and not is_at_end()) {
            auto st = parse_stat();
            if (st) body_block->statements.push_back(st.value());
            if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
        }
        consume(TokenType::END);
        // for_num->body = std::move(body_block);
        // return for_num;
        return make_node(ForNumericStatement {
            .variable_name = var_name,
            .expressions = { startexpr.value(), endexpr.value(), stepexpr },
            .body = body_block,
        });
    } else {
        std::vector<std::string_view> name_list;
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
            if (st) body_block->statements.push_back(st.value());
            if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
        }
        consume(TokenType::END);
        // auto for_in = make_node<ForInStatement>();
        // for_in->names = std::move(name_list);
        // for_in->exprs = std::move(exprs);
        // for_in->body = std::move(body_block);
        // return for_in;
        return make_node(ForInStatement {
            .names = std::move(name_list),
            .expressions = std::move(exprs),
            .body = body_block,
        });
    }
}

std::optional<UnionSlice<ast::DoStatement>> Parser::parse_do()
{
    consume(TokenType::DO);
    auto block_node = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) block_node->statements.push_back(st.value());
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    return make_node(DoStatement { .body = block_node });
}

std::optional<UnionSlice<ast::FunctionDeclarationStatement>> Parser::parse_function_decl(Visibility vis, bool is_macroexp)
{
    if (not check(TokenType::NAME)) push_error(ExpectedToken(TokenType::NAME, peek_token()));
    std::vector<std::string_view> name_path;
    std::string_view method_name;
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
    auto ty_params = std::vector<GenericTypeParameter>();
    if (match(TokenType::LESS)) { parse_generic_list(&ty_params); }
    consume(TokenType::L_PAREN);
    auto params = std::vector<FunctionBody::Parameter>();
    if (not check(TokenType::R_PAREN)) {
        // auto func_body = make_node<FunctionBody>();
        while (true) {
            if (check(TokenType::VAR_ARG)) {
                _pos++;
                auto var_type =  std::optional<Type>(nullopt);
                if (match(TokenType::COLON)) { var_type = parse_type(); }
                params.push_back({ 
                    .name = "...",
                    .is_varadict = true,
                    .is_optional = false,
                    .type = var_type.value(),
                });
                break;
            }
            if (not check(TokenType::NAME)) {
                // $push_error("expected parameter name or '...'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::R_PAREN)) break;
                _pos++;
            } else {
                auto param_name = peek_token().text;
                _pos++;
                bool opt_flag = false;
                if (match(TokenType::QUESTION)) opt_flag = true;
                auto type_ann = std::optional<Type>(nullopt);
                if (match(TokenType::COLON)) { type_ann = parse_type(); }
                params.push_back({ 
                    .name = param_name,
                    .is_varadict = false,
                    .is_optional = opt_flag,
                    .type = type_ann.value(),
                });
            }
            if (not match(TokenType::COMMA)) break;
        }
    }
    consume(TokenType::R_PAREN);
    std::vector<Type> ret_types;
    bool ret_var_arg = false;

    if (match(TokenType::COLON)) {
        ret_types = parse_return_type_list(&ret_var_arg);
        //func_body->varadict_return = ret_var_arg;
    }
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(st.value());
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);

    return make_node(FunctionDeclarationStatement {
        .visibility = vis,
        .name_path = name_path,
        .method_name = method_name,
        .is_method = is_method,
        .is_macro = is_macroexp,
        .body = make_node(FunctionBody {
            .type_parameters = ty_params,
            .parameters = params,
            .return_types = ret_types,
            .varadict_return = ret_var_arg,
            .body = body_block,
        }),
    });
}

UnionSlice<ast::VariableDeclarationStatement> Parser::parse_var_decl(Visibility vis)
{
    auto var_stmt = make_node(VariableDeclarationStatement {
        .visibility = vis,
        .names = {},
        .types = {},
        .values = {},
    });
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

std::optional<UnionSlice<ast::RecordDeclarationStatement>> Parser::parse_record_decl(Visibility vis, bool is_interface)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullopt;
    }
    auto name = peek_token().text;
    _pos++;
    auto body = parse_record_body();
    // return make_node<RecordDeclarationStatement>(is_interface, vis, std::move(name), std::move(body));
    return make_node(RecordDeclarationStatement {
        .is_interface = is_interface,
        .visibility = vis,
        .name = name,
        .body = body,
    });
}

std::optional<UnionSlice<ast::EnumDeclarationStatement>> Parser::parse_enum_decl(Visibility vis)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullopt;
    }
    auto name = peek_token().text;
    _pos++;
    auto body = parse_enum_body();
    // return make_node<EnumDeclarationStatement>(vis, std::move(name), std::move(body));
    return make_node(EnumDeclarationStatement {
        .visibility = vis,
        .name = name,
        .body = body,
    });
}

std::optional<UnionSlice<ast::TypeAliasStatement>> Parser::parse_type_alias_decl(Visibility vis)
{
    if (not check(TokenType::NAME)) {
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullopt;
    }
    auto name = peek_token().text;
    _pos++;
    // ast::Pointer<TypeNode> type_value;
    auto type_value = std::optional<Type>(nullopt);
    std::vector<GenericTypeParameter> type_args;
    if (match(TokenType::LESS)) { parse_generic_list(&type_args); }

    if (match(TokenType::ASSIGN)) {
        if (check(TokenType::RECORD)) {
            _pos++;
            // type_value = make_node<TypeRecordNode>(parse_record_body());
            type_value = make_node(RecordType {
                .body = parse_record_body(),
            });
        } else if (check(TokenType::ENUM)) {
            _pos++;
            type_value = make_node(EnumType {
                .elements = parse_enum_body()->elements
            });
        } else {
            type_value = parse_type();
        }
    } else {
        if (vis == Visibility::LOCAL)
            push_error(ExpectedToken(TokenType::ASSIGN, peek_token()));
    }
    // return make_node<TypeAliasStatement>(vis, std::move(name), std::move(type_args), std::move(type_value));
    return make_node(TypeAliasStatement {
        .visibility = vis,
        .name = name,
        .type_parameters = type_args,
        .type = type_value.value(),
    });
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

std::vector<std::string_view> Parser::parse_name_list()
{
    std::vector<std::string_view> list;
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

// ast::Pointer<ExpressionNode> Parser::parse_expression() { return parse_exp_rec(1); }
std::optional<Expression> Parser::parse_expression()
{
    return parse_exp_rec(1);
    // auto expr = parse_prefix_expression();
    // if (not expr) return nullopt;
    // auto base = expr.value();
    // while (true) {
    //     if (match(TokenType::L_PAREN)) {
    //         std::vector<ast::Pointer<ExpressionNode>> args;
    //         if (not check(TokenType::R_PAREN)) { args = parse_expression_list(); }
    //         consume(TokenType::R_PAREN);
    //         auto call = make_node<FunctionCallExpression>(std::move(base), "");
    //         for (auto &arg : args) call->arguments.push_back(std::move(arg));
    //         base = std::move(call);
    //     } else {
    //         break;
    //     }
    // }
    // return base;
}

std::vector<ast::Expression> Parser::parse_expression_list()
{
    auto exprs = std::vector<Expression>();
    auto first = parse_expression();
    if (first) exprs.push_back(first.value());
    else {
        // $push_error("expected expression after '('");
        push_error(ExpectedExpression(peek_token()));
        return exprs;
    }
    while (match(TokenType::COMMA)) {
        auto e = parse_expression();
        if (e) {
            exprs.push_back(e.value());
        } else {
            // $push_error("expected expression after ','");
            push_error(ExpectedExpression(peek_token()));
            if (not check(TokenType::COMMA)) break;
        }
    }
    return exprs;
}

std::optional<ast::PrefixExpression> Parser::parse_prefix_expression(bool *is_call)
{
    std::optional<ast::PrefixExpression> base;
    bool called = false;
    if (match(TokenType::L_PAREN)) {
        base = parse_expression();
        consume(TokenType::R_PAREN);
    } else if (check(TokenType::NAME)) {
        auto s = peek_token().text;
        base = make_node(NameExpression { .name = s }); 
        _pos++;
    } else {
        // $push_error(std::format("expected '(' or Name, got {}", peek_token()));
        push_error(ExpectedToken(TokenType::L_PAREN, peek_token()));
        return nullopt;
    }
    while (true) {
        if (match(TokenType::COLON)) {
            if (not check(TokenType::NAME)) {
                // $push_error("expected method name after ':'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                break;
            }
            auto method = peek_token().text;
            _pos++;
            auto args = std::vector<Expression>();
            if (match(TokenType::L_PAREN)) {
                if (not check(TokenType::R_PAREN)) { args = parse_expression_list(); }
                consume(TokenType::R_PAREN);
            } else if (check(TokenType::L_BRACE)) {
                args.push_back(parse_table_constructor());
            } else if (check(TokenType::STRING)) {
                auto x = peek_token().text;
                args.push_back(make_node(StringExpression { .value = x }));
                _pos++;
            } else {
                push_error(ExpectedArguments());
                // $push_error("expected arguments after method call");
            }
            called = true;
            base = make_node(FunctionCallExpression {
                .base = base.value(),
                .method_name = method,
                .arguments = args
            });
        } else if (match(TokenType::L_PAREN)) {
            auto args = std::vector<Expression>();
            if (not check(TokenType::R_PAREN)) { args = parse_expression_list(); }
            consume(TokenType::R_PAREN);
            called = true;
            base = make_node(FunctionCallExpression {
                .base = base.value(),
                .method_name = "",
                .arguments = args
            });
        } else if (check(TokenType::L_BRACE)) {
            auto table_arg = parse_table_constructor();
            called = true;
            base = make_node(FunctionCallExpression {
                .base = base.value(),
                .method_name = "",
                .arguments = { table_arg }
            });
        } else if (check(TokenType::STRING)) {
            auto lit = peek_token().text;
            _pos++;
            called = true;
            base = make_node(FunctionCallExpression {
                .base = base.value(),
                .method_name = "",
                .arguments = { make_node(StringExpression { .value = lit }) }
            });
        } else if (match(TokenType::DOT)) {
            if (not check(TokenType::NAME)) {
                bool ok = false;
                const auto mkname = [this](const std::string_view &id) {
                    return Token { TokenType::NAME, id, peek_token().line, peek_token().col };
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
            auto field = peek_token().text;
            _pos++;
            
            base = make_node(FieldExpression {
                .object = base.value(),
                .field = field
            });
        } else if (match(TokenType::L_BRACKET)) {
            auto index_expression = parse_expression();
            consume(TokenType::R_BRACKET);
            base = make_node(IndexExpression {
                .table = base.value(),
                .index = index_expression.value()
            });
        } else {
            break;
        }
    }
    if (is_call) *is_call = called;
    return base;
}

std::optional<ast::PrefixExpression> Parser::parse_var_expression()
{
    auto expr = parse_prefix_expression(nullptr);
    if (not expr) return nullopt;
    if (expr->is<ast::FunctionCallExpression>()) {
        // $push_error("unexpected function call in assignment");
        push_error(UnexpectedFunctionCall());
        return make_node(NameExpression { 
            .name = "_error_"
        });
    }
    return expr;
}

std::optional<ast::PrimaryExpression> Parser::parse_primary_expression()
{
    TokenType t = peek_token().type;
    if (t == TokenType::NIL) {
        _pos++;
        return make_node(NilExpression {});
    }
    if (t == TokenType::TRUE) {
        _pos++;
        return make_node(BooleanExpression { .value = true });
    }
    if (t == TokenType::FALSE) {
        _pos++;
        return make_node(BooleanExpression { .value = false });
    }
    if (t == TokenType::NUMBER) {
        auto num = peek_token().text;
        _pos++;
        return make_node(NumberExpression { .value = num });
    }
    if (t == TokenType::STRING) {
        auto str = peek_token().text;
        _pos++;
        return make_node(StringExpression { .value = str });
    }
    if (t == TokenType::VAR_ARG) {
        _pos++;
        return make_node(VarargExpression {});
    }
    if (t == TokenType::FUNCTION) { return parse_function_def_expression(); }
    if (t == TokenType::L_BRACE) { return parse_table_constructor(); }
    if (t == TokenType::NAME or t == TokenType::L_PAREN or Token::type_is_teal_keyword(t)) {
        return parse_prefix_expression(nullptr);
    }
    // $push_error(std::format("unexpected token `{}` (type: {}) in expression", peek_token().text, peek_token().type));
    push_error(UnexpectedToken(peek_token()));
    return nullopt;
}

ast::Expression Parser::parse_exp_rec(int min_prec)
{
    //TODO: Clean this all up, very messy
    auto lopt = parse_unary_expression();
    if (not lopt) {
        // $push_error("expected expression after unary operator");
        push_error(ExpectedExpression(peek_token()));
        return make_node(NameExpression { .name = "ERROR" });
    }
    Expression left = *lopt;
    while (true) {
        TokenType type = peek_token().type;
        int prec = get_binary_precedence(type);
        if (prec < min_prec) break;
        if (type == TokenType::AS) {
            _pos++;
            auto cast_types = std::vector<Type>();
            if (match(TokenType::L_PAREN)) {
                if (not check(TokenType::R_PAREN)) {
                    cast_types = parse_type_list();
                    if (match(TokenType::VAR_ARG)) { }
                }
                consume(TokenType::R_PAREN);
            } else {
                auto ty = parse_type();
                if (not ty) {
                    // $push_error("expected type after 'as'");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    return left;
                }
                cast_types.push_back(ty.value());
            }
            left = make_node(CastExpression {
                .expression = left,
                .target_types = cast_types,  
            });
            continue;
        } else if (type == TokenType::IS) {
            _pos++;
            auto type_node = parse_type();
            if (not type_node) {
                // $push_error("expected type after 'is'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                return left;
            }
            left = make_node(IsTypeExpression {
                .expression = left,
                .type = type_node.value(),
            });
            continue;
        }
        bool right_assoc = is_right_associative(type);
        int next_min_prec = (right_assoc ? prec : prec + 1);
        _pos++;
        auto right = parse_exp_rec(next_min_prec);
        left = make_node(BinaryOperationExpression {
            .left = left,
            .right = right,
            .operation = type,
        });
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

std::optional<UnionSliceOf_t<ast::PrimaryExpression, Union<ast::UnaryOperationExpression>>> Parser::parse_unary_expression()
{
    if (check(TokenType::NOT) or check(TokenType::SUB) or check(TokenType::LENGTH) or check(TokenType::BIT_XOR)) {
        TokenType token = peek_token().type;
        _pos++;
        auto operand = parse_unary_expression();
        if (not operand) {
            // $push_error("expected operand after unary operator");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            return nullopt;
        }
        return make_node(UnaryOperationExpression {
            .operand = operand.value(),
            .operation = token,
        });
    }
    auto primary = parse_primary_expression();
    if (not primary) {
        // $push_error("expected primary expression");
        push_error(ExpectedToken(TokenType::NAME, peek_token()));
        return nullopt;
    }
    return primary;
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

bool Parser::parse_typeargs(std::vector<ast::Type> *types)
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
        types->push_back(ty.value());
        if (not match(TokenType::COMMA)) break;
    }

    consume(TokenType::GREATER);
    return true;
}

UnionSlice<ast::FunctionDefinitionExpression> Parser::parse_function_def_expression()
{
    consume(TokenType::FUNCTION);
    // auto func_body = make_node<FunctionBody>();
    auto tparams = std::vector<GenericTypeParameter>();
    auto params = std::vector<FunctionBody::Parameter>();
    if (match(TokenType::LESS)) { parse_generic_list(&tparams); }
    consume(TokenType::L_PAREN);
    if (not check(TokenType::R_PAREN)) {
        auto any_ty = make_node(BasicType { .name = "any" });
        while (true) {
            if (check(TokenType::VAR_ARG)) {
                _pos++;

                //TODO: Need much better way to handle this
                Type var_type = any_ty;
                if (match(TokenType::COLON)) { var_type = parse_type().value_or(any_ty); }
                params.push_back({ "...", true, false, var_type });
                break;
            }
            if (not check(TokenType::NAME)) {
                // $push_error("expected parameter name or '...'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                if (check(TokenType::R_PAREN)) break;
                _pos++;
            } else {
                auto param_name = peek_token().text;
                _pos++;
                bool opt_flag = false;
                if (match(TokenType::QUESTION)) opt_flag = true;
                if (match(TokenType::COLON)) {
                    auto type_ann = parse_type();
                    params.push_back({ param_name, false, opt_flag, type_ann.value_or(any_ty) });
                } else {
                    //TODO: In teal currently its not allowed to have a parameter without a type, but it might be a good idea to support
                    push_error(ExpectedToken(TokenType::COLON, peek_token()));
                    params.push_back({ param_name, false, opt_flag, any_ty });
                }
            }
            if (not match(TokenType::COMMA)) break;
        }
    }
    consume(TokenType::R_PAREN);

    auto ret_types = std::vector<Type>();
    bool ret_var_arg = false;
    if (match(TokenType::COLON)) {
        ret_types = parse_return_type_list(&ret_var_arg);
    }
    auto body_block = make_node<Block>();
    while (not check(TokenType::END) and not is_at_end()) {
        auto st = parse_stat();
        if (st) body_block->statements.push_back(st.value());
        if (check(TokenType::END) or check(TokenType::END_OF_FILE)) break;
    }
    consume(TokenType::END);
    // func_body->body = std::move(body_block);
    // return make_node<FunctionDefinitionExpression>(std::move(func_body));
    return make_node(FunctionDefinitionExpression {
        .body = make_node(FunctionBody {
            .type_parameters = tparams,
            .parameters = params,
            .return_types = ret_types,
            .varadict_return = ret_var_arg,
            .body = body_block,
        }),
    });
}

UnionSlice<ast::TableConstructorExpression> Parser::parse_table_constructor()
{
    consume(TokenType::L_BRACE);
    auto table = make_node<TableConstructorExpression>();
    if (not check(TokenType::R_BRACE)) {
        while (true) {
            // auto field = TableConstructorExpression::Field();
            //we dont wanna init yet
            Storage<TableConstructorExpression::Field> field_storage;
            //[...]
            if (match(TokenType::L_BRACKET)) {
                auto key_expr = parse_expression();
                consume(TokenType::R_BRACKET);
                consume(TokenType::ASSIGN);
                field_storage = TableConstructorExpression::KeyValuePair {
                    .key = key_expr.value(),
                    .value = parse_expression().value(),
                    .type = nullopt
                };
            } else if (check(TokenType::NAME) and (
                    (peek_token(1).type == TokenType::COLON and peek_token(3).type != TokenType::L_PAREN)
                    or peek_token(1).type == TokenType::ASSIGN
                )) {
                auto key = peek_token().text;

                //{ k: string = "test", self:get_value() } are both valid
                auto ty = std::optional<Type>(nullopt);
                if (_pos + 1 < _tokens.size()
                    and (peek_token(1).type == TokenType::COLON or peek_token(1).type == TokenType::ASSIGN)) {
                    _pos++;
                    if (match(TokenType::COLON)) { ty = parse_type(); }
                    consume(TokenType::ASSIGN);
                }
                auto value = parse_expression();
                field_storage = TableConstructorExpression::KeyValuePair {
                    .key = key,
                    .value = value.value(),
                    .type = ty
                };
            } else {
                field_storage = parse_expression().value();
            }

            table->fields.push_back(field_storage.get());
            if (match(TokenType::COMMA) or match(TokenType::SEMICOLON)) {
                if (check(TokenType::R_BRACE)) break;
                else continue;
            } else break;
        }
    }
    consume(TokenType::R_BRACE);
    return table;
}

std::optional<Type> Parser::parse_type()
{
    auto nil_ty = make_node(BasicType { .name = "nil" });
    auto first_type = nil_ty;
    if (match(TokenType::L_PAREN)) {
        auto ty = parse_type(); 
        if (not ty) {
            // $push_error("expected type after '('");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            return nullopt;
        }
        first_type = ty.value();
        consume(TokenType::R_PAREN);
    } else {
        auto ty = parse_base_type();
        if (not ty) {
            // $push_error("expected type after '('");
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            return nullopt;
        }
        first_type = ty.value();
    }
    if (check(TokenType::BIT_OR)) {
        auto union_node = make_node(UnionType {});
        union_node->options.push_back(first_type);
        while (match(TokenType::BIT_OR)) {
            auto next_type = parse_base_type();
            if (not next_type) next_type = nil_ty;
            union_node->options.push_back(next_type.value());
        }
        return union_node;
    }
    return first_type;
}

constexpr inline bool is_primitive(const std::string_view &name)
{
    return name == "string" or name == "integer" or name == "number" or name == "boolean" or name == "nil";
}

std::optional<ast::Type> Parser::parse_base_type()
{
    if (check(TokenType::NAME) or check(TokenType::NIL)) {
        auto name = peek_token().text;
        if (is_primitive(name)) {
            _pos++;
            return make_node(BasicType {
                .name = name
            });
        } else return parse_nominal_type();
    }
    if (check(TokenType::L_BRACE)) {
        auto nil_ty = make_node(BasicType { .name = "nil" });
        _pos++;
        auto first_type = parse_type();
        if (not first_type) first_type = nil_ty;
        if (match(TokenType::COLON)) {
            auto second_type = parse_type();
            if (not second_type) second_type = nil_ty;
            consume(TokenType::R_BRACE);
            return make_node(TableType {
                .element_types = { second_type.value() },
                .key_type = first_type.value(),
                .is_map = true,
            });
            // auto map_node = make_node<TableTypeNode>();
            // map_node->is_map = true;
            // map_node->key_type = std::move(first_type);
            // map_node->element_types.push_back(std::move(second_type));
            // return map_node;
        } else {
            auto types = std::vector<Type>();
            types.push_back(first_type.value());
            while (match(TokenType::COMMA)) {
                auto t = parse_type();
                if (not t) t = nil_ty;
                types.push_back(t.value());
            }
            consume(TokenType::R_BRACE);
            if (types.size() == 1) {
                return make_node(TableType {
                    .element_types = { types[0] },
                    .key_type = nil_ty,
                    .is_map = false,
                });
            } else {
                return make_node(TableType {
                    .element_types = types,
                    .key_type = nil_ty,
                    .is_map = false,
                });
            }
        }
    }
    if (check(TokenType::FUNCTION)) { return parse_function_type(); }
    // $push_error(std::format("expected type, got {}", peek_token()));
    push_error(ExpectedToken(TokenType::NAME, peek_token()));
    return nullopt;
}

std::optional<UnionSlice<ast::NominalType>> Parser::parse_nominal_type()
{
    std::vector<std::string_view> name_parts;
    if (not check(TokenType::NAME)) { return nullopt; }
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
    auto type_args = std::vector<Type>();
    if (match(TokenType::LESS)) {
        if (not parse_typeargs(&type_args)) return nullopt;
    }
    return make_node(NominalType {
        .name_parts = name_parts,
        .type_arguments = type_args,
    });
}

std::optional<UnionSlice<ast::FunctionType>> Parser::parse_function_type()
{
    consume(TokenType::FUNCTION);
    auto node = make_node(FunctionType {});
    if (match(TokenType::LESS)) { parse_generic_list(&node->type_parameters); }
    consume(TokenType::L_PAREN);
    if (not check(TokenType::R_PAREN)) {
        while (true) {
            // FunctionTypeNode::ParameterType param;
            Storage<Type> type;
            std::string_view name;
            bool is_optional = false;
            if (check(TokenType::NAME) or check(TokenType::VAR_ARG)) {
                if (peek_token(1).type == TokenType::QUESTION or peek_token(1).type == TokenType::COLON) {
                    name = peek_token().text;
                    _pos++;
                    is_optional = false;
                    if (match(TokenType::QUESTION)) is_optional = true;
                    consume(TokenType::COLON);
                } else {
                    name = std::string_view();
                    is_optional = false;
                }
                auto ty = parse_type();
                if (not ty) {
                    // $push_error("expected type after ':'");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    return nullopt;
                }
                type = ty.value();
            } else if (match(TokenType::QUESTION)) {
                name = std::string_view();
                is_optional = true;
                auto ty = parse_type();
                if (not ty) {
                    // $push_error("expected type after ':'");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    return nullopt;
                }
                type = ty.value();
            } else {
                name = std::string_view();
                is_optional = false;
                auto ty = parse_type();
                if (not ty) {
                    // $push_error("expected type after ':'");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    return nullopt;
                }
                type = ty.value();
            }
            node->parameters.push_back({
                .name = name,
                .is_optional = is_optional,
                .type = type.get()
            });
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

std::vector<ast::Type> Parser::parse_type_list()
{
    auto types = std::vector<ast::Type>();
    auto first = parse_type();
    auto nil_ty = make_node(BasicType { .name = "nil" });
    if (first) types.push_back(first.value());
    else types.push_back(nil_ty);
    while (match(TokenType::COMMA)) {
        auto t = parse_type();
        if (t) types.push_back(t.value());
        else types.push_back(nil_ty);
    }
    return types;
}

// std::vector<FunctionTypeNode::ParameterType> Parser::parse_param_type_list() { return {}; }

std::vector<ast::Type> Parser::parse_return_type_list(bool *var_arg)
{
    auto types = std::vector<ast::Type>();
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

UnionSlice<ast::RecordBody> Parser::parse_record_body()
{
    auto type_parameters = std::vector<GenericTypeParameter>();
    if (match(TokenType::LESS)) {
        parse_generic_list(&type_parameters);
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
    auto rb = make_node(RecordBody {
        .type_parameters = type_parameters,
        .structural_ext = std::nullopt,
        .interface_ext = std::vector<Type>(),
        .where_clause = std::nullopt,
        .entries = std::vector<RecordBody::Entry>(),
    });
    if (match(TokenType::IS)) { parse_interface_list(rb); }
    if (match(TokenType::WHERE)) {
        const Token &tk = peek_token();
        auto expr = parse_expression();
        if (not expr) push_error(ExpectedExpression(tk));
        else rb->where_clause = expr.value();
    }
    while (not check(TokenType::END) and not is_at_end()) {
        if (check(TokenType::NAME) and peek_token().text == "userdata") {
            _pos++;
            rb->entries.emplace_back(RecordBody::Userdata {});
            continue;
        }
        if (check(TokenType::TYPE) and peek_token(1).type != TokenType::COLON) {
            consume(TokenType::TYPE);
            std::string_view type_name = "<unnamed>";
            if (not check(TokenType::NAME)) {
                // $push_error("expected name after 'type'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                type_name = peek_token().text;
                _pos++;
            }
            consume(TokenType::ASSIGN);
            auto type_value = parse_type();
            if (not type_value) {
                // $push_error("expected type after '='");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                rb->entries.emplace_back(RecordBody::TypeAlias {
                    .name = type_name,
                    .type = type_value.value(),
                });
            }
            continue;
        }
        if (match(TokenType::RECORD)) {
            std::string_view type_name = "<unnamed>";
            if (not check(TokenType::NAME)) {
                // $push_error(std::format("expected name after 'record', got {}", peek_token()));
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                type_name = peek_token().text;
                _pos++;
            }
            auto body = parse_record_body();
            rb->entries.emplace_back(RecordBody::Record {
                .name = type_name,
                .body = body,
            });
            // entry.nested_body = parse_record_body();
            // entry.entry_kind = RecordBody::Entry::Kind::RECORD;
            // rb->entries.push_back(std::move(entry));
            continue;
        }
        if (match(TokenType::ENUM)) {
            std::string_view enum_name = "<unnamed>";
            if (not check(TokenType::NAME)) {
                // $push_error("expected name after 'enum'");
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                enum_name = peek_token().text;
                _pos++;
            }
            auto enum_body = parse_enum_body();
            rb->entries.emplace_back(RecordBody::Enum {
                .name = enum_name,
                .body = enum_body,
            });
            continue;
        }
        if (match(TokenType::INTERFACE)) {
            std::string_view iface_name = "<unnamed>";
            if (not check(TokenType::NAME)) {
                // $push_error(std::format("expected name after 'interface', got {}", peek_token()));
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
            } else {
                iface_name = peek_token().text;
                _pos++;
            }
            auto iface_body = parse_record_body();
            rb->entries.emplace_back(RecordBody::Interface { { iface_name, iface_body } });
            continue;
        }
        bool is_meta = false;
        if (check(TokenType::NAME) and peek_token().text == "metamethod") {
            _pos++;
            is_meta = true;
        }
        std::string_view field_name = "";
        if (check(TokenType::NAME)) {
            field_name = peek_token().text;
            _pos++;
        } else if (match(TokenType::L_BRACKET)) {
            if (not check(TokenType::STRING)) {
                // $push_error("expected literal string key in record field");
                push_error(ExpectedToken(TokenType::STRING, peek_token()));
            } else {
                field_name = peek_token().text;
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
        auto field_type = parse_type();
        if (not field_type) {
            push_error(ExpectedToken(TokenType::NAME, peek_token()));
            continue;
        }
        rb->entries.emplace_back(RecordBody::Field {
            .is_metamethod = is_meta,
            .name = field_name,
            .type = field_type.value(),
        });
    }
    consume(TokenType::END);
    return rb;
}

UnionSlice<ast::EnumBody> Parser::parse_enum_body()
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

void Parser::parse_interface_list(UnionSlice<RecordBody> rb)
{
    if (match(TokenType::L_BRACE)) {
        rb->structural_ext = parse_type();
        consume(TokenType::R_BRACE);
        if (match(TokenType::COMMA)) {
            do {
                auto nom = parse_nominal_type();
                if (nom) rb->interface_ext.push_back(nom.value());
                else {
                    // $push_error("expected interface name");
                    push_error(ExpectedToken(TokenType::NAME, peek_token()));
                    break;
                }
            } while (match(TokenType::COMMA));
        }
    } else {
        auto nom = parse_nominal_type();
        if (nom) rb->interface_ext.push_back(nom.value());
        else push_error(ExpectedToken(TokenType::NAME, peek_token()));
        while (match(TokenType::COMMA)) {
            auto nom2 = parse_nominal_type();
            if (nom2) rb->interface_ext.push_back(nom2.value());
            else {
                push_error(ExpectedToken(TokenType::NAME, peek_token()));
                break;
            }
        }
    }
}
