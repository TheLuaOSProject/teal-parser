#include "Parser.hpp"
#include "Lexer.hpp"

#include <format>

using namespace teal;

template<>
struct std::formatter<Token> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

    constexpr auto format(const Token &tk, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{} at {}:{}", tk.toString(), tk.line, tk.col);
    }
};

template<>
struct std::formatter<TokenType> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

    constexpr auto format(const TokenType &tk, std::format_context& ctx) const {
        using enum TokenType;

        std::string_view name;
        switch (tk) {
            case EOF_: name = "EOF"; break;
            case Name: name = "Name"; break;
            case Number: name = "Number"; break;
            case String: name = "String"; break;
            case K_nil: name = "nil"; break;
            case K_true: name = "true"; break;
            case K_false: name = "false"; break;
            case K_function: name = "function"; break;
            case K_end: name = "end"; break;
            case K_do: name = "do"; break;
            case K_if: name = "if"; break;
            case K_then: name = "then"; break;
            case K_else: name = "else"; break;
            case K_elseif: name = "elseif"; break;
            case K_while: name = "while"; break;
            case K_repeat: name = "repeat"; break;
            case K_until: name = "until"; break;
            case K_for: name = "for"; break;
            case K_in: name = "in"; break;
            case K_break: name = "break"; break;
            case K_goto: name = "goto"; break;
            case K_return: name = "return"; break;
            case K_local: name = "local"; break;
            case K_global: name = "global"; break;
            case K_record: name = "record"; break;
            case K_interface: name = "interface"; break;
            case K_enum: name = "enum"; break;
            case K_type: name = "type"; break;
            // case K_require: name = "require"; break;
            case K_where: name = "where"; break;
            case K_and: name = "and"; break;
            case K_or: name = "or"; break;
            case K_not: name = "not"; break;
            case K_as: name = "as"; break;
            case K_is: name = "is"; break;
            case Op_Assign: name = "="; break;
            case Op_Equals: name = "=="; break;
            case Op_NotEq: name = "~="; break;
            case Op_Less: name = "<"; break;
            case Op_LessEq: name = "<="; break;
            case Op_Greater: name = ">"; break;
            case Op_GreaterEq: name = ">="; break;
            case Op_Concat: name = ".."; break;
            case Op_Add: name = "+"; break;
            case Op_Sub: name = "-"; break;
            case Op_Mul: name = "*"; break;
            case Op_Div: name = "/"; break;
            case Op_FloorDiv: name = "//"; break;
            case Op_Mod: name = "%"; break;
            case Op_Pow: name = "^"; break;
            case Op_BitAnd: name = "&"; break;
            case Op_BitOr: name = "|"; break;
            case Op_BitXor: name = "~"; break;
            case Op_ShiftL: name = "<<"; break;
            case Op_ShiftR: name = ">>"; break;
            case Op_Len: name = "#"; break;
            case Op_LParen: name = "("; break;
            case Op_RParen: name = ")"; break;
            case Op_LBrace: name = "{"; break;
            case Op_RBrace: name = "}"; break;
            case Op_LBracket: name = "["; break;
            case Op_RBracket: name = "]"; break;
            case Op_Comma: name = ","; break;
            case Op_Semicolon: name = ";"; break;
            case Op_Colon: name = ":"; break;
            case Op_DoubleColon: name = "::"; break;
            case Op_Dot: name = "."; break;
            case Op_VarArg: name = "..."; break;
            case Op_Question: name = "?"; break;
        }
        return std::format_to(ctx.out(), "{}", name);
    }
};


    std::unique_ptr<Block> Parser::parseChunk() {
        auto block = std::make_unique<Block>();
        while (!isAtEnd()) {
            if (check(TokenType::EOF_)) break;
            auto stmt = parseStat();
            if (stmt) block->statements.push_back(std::move(stmt));
        }
        return block;
    }

    std::unique_ptr<Statement> Parser::parseStat() {
        if (match(TokenType::Op_Semicolon)) {  // skip empty statement
            return nullptr;
        }
        TokenType t = peekToken().type;
        switch (t) {
        case TokenType::K_if:       return parseIf();
        case TokenType::K_while:    return parseWhile();
        case TokenType::K_repeat:   return parseRepeat();
        case TokenType::K_for:      return parseFor();
        case TokenType::K_do:       return parseDo();
        case TokenType::K_function:
            _pos++;
            return parseFunctionDecl(false, false);
        case TokenType::K_local:
            _pos++;
            if (match(TokenType::K_function)) {
                return parseFunctionDecl(true, false);
            } else if (match(TokenType::K_record)) {
                return parseRecordDecl(true, false, false);
            } else if (match(TokenType::K_interface)) {
                return parseRecordDecl(true, false, true);
            } else if (match(TokenType::K_enum)) {
                return parseEnumDecl(true, false);
            } else if (match(TokenType::K_type)) {
                return parseTypeAliasDecl(true, false);
            } else {
                return parseVarDecl(true, false);
            }
        case TokenType::K_global:
            _pos++;
            if (match(TokenType::K_function)) {
                return parseFunctionDecl(false, true);
            } else if (match(TokenType::K_record)) {
                return parseRecordDecl(false, true, false);
            } else if (match(TokenType::K_interface)) {
                return parseRecordDecl(false, true, true);
            } else if (match(TokenType::K_enum)) {
                return parseEnumDecl(false, true);
            } else if (match(TokenType::K_type)) {
                return parseTypeAliasDecl(false, true);
            } else {
                return parseVarDecl(false, true);
            }
        case TokenType::K_return: {
            _pos++;

            auto retStmt = std::make_unique<ReturnStatement>();
            if (not check(TokenType::Op_Semicolon) and not check(TokenType::K_end) and
                not check(TokenType::K_else) and not check(TokenType::K_elseif) and
                not check(TokenType::K_until) and not check(TokenType::EOF_)) {
                retStmt->values = parseExpressionList();
            }
            match(TokenType::Op_Semicolon);
            return retStmt;
        }
        case TokenType::K_break:
            _pos++;
            return std::make_unique<BreakStatement>();
        case TokenType::K_goto: {
            _pos++;
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected label name after 'goto'", peekToken().line, peekToken().col});
                return nullptr;
            }
            std::string labelName = peekToken().text;
            _pos++;
            return std::make_unique<GotoStatement>(labelName);
        }
        case TokenType::Op_DoubleColon:
            return parseLabel();
        default:
            return parseAssignmentOrCall();
        }
    }

    std::unique_ptr<Statement> Parser::parseAssignmentOrCall() {
        auto prefix = parsePrefixExpression();
        if (!prefix) {
            skipToNextStatement();
            return nullptr;
        }
        if (check(TokenType::Op_Assign) or check(TokenType::Op_Comma)) {
            // It's an assignment
            if (dynamic_cast<FunctionCallExpression*>(prefix.get())) {
                _errors.push_back({"cannot assign to function call", peekToken().line, peekToken().col});
                prefix = std::make_unique<NameExpression>("_error_");
            }
            auto assign = std::make_unique<AssignmentStatement>();
            assign->lhs.push_back(std::move(prefix));
            while (match(TokenType::Op_Comma)) {
                auto var = parseVarExpression();
                if (!var) {
                    _errors.push_back({"expected variable", peekToken().line, peekToken().col});
                    if (check(TokenType::Op_Comma)) { _pos++; continue; }
                    break;
                }
                assign->lhs.push_back(std::move(var));
            }
            consume(TokenType::Op_Assign, "expected '=' in assignment");
            assign->rhs = parseExpressionList();
            if (assign->rhs.empty()) {
                _errors.push_back({"expected expression after '='", peekToken().line, peekToken().col});
            }
            return assign;
        } else {
            // Must be a function call statement
            auto callExpression = dynamic_cast<FunctionCallExpression*>(prefix.get());
            if (!callExpression) {
                _errors.push_back({"unexpected expression statement", peekToken().line, peekToken().col});
                skipToNextStatement();
                return nullptr;
            }
            // Transfer ownership of the FunctionCallExpression
            std::unique_ptr<FunctionCallExpression> callNode(static_cast<FunctionCallExpression*>(prefix.release()));
            return std::make_unique<CallStatement>(std::move(callNode));
        }
    }

    std::unique_ptr<Statement> Parser::parseLabel() {
        consume(TokenType::Op_DoubleColon, "internal error: '::' expected for label");
        if (!check(TokenType::Name)) {
            _errors.push_back({"expected label name after '::'", peekToken().line, peekToken().col});
            return nullptr;
        }
        std::string name = peekToken().text;
        _pos++;
        consume(TokenType::Op_DoubleColon, "expected '::' after label name");
        return std::make_unique<LabelStatement>(name);
    }

    std::unique_ptr<Statement> Parser::parseIf() {
        consume(TokenType::K_if, "internal error: 'if' expected");
        auto ifStmt = std::make_unique<IfStatement>();
        auto cond = parseExpression();
        if (!cond) {
            _errors.push_back({"expected condition after 'if'", peekToken().line, peekToken().col});
        }
        consume(TokenType::K_then, "expected 'then' after condition");
        // Parse the 'then' block
        auto thenBlock = std::make_unique<Block>();
        while (not check(TokenType::K_end) and not check(TokenType::K_else) and
               not check(TokenType::K_elseif) and not isAtEnd()) {
            auto st = parseStat();
            if (st) thenBlock->statements.push_back(std::move(st));
            if (check(TokenType::K_end) or check(TokenType::K_else) or
                check(TokenType::K_elseif) or check(TokenType::K_until) or check(TokenType::EOF_)) {
                break;
            }
        }
        ifStmt->ifBranches.push_back({ std::move(cond), std::move(thenBlock) });
        // Parse any number of elseif clauses
        while (match(TokenType::K_elseif)) {
            auto elseifCond = parseExpression();
            if (!elseifCond) {
                _errors.push_back({"expected condition after 'elseif'", peekToken().line, peekToken().col});
            }
            consume(TokenType::K_then, "expected 'then' after condition");
            auto elseifBlock = std::make_unique<Block>();
            while (!check(TokenType::K_end) and !check(TokenType::K_else) and
                   !check(TokenType::K_elseif) and !isAtEnd()) {
                auto st = parseStat();
                if (st) elseifBlock->statements.push_back(std::move(st));
                if (check(TokenType::K_end) or check(TokenType::K_else) or
                    check(TokenType::K_elseif) or check(TokenType::K_until) or check(TokenType::EOF_)) {
                    break;
                }
            }
            ifStmt->ifBranches.push_back({ std::move(elseifCond), std::move(elseifBlock) });
        }
        // Optional else clause
        if (match(TokenType::K_else)) {
            auto elseBlock = std::make_unique<Block>();
            while (!check(TokenType::K_end) and !isAtEnd()) {
                auto st = parseStat();
                if (st) elseBlock->statements.push_back(std::move(st));
                if (check(TokenType::K_end) or check(TokenType::K_until) or check(TokenType::EOF_)) break;
            }
            ifStmt->elseBlock = std::move(elseBlock);
        }
        consume(TokenType::K_end, "expected 'end' to close 'if'");
        return ifStmt;
    }

    std::unique_ptr<Statement> Parser::parseWhile() {
        consume(TokenType::K_while, "internal error: 'while' expected");
        auto whileStmt = std::make_unique<WhileStatement>();
        whileStmt->condition = parseExpression();
        if (!whileStmt->condition) {
            _errors.push_back({"expected condition after 'while'", peekToken().line, peekToken().col});
        }
        consume(TokenType::K_do, "expected 'do' after condition");
        auto bodyBlock = std::make_unique<Block>();
        while (!check(TokenType::K_end) and !isAtEnd()) {
            auto st = parseStat();
            if (st) bodyBlock->statements.push_back(std::move(st));
            if (check(TokenType::K_end) or check(TokenType::K_until) or check(TokenType::EOF_)) break;
        }
        consume(TokenType::K_end, "expected 'end' to close 'while'");
        whileStmt->body = std::move(bodyBlock);
        return whileStmt;
    }

    std::unique_ptr<Statement> Parser::parseRepeat() {
        consume(TokenType::K_repeat, "internal error: 'repeat' expected");
        auto repeatStmt = std::make_unique<RepeatStatement>();
        auto bodyBlock = std::make_unique<Block>();
        while (!check(TokenType::K_until) and !isAtEnd()) {
            auto st = parseStat();
            if (st) bodyBlock->statements.push_back(std::move(st));
            if (check(TokenType::K_until) or check(TokenType::EOF_)) break;
        }
        consume(TokenType::K_until, "expected 'until' after 'repeat' block");
        repeatStmt->body = std::move(bodyBlock);
        repeatStmt->condition = parseExpression();
        if (!repeatStmt->condition) {
            _errors.push_back({"expected condition after 'until'", peekToken().line, peekToken().col});
        }
        return repeatStmt;
    }

    std::unique_ptr<Statement> Parser::parseFor() {
        consume(TokenType::K_for, "internal error: 'for' expected");
        if (!check(TokenType::Name)) {
            _errors.push_back({"expected identifier after 'for'", peekToken().line, peekToken().col});
            skipToNextStatement();
            return nullptr;
        }
        std::string varName = peekToken().text;
        _pos++;
        if (match(TokenType::Op_Assign)) {
            // Numeric for: for Name = exp, exp [, exp] do ... end
            auto forNum = std::make_unique<ForNumericStatement>();
            forNum->varName = varName;
            forNum->startExp = parseExpression();
            consume(TokenType::Op_Comma, "expected ',' after start value");
            forNum->endExp = parseExpression();
            if (match(TokenType::Op_Comma)) {
                forNum->stepExp = parseExpression();
            }
            consume(TokenType::K_do, "expected 'do' in numeric for");
            auto bodyBlock = std::make_unique<Block>();
            while (!check(TokenType::K_end) and !isAtEnd()) {
                auto st = parseStat();
                if (st) bodyBlock->statements.push_back(std::move(st));
                if (check(TokenType::K_end) or check(TokenType::EOF_)) break;
            }
            consume(TokenType::K_end, "expected 'end' to close 'for'");
            forNum->body = std::move(bodyBlock);
            return forNum;
        } else {
            // Generic for: for Name[, Name...] in explist do ... end
            std::vector<std::string> nameList;
            nameList.push_back(varName);
            while (match(TokenType::Op_Comma)) {
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name in for-in loop", peekToken().line, peekToken().col});
                    break;
                }
                nameList.push_back(peekToken().text);
                _pos++;
            }
            consume(TokenType::K_in, "expected 'in' in for loop");
            auto exprs = parseExpressionList();
            consume(TokenType::K_do, "expected 'do' in for loop");
            auto bodyBlock = std::make_unique<Block>();
            while (!check(TokenType::K_end) and !isAtEnd()) {
                auto st = parseStat();
                if (st) bodyBlock->statements.push_back(std::move(st));
                if (check(TokenType::K_end) or check(TokenType::EOF_)) break;
            }
            consume(TokenType::K_end, "expected 'end' to close 'for'");
            auto forIn = std::make_unique<ForInStatement>();
            forIn->names = std::move(nameList);
            forIn->exprs = std::move(exprs);
            forIn->body = std::move(bodyBlock);
            return forIn;
        }
    }

    std::unique_ptr<Statement> Parser::parseDo() {
        consume(TokenType::K_do, "internal error: 'do' expected");
        auto blockNode = std::make_unique<Block>();
        while (!check(TokenType::K_end) and !isAtEnd()) {
            auto st = parseStat();
            if (st) blockNode->statements.push_back(std::move(st));
            if (check(TokenType::K_end) or check(TokenType::EOF_)) break;
        }
        consume(TokenType::K_end, "expected 'end' to close 'do' block");
        return std::make_unique<DoStatement>(std::move(blockNode));
    }

    std::unique_ptr<Statement> Parser::parseFunctionDecl(bool isLocal, bool isGlobal) {
        // 'function' keyword already consumed
        if (!check(TokenType::Name)) {
            _errors.push_back({"expected function name after 'function'", peekToken().line, peekToken().col});
        }
        std::vector<std::string> namePath;
        std::string methodName;
        bool isMethod = false;
        if (check(TokenType::Name)) {
            namePath.push_back(peekToken().text);
            _pos++;
        }
        while (true) {
            if (match(TokenType::Op_Dot)) {
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name after '.' in function name", peekToken().line, peekToken().col});
                    break;
                }
                namePath.push_back(peekToken().text);
                _pos++;
            } else if (match(TokenType::Op_Colon)) {
                isMethod = true;
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name after ':' in function name", peekToken().line, peekToken().col});
                } else {
                    methodName = peekToken().text;
                    _pos++;
                }
                break;
            } else {
                break;
            }
        }
        // Parse function body (parameters and return types)
        auto funcBody = std::make_unique<FunctionBody>();
        if (match(TokenType::Op_Less)) {  // generic type parameters
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected type parameter name", peekToken().line, peekToken().col});
            }
            while (check(TokenType::Name)) {
                funcBody->typeParams.push_back(peekToken().text);
                _pos++;
                if (!match(TokenType::Op_Comma)) break;
            }
            consume(TokenType::Op_Greater, "expected '>' after type parameters");
        }
        consume(TokenType::Op_LParen, "expected '(' after function name");
        if (!check(TokenType::Op_RParen)) {
            while (true) {
                if (check(TokenType::Op_VarArg)) {  // '...'
                    _pos++;
                    std::unique_ptr<TypeNode> varType;
                    if (match(TokenType::Op_Colon)) {
                        varType = parseType();
                    }
                    funcBody->params.push_back({ "...", true, false, std::move(varType) });
                    break;
                }
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected parameter name or '...'", peekToken().line, peekToken().col});
                    if (check(TokenType::Op_RParen)) break;
                    _pos++;
                } else {
                    std::string paramName = peekToken().text;
                    _pos++;
                    bool optFlag = false;
                    if (match(TokenType::Op_Question)) optFlag = true;
                    std::unique_ptr<TypeNode> typeAnn;
                    if (match(TokenType::Op_Colon)) {
                        typeAnn = parseType();
                    }
                    funcBody->params.push_back({ paramName, false, optFlag, std::move(typeAnn) });
                }
                if (!match(TokenType::Op_Comma)) break;
            }
        }
        consume(TokenType::Op_RParen, "expected ')' after parameters");
        if (match(TokenType::Op_Colon)) {
            bool retVarArg = false;
            funcBody->returnTypes = parseReturnTypeList(retVarArg);
            funcBody->returnVarArg = retVarArg;
        }
        // Parse function body block
        auto bodyBlock = std::make_unique<Block>();
        while (!check(TokenType::K_end) and !isAtEnd()) {
            auto st = parseStat();
            if (st) bodyBlock->statements.push_back(std::move(st));
            if (check(TokenType::K_end) or check(TokenType::EOF_)) break;
        }
        consume(TokenType::K_end, "expected 'end' to close function");
        funcBody->body = std::move(bodyBlock);
        // Construct AST node
        auto funcDecl = std::make_unique<FunctionDeclarationStatement>(isLocal, isGlobal);
        funcDecl->namePath = std::move(namePath);
        funcDecl->methodName = methodName;
        funcDecl->isMethod = isMethod;
        funcDecl->body = std::move(funcBody);
        return funcDecl;
    }

    std::unique_ptr<Statement> Parser::parseVarDecl(bool isLocal, bool isGlobal) {
        auto varStmt = std::make_unique<VariableDeclarationStatement>(isLocal, isGlobal);
        auto names = parseAttNameList();
        if (names.empty()) {
            _errors.push_back({"expected variable name", peekToken().line, peekToken().col});
        }
        varStmt->names = std::move(names);
        if (match(TokenType::Op_Colon)) {
            varStmt->types = parseTypeList();
        }
        if (match(TokenType::Op_Assign)) {
            varStmt->values = parseExpressionList();
        }
        if (!isLocal and isGlobal) {
            // In Teal, global var must have a type or an initializer
            if (varStmt->types.empty() and varStmt->values.empty()) {
                _errors.push_back({"global variable must have type or initial value", peekToken().line, peekToken().col});
            }
        }
        return varStmt;
    }

    std::unique_ptr<Statement> Parser::parseRecordDecl(bool isLocal, bool isGlobal, bool isInterface) {
        // 'record' or 'interface' already consumed
        if (!check(TokenType::Name)) {
            _errors.push_back({std::string("expected name after '") + (isInterface ? "interface" : "record") + "'", peekToken().line, peekToken().col});
            return nullptr;
        }
        std::string name = peekToken().text;
        _pos++;
        auto body = parseRecordBody();
        return std::make_unique<RecordDeclarationStatement>(isInterface, isLocal, isGlobal, name, std::move(body));
    }

    std::unique_ptr<Statement> Parser::parseEnumDecl(bool isLocal, bool isGlobal) {
        // 'enum' already consumed
        if (!check(TokenType::Name)) {
            _errors.push_back({"expected name after 'enum'", peekToken().line, peekToken().col});
            return nullptr;
        }
        std::string name = peekToken().text;
        _pos++;
        auto body = parseEnumBody();
        return std::make_unique<EnumDeclarationStatement>(isLocal, isGlobal, name, std::move(body));
    }

    std::unique_ptr<Statement> Parser::parseTypeAliasDecl(bool isLocal, bool isGlobal) {
        // 'type' already consumed
        if (!check(TokenType::Name)) {
            _errors.push_back({"expected name after 'type'", peekToken().line, peekToken().col});
            return nullptr;
        }
        std::string name = peekToken().text;
        _pos++;
        std::unique_ptr<TypeNode> typeValue;
        if (match(TokenType::Op_Assign)) {
            // newtype alternatives: record, enum, require, or general type
            if (check(TokenType::K_record)) {
                _pos++;
                typeValue = std::make_unique<TypeRecordNode>( parseRecordBody() );
            } else if (check(TokenType::K_enum)) {
                _pos++;
                typeValue = std::make_unique<TypeEnumNode>( parseEnumBody()->elements );
            }
            // else if (check(TokenType::K_require)) {
            //     _pos++;
            //     _errors.push_back({"type require not supported", currentToken().line, currentToken().col});
            //     // consume(TokenType::Op_LParen, "expected '(' after 'require'");
            //     // std::string moduleName = "";
            //     // if (!check(TokenType::String)) {
            //     //     _errors.push_back({"expected module string in require", currentToken().line, currentToken().col});
            //     // }
            //     // if (check(TokenType::String)) {
            //     //     moduleName = currentToken().text;
            //     //     pos++;
            //     // }
            //     // consume(TokenType::Op_RParen, "expected ')'");
            //     // std::vector<std::string> typeNames;
            //     // while (match(TokenType::Op_Dot)) {
            //     //     if (!check(TokenType::Name)) {
            //     //         _errors.push_back({"expected type name after '.' in require", currentToken().line, currentToken().col});
            //     //         break;
            //     //     }
            //     //     typeNames.push_back(currentToken().text);
            //     //     pos++;
            //     // }
            //     // typeValue = std::make_unique<RequireTypeNode>(moduleName, typeNames);
            // }
            else {
                typeValue = parseType();
            }
        } else {
            if (isLocal) {
                _errors.push_back({"expected '=' in local type alias", peekToken().line, peekToken().col});
            }
            // global type alias may be declared without definition (forward declaration)
        }
        return std::make_unique<TypeAliasStatement>(isLocal, isGlobal, name, std::move(typeValue));
    }

    std::vector<VariableDeclarationStatement::NameAttrib> Parser::parseAttNameList() {
        std::vector<VariableDeclarationStatement::NameAttrib> list;
        if (!check(TokenType::Name)) {
            return list;
        }
        do {
            VariableDeclarationStatement::NameAttrib na;
            na.name = peekToken().text;
            _pos++;
            if (match(TokenType::Op_Less)) {  // attribute like <const>
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected attribute name in '< >'", peekToken().line, peekToken().col});
                } else {
                    na.attrib = peekToken().text;
                    _pos++;
                }
                consume(TokenType::Op_Greater, "expected '>' after attribute");
            }
            list.push_back(std::move(na));
        } while (match(TokenType::Op_Comma));
        return list;
    }

    std::vector<std::string> Parser::parseNameList() {
        std::vector<std::string> list;
        if (!check(TokenType::Name)) {
            return list;
        }
        list.push_back(peekToken().text);
        _pos++;
        while (match(TokenType::Op_Comma)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected name after ','", peekToken().line, peekToken().col});
                break;
            }
            list.push_back(peekToken().text);
            _pos++;
        }
        return list;
    }

    std::unique_ptr<Expression> Parser::parseExpression() {
        return parseExpRec(1);  // start with lowest precedence level
    }

    std::vector<std::unique_ptr<Expression>> Parser::parseExpressionList() {
        std::vector<std::unique_ptr<Expression>> exprs;
        auto first = parseExpression();
        if (first) exprs.push_back(std::move(first));
        while (match(TokenType::Op_Comma)) {
            auto e = parseExpression();
            if (e) {
                exprs.push_back(std::move(e));
            } else {
                _errors.push_back({"expected expression after ','", peekToken().line, peekToken().col});
                if (!check(TokenType::Op_Comma)) break;
            }
        }
        return exprs;
    }

    std::unique_ptr<Expression> Parser::parsePrefixExpression() {
        std::unique_ptr<Expression> base;
        if (match(TokenType::Op_LParen)) {  // parenthesized expression
            base = parseExpression();
            consume(TokenType::Op_RParen, "expected ')'");
        } else if (check(TokenType::Name) /*or check(TokenType::K_require)*/) {
            base = std::make_unique<NameExpression>(peekToken().text);
            _pos++;
        } else {
            _errors.push_back({"expected '(' or Name", peekToken().line, peekToken().col});
            return nullptr;
        }
        // Parse any number of suffix operators: field access, index, call, method call
        while (true) {
            if (match(TokenType::Op_Colon)) {
                // Method call: object:method(args)
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected method name after ':'", peekToken().line, peekToken().col});
                    break;
                }
                std::string method = peekToken().text;
                _pos++;
                // Parse call arguments after method name
                std::vector<std::unique_ptr<Expression>> args;
                if (match(TokenType::Op_LParen)) {
                    if (!check(TokenType::Op_RParen)) {
                        args = parseExpressionList();
                    }
                    consume(TokenType::Op_RParen, "expected ')' after arguments");
                } else if (check(TokenType::Op_LBrace)) {
                    args.push_back(parseTableConstructor());
                } else if (check(TokenType::String)) {
                    args.push_back(std::make_unique<StringExpression>(peekToken().text));
                    _pos++;
                } else {
                    _errors.push_back({"expected arguments after method call", peekToken().line, peekToken().col});
                }
                auto call = std::make_unique<FunctionCallExpression>(std::move(base), method);
                for (auto &arg : args) call->args.push_back(std::move(arg));
                base = std::move(call);
            } else if (match(TokenType::Op_LParen)) {
                // Function call: func(exprList)
                std::vector<std::unique_ptr<Expression>> args;
                if (!check(TokenType::Op_RParen)) {
                    args = parseExpressionList();
                }
                consume(TokenType::Op_RParen, "expected ')'");
                auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
                for (auto &arg : args) call->args.push_back(std::move(arg));
                base = std::move(call);
            } else if (check(TokenType::Op_LBrace)) {
                // Constructor call: func{tableLit}
                auto tableArg = parseTableConstructor();
                auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
                call->args.push_back(std::move(tableArg));
                base = std::move(call);
            } else if (check(TokenType::String)) {
                // String literal call: func"literal"
                std::string lit = peekToken().text;
                _pos++;
                auto call = std::make_unique<FunctionCallExpression>(std::move(base), "");
                call->args.push_back(std::make_unique<StringExpression>(lit));
                base = std::move(call);
            } else if (match(TokenType::Op_Dot)) {
                // Field access: prefix.Name
                if (not check(TokenType::Name)) {
                    //If its `type`, `record`, `enum` or `inteerface` then we can turn that token into a `name` token with that key as the ident

                    bool ok = false;
                    const auto mkname = [this](const std::string_view &id) {
                        return Token { TokenType::Name, std::string(id), peekToken().line, peekToken().col };
                    };
                    switch (peekToken().type) {
                    #define $check(n) case TokenType::K_##n: _tokens[_pos] = mkname(#n); ok = true; break
                        $check(type);
                        $check(record);
                        $check(interface);
                        $check(enum);
                    #undef $check
                    default:
                        ok = false;
                        break;
                    }

                    if (not ok) {
                        _errors.push_back({std::format("expected field name after '.', got `{}`", peekToken().type), peekToken().line, peekToken().col});
                        break;
                    }
                }
                std::string field = peekToken().text;
                _pos++;
                base = std::make_unique<FieldExpression>(std::move(base), field);
            } else if (match(TokenType::Op_LBracket)) {
                // Index access: prefix[exp]
                auto indexExpression = parseExpression();
                consume(TokenType::Op_RBracket, "expected ']'");
                base = std::make_unique<IndexExpression>(std::move(base), std::move(indexExpression));
            } else {
                break;
            }
        }
        return base;
    }

    std::unique_ptr<Expression> Parser::parseVarExpression() {
        // Parse a prefix expression and ensure it's a valid lvalue (not a function call)
        auto expr = parsePrefixExpression();
        if (!expr) return nullptr;
        if (dynamic_cast<FunctionCallExpression*>(expr.get())) {
            _errors.push_back({"unexpected function call in assignment", peekToken().line, peekToken().col});
            return std::make_unique<NameExpression>("_error_");
        }
        return expr;
    }

    std::unique_ptr<Expression> Parser::parsePrimaryExpression() {
        TokenType t = peekToken().type;

        //These can be used as ident names unless theres an ident in front.
        //i.e `record Test`
        //vs `t.record = "hi"`
        //TODO: This doesn't account for stuff like `local type a = record ...`
        // if (peekToken().isTealKeyword() and peekToken(1).type != TokenType::Name) {
        //     _tokens[_pos] = peekToken().tealToName().value();
        // }

        if (t == TokenType::K_nil)    { _pos++; return std::make_unique<NilExpression>(); }
        if (t == TokenType::K_true)   { _pos++; return std::make_unique<BooleanExpression>(true); }
        if (t == TokenType::K_false)  { _pos++; return std::make_unique<BooleanExpression>(false); }
        if (t == TokenType::Number)   { std::string num = peekToken().text; _pos++; return std::make_unique<NumberExpression>(num); }
        if (t == TokenType::String)   { std::string str = peekToken().text; _pos++; return std::make_unique<StringExpression>(str); }
        if (t == TokenType::Op_VarArg){ _pos++; return std::make_unique<VarargExpression>(); }
        if (t == TokenType::K_function) {
            return parseFunctionDefExpression();
        }
        if (t == TokenType::Op_LBrace) {
            return parseTableConstructor();
        }
        if (t == TokenType::Name or t == TokenType::Op_LParen /*or t == TokenType::K_require*/) {
            return parsePrefixExpression();
        }
        _errors.push_back({std::format("unexpected token `{}` (type: {}) in expression", peekToken().text, peekToken().type), peekToken().line, peekToken().col});
        return nullptr;
    }

    std::unique_ptr<Expression> Parser::parseExpRec(int minPrec) {
        // Parse an expression using precedence climbing
        auto left = parseUnaryExpression();
        while (true) {
            TokenType opType = peekToken().type;
            int prec = getBinaryPrecedence(opType);
            if (prec < minPrec) break;
            if (opType == TokenType::K_as) {
                // Cast operator (highest precedence, right-associative by grammar)
                _pos++;
                std::vector<std::unique_ptr<TypeNode>> castTypes;
                if (match(TokenType::Op_LParen)) {
                    if (!check(TokenType::Op_RParen)) {
                        castTypes = parseTypeList();
                        if (match(TokenType::Op_VarArg)) {
                            // '...' in cast typelist (ignore, Teal may not allow but handle gracefully)
                        }
                    }
                    consume(TokenType::Op_RParen, "expected ')'");
                } else {
                    auto typeNode = parseType();
                    if (typeNode) castTypes.push_back(std::move(typeNode));
                }
                left = std::make_unique<CastExpression>(std::move(left), std::move(castTypes));
                continue;
            } else if (opType == TokenType::K_is) {
                // Type test operator
                _pos++;
                auto typeNode = parseType();
                left = std::make_unique<IsTypeExpression>(std::move(left), std::move(typeNode));
                continue;
            }
            // Binary operators
            bool rightAssoc = isRightAssociative(opType);
            int nextMinPrec = (rightAssoc ? prec : prec + 1);
            _pos++;
            auto right = parseExpRec(nextMinPrec);
            left = std::make_unique<BinaryOperationExpression>(opType, std::move(left), std::move(right));
        }
        return left;
    }

    int Parser::getBinaryPrecedence(TokenType op) {
        switch (op) {
        case TokenType::K_or:          return 1;
        case TokenType::K_and:         return 2;
        case TokenType::K_is:          return 3;
        case TokenType::Op_Less: case TokenType::Op_LessEq:
        case TokenType::Op_Greater: case TokenType::Op_GreaterEq:
        case TokenType::Op_Equals: case TokenType::Op_NotEq:   return 4;
        case TokenType::Op_BitOr:      return 5;
        case TokenType::Op_BitXor:     return 6;
        case TokenType::Op_BitAnd:     return 7;
        case TokenType::Op_ShiftL: case TokenType::Op_ShiftR:  return 8;
        case TokenType::Op_Concat:     return 9;
        case TokenType::Op_Add: case TokenType::Op_Sub:        return 10;
        case TokenType::Op_Mul: case TokenType::Op_Div:
        case TokenType::Op_FloorDiv: case TokenType::Op_Mod:   return 11;
        case TokenType::Op_Pow:        return 13;
        case TokenType::K_as:          return 14;
        default:                      return -1;
        }
    }

    bool Parser::isRightAssociative(TokenType op) {
        // Only concatenation (..) and exponentiation (^) are right-associative in Lua
        return (op == TokenType::Op_Concat or op == TokenType::Op_Pow);
    }

    std::unique_ptr<Expression> Parser::parseUnaryExpression() {
        if (check(TokenType::K_not) or check(TokenType::Op_Sub) or
            check(TokenType::Op_Len) or check(TokenType::Op_BitXor)) {
            TokenType opToken = peekToken().type;
            _pos++;
            auto operand = parseUnaryExpression();
            return std::make_unique<UnaryOperationExpression>(opToken, std::move(operand));
        }
        return parsePrimaryExpression();
    }

    std::unique_ptr<Expression> Parser::parseFunctionDefExpression() {
        consume(TokenType::K_function, "internal error: 'function' expected");
        auto funcBody = std::make_unique<FunctionBody>();
        if (match(TokenType::Op_Less)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected type parameter name", peekToken().line, peekToken().col});
            }
            while (check(TokenType::Name)) {
                funcBody->typeParams.push_back(peekToken().text);
                _pos++;
                if (!match(TokenType::Op_Comma)) break;
            }
            consume(TokenType::Op_Greater, "expected '>' after type parameters");
        }
        consume(TokenType::Op_LParen, "expected '(' in function literal");
        if (!check(TokenType::Op_RParen)) {
            while (true) {
                if (check(TokenType::Op_VarArg)) {
                    _pos++;
                    std::unique_ptr<TypeNode> varType;
                    if (match(TokenType::Op_Colon)) {
                        varType = parseType();
                    }
                    funcBody->params.push_back({ "...", true, false, std::move(varType) });
                    break;
                }
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected parameter name or '...'", peekToken().line, peekToken().col});
                    if (check(TokenType::Op_RParen)) break;
                    _pos++;
                } else {
                    std::string paramName = peekToken().text;
                    _pos++;
                    bool optFlag = false;
                    if (match(TokenType::Op_Question)) optFlag = true;
                    std::unique_ptr<TypeNode> typeAnn;
                    if (match(TokenType::Op_Colon)) {
                        typeAnn = parseType();
                    }
                    funcBody->params.push_back({ paramName, false, optFlag, std::move(typeAnn) });
                }
                if (!match(TokenType::Op_Comma)) break;
            }
        }
        consume(TokenType::Op_RParen, "expected ')' in function literal");
        if (match(TokenType::Op_Colon)) {
            bool retVarArg = false;
            funcBody->returnTypes = parseReturnTypeList(retVarArg);
            funcBody->returnVarArg = retVarArg;
        }
        auto bodyBlock = std::make_unique<Block>();
        while (!check(TokenType::K_end) and !isAtEnd()) {
            auto st = parseStat();
            if (st) bodyBlock->statements.push_back(std::move(st));
            if (check(TokenType::K_end) or check(TokenType::EOF_)) break;
        }
        consume(TokenType::K_end, "expected 'end' to close function");
        funcBody->body = std::move(bodyBlock);
        return std::make_unique<FunctionDefinitionExpression>(std::move(funcBody));
    }

    std::unique_ptr<Expression> Parser::parseTableConstructor() {
        consume(TokenType::Op_LBrace, "internal error: '{' expected");
        auto table = std::make_unique<TableConstructorExpression>();
        if (!check(TokenType::Op_RBrace)) {
            while (true) {
                TableConstructorExpression::Field field;
                if (match(TokenType::Op_LBracket)) {
                    // [exp] = exp field
                    auto keyExpression = parseExpression();
                    consume(TokenType::Op_RBracket, "expected ']'");
                    consume(TokenType::Op_Assign, "expected '=' after key");
                    field.keyExpr = std::move(keyExpression);
                    field.value = parseExpression();
                } else if (check(TokenType::Name)) {
                    // Name = exp field (with optional type annotation)
                    std::string name = peekToken().text;
                    // Look ahead for ':' or '='
                    if (_pos + 1 < _tokens.size() and (_tokens[_pos+1].type == TokenType::Op_Colon or _tokens[_pos+1].type == TokenType::Op_Assign)) {
                        _pos++;
                        field.nameKey = name;
                        if (match(TokenType::Op_Colon)) {
                            field.typeAnn = parseType();
                        }
                        consume(TokenType::Op_Assign, "expected '=' after field name");
                        field.value = parseExpression();
                    } else {
                        // Otherwise treat as an expression value (e.g. in array literal context)
                        field.value = parseExpression();
                    }
                } else {
                    // Implicit array value field
                    field.value = parseExpression();
                }
                table->fields.push_back(std::move(field));
                if (match(TokenType::Op_Comma) or match(TokenType::Op_Semicolon)) {
                    if (check(TokenType::Op_RBrace)) break;  // allow trailing separator
                    continue;
                } else {
                    break;
                }
            }
        }
        consume(TokenType::Op_RBrace, "expected '}'");
        return table;
    }

    std::unique_ptr<TypeNode> Parser::parseType() {
        std::unique_ptr<TypeNode> firstType;
        if (match(TokenType::Op_LParen)) {
            firstType = parseType();
            consume(TokenType::Op_RParen, "expected ')'");
        } else {
            firstType = parseBaseType();
        }
        if (!firstType) {
            firstType = std::make_unique<BasicTypeNode>("nil");
        }
        if (check(TokenType::Op_BitOr)) {
            // parse union types
            auto unionNode = std::make_unique<UnionTypeNode>();
            unionNode->options.push_back(std::move(firstType));
            while (match(TokenType::Op_BitOr)) {
                auto nextType = parseBaseType();
                if (!nextType) nextType = std::make_unique<BasicTypeNode>("nil");
                unionNode->options.push_back(std::move(nextType));
            }
            return unionNode;
        }
        return firstType;
    }

    std::unique_ptr<TypeNode> Parser::parseBaseType() {
        if (check(TokenType::Name)) {
            std::string name = peekToken().text;
            // Check if it's a primitive type name
            if (name == "string" or name == "number" or name == "boolean" or name == "nil") {
                _pos++;
                return std::make_unique<BasicTypeNode>(name);
            }
        }
        if (check(TokenType::Name)) {
            return parseNominalType();
        }
        if (check(TokenType::Op_LBrace)) {
            _pos++;
            auto firstType = parseType();
            if (!firstType) firstType = std::make_unique<BasicTypeNode>("nil");
            if (match(TokenType::Op_Colon)) {
                // Map type: { keyType : valueType }
                auto secondType = parseType();
                if (!secondType) secondType = std::make_unique<BasicTypeNode>("nil");
                consume(TokenType::Op_RBrace, "expected '}'");
                auto mapNode = std::make_unique<TableTypeNode>();
                mapNode->isMap = true;
                mapNode->keyType = std::move(firstType);
                mapNode->elementTypes.push_back(std::move(secondType));
                return mapNode;
            } else {
                // Array/tuple type: { type, ... }
                std::vector<std::unique_ptr<TypeNode>> types;
                types.push_back(std::move(firstType));
                while (match(TokenType::Op_Comma)) {
                    auto t = parseType();
                    if (!t) t = std::make_unique<BasicTypeNode>("nil");
                    types.push_back(std::move(t));
                }
                consume(TokenType::Op_RBrace, "expected '}'");
                if (types.size() == 1) {
                    // Single type in braces -> array type
                    auto arrNode = std::make_unique<TableTypeNode>();
                    arrNode->isMap = false;
                    arrNode->elementTypes.push_back(std::move(types[0]));
                    return arrNode;
                } else {
                    // Multiple types -> tuple type
                    auto tupleNode = std::make_unique<TableTypeNode>();
                    tupleNode->isMap = false;
                    for (auto &tt : types) tupleNode->elementTypes.push_back(std::move(tt));
                    return tupleNode;
                }
            }
        }
        if (check(TokenType::K_function)) {
            return parseFunctionType();
        }
        _errors.push_back({"expected type", peekToken().line, peekToken().col});
        return nullptr;
    }

    std::unique_ptr<TypeNode> Parser::parseNominalType() {
        // Parse a qualified name (Name or Name.Name...) with optional <...> type args
        std::vector<std::string> nameParts;
        if (!check(TokenType::Name)) {
            return nullptr;
        }
        nameParts.push_back(peekToken().text);
        _pos++;
        while (match(TokenType::Op_Dot)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected name after '.' in type name", peekToken().line, peekToken().col});
                break;
            }
            nameParts.push_back(peekToken().text);
            _pos++;
        }
        std::vector<std::string> typeArgs;
        if (match(TokenType::Op_Less)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected type name in type arguments", peekToken().line, peekToken().col});
            }
            while (check(TokenType::Name)) {
                typeArgs.push_back(peekToken().text);
                _pos++;
                if (!match(TokenType::Op_Comma)) break;
            }
            consume(TokenType::Op_Greater, "expected '>' after type arguments");
        }
        return std::make_unique<NominalTypeNode>(nameParts, typeArgs);
    }

    std::unique_ptr<TypeNode> Parser::parseFunctionType() {
        consume(TokenType::K_function, "internal error: 'function' expected");
        auto node = std::make_unique<FunctionTypeNode>();
        if (match(TokenType::Op_Less)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected type parameter name", peekToken().line, peekToken().col});
            }
            while (check(TokenType::Name)) {
                node->typeParams.push_back(peekToken().text);
                _pos++;
                if (!match(TokenType::Op_Comma)) break;
            }
            consume(TokenType::Op_Greater, "expected '>' after type parameters");
        }
        consume(TokenType::Op_LParen, "expected '(' in function type");
        if (!check(TokenType::Op_RParen)) {
            while (true) {
                FunctionTypeNode::ParamType param;
                if (check(TokenType::Name)) {
                    param.name = peekToken().text;
                    _pos++;
                    param.isOptional = false;
                    if (match(TokenType::Op_Question)) param.isOptional = true;
                    consume(TokenType::Op_Colon, "expected ':' after parameter name");
                    param.type = parseType();
                } else if (match(TokenType::Op_Question)) {
                    param.name.reset();
                    param.isOptional = true;
                    param.type = parseType();
                } else {
                    param.name.reset();
                    param.isOptional = false;
                    param.type = parseType();
                }
                node->params.push_back(std::move(param));
                if (!match(TokenType::Op_Comma)) break;
            }
        }
        consume(TokenType::Op_RParen, "expected ')'");
        if (match(TokenType::Op_Colon)) {
            bool varArg = false;
            node->returnTypes = parseReturnTypeList(varArg);
            node->returnVarArg = varArg;
        }
        return node;
    }

    std::vector<std::unique_ptr<TypeNode>> Parser::parseTypeList() {
        std::vector<std::unique_ptr<TypeNode>> types;
        auto first = parseType();
        if (first) types.push_back(std::move(first));
        else types.push_back(std::make_unique<BasicTypeNode>("nil"));
        while (match(TokenType::Op_Comma)) {
            auto t = parseType();
            if (t) types.push_back(std::move(t));
            else types.push_back(std::make_unique<BasicTypeNode>("nil"));
        }
        return types;
    }

    std::vector<FunctionTypeNode::ParamType> Parser::parseParamTypeList() {
        // (Not used separately in this implementation; integrated into parseFunctionType)
        return {};
    }

    std::vector<std::unique_ptr<TypeNode>> Parser::parseReturnTypeList(bool &varArg) {
        std::vector<std::unique_ptr<TypeNode>> types;
        varArg = false;
        if (match(TokenType::Op_LParen)) {
            if (!check(TokenType::Op_RParen)) {
                types = parseTypeList();
                if (match(TokenType::Op_VarArg)) varArg = true;
            }
            consume(TokenType::Op_RParen, "expected ')'");
        } else {
            types = parseTypeList();
            if (match(TokenType::Op_VarArg)) varArg = true;
        }
        return types;
    }

    std::unique_ptr<RecordBody> Parser::parseRecordBody() {
        auto rb = std::make_unique<RecordBody>();
        if (match(TokenType::Op_Less)) {
            if (!check(TokenType::Name)) {
                _errors.push_back({"expected type parameter name", peekToken().line, peekToken().col});
            }
            while (check(TokenType::Name)) {
                rb->typeParams.push_back(peekToken().text);
                _pos++;
                if (!match(TokenType::Op_Comma)) break;
            }
            consume(TokenType::Op_Greater, "expected '>' after type parameters");
        }
        if (match(TokenType::K_is)) {
            parseInterfaceList(*rb);
        }
        if (match(TokenType::K_where)) {
            rb->whereClause = parseExpression();
            if (!rb->whereClause) {
                _errors.push_back({"expected expression after 'where'", peekToken().line, peekToken().col});
            }
        }
        while (!check(TokenType::K_end) and !isAtEnd()) {
            RecordBody::Entry entry;
            if (check(TokenType::Name) and peekToken().text == "userdata") {
                _pos++;
                entry.kind = RecordBody::Entry::Kind::Userdata;
                rb->entries.push_back(std::move(entry));
                continue;
            }
            if (match(TokenType::K_type)) {
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name after 'type'", peekToken().line, peekToken().col});
                } else {
                    entry.typeName = peekToken().text;
                    _pos++;
                }
                consume(TokenType::Op_Assign, "expected '=' after type name");
                entry.typeValue = parseType();
                entry.kind = RecordBody::Entry::Kind::TypeAlias;
                rb->entries.push_back(std::move(entry));
                continue;
            }
            if (match(TokenType::K_record)) {
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name after 'record'", peekToken().line, peekToken().col});
                } else {
                    entry.nestedName = peekToken().text;
                    _pos++;
                }
                entry.nestedBody = parseRecordBody();
                entry.kind = RecordBody::Entry::Kind::Record;
                rb->entries.push_back(std::move(entry));
                continue;
            }
            if (match(TokenType::K_enum)) {
                if (!check(TokenType::Name)) {
                    _errors.push_back({"expected name after 'enum'", peekToken().line, peekToken().col});
                } else {
                    entry.nestedName = peekToken().text;
                    _pos++;
                }
                entry.nestedBody = parseEnumBody();
                entry.kind = RecordBody::Entry::Kind::Enum;
                rb->entries.push_back(std::move(entry));
                continue;
            }
            bool isMeta = false;
            if (check(TokenType::Name) and peekToken().text == "metamethod") {
                _pos++;
                isMeta = true;
            }
            if (check(TokenType::Name)) {
                entry.fieldName = peekToken().text;
                _pos++;
            } else if (match(TokenType::Op_LBracket)) {
                if (!check(TokenType::String)) {
                    _errors.push_back({"expected literal string key in record field", peekToken().line, peekToken().col});
                } else {
                    entry.fieldKeyLiteral = peekToken().text;
                    _pos++;
                }
                consume(TokenType::Op_RBracket, "expected ']'");
            } else {
                if (check(TokenType::K_end) or isAtEnd()) break;
                _errors.push_back({"unexpected token in record body", peekToken().line, peekToken().col});
                _pos++;
                continue;
            }
            consume(TokenType::Op_Colon, "expected ':' after record key");
            entry.fieldType = parseType();
            entry.isMetamethod = isMeta;
            entry.kind = RecordBody::Entry::Kind::Field;
            rb->entries.push_back(std::move(entry));
        }
        consume(TokenType::K_end, "expected 'end' to close record");
        return rb;
    }

    std::unique_ptr<EnumBody> Parser::parseEnumBody() {
        auto body = std::make_unique<EnumBody>();
        while (!check(TokenType::K_end) and !isAtEnd()) {
            if (check(TokenType::String)) {
                body->elements.push_back(peekToken().text);
                _pos++;
                if (match(TokenType::Op_Comma) or match(TokenType::Op_Semicolon)) {
                    continue;
                }
            } else if (check(TokenType::K_end)) {
                break;
            } else {
                _errors.push_back({"expected string in enum", peekToken().line, peekToken().col});
                _pos++;
            }
        }
        consume(TokenType::K_end, "expected 'end' to close enum");
        return body;
    }

    void Parser::parseInterfaceList(RecordBody &rb) {
        // Parse the interfacelist in a record 'is' clause
        if (match(TokenType::Op_LBrace)) {
            // structural type provided
            rb.structuralExt = parseType();
            consume(TokenType::Op_RBrace, "expected '}' in interface list");
            if (match(TokenType::Op_Comma)) {
                do {
                    auto nom = parseNominalType();
                    if (nom) rb.interfaceExt.push_back(std::move(nom));
                    else {
                        _errors.push_back({"expected interface name", peekToken().line, peekToken().col});
                        break;
                    }
                } while (match(TokenType::Op_Comma));
            }
        } else {
            auto nom = parseNominalType();
            if (nom) rb.interfaceExt.push_back(std::move(nom));
            else _errors.push_back({"expected interface name", peekToken().line, peekToken().col});
            while (match(TokenType::Op_Comma)) {
                auto nom2 = parseNominalType();
                if (nom2) rb.interfaceExt.push_back(std::move(nom2));
                else {
                    _errors.push_back({"expected interface name", peekToken().line, peekToken().col});
                    break;
                }
            }
        }
    }

