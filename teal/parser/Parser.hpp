#pragma once

#include "AST.hpp"

namespace teal
{

    class Parser {
    public:
        Parser(std::vector<Token> toks) : _tokens(std::move(toks)), _pos(0) {}
        std::unique_ptr<Block> parseChunk();
        const std::vector<ParseError> &errors() const { return _errors; }
    private:
        std::vector<Token> _tokens;
        size_t _pos;
        std::vector<ParseError> _errors;
        // Helper functions to inspect and consume tokens
        const Token &peekToken(int forward = 0) const { return _tokens[_pos+forward]; }
        bool isAtEnd() const { return peekToken().type == TokenType::EOF_; }
        bool check(TokenType t) const { return peekToken().type == t; }
        bool match(TokenType t) { if (check(t)) { _pos++; return true; } return false; }
        bool matchAny(std::initializer_list<TokenType> types) {
            if (check(TokenType::EOF_)) return false;
            for (TokenType t : types) {
                if (check(t)) { _pos++; return true; }
            }
            return false;
        }
        std::optional<Token> consume(TokenType t, const std::string &errMsg) {
            if (check(t)) {
                Token tok = peekToken();
                _pos++;
                return tok;
            } else {
                _errors.push_back({ errMsg, peekToken().line, peekToken().col });
                // Return a dummy token of the expected type (without consuming current token)
                return std::nullopt;
            }
        }
        void skipToNextStatement() {
            while (!isAtEnd()) {
                TokenType t = peekToken().type;
                if (t == TokenType::Op_Semicolon or t == TokenType::K_return or t == TokenType::K_break or
                    t == TokenType::K_global or t == TokenType::K_local or t == TokenType::K_if or t == TokenType::K_while or
                    t == TokenType::K_for or t == TokenType::K_function or t == TokenType::K_repeat or
                    t == TokenType::K_end or t == TokenType::K_until or t == TokenType::K_else or t == TokenType::K_elseif) {
                    return;  // stop at a likely statement boundary or block terminator
                }
                _pos++;
            }
        }
        // Recursive descent parsing functions
        std::unique_ptr<Statement> parseStat();
        std::unique_ptr<Statement> parseAssignmentOrCall();
        std::unique_ptr<Statement> parseLabel();
        std::unique_ptr<Statement> parseIf();
        std::unique_ptr<Statement> parseWhile();
        std::unique_ptr<Statement> parseRepeat();
        std::unique_ptr<Statement> parseFor();
        std::unique_ptr<Statement> parseDo();
        std::unique_ptr<Statement> parseFunctionDecl(bool isLocal, bool isGlobal);
        std::unique_ptr<Statement> parseVarDecl(bool isLocal, bool isGlobal);
        std::unique_ptr<Statement> parseRecordDecl(bool isLocal, bool isGlobal, bool isInterface);
        std::unique_ptr<Statement> parseEnumDecl(bool isLocal, bool isGlobal);
        std::unique_ptr<Statement> parseTypeAliasDecl(bool isLocal, bool isGlobal);
        std::vector<VariableDeclarationStatement::NameAttrib> parseAttNameList();
        std::vector<std::string> parseNameList();
        std::unique_ptr<Expression> parseExpression();
        std::vector<std::unique_ptr<Expression>> parseExpressionList();
        std::unique_ptr<Expression> parsePrefixExpression();
        std::unique_ptr<Expression> parseVarExpression();
        std::unique_ptr<Expression> parsePrimaryExpression();
        std::unique_ptr<Expression> parseExpRec(int minPrec);
        int getBinaryPrecedence(TokenType op);
        bool isRightAssociative(TokenType op);
        std::unique_ptr<Expression> parseUnaryExpression();
        std::unique_ptr<Expression> parseFunctionDefExpression();
        std::unique_ptr<Expression> parseTableConstructor();
        std::unique_ptr<TypeNode> parseType();
        std::unique_ptr<TypeNode> parseBaseType();
        std::unique_ptr<TypeNode> parseNominalType();
        std::unique_ptr<TypeNode> parseFunctionType();
        std::vector<std::unique_ptr<TypeNode>> parseTypeList();
        std::vector<FunctionTypeNode::ParamType> parseParamTypeList();
        std::vector<std::unique_ptr<TypeNode>> parseReturnTypeList(bool &varArg);
        std::unique_ptr<RecordBody> parseRecordBody();
        std::unique_ptr<EnumBody> parseEnumBody();
        void parseInterfaceList(RecordBody &rb);
    };
};
