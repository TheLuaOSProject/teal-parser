#include "TypeChecker.hpp"
#include <algorithm>
#include <cmath>

using namespace teal::parser::ast;
using namespace teal::parser::typecheck;

TypeChecker::TypeChecker() : loopDepth(0) { }

void TypeChecker::check(Block *ast)
{
    for (auto &stmt : ast->statements) {
        if (not stmt) continue;
        if (auto rec = dynamic_cast<RecordDeclarationStatement *>(stmt.get())) {
            declareType(rec, nullptr);
        } else if (auto en = dynamic_cast<EnumDeclarationStatement *>(stmt.get())) {
            declareType(en, nullptr);
        } else if (auto ta = dynamic_cast<TypeAliasStatement *>(stmt.get())) {
            declareType(ta, nullptr);
        } else if (auto func = dynamic_cast<FunctionDeclarationStatement *>(stmt.get())) {
            checkFunctionDecl(func);
        } else if (auto var = dynamic_cast<VariableDeclarationStatement *>(stmt.get())) {
            checkVarDecl(var);
        } else if (auto assign = dynamic_cast<AssignmentStatement *>(stmt.get())) {
            checkAssignment(assign);
        } else if (auto ifStmt = dynamic_cast<IfStatement *>(stmt.get())) {
            checkIf(ifStmt);
        } else if (auto whileStmt = dynamic_cast<WhileStatement *>(stmt.get())) {
            checkWhile(whileStmt);
        } else if (auto repeatStmt = dynamic_cast<RepeatStatement *>(stmt.get())) {
            checkRepeat(repeatStmt);
        } else if (auto forNum = dynamic_cast<ForNumericStatement *>(stmt.get())) {
            checkForNumeric(forNum);
        } else if (auto forIn = dynamic_cast<ForInStatement *>(stmt.get())) {
            checkForIn(forIn);
        }
    }
    // second pass (define types) happens during statement checking
}

void TypeChecker::declareType(RecordDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    TypeSymbol::Kind k = node->is_interface ? TypeSymbol::Kind::Interface : TypeSymbol::Kind::Record;
    auto typeSym = std::make_shared<TypeSymbol>(k, node->name);
    typeSym->is_interface = node->is_interface;
    typeSym->ast_record_body = node->body.get();
    if (parent) {
        parent->nested_types[node->name] = std::move(typeSym);
        typeSym->parent = parent;
    }
    for (auto &gp : node->body->type_parameters) {
        TypePtr constraintType = nullptr;
        if (gp.is.has_value()) {
            std::shared_ptr<TypeSymbol> constraintSym = findTypeSymbol({ gp.is.value() });
            if (not constraintSym) {
                errors.addError(node->line, node->column, "Unknown type in generic constraint: " + gp.is.value());
            } else {
                constraintType = constraintSym->type;
                if (constraintSym->kind != TypeSymbol::Kind::Interface) { errors.addError(node->line, node->column, "Generic constraint must be an interface type"); }
            }
        }
        TypePtr tvar = Type::makeTypeVar(gp.name, constraintType);
        tvar->type_var->ownerTypeSymbol = typeSym;
        typeSym->type_params.push_back(tvar);
    }
    TypePtr recType = Type::makeRecord(typeSym);
    if (not parent) {
        bool ok = symbols.define(node->name, recType, true, node->visibility == Visibility::GLOBAL);
        if (not ok) errors.addError(node->line, node->column, "Duplicate type name: " + node->name);
    }
}

void TypeChecker::declareType(EnumDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    auto typeSym = std::make_shared<TypeSymbol>(TypeSymbol::Kind::Enum, node->name);
    typeSym->parent = parent;
    if (node->body) {
        for (auto &elem : node->body->elements) { typeSym->enum_values.push_back(elem); }
    }

    TypePtr enumType = Type::makeEnum(typeSym);
    if (parent) {
        parent->nested_types[node->name] = typeSym;
    } else {
        bool ok = symbols.define(node->name, enumType, true, node->visibility == Visibility::GLOBAL);
        if (not ok) errors.addError(node->line, node->column, "Duplicate type name: " + node->name);
    }
}

void TypeChecker::declareType(TypeAliasStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    auto typeSym = std::make_shared<TypeSymbol>(TypeSymbol::Kind::Alias, node->name);
    typeSym->parent = parent;
    for (auto &gp : node->type_parameters) {
        TypePtr constraintType = nullptr;
        if (gp.is.has_value()) {
            std::shared_ptr<TypeSymbol> constraintSym = findTypeSymbol({ gp.is.value() });
            if (constraintSym) {
                constraintType = constraintSym->type;
            } else {
                errors.addError(node->line, node->column, "Unknown constraint type " + gp.is.value() + " for alias " + node->name);
            }
        }
        TypePtr tvar = Type::makeTypeVar(gp.name, constraintType);
        tvar->type_var->ownerTypeSymbol = typeSym;
        typeSym->type_params.push_back(tvar);
    }
    typeSym->ast_type_alias = node;
    typeSym->type = nullptr;
    if (parent) { parent->nested_types[node->name] = typeSym; }
}

std::shared_ptr<TypeSymbol> TypeChecker::findTypeSymbol(const std::vector<std::string> &nameParts)
{
    if (nameParts.empty()) return nullptr;
    VarInfo *v = symbols.lookupGlobal(nameParts[0]);
    std::shared_ptr<TypeSymbol> sym = nullptr;
    if (v and v->type) {
        TypePtr t = v->type;
        if (t->kind == Type::Kind::Record) sym = t->record;
        else if (t->kind == Type::Kind::Enum) sym = t->enum_type;
    }
    for (size_t i = 1; sym and i < nameParts.size(); ++i) {
        auto it = sym->nested_types.find(nameParts[i]);
        sym = (it != sym->nested_types.end()) ? it->second : nullptr;
    }
    return sym;
}

void TypeChecker::defineType(std::shared_ptr<TypeSymbol> typeSymbol)
{
    if (not typeSymbol) return;
    if (typeSymbol->kind == TypeSymbol::Kind::Record or typeSymbol->kind == TypeSymbol::Kind::Interface) {
        RecordBody *body = typeSymbol->ast_record_body;
        if (body) {
            if (body->structural_ext) {
                TypePtr extType = resolveTypeNode(body->structural_ext.get(), typeSymbol);
                if (extType) {
                    if (extType->kind == Type::Kind::Array) {
                        typeSymbol->array_element_type = extType->element;
                    } else if (extType->kind == Type::Kind::Map) {
                        typeSymbol->array_element_type = extType->value;
                    } else {
                        errors.addError(body->line, body->column, "Invalid structural extension type");
                    }
                }
            }
            for (auto &intfNode : body->interface_ext) {
                TypePtr intfType = resolveTypeNode(intfNode.get());
                if (intfType) {
                    if (intfType->kind == Type::Kind::Record and intfType->record->kind == TypeSymbol::Kind::Interface) {
                        typeSymbol->interfaces.push_back(intfType);
                    } else {
                        errors.addError(body->line, body->column, "Type is not an interface: " + intfType->toString());
                    }
                }
            }
        }
        for (auto &[key, val] : typeSymbol->nested_types) {
            defineType(val);
            if (val->type) { typeSymbol->fields[key] = val->type; }
        }
    } else if (typeSymbol->kind == TypeSymbol::Kind::Alias) {
        if (typeSymbol->ast_type_alias) {
            TypePtr aliasType = resolveTypeNode(typeSymbol->ast_type_alias->type.get(), typeSymbol);
            if (aliasType) {
                typeSymbol->alias_target = aliasType;
            } else {
                errors.addError(typeSymbol->ast_type_alias->line, typeSymbol->ast_type_alias->column, "Cannot resolve alias target type");
            }
        }
    }
}

TypePtr TypeChecker::resolveTypeNode(TypeNode *typeNode, std::shared_ptr<TypeSymbol> currentTypeSym)
{
    if (not typeNode) return nullptr;
    if (auto basic = dynamic_cast<BasicTypeNode *>(typeNode)) {
        std::string name = basic->name;
        if (name == "number") return Type::makeNumber();
        if (name == "integer") return Type::makeInteger();
        if (name == "string") return Type::makeString();
        if (name == "boolean") return Type::makeBoolean();
        if (name == "nil") return Type::makeNil();
        if (name == "any") return Type::makeAny();
        if (currentTypeSym) {
            for (auto &tv : currentTypeSym->type_params) {
                if (tv->kind == Type::Kind::TypeVar and tv->type_var->name == name) { return tv; }
            }
        }
        std::shared_ptr<TypeSymbol> sym = findTypeSymbol({ name });
        if (sym) { return sym->type ? sym->type : Type::makeAny(); }
        errors.addError(typeNode->line, typeNode->column, "Unknown type: " + name);
        return Type::makeAny();
    }
    if (auto nom = dynamic_cast<NominalTypeNode *>(typeNode)) {
        std::shared_ptr<TypeSymbol> sym = findTypeSymbol(nom->name_parts);
        if (not sym) {
            errors.addError(typeNode->line, typeNode->column, "Unknown type: " + nom->name_parts.front());
            return Type::makeAny();
        }
        std::vector<TypePtr> typeArgs;
        for (auto &argNode : nom->type_arguments) { typeArgs.push_back(resolveTypeNode(argNode.get(), currentTypeSym)); }
        if (not sym->type_params.empty() and typeArgs.size() != sym->type_params.size()) { errors.addError(typeNode->line, typeNode->column, "Generic type parameters count mismatch for " + sym->name); }
        if (sym->kind == TypeSymbol::Kind::Record or sym->kind == TypeSymbol::Kind::Interface) {
            return Type::makeRecord(sym, typeArgs);
        } else if (sym->kind == TypeSymbol::Kind::Enum) {
            if (not typeArgs.empty()) { errors.addError(typeNode->line, typeNode->column, "Enum type cannot have type parameters"); }
            return sym->type;
        } else if (sym->kind == TypeSymbol::Kind::Alias) {
            if (not sym->alias_target) { defineType(sym); }
            if (sym->alias_target) { return sym->alias_target; }
            return Type::makeAny();
        }
        return sym->type ? sym->type : Type::makeAny();
    }
    if (auto table = dynamic_cast<TableTypeNode *>(typeNode)) {
        if (table->is_map) {
            TypePtr keyType = resolveTypeNode(table->key_type.get(), currentTypeSym);
            TypePtr valType = table->element_types.empty() ? nullptr : resolveTypeNode(table->element_types[0].get(), currentTypeSym);
            if (not keyType or not valType) return Type::makeAny();
            return Type::makeMap(keyType, valType);
        } else {
            if (table->element_types.size() > 1) {
                std::vector<TypePtr> elems;
                for (auto &elemNode : table->element_types) { elems.push_back(resolveTypeNode(elemNode.get(), currentTypeSym)); }
                return Type::makeTuple(elems);
            } else if (table->element_types.size() == 1) {
                TypePtr elemType = resolveTypeNode(table->element_types[0].get(), currentTypeSym);
                return Type::makeArray(elemType);
            } else {
                return Type::makeArray(Type::makeAny());
            }
        }
    }
    if (auto funcNode = dynamic_cast<FunctionTypeNode *>(typeNode)) {
        std::vector<TypePtr> params;
        std::vector<bool> paramOptional;
        for (auto &p : funcNode->parameters) {
            TypePtr pType = p.type ? resolveTypeNode(p.type.get(), currentTypeSym) : Type::makeAny();
            params.push_back(pType);
            paramOptional.push_back(p.is_optional);
        }
        std::vector<TypePtr> rets;
        for (auto &rnode : funcNode->return_types) { rets.push_back(resolveTypeNode(rnode.get(), currentTypeSym)); }
        bool varRet = funcNode->varadict_return;
        std::vector<TypePtr> fTypeParams;
        for (auto &gtp : funcNode->type_parameters) {
            TypePtr ctype = nullptr;
            if (gtp.is.has_value()) {
                std::shared_ptr<TypeSymbol> csym = findTypeSymbol({ gtp.is.value() });
                if (csym) ctype = csym->type;
            }
            TypePtr tvar = Type::makeTypeVar(gtp.name, ctype);
            fTypeParams.push_back(tvar);
        }
        return Type::makeFunction(params, paramOptional, false, rets, varRet, fTypeParams);
    }
    if (auto unionNode = dynamic_cast<UnionTypeNode *>(typeNode)) {
        std::vector<TypePtr> options;
        for (auto &optNode : unionNode->options) { options.push_back(resolveTypeNode(optNode.get(), currentTypeSym)); }
        return Type::makeUnion(options);
    }
    return Type::makeAny();
}

std::unordered_map<std::string, TypeChecker::NarrowInfo> TypeChecker::analyzeCondition(Expression *cond)
{
    std::unordered_map<std::string, NarrowInfo> result;
    if (not cond) return result;
    if (auto isExp = dynamic_cast<IsTypeExpression *>(cond)) {
        Expression *expr = isExp->expression.get();
        TypeNode *typeNode = dynamic_cast<TypeNode *>(isExp->type.get());
        if (auto nameExp = dynamic_cast<NameExpression *>(expr)) {
            std::string varName = nameExp->name;
            VarInfo *var = symbols.lookup(varName);
            if (var) {
                TypePtr varType = var->type;
                TypePtr targetType = resolveTypeNode(typeNode);
                if (targetType) {
                    TypePtr thenType = targetType;
                    if (varType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> inter;
                        for (auto &opt : varType->union_members) {
                            if (opt->isAssignableTo(targetType) and targetType->isAssignableTo(opt)) { inter.push_back(opt); }
                        }
                        if (not inter.empty()) { thenType = inter.size() == 1 ? inter[0] : Type::makeUnion(inter); }
                    }
                    TypePtr elseType;
                    if (varType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> remaining;
                        for (auto &opt : varType->union_members) {
                            if (not opt->isAssignableTo(targetType)) { remaining.push_back(opt); }
                        }
                        elseType = remaining.empty() ? Type::makeNil() : (remaining.size() == 1 ? remaining[0] : Type::makeUnion(remaining));
                    } else {
                        elseType = varType;
                    }
                    result[varName] = { thenType, elseType };
                }
            }
        }
    } else if (auto binOp = dynamic_cast<BinaryOperationExpression *>(cond)) {
        auto op = binOp->operation;
        Expression *left = binOp->left.get();
        Expression *right = binOp->right.get();
        bool equality = (op == TokenType::EQUALS);
        bool inequality = (op == TokenType::NOT_EQ);
        if ((equality or inequality) and ((dynamic_cast<NilExpression *>(left) and dynamic_cast<NameExpression *>(right)) or (dynamic_cast<NameExpression *>(left) and dynamic_cast<NilExpression *>(right)))) {
            NameExpression *nameExp = dynamic_cast<NameExpression *>(dynamic_cast<NilExpression *>(left) ? right : left);
            std::string varName = nameExp->name;
            VarInfo *var = symbols.lookup(varName);
            if (var) {
                TypePtr varType = var->type;
                if (inequality) {
                    TypePtr thenType;
                    if (varType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> rem;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind != Type::Kind::Nil) rem.push_back(opt);
                        }
                        thenType = rem.empty() ? Type::makeNil() : (rem.size() == 1 ? rem[0] : Type::makeUnion(rem));
                    } else {
                        thenType = (varType->kind == Type::Kind::Nil) ? Type::makeNil() : varType;
                    }
                    TypePtr elseType = Type::makeNil();
                    result[varName] = { thenType, elseType };
                } else if (equality) {
                    TypePtr thenType = Type::makeNil();
                    TypePtr elseType;
                    if (varType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> rem;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind != Type::Kind::Nil) rem.push_back(opt);
                        }
                        elseType = rem.empty() ? Type::makeNil() : (rem.size() == 1 ? rem[0] : Type::makeUnion(rem));
                    } else {
                        elseType = (varType->kind == Type::Kind::Nil) ? Type::makeNil() : varType;
                    }
                    result[varName] = { thenType, elseType };
                }
            }
        }
    } else if (auto unOp = dynamic_cast<UnaryOperationExpression *>(cond)) {
        if (unOp->operation == TokenType::NOT) {
            if (auto nameExp = dynamic_cast<NameExpression *>(unOp->operand.get())) {
                std::string varName = nameExp->name;
                VarInfo *var = symbols.lookup(varName);
                if (var) {
                    TypePtr varType = var->type;
                    TypePtr thenType;
                    if (varType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> falsy;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind == Type::Kind::Nil or opt->kind == Type::Kind::Boolean) { falsy.push_back(opt->kind == Type::Kind::Boolean ? Type::makeBoolean() : opt); }
                        }
                        thenType = falsy.empty() ? Type::makeNil() : (falsy.size() == 1 ? falsy[0] : Type::makeUnion(falsy));
                    } else {
                        thenType = (varType->kind == Type::Kind::Nil or varType->kind == Type::Kind::Boolean) ? varType : Type::makeNil();
                    }
                    TypePtr elseType = varType;
                    if (elseType->kind == Type::Kind::Union) {
                        std::vector<TypePtr> truthy;
                        for (auto &opt : elseType->union_members) {
                            if (opt->kind != Type::Kind::Nil and opt->kind != Type::Kind::Boolean) { truthy.push_back(opt); }
                        }
                        elseType = truthy.empty() ? Type::makeNil() : (truthy.size() == 1 ? truthy[0] : Type::makeUnion(truthy));
                    } else if (elseType->kind == Type::Kind::Nil) {
                        elseType = Type::makeNil();
                    }
                    result[varName] = { thenType, elseType };
                }
            }
        }
    } else if (auto nameExp = dynamic_cast<NameExpression *>(cond)) {
        std::string varName = nameExp->name;
        VarInfo *var = symbols.lookup(varName);
        if (var) {
            TypePtr varType = var->type;
            TypePtr thenType = varType;
            TypePtr elseType = Type::makeNil();
            if (varType->kind == Type::Kind::Union) {
                std::vector<TypePtr> truthy;
                std::vector<TypePtr> falsy;
                for (auto &opt : varType->union_members) {
                    if (opt->kind != Type::Kind::Nil and opt->kind != Type::Kind::Boolean) truthy.push_back(opt);
                    if (opt->kind == Type::Kind::Nil or opt->kind == Type::Kind::Boolean) falsy.push_back(opt->kind == Type::Kind::Boolean ? Type::makeBoolean() : opt);
                }
                if (not truthy.empty()) thenType = truthy.size() == 1 ? truthy[0] : Type::makeUnion(truthy);
                elseType = falsy.empty() ? Type::makeNil() : (falsy.size() == 1 ? falsy[0] : Type::makeUnion(falsy));
            } else {
                if (varType->kind == Type::Kind::Nil) thenType = Type::makeNil();
                if (varType->kind == Type::Kind::Boolean or varType->kind == Type::Kind::Nil) elseType = varType;
            }
            result[varName] = { thenType, elseType };
        }
    }
    return result;
}

void TypeChecker::checkIf(IfStatement *node)
{
    for (size_t i = 0; i < node->if_branches.size(); ++i) {
        auto &branch = node->if_branches[i];
        auto narMap = analyzeCondition(branch.condition.get());
        std::vector<std::pair<VarInfo *, TypePtr>> savedTypes;
        for (auto &nar : narMap) {
            VarInfo *var = symbols.lookup(nar.first);
            if (var) {
                savedTypes.emplace_back(var, var->type);
                var->type = nar.second.thenType;
            }
        }
        TypePtr condType = checkExpression(branch.condition.get());
        symbols.pushScope();
        checkStatement(branch.block.get());
        symbols.popScope();
        for (auto &pr : savedTypes) { pr.first->type = pr.second; }
    }
    if (node->else_block) {
        symbols.pushScope();
        checkStatement(node->else_block.get());
        symbols.popScope();
    }
}

void TypeChecker::checkWhile(WhileStatement *node)
{
    loopDepth++;
    checkExpression(node->condition.get());
    symbols.pushScope();
    checkStatement(node->body.get());
    symbols.popScope();
    loopDepth--;
}

void TypeChecker::checkRepeat(RepeatStatement *node)
{
    loopDepth++;
    symbols.pushScope();
    checkStatement(node->body.get());
    checkExpression(node->condition.get());
    symbols.popScope();
    loopDepth--;
}

void TypeChecker::checkForNumeric(ForNumericStatement *node)
{
    TypePtr startType = node->expressions.start ? checkExpression(node->expressions.start.get()) : nullptr;
    TypePtr endType = node->expressions.end ? checkExpression(node->expressions.end.get()) : nullptr;
    TypePtr stepType = node->expressions.step ? checkExpression(node->expressions.step.get()) : nullptr;
    if (not stepType) stepType = Type::makeInteger();
    TypePtr loopVarType;
    if ((startType == nullptr or startType->kind == Type::Kind::Integer) and (endType == nullptr or endType->kind == Type::Kind::Integer) and (stepType->kind == Type::Kind::Integer)) {
        loopVarType = Type::makeInteger();
    } else {
        loopVarType = Type::makeNumber();
    }
    symbols.pushScope();
    symbols.define(node->variable_name, loopVarType, false, false);
    loopDepth++;
    checkStatement(node->body.get());
    loopDepth--;
    symbols.popScope();
}

void TypeChecker::checkForIn(ForInStatement *node)
{
    std::vector<TypePtr> exprTypes;
    for (auto &expr : node->exprs) { exprTypes.push_back(checkExpression(expr.get())); }
    std::vector<TypePtr> nameTypes;
    if (not exprTypes.empty() and exprTypes[0]->kind == Type::Kind::Function) {
        auto funcType = exprTypes[0]->func.get();
        if (funcType) {
            for (size_t i = 0; i < node->names.size(); ++i) {
                TypePtr t = (i < funcType->return_types.size()) ? funcType->return_types[i] : Type::makeNil();
                nameTypes.push_back(t);
            }
        }
    }
    while (nameTypes.size() < node->names.size()) { nameTypes.push_back(Type::makeAny()); }
    symbols.pushScope();
    for (size_t i = 0; i < node->names.size(); ++i) { symbols.define(node->names[i], nameTypes[i], false, false); }
    loopDepth++;
    checkStatement(node->body.get());
    loopDepth--;
    symbols.popScope();
}

void TypeChecker::checkFunctionDecl(FunctionDeclarationStatement *node)
{
    if (not node->body) {
        errors.addError(node->line, node->column, "Function declaration missing body");
        return;
    }
    std::vector<TypePtr> funcTypeParams;
    for (auto &gp : node->body->type_parameters) {
        TypePtr ctype = nullptr;
        if (gp.is.has_value()) {
            std::shared_ptr<TypeSymbol> csym = findTypeSymbol({ gp.is.value() });
            if (csym) ctype = csym->type;
        }
        TypePtr tvar = Type::makeTypeVar(gp.name, ctype);
        funcTypeParams.push_back(tvar);
    }
    std::vector<TypePtr> paramTypes;
    std::vector<bool> paramOptional;
    TypePtr selfType = nullptr;
    if (node->is_method) {
        if (not node->name_path.empty()) {
            std::shared_ptr<TypeSymbol> classSym = findTypeSymbol(node->name_path);
            if (classSym) {
                selfType = classSym->type;
            } else {
                VarInfo *var = symbols.lookup(node->name_path[0]);
                if (var) {
                    selfType = var->type;
                    for (size_t i = 1; selfType and i < node->name_path.size(); ++i) {
                        std::string fieldName = node->name_path[i];
                        if (selfType->kind == Type::Kind::Record) {
                            auto it = selfType->record->fields.find(fieldName);
                            selfType = (it != selfType->record->fields.end()) ? it->second : Type::makeAny();
                        } else if (selfType->kind == Type::Kind::Map and selfType->key->kind == Type::Kind::String) {
                            selfType = selfType->value;
                        } else if (selfType->kind == Type::Kind::Any) {
                            selfType = Type::makeAny();
                        } else {
                            errors.addError(node->line, node->column, "Cannot resolve method owner type");
                            selfType = Type::makeAny();
                        }
                    }
                }
            }
        }
        if (not selfType) {
            errors.addError(node->line, node->column, "Unable to determine type of 'self' for method");
            selfType = Type::makeAny();
        }
        paramTypes.push_back(selfType);
        paramOptional.push_back(false);
    }
    for (auto &param : node->body->parameters) {
        TypePtr pType = param.type ? resolveTypeNode(param.type.get()) : Type::makeAny();
        paramTypes.push_back(pType);
        paramOptional.push_back(param.is_optional);
    }
    std::vector<TypePtr> returnTypes;
    for (auto &retNode : node->body->return_types) { returnTypes.push_back(resolveTypeNode(retNode.get())); }
    bool retVar = node->body->varadict_return;
    TypePtr funcType = Type::makeFunction(paramTypes, paramOptional, false, returnTypes, retVar, funcTypeParams);
    std::string funcName = node->method_name.empty() ? (not node->name_path.empty() ? node->name_path.back() : "") : node->method_name;
    if (node->is_method) {
        if (not node->name_path.empty()) {
            std::shared_ptr<TypeSymbol> classSym = findTypeSymbol(node->name_path);
            if (classSym) {
                auto &fields = classSym->fields;
                if (fields.find(node->method_name) != fields.end()) {
                    TypePtr existingType = fields[node->method_name];
                    if (not funcType->equals(existingType)) { errors.addError(node->line, node->column, "Method signature does not match previously declared type for " + node->method_name); }
                }
                fields[node->method_name] = funcType;
            } else {
                errors.addError(node->line, node->column, "Unknown class for method: " + node->name_path[0]);
            }
        }
    } else {
        if (not node->name_path.empty()) {
            VarInfo *baseVar = symbols.lookup(node->name_path[0]);
            if (not baseVar) {
                errors.addError(node->line, node->column, "Unknown variable " + node->name_path[0] + " in function name");
            } else {
                TypePtr baseType = baseVar->type;
                for (size_t i = 1; i < node->name_path.size(); ++i) {
                    std::string field = node->name_path[i];
                    if (baseType->kind == Type::Kind::Record) {
                        auto it = baseType->record->fields.find(field);
                        if (it == baseType->record->fields.end()) {
                            TypePtr newTable = Type::makeMap(Type::makeString(), Type::makeAny());
                            baseType->record->fields[field] = newTable;
                            baseType = newTable;
                        } else {
                            baseType = it->second;
                        }
                    } else if (baseType->kind == Type::Kind::Map and baseType->key->kind == Type::Kind::String) {
                        baseType = baseType->value;
                    } else if (baseType->kind == Type::Kind::Any) {
                        baseType = Type::makeAny();
                    } else {
                        errors.addError(node->line, node->column, "Cannot assign nested function to type " + baseType->toString());
                        baseType = Type::makeAny();
                    }
                }
                if (baseType->kind == Type::Kind::Record) {
                    baseType->record->fields[node->method_name] = funcType;
                } else if (baseType->kind == Type::Kind::Map and baseType->key->kind == Type::Kind::String) {
                    if (baseType->value->kind == Type::Kind::Any) { baseType->value = funcType; }
                }
            }
        } else {
            bool ok = symbols.define(funcName, funcType, false, node->visibility == Visibility::GLOBAL);
            if (not ok) {
                VarInfo *existing = symbols.lookup(funcName);
                if (existing and not funcType->equals(existing->type)) { errors.addError(node->line, node->column, "Function redefinition with different type for " + funcName); }
            }
        }
    }
    symbols.pushScope();
    if (node->is_method) { symbols.define("self", paramTypes[0], false, false); }
    size_t startIndex = node->is_method ? 1 : 0;
    for (size_t i = startIndex; i < node->body->parameters.size() + (node->is_method ? 1 : 0); ++i) {
        size_t astIndex = node->is_method ? i - 1 : i;
        std::string paramName = node->body->parameters[astIndex].name;
        if (paramName.empty()) paramName = "_arg" + std::to_string(i);
        symbols.define(paramName, paramTypes[i], false, false);
    }
    if (node->body->body) { checkStatement(node->body->body.get()); }
    symbols.popScope();
}

void TypeChecker::checkVarDecl(VariableDeclarationStatement *node)
{
    size_t nVars = node->names.size();
    size_t nTypes = node->types.size();
    std::vector<TypePtr> valueTypes;
    for (auto &val : node->values) { valueTypes.push_back(checkExpression(val.get())); }
    for (size_t i = valueTypes.size(); i < nVars; ++i) { valueTypes.push_back(Type::makeNil()); }
    for (size_t i = 0; i < nVars; ++i) {
        std::string varName = node->names[i].name;
        bool is_const = (node->names[i].attribute and node->names[i].attribute.value() == "const");
        std::optional<std::string> attr = node->names[i].attribute;
        TypePtr declaredType = (i < nTypes and node->types[i]) ? resolveTypeNode(node->types[i].get()) : nullptr;
        TypePtr initType = (i < valueTypes.size() ? valueTypes[i] : Type::makeNil());
        TypePtr varType;
        if (not declaredType) {
            if (initType->kind == Type::Kind::Nil) {
                errors.addError(node->line, node->column, "Cannot infer type for " + varName + " from nil; please provide a type");
                varType = Type::makeAny();
            } else {
                varType = initType;
            }
        } else {
            varType = declaredType;
        }
        if (initType and declaredType and not initType->isAssignableTo(declaredType)) { errors.addError(node->line, node->column, "Type mismatch in initialization of " + varName + ": cannot assign " + initType->toString() + " to " + declaredType->toString()); }
        bool ok = symbols.define(varName, varType, is_const, node->visibility == Visibility::GLOBAL, attr);
        if (not ok) { errors.addError(node->line, node->column, "Duplicate definition of variable " + varName); }
        if (attr and attr.value() == "total" and varType->kind == Type::Kind::Map) {
            if (i < node->values.size()) {
                if (auto tableLit = dynamic_cast<TableConstructorExpression *>(node->values[i].get())) {
                    if (varType->key->kind == Type::Kind::Enum) {
                        std::shared_ptr<TypeSymbol> enumSym = varType->key->enum_type;
                        for (auto &enumVal : enumSym->enum_values) {
                            bool found = false;
                            for (auto &field : tableLit->fields) {
                                if (not std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                                    const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                                    if (std::holds_alternative<std::string>(kv.key) and std::get<std::string>(kv.key) == enumVal) {
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if (not found) { errors.addError(node->line, node->column, "Table not total: missing key \"" + enumVal + "\""); }
                        }
                    } else if (varType->key->kind == Type::Kind::Boolean) {
                        bool hasTrue = false, hasFalse = false;
                        for (auto &field : tableLit->fields) {
                            if (not std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                                const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                                if (std::holds_alternative<std::string>(kv.key)) {
                                    std::string keyStr = std::get<std::string>(kv.key);
                                    if (keyStr == "true") hasTrue = true;
                                    if (keyStr == "false") hasFalse = true;
                                }
                            }
                        }
                        if (not hasTrue or not hasFalse) { errors.addError(node->line, node->column, "Table not total: missing key " + std::string(not hasTrue ? "\"true\"" : "\"false\"")); }
                    }
                }
            }
        }
    }
}

void TypeChecker::checkAssignment(AssignmentStatement *node)
{
    std::vector<TypePtr> rightTypes;
    for (auto &expr : node->right) { rightTypes.push_back(checkExpression(expr.get())); }
    if (rightTypes.size() < node->left.size()) {
        while (rightTypes.size() < node->left.size()) { rightTypes.push_back(Type::makeNil()); }
    }
    for (size_t i = 0; i < node->left.size(); ++i) {
        Expression *lhs = node->left[i].get();
        TypePtr rhsType = (i < rightTypes.size() ? rightTypes[i] : Type::makeNil());
        if (auto nameExp = dynamic_cast<NameExpression *>(lhs)) {
            std::string name = nameExp->name;
            VarInfo *var = symbols.lookup(name);
            if (not var) {
                errors.addError(lhs->line, lhs->column, "Undefined variable " + name);
                continue;
            }
            if (var->is_const) { errors.addError(lhs->line, lhs->column, "Cannot assign to constant variable " + name); }
            if (not rhsType->isAssignableTo(var->type)) { errors.addError(lhs->line, lhs->column, "Type mismatch: cannot assign " + rhsType->toString() + " to " + var->type->toString()); }
        } else if (auto idxExp = dynamic_cast<IndexExpression *>(lhs)) {
            TypePtr tableType = checkExpression(idxExp->table.get());
            TypePtr indexType = checkExpression(idxExp->index.get());
            if (tableType->kind == Type::Kind::Array) {
                if (indexType->kind != Type::Kind::Number and indexType->kind != Type::Kind::Integer) { errors.addError(lhs->line, lhs->column, "Array index is not a number"); }
                if (not rhsType->isAssignableTo(tableType->element)) { errors.addError(lhs->line, lhs->column, "Type mismatch in array assignment: expected " + tableType->element->toString()); }
            } else if (tableType->kind == Type::Kind::Map) {
                if (not indexType->isAssignableTo(tableType->key)) { errors.addError(lhs->line, lhs->column, "Map index type mismatch: cannot index " + tableType->toString() + " with " + indexType->toString()); }
                if (not rhsType->isAssignableTo(tableType->value)) { errors.addError(lhs->line, lhs->column, "Type mismatch in map assignment: expected " + tableType->value->toString()); }
            } else if (tableType->kind == Type::Kind::Tuple) {
                if (indexType->kind != Type::Kind::Number and indexType->kind != Type::Kind::Integer) { errors.addError(lhs->line, lhs->column, "Tuple index is not a number"); }
            } else if (tableType->kind == Type::Kind::Record) {
                if (tableType->record->array_element_type) {
                    if (indexType->kind != Type::Kind::Number and indexType->kind != Type::Kind::Integer) { errors.addError(lhs->line, lhs->column, "Array part index is not a number"); }
                    if (not rhsType->isAssignableTo(tableType->record->array_element_type)) { errors.addError(lhs->line, lhs->column, "Type mismatch in array part assignment: expected " + tableType->record->array_element_type->toString()); }
                } else {
                    errors.addError(lhs->line, lhs->column, "Cannot index type " + tableType->toString());
                }
            }
        } else if (auto fieldExp = dynamic_cast<FieldExpression *>(lhs)) {
            TypePtr objType = checkExpression(fieldExp->object.get());
            std::string fieldName = fieldExp->field;
            if (objType->kind == Type::Kind::Record) {
                auto &fields = objType->record->fields;
                auto it = fields.find(fieldName);
                if (it != fields.end()) {
                    TypePtr fieldType = it->second;
                    if (not rhsType->isAssignableTo(fieldType)) { errors.addError(lhs->line, lhs->column, "Type mismatch for field '" + fieldName + "': cannot assign " + rhsType->toString() + " to " + fieldType->toString()); }
                } else {
                    if (objType->record->kind != TypeSymbol::Kind::Interface) {
                        errors.addError(lhs->line, lhs->column, "Field " + fieldName + " does not exist in record " + objType->record->name);
                    } else {
                        errors.addError(lhs->line, lhs->column, "Cannot assign to field of interface type");
                    }
                }
            } else if (objType->kind == Type::Kind::Map and objType->key->kind == Type::Kind::String) {
                if (objType->value->kind != Type::Kind::Any and not rhsType->isAssignableTo(objType->value)) { errors.addError(lhs->line, lhs->column, "Type mismatch for table field '" + fieldName + "'"); }
                if (objType->value->kind == Type::Kind::Any) { objType->value = rhsType; }
            } else if (objType->kind != Type::Kind::Any) {
                errors.addError(lhs->line, lhs->column, "Cannot access field of type " + objType->toString());
            }
        } else {
            errors.addError(node->line, node->column, "Invalid assignment target");
        }
    }
}

TypePtr TypeChecker::checkExpression(Expression *expr) { return checkExpression(expr, nullptr); }

TypePtr TypeChecker::checkExpression(Expression *expr, const TypePtr &expectedType)
{
    if (not expr) return Type::makeNil();
    if (auto num = dynamic_cast<NumberExpression *>(expr)) {
        bool isInt = true;
        for (char c : num->value) {
            if (c == '.' or c == 'e' or c == 'E') {
                isInt = false;
                break;
            }
        }
        return isInt ? Type::makeInteger() : Type::makeNumber();
    }
    if (dynamic_cast<StringExpression *>(expr)) { return Type::makeString(); }
    if (dynamic_cast<BooleanExpression *>(expr)) { return Type::makeBoolean(); }
    if (dynamic_cast<NilExpression *>(expr)) { return Type::makeNil(); }
    if (auto nameExp = dynamic_cast<NameExpression *>(expr)) {
        VarInfo *var = symbols.lookup(nameExp->name);
        if (not var) {
            errors.addError(expr->line, expr->column, "Undefined variable " + nameExp->name);
            return Type::makeAny();
        }
        return var->type;
    }
    if (auto callExp = dynamic_cast<FunctionCallExpression *>(expr)) {
        TypePtr baseType = checkExpression(callExp->base.get());
        std::vector<TypePtr> argTypes;
        for (auto &arg : callExp->arguments) { argTypes.push_back(checkExpression(arg.get())); }
        if (baseType->kind == Type::Kind::Function) {
            auto sig = baseType->func.get();
            size_t provided = argTypes.size();
            size_t minRequired = 0;
            for (bool opt : sig->param_optional) {
                if (not opt) minRequired++;
            }
            if (provided < minRequired or (not sig->param_varargs and provided > sig->param_types.size())) {
                errors.addError(expr->line, expr->column, "Incorrect number of arguments in function call");
            } else {
                size_t checkCount = std::min(provided, sig->param_types.size());
                for (size_t i = 0; i < checkCount; ++i) {
                    if (not argTypes[i]->isAssignableTo(sig->param_types[i])) { errors.addError(expr->line, expr->column, "Argument " + std::to_string(i + 1) + " type mismatch: expected " + sig->param_types[i]->toString() + ", got " + argTypes[i]->toString()); }
                }
            }
            if (not sig->return_types.empty()) {
                return sig->return_types[0];
            } else if (sig->return_varargs) {
                return Type::makeAny();
            } else {
                return Type::makeNil();
            }
        } else if (baseType->kind == Type::Kind::Any) {
            return Type::makeAny();
        } else {
            errors.addError(expr->line, expr->column, "Attempt to call non-function type " + baseType->toString());
            return Type::makeAny();
        }
    }
    if (auto binOp = dynamic_cast<BinaryOperationExpression *>(expr)) {
        TypePtr ltype = checkExpression(binOp->left.get());
        TypePtr rtype = checkExpression(binOp->right.get());
        switch (binOp->operation) {
        case TokenType::ADD:
        case TokenType::SUB:
        case TokenType::MUL:
        case TokenType::DIV:
        case TokenType::MOD:
        case TokenType::POW:
            if (not ltype->isAssignableTo(Type::makeNumber()) or not rtype->isAssignableTo(Type::makeNumber())) { errors.addError(expr->line, expr->column, "Arithmetic operator applied to non-numeric type"); }
            if (ltype->kind == Type::Kind::Integer and rtype->kind == Type::Kind::Integer and binOp->operation != TokenType::DIV) {
                return Type::makeInteger();
            } else {
                return Type::makeNumber();
            }
        case TokenType::CONCAT:
            if (not ltype->isAssignableTo(Type::makeString()) and not ltype->isAssignableTo(Type::makeNumber())) { errors.addError(expr->line, expr->column, "Concat left operand not string or number"); }
            if (not rtype->isAssignableTo(Type::makeString()) and not rtype->isAssignableTo(Type::makeNumber())) { errors.addError(expr->line, expr->column, "Concat right operand not string or number"); }
            return Type::makeString();
        case TokenType::EQUALS:
        case TokenType::NOT_EQ:
        case TokenType::LESS:
        case TokenType::LESS_EQ:
        case TokenType::GREATER:
        case TokenType::GREATER_EQ:
            if (not ltype->equals(rtype) and ltype->kind != Type::Kind::Any and rtype->kind != Type::Kind::Any) { errors.addError(expr->line, expr->column, "Comparing incompatible types " + ltype->toString() + " and " + rtype->toString()); }
            return Type::makeBoolean();
        case TokenType::AND: {
            TypePtr resultType;
            if (ltype->kind == Type::Kind::Union) {
                bool hasFalsy = false;
                for (auto &opt : ltype->union_members) {
                    if (opt->kind == Type::Kind::Nil or opt->kind == Type::Kind::Boolean) {
                        hasFalsy = true;
                        break;
                    }
                }
                resultType = hasFalsy ? Type::makeUnion({ Type::makeNil(), rtype }) : rtype;
            } else {
                resultType = (ltype->kind == Type::Kind::Nil or ltype->kind == Type::Kind::Boolean) ? Type::makeUnion({ ltype, rtype }) : rtype;
            }
            return resultType ? resultType : rtype;
        }
        case TokenType::OR: {
            TypePtr resultType;
            if (ltype->kind == Type::Kind::Union) {
                bool hasFalsy = false;
                std::vector<TypePtr> truthies;
                for (auto &opt : ltype->union_members) {
                    if (opt->kind == Type::Kind::Nil or opt->kind == Type::Kind::Boolean) {
                        hasFalsy = true;
                    } else {
                        truthies.push_back(opt);
                    }
                }
                resultType = hasFalsy ? Type::makeUnion({ rtype, ltype }) : ltype;
            } else {
                resultType = (ltype->kind == Type::Kind::Nil or ltype->kind == Type::Kind::Boolean) ? Type::makeUnion({ ltype, rtype }) : ltype;
            }
            return resultType ? resultType : Type::makeUnion({ ltype, rtype });
        }
        default:
            return Type::makeAny();
        }
    }
    if (auto unOp = dynamic_cast<UnaryOperationExpression *>(expr)) {
        TypePtr operandType = checkExpression(unOp->operand.get());
        switch (unOp->operation) {
        case TokenType::SUB:
            if (not operandType->isAssignableTo(Type::makeNumber())) { errors.addError(expr->line, expr->column, "Unary minus on non-numeric type"); }
            return operandType->kind == Type::Kind::Integer ? Type::makeInteger() : Type::makeNumber();
        case TokenType::NOT:
            return Type::makeBoolean();
        case TokenType::LENGTH:
            if (operandType->kind == Type::Kind::String or operandType->kind == Type::Kind::Array or operandType->kind == Type::Kind::Map or operandType->kind == Type::Kind::Tuple) {
                return Type::makeNumber();
            } else if (operandType->kind == Type::Kind::Record and operandType->record->array_element_type) {
                return Type::makeNumber();
            } else if (operandType->kind == Type::Kind::Any) {
                return Type::makeNumber();
            } else {
                errors.addError(expr->line, expr->column, "Length operator not supported on type " + operandType->toString());
                return Type::makeNumber();
            }
        default:
            return Type::makeAny();
        }
    }
    if (auto isExp = dynamic_cast<IsTypeExpression *>(expr)) {
        checkExpression(isExp->expression.get());
        return Type::makeBoolean();
    }
    if (auto castExp = dynamic_cast<CastExpression *>(expr)) {
        TypePtr exprType = checkExpression(castExp->expression.get());
        if (not castExp->target_types.empty()) {
            TypePtr targetType = resolveTypeNode(castExp->target_types[0].get());
            if (!(exprType->kind == Type::Kind::Any or targetType->kind == Type::Kind::Any)) {
                if (exprType->isAssignableTo(targetType)) {
                    // already assignable, cast not needed
                }
            }
            return targetType;
        }
        return Type::makeAny();
    }
    if (auto tableLit = dynamic_cast<TableConstructorExpression *>(expr)) {
        if (expectedType) {
            if (expectedType->kind == Type::Kind::Array or expectedType->kind == Type::Kind::Tuple) {
                TypePtr elemType = (expectedType->kind == Type::Kind::Array) ? expectedType->element : nullptr;
                std::vector<TypePtr> tupleTypes;
                if (expectedType->kind == Type::Kind::Tuple) { tupleTypes = expectedType->tuple_types; }
                size_t index = 0;
                for (auto &field : tableLit->fields) {
                    TypePtr valType;
                    if (std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                        valType = checkExpression(std::get<std::unique_ptr<Expression>>(field).get());
                        if (expectedType->kind == Type::Kind::Array) {
                            if (not valType->isAssignableTo(elemType)) { errors.addError(expr->line, expr->column, "Array element type mismatch: expected " + elemType->toString()); }
                        } else if (expectedType->kind == Type::Kind::Tuple) {
                            if (index < tupleTypes.size() and not valType->isAssignableTo(tupleTypes[index])) { errors.addError(expr->line, expr->column, "Tuple element " + std::to_string(index + 1) + " type mismatch"); }
                        }
                    } else {
                        const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                        TypePtr keyType = std::holds_alternative<std::string>(kv.key) ? Type::makeString() : checkExpression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                        TypePtr valueType = checkExpression(kv.value.get());
                        if (expectedType->kind == Type::Kind::Map) {
                            if (not keyType->isAssignableTo(expectedType->key)) { errors.addError(expr->line, expr->column, "Table key type mismatch: expected " + expectedType->key->toString()); }
                            if (not valueType->isAssignableTo(expectedType->value)) { errors.addError(expr->line, expr->column, "Table value type mismatch: expected " + expectedType->value->toString()); }
                        } else {
                            errors.addError(expr->line, expr->column, "Key-value in array literal not allowed");
                        }
                    }
                    index++;
                }
                return expectedType;
            } else if (expectedType->kind == Type::Kind::Map) {
                TypePtr keyType = expectedType->key;
                TypePtr valType = expectedType->value;
                for (auto &field : tableLit->fields) {
                    if (std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                        errors.addError(expr->line, expr->column, "Array entry in map literal not allowed");
                    } else {
                        const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                        TypePtr kType = std::holds_alternative<std::string>(kv.key) ? Type::makeString() : checkExpression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                        TypePtr vType = checkExpression(kv.value.get());
                        if (not kType->isAssignableTo(keyType)) { errors.addError(expr->line, expr->column, "Table key type mismatch"); }
                        if (not vType->isAssignableTo(valType)) { errors.addError(expr->line, expr->column, "Table value type mismatch"); }
                    }
                }
                return expectedType;
            }
        }
        bool hasKV = false;
        TypePtr arrayElemType;
        TypePtr mapKeyType;
        TypePtr mapValType;
        for (auto &field : tableLit->fields) {
            if (std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                if (hasKV) { errors.addError(expr->line, expr->column, "Mixed array and map literal entries"); }
                TypePtr vType = checkExpression(std::get<std::unique_ptr<Expression>>(field).get());
                if (not arrayElemType) arrayElemType = vType;
                else if (not vType->equals(arrayElemType)) {
                    arrayElemType = Type::makeUnion({ arrayElemType, vType });
                }
            } else {
                hasKV = true;
                const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                TypePtr kType = std::holds_alternative<std::string>(kv.key) ? Type::makeString() : checkExpression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                TypePtr vType = checkExpression(kv.value.get());
                if (not mapKeyType) {
                    mapKeyType = kType;
                    mapValType = vType;
                } else {
                    if (not kType->equals(mapKeyType)) { mapKeyType = Type::makeUnion({ mapKeyType, kType }); }
                    if (not vType->equals(mapValType)) { mapValType = Type::makeUnion({ mapValType, vType }); }
                }
            }
        }
        if (not hasKV) {
            if (not arrayElemType) { return Type::makeArray(Type::makeAny()); }
            if (arrayElemType->kind == Type::Kind::Union) {
                return Type::makeArray(arrayElemType);
            } else {
                return Type::makeArray(arrayElemType);
            }
        } else if (not arrayElemType) {
            if (not mapKeyType) { return Type::makeMap(Type::makeAny(), Type::makeAny()); }
            return Type::makeMap(mapKeyType, mapValType);
        } else {
            return Type::makeAny();
        }
    }
    return Type::makeAny();
}

void TypeChecker::checkStatement(Statement *stmt)
{
    if (not stmt) return;
    if (auto block = dynamic_cast<Block *>(stmt)) {
        symbols.pushScope();
        for (auto &s : block->statements) { checkStatement(s.get()); }
        symbols.popScope();
    } else if (auto ret = dynamic_cast<ReturnStatement *>(stmt)) {
        std::vector<TypePtr> retTypes;
        for (auto &val : ret->values) { retTypes.push_back(checkExpression(val.get())); }
        // Compare retTypes with current function's return types if we had context
    } else if (dynamic_cast<BreakStatement *>(stmt)) {
        if (loopDepth <= 0) { errors.addError(stmt->line, stmt->column, "break statement not within a loop"); }
    } else if (auto ifs = dynamic_cast<IfStatement *>(stmt)) {
        checkIf(ifs);
    } else if (auto wh = dynamic_cast<WhileStatement *>(stmt)) {
        checkWhile(wh);
    } else if (auto rep = dynamic_cast<RepeatStatement *>(stmt)) {
        checkRepeat(rep);
    } else if (auto fornum = dynamic_cast<ForNumericStatement *>(stmt)) {
        checkForNumeric(fornum);
    } else if (auto forin = dynamic_cast<ForInStatement *>(stmt)) {
        checkForIn(forin);
    } else if (auto funcDecl = dynamic_cast<FunctionDeclarationStatement *>(stmt)) {
        checkFunctionDecl(funcDecl);
    } else if (auto varDecl = dynamic_cast<VariableDeclarationStatement *>(stmt)) {
        checkVarDecl(varDecl);
    } else if (auto recordDecl = dynamic_cast<RecordDeclarationStatement *>(stmt)) {
        defineType(findTypeSymbol({ recordDecl->name }));
    } else if (auto _ = dynamic_cast<EnumDeclarationStatement *>(stmt)) {
        // no additional check needed for enum beyond first pass
    } else if (auto aliasDecl = dynamic_cast<TypeAliasStatement *>(stmt)) {
        defineType(findTypeSymbol({ aliasDecl->name }));
    } else if (auto assign = dynamic_cast<AssignmentStatement *>(stmt)) {
        checkAssignment(assign);
    } else if (auto callStmt = dynamic_cast<CallStatement *>(stmt)) {
        if (callStmt->call) { checkExpression(callStmt->call.get()); }
    }
}
