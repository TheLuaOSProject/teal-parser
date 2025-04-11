#include "TypeChecker.hpp"
#include <algorithm>
#include <cmath>

using namespace teal::parser::ast;
using namespace teal::parser::typecheck;

TypeChecker::TypeChecker() : _loop_depth(0) { }

void TypeChecker::check(Block *ast)
{
    for (auto &stmt : ast->statements) {
        if (not stmt) continue;
        if (auto rec = dynamic_cast<RecordDeclarationStatement *>(stmt.get())) {
            declare_type(rec, nullptr);
        } else if (auto en = dynamic_cast<EnumDeclarationStatement *>(stmt.get())) {
            declare_type(en, nullptr);
        } else if (auto ta = dynamic_cast<TypeAliasStatement *>(stmt.get())) {
            declare_type(ta, nullptr);
        } else if (auto func = dynamic_cast<FunctionDeclarationStatement *>(stmt.get())) {
            check_function_declaration(func);
        } else if (auto var = dynamic_cast<VariableDeclarationStatement *>(stmt.get())) {
            check_variable_declaration(var);
        } else if (auto assign = dynamic_cast<AssignmentStatement *>(stmt.get())) {
            check_assignment(assign);
        } else if (auto ifStmt = dynamic_cast<IfStatement *>(stmt.get())) {
            check_if(ifStmt);
        } else if (auto whileStmt = dynamic_cast<WhileStatement *>(stmt.get())) {
            check_while(whileStmt);
        } else if (auto repeatStmt = dynamic_cast<RepeatStatement *>(stmt.get())) {
            check_repeat(repeatStmt);
        } else if (auto forNum = dynamic_cast<ForNumericStatement *>(stmt.get())) {
            check_numeric_for(forNum);
        } else if (auto forIn = dynamic_cast<ForInStatement *>(stmt.get())) {
            check_for_in(forIn);
        }
    }
    // second pass (define types) happens during statement checking
}

void TypeChecker::declare_type(RecordDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    TypeSymbol::Kind k = node->is_interface ? TypeSymbol::Kind::INTERFACE : TypeSymbol::Kind::RECORD;
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
            std::shared_ptr<TypeSymbol> constraintSym = find_type_symbol({ gp.is.value() });
            if (not constraintSym) {
                _errors.add_error(node->line, node->column, "Unknown type in generic constraint: " + gp.is.value());
            } else {
                constraintType = constraintSym->type;
                if (constraintSym->kind != TypeSymbol::Kind::INTERFACE) { _errors.add_error(node->line, node->column, "Generic constraint must be an interface type"); }
            }
        }
        TypePtr tvar = Type::make_type_variable(gp.name, constraintType);
        tvar->type_variable->owner_type = typeSym;
        typeSym->type_parameters.push_back(tvar);
    }
    TypePtr recType = Type::make_record(typeSym);
    if (not parent) {
        bool ok = _symbols.define(node->name, recType, true, node->visibility == Visibility::GLOBAL);
        if (not ok) _errors.add_error(node->line, node->column, "Duplicate type name: " + node->name);
    }
}

void TypeChecker::declare_type(EnumDeclarationStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    auto typeSym = std::make_shared<TypeSymbol>(TypeSymbol::Kind::ENUM, node->name);
    typeSym->parent = parent;
    if (node->body) {
        for (auto &elem : node->body->elements) { typeSym->enum_values.push_back(elem); }
    }

    TypePtr enumType = Type::make_enum(typeSym);
    if (parent) {
        parent->nested_types[node->name] = typeSym;
    } else {
        bool ok = _symbols.define(node->name, enumType, true, node->visibility == Visibility::GLOBAL);
        if (not ok) _errors.add_error(node->line, node->column, "Duplicate type name: " + node->name);
    }
}

void TypeChecker::declare_type(TypeAliasStatement *node, std::shared_ptr<TypeSymbol> parent)
{
    auto typeSym = std::make_shared<TypeSymbol>(TypeSymbol::Kind::ALIAS, node->name);
    typeSym->parent = parent;
    for (auto &gp : node->type_parameters) {
        TypePtr constraintType = nullptr;
        if (gp.is.has_value()) {
            std::shared_ptr<TypeSymbol> constraintSym = find_type_symbol({ gp.is.value() });
            if (constraintSym) {
                constraintType = constraintSym->type;
            } else {
                _errors.add_error(node->line, node->column, "Unknown constraint type " + gp.is.value() + " for alias " + node->name);
            }
        }
        TypePtr tvar = Type::make_type_variable(gp.name, constraintType);
        tvar->type_variable->owner_type = typeSym;
        typeSym->type_parameters.push_back(tvar);
    }
    typeSym->ast_type_alias = node;
    typeSym->type = nullptr;
    if (parent) { parent->nested_types[node->name] = typeSym; }
}

std::shared_ptr<TypeSymbol> TypeChecker::find_type_symbol(const std::vector<std::string> &nameParts)
{
    if (nameParts.empty()) return nullptr;
    VariableInfo *v = _symbols.lookup_global(nameParts[0]);
    std::shared_ptr<TypeSymbol> sym = nullptr;
    if (v and v->type) {
        TypePtr t = v->type;
        if (t->kind == Type::Kind::RECORD) sym = t->record;
        else if (t->kind == Type::Kind::ENUM) sym = t->enum_type;
    }
    for (size_t i = 1; sym and i < nameParts.size(); ++i) {
        auto it = sym->nested_types.find(nameParts[i]);
        sym = (it != sym->nested_types.end()) ? it->second : nullptr;
    }
    return sym;
}

void TypeChecker::define_type(std::shared_ptr<TypeSymbol> typeSymbol)
{
    if (not typeSymbol) return;
    if (typeSymbol->kind == TypeSymbol::Kind::RECORD or typeSymbol->kind == TypeSymbol::Kind::INTERFACE) {
        RecordBody *body = typeSymbol->ast_record_body;
        if (body) {
            if (body->structural_ext) {
                TypePtr extType = resolve_type_node(body->structural_ext.get(), typeSymbol);
                if (extType) {
                    if (extType->kind == Type::Kind::ARRAY) {
                        typeSymbol->array_element_type = extType->element;
                    } else if (extType->kind == Type::Kind::MAP) {
                        typeSymbol->array_element_type = extType->value;
                    } else {
                        _errors.add_error(body->line, body->column, "Invalid structural extension type");
                    }
                }
            }
            for (auto &intfNode : body->interface_ext) {
                TypePtr intfType = resolve_type_node(intfNode.get());
                if (intfType) {
                    if (intfType->kind == Type::Kind::RECORD and intfType->record->kind == TypeSymbol::Kind::INTERFACE) {
                        typeSymbol->interfaces.push_back(intfType);
                    } else {
                        _errors.add_error(body->line, body->column, "Type is not an interface: " + intfType->to_string());
                    }
                }
            }
        }
        for (auto &[key, val] : typeSymbol->nested_types) {
            define_type(val);
            if (val->type) { typeSymbol->fields[key] = val->type; }
        }
    } else if (typeSymbol->kind == TypeSymbol::Kind::ALIAS) {
        if (typeSymbol->ast_type_alias) {
            TypePtr aliasType = resolve_type_node(typeSymbol->ast_type_alias->type.get(), typeSymbol);
            if (aliasType) {
                typeSymbol->alias_target = aliasType;
            } else {
                _errors.add_error(typeSymbol->ast_type_alias->line, typeSymbol->ast_type_alias->column, "Cannot resolve alias target type");
            }
        }
    }
}

TypePtr TypeChecker::resolve_type_node(TypeNode *typeNode, std::shared_ptr<TypeSymbol> currentTypeSym)
{
    if (not typeNode) return nullptr;
    if (auto basic = dynamic_cast<BasicTypeNode *>(typeNode)) {
        std::string name = basic->name;
        if (name == "number") return Type::make_number();
        if (name == "integer") return Type::make_integer();
        if (name == "string") return Type::make_string();
        if (name == "boolean") return Type::make_boolean();
        if (name == "nil") return Type::make_nil();
        if (name == "any") return Type::make_any();
        if (currentTypeSym) {
            for (auto &tv : currentTypeSym->type_parameters) {
                if (tv->kind == Type::Kind::TYPE_VARIABLE and tv->type_variable->name == name) { return tv; }
            }
        }
        std::shared_ptr<TypeSymbol> sym = find_type_symbol({ name });
        if (sym) { return sym->type ? sym->type : Type::make_any(); }
        _errors.add_error(typeNode->line, typeNode->column, "Unknown type: " + name);
        return Type::make_any();
    }
    if (auto nom = dynamic_cast<NominalTypeNode *>(typeNode)) {
        std::shared_ptr<TypeSymbol> sym = find_type_symbol(nom->name_parts);
        if (not sym) {
            _errors.add_error(typeNode->line, typeNode->column, "Unknown type: " + nom->name_parts.front());
            return Type::make_any();
        }
        std::vector<TypePtr> typeArgs;
        for (auto &argNode : nom->type_arguments) { typeArgs.push_back(resolve_type_node(argNode.get(), currentTypeSym)); }
        if (not sym->type_parameters.empty() and typeArgs.size() != sym->type_parameters.size()) { _errors.add_error(typeNode->line, typeNode->column, "Generic type parameters count mismatch for " + sym->name); }
        if (sym->kind == TypeSymbol::Kind::RECORD or sym->kind == TypeSymbol::Kind::INTERFACE) {
            return Type::make_record(sym, typeArgs);
        } else if (sym->kind == TypeSymbol::Kind::ENUM) {
            if (not typeArgs.empty()) { _errors.add_error(typeNode->line, typeNode->column, "Enum type cannot have type parameters"); }
            return sym->type;
        } else if (sym->kind == TypeSymbol::Kind::ALIAS) {
            if (not sym->alias_target) { define_type(sym); }
            if (sym->alias_target) { return sym->alias_target; }
            return Type::make_any();
        }
        return sym->type ? sym->type : Type::make_any();
    }
    if (auto table = dynamic_cast<TableTypeNode *>(typeNode)) {
        if (table->is_map) {
            TypePtr keyType = resolve_type_node(table->key_type.get(), currentTypeSym);
            TypePtr valType = table->element_types.empty() ? nullptr : resolve_type_node(table->element_types[0].get(), currentTypeSym);
            if (not keyType or not valType) return Type::make_any();
            return Type::make_map(keyType, valType);
        } else {
            if (table->element_types.size() > 1) {
                std::vector<TypePtr> elems;
                for (auto &elemNode : table->element_types) { elems.push_back(resolve_type_node(elemNode.get(), currentTypeSym)); }
                return Type::make_tuple(elems);
            } else if (table->element_types.size() == 1) {
                TypePtr elemType = resolve_type_node(table->element_types[0].get(), currentTypeSym);
                return Type::make_array(elemType);
            } else {
                return Type::make_array(Type::make_any());
            }
        }
    }
    if (auto funcNode = dynamic_cast<FunctionTypeNode *>(typeNode)) {
        std::vector<TypePtr> params;
        std::vector<bool> paramOptional;
        for (auto &p : funcNode->parameters) {
            TypePtr pType = p.type ? resolve_type_node(p.type.get(), currentTypeSym) : Type::make_any();
            params.push_back(pType);
            paramOptional.push_back(p.is_optional);
        }
        std::vector<TypePtr> rets;
        for (auto &rnode : funcNode->return_types) { rets.push_back(resolve_type_node(rnode.get(), currentTypeSym)); }
        bool varRet = funcNode->varadict_return;
        std::vector<TypePtr> fTypeParams;
        for (auto &gtp : funcNode->type_parameters) {
            TypePtr ctype = nullptr;
            if (gtp.is.has_value()) {
                std::shared_ptr<TypeSymbol> csym = find_type_symbol({ gtp.is.value() });
                if (csym) ctype = csym->type;
            }
            TypePtr tvar = Type::make_type_variable(gtp.name, ctype);
            fTypeParams.push_back(tvar);
        }
        return Type::make_function(params, paramOptional, false, rets, varRet, fTypeParams);
    }
    if (auto unionNode = dynamic_cast<UnionTypeNode *>(typeNode)) {
        std::vector<TypePtr> options;
        for (auto &optNode : unionNode->options) { options.push_back(resolve_type_node(optNode.get(), currentTypeSym)); }
        return Type::make_union(options);
    }
    return Type::make_any();
}

std::unordered_map<std::string, TypeChecker::NarrowInfo> TypeChecker::analyze_condition(Expression *cond)
{
    std::unordered_map<std::string, NarrowInfo> result;
    if (not cond) return result;
    if (auto isExp = dynamic_cast<IsTypeExpression *>(cond)) {
        Expression *expr = isExp->expression.get();
        TypeNode *typeNode = dynamic_cast<TypeNode *>(isExp->type.get());
        if (auto nameExp = dynamic_cast<NameExpression *>(expr)) {
            std::string varName = nameExp->name;
            VariableInfo *var = _symbols.lookup(varName);
            if (var) {
                TypePtr varType = var->type;
                TypePtr targetType = resolve_type_node(typeNode);
                if (targetType) {
                    TypePtr thenType = targetType;
                    if (varType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> inter;
                        for (auto &opt : varType->union_members) {
                            if (opt->is_assignable_to(targetType) and targetType->is_assignable_to(opt)) { inter.push_back(opt); }
                        }
                        if (not inter.empty()) { thenType = inter.size() == 1 ? inter[0] : Type::make_union(inter); }
                    }
                    TypePtr elseType;
                    if (varType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> remaining;
                        for (auto &opt : varType->union_members) {
                            if (not opt->is_assignable_to(targetType)) { remaining.push_back(opt); }
                        }
                        elseType = remaining.empty() ? Type::make_nil() : (remaining.size() == 1 ? remaining[0] : Type::make_union(remaining));
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
            VariableInfo *var = _symbols.lookup(varName);
            if (var) {
                TypePtr varType = var->type;
                if (inequality) {
                    TypePtr thenType;
                    if (varType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> rem;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind != Type::Kind::NIL) rem.push_back(opt);
                        }
                        thenType = rem.empty() ? Type::make_nil() : (rem.size() == 1 ? rem[0] : Type::make_union(rem));
                    } else {
                        thenType = (varType->kind == Type::Kind::NIL) ? Type::make_nil() : varType;
                    }
                    TypePtr elseType = Type::make_nil();
                    result[varName] = { thenType, elseType };
                } else if (equality) {
                    TypePtr thenType = Type::make_nil();
                    TypePtr elseType;
                    if (varType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> rem;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind != Type::Kind::NIL) rem.push_back(opt);
                        }
                        elseType = rem.empty() ? Type::make_nil() : (rem.size() == 1 ? rem[0] : Type::make_union(rem));
                    } else {
                        elseType = (varType->kind == Type::Kind::NIL) ? Type::make_nil() : varType;
                    }
                    result[varName] = { thenType, elseType };
                }
            }
        }
    } else if (auto unOp = dynamic_cast<UnaryOperationExpression *>(cond)) {
        if (unOp->operation == TokenType::NOT) {
            if (auto nameExp = dynamic_cast<NameExpression *>(unOp->operand.get())) {
                std::string varName = nameExp->name;
                VariableInfo *var = _symbols.lookup(varName);
                if (var) {
                    TypePtr varType = var->type;
                    TypePtr thenType;
                    if (varType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> falsy;
                        for (auto &opt : varType->union_members) {
                            if (opt->kind == Type::Kind::NIL or opt->kind == Type::Kind::BOOLEAN) { falsy.push_back(opt->kind == Type::Kind::BOOLEAN ? Type::make_boolean() : opt); }
                        }
                        thenType = falsy.empty() ? Type::make_nil() : (falsy.size() == 1 ? falsy[0] : Type::make_union(falsy));
                    } else {
                        thenType = (varType->kind == Type::Kind::NIL or varType->kind == Type::Kind::BOOLEAN) ? varType : Type::make_nil();
                    }
                    TypePtr elseType = varType;
                    if (elseType->kind == Type::Kind::UNION) {
                        std::vector<TypePtr> truthy;
                        for (auto &opt : elseType->union_members) {
                            if (opt->kind != Type::Kind::NIL and opt->kind != Type::Kind::BOOLEAN) { truthy.push_back(opt); }
                        }
                        elseType = truthy.empty() ? Type::make_nil() : (truthy.size() == 1 ? truthy[0] : Type::make_union(truthy));
                    } else if (elseType->kind == Type::Kind::NIL) {
                        elseType = Type::make_nil();
                    }
                    result[varName] = { thenType, elseType };
                }
            }
        }
    } else if (auto nameExp = dynamic_cast<NameExpression *>(cond)) {
        std::string varName = nameExp->name;
        VariableInfo *var = _symbols.lookup(varName);
        if (var) {
            TypePtr varType = var->type;
            TypePtr thenType = varType;
            TypePtr elseType = Type::make_nil();
            if (varType->kind == Type::Kind::UNION) {
                std::vector<TypePtr> truthy;
                std::vector<TypePtr> falsy;
                for (auto &opt : varType->union_members) {
                    if (opt->kind != Type::Kind::NIL and opt->kind != Type::Kind::BOOLEAN) truthy.push_back(opt);
                    if (opt->kind == Type::Kind::NIL or opt->kind == Type::Kind::BOOLEAN) falsy.push_back(opt->kind == Type::Kind::BOOLEAN ? Type::make_boolean() : opt);
                }
                if (not truthy.empty()) thenType = truthy.size() == 1 ? truthy[0] : Type::make_union(truthy);
                elseType = falsy.empty() ? Type::make_nil() : (falsy.size() == 1 ? falsy[0] : Type::make_union(falsy));
            } else {
                if (varType->kind == Type::Kind::NIL) thenType = Type::make_nil();
                if (varType->kind == Type::Kind::BOOLEAN or varType->kind == Type::Kind::NIL) elseType = varType;
            }
            result[varName] = { thenType, elseType };
        }
    }
    return result;
}

void TypeChecker::check_if(IfStatement *node)
{
    for (size_t i = 0; i < node->if_branches.size(); ++i) {
        auto &branch = node->if_branches[i];
        auto narMap = analyze_condition(branch.condition.get());
        std::vector<std::pair<VariableInfo *, TypePtr>> savedTypes;
        for (auto &nar : narMap) {
            VariableInfo *var = _symbols.lookup(nar.first);
            if (var) {
                savedTypes.emplace_back(var, var->type);
                var->type = nar.second.then_type;
            }
        }
        TypePtr condType = check_expression(branch.condition.get());
        _symbols.push_scope();
        check_statement(branch.block.get());
        _symbols.pop_scope();
        for (auto &pr : savedTypes) { pr.first->type = pr.second; }
    }
    if (node->else_block) {
        _symbols.push_scope();
        check_statement(node->else_block.get());
        _symbols.pop_scope();
    }
}

void TypeChecker::check_while(WhileStatement *node)
{
    _loop_depth++;
    check_expression(node->condition.get());
    _symbols.push_scope();
    check_statement(node->body.get());
    _symbols.pop_scope();
    _loop_depth--;
}

void TypeChecker::check_repeat(RepeatStatement *node)
{
    _loop_depth++;
    _symbols.push_scope();
    check_statement(node->body.get());
    check_expression(node->condition.get());
    _symbols.pop_scope();
    _loop_depth--;
}

void TypeChecker::check_numeric_for(ForNumericStatement *node)
{
    TypePtr startType = node->expressions.start ? check_expression(node->expressions.start.get()) : nullptr;
    TypePtr endType = node->expressions.end ? check_expression(node->expressions.end.get()) : nullptr;
    TypePtr stepType = node->expressions.step ? check_expression(node->expressions.step.get()) : nullptr;
    if (not stepType) stepType = Type::make_integer();
    TypePtr loopVarType;
    if ((startType == nullptr or startType->kind == Type::Kind::INTEGER) and (endType == nullptr or endType->kind == Type::Kind::INTEGER) and (stepType->kind == Type::Kind::INTEGER)) {
        loopVarType = Type::make_integer();
    } else {
        loopVarType = Type::make_number();
    }
    _symbols.push_scope();
    _symbols.define(node->variable_name, loopVarType, false, false);
    _loop_depth++;
    check_statement(node->body.get());
    _loop_depth--;
    _symbols.pop_scope();
}

void TypeChecker::check_for_in(ForInStatement *node)
{
    std::vector<TypePtr> exprTypes;
    for (auto &expr : node->exprs) { exprTypes.push_back(check_expression(expr.get())); }
    std::vector<TypePtr> nameTypes;
    if (not exprTypes.empty() and exprTypes[0]->kind == Type::Kind::FUNCTION) {
        auto funcType = exprTypes[0]->function.get();
        if (funcType) {
            for (size_t i = 0; i < node->names.size(); ++i) {
                TypePtr t = (i < funcType->return_types.size()) ? funcType->return_types[i] : Type::make_nil();
                nameTypes.push_back(t);
            }
        }
    }
    while (nameTypes.size() < node->names.size()) { nameTypes.push_back(Type::make_any()); }
    _symbols.push_scope();
    for (size_t i = 0; i < node->names.size(); ++i) { _symbols.define(node->names[i], nameTypes[i], false, false); }
    _loop_depth++;
    check_statement(node->body.get());
    _loop_depth--;
    _symbols.pop_scope();
}

void TypeChecker::check_function_declaration(FunctionDeclarationStatement *node)
{
    if (not node->body) {
        _errors.add_error(node->line, node->column, "Function declaration missing body");
        return;
    }
    std::vector<TypePtr> funcTypeParams;
    for (auto &gp : node->body->type_parameters) {
        TypePtr ctype = nullptr;
        if (gp.is.has_value()) {
            std::shared_ptr<TypeSymbol> csym = find_type_symbol({ gp.is.value() });
            if (csym) ctype = csym->type;
        }
        TypePtr tvar = Type::make_type_variable(gp.name, ctype);
        funcTypeParams.push_back(tvar);
    }
    std::vector<TypePtr> paramTypes;
    std::vector<bool> paramOptional;
    TypePtr selfType = nullptr;
    if (node->is_method) {
        if (not node->name_path.empty()) {
            std::shared_ptr<TypeSymbol> classSym = find_type_symbol(node->name_path);
            if (classSym) {
                selfType = classSym->type;
            } else {
                VariableInfo *var = _symbols.lookup(node->name_path[0]);
                if (var) {
                    selfType = var->type;
                    for (size_t i = 1; selfType and i < node->name_path.size(); ++i) {
                        std::string fieldName = node->name_path[i];
                        if (selfType->kind == Type::Kind::RECORD) {
                            auto it = selfType->record->fields.find(fieldName);
                            selfType = (it != selfType->record->fields.end()) ? it->second : Type::make_any();
                        } else if (selfType->kind == Type::Kind::MAP and selfType->key->kind == Type::Kind::STRING) {
                            selfType = selfType->value;
                        } else if (selfType->kind == Type::Kind::ANY) {
                            selfType = Type::make_any();
                        } else {
                            _errors.add_error(node->line, node->column, "Cannot resolve method owner type");
                            selfType = Type::make_any();
                        }
                    }
                }
            }
        }
        if (not selfType) {
            _errors.add_error(node->line, node->column, "Unable to determine type of 'self' for method");
            selfType = Type::make_any();
        }
        paramTypes.push_back(selfType);
        paramOptional.push_back(false);
    }
    for (auto &param : node->body->parameters) {
        TypePtr pType = param.type ? resolve_type_node(param.type.get()) : Type::make_any();
        paramTypes.push_back(pType);
        paramOptional.push_back(param.is_optional);
    }
    std::vector<TypePtr> returnTypes;
    for (auto &retNode : node->body->return_types) { returnTypes.push_back(resolve_type_node(retNode.get())); }
    bool retVar = node->body->varadict_return;
    TypePtr funcType = Type::make_function(paramTypes, paramOptional, false, returnTypes, retVar, funcTypeParams);
    std::string funcName = node->method_name.empty() ? (not node->name_path.empty() ? node->name_path.back() : "") : node->method_name;
    if (node->is_method) {
        if (not node->name_path.empty()) {
            std::shared_ptr<TypeSymbol> classSym = find_type_symbol(node->name_path);
            if (classSym) {
                auto &fields = classSym->fields;
                if (fields.find(node->method_name) != fields.end()) {
                    TypePtr existingType = fields[node->method_name];
                    if (not funcType->equals(existingType)) { _errors.add_error(node->line, node->column, "Method signature does not match previously declared type for " + node->method_name); }
                }
                fields[node->method_name] = funcType;
            } else {
                _errors.add_error(node->line, node->column, "Unknown class for method: " + node->name_path[0]);
            }
        }
    } else {
        if (not node->name_path.empty()) {
            VariableInfo *baseVar = _symbols.lookup(node->name_path[0]);
            if (not baseVar) {
                _errors.add_error(node->line, node->column, "Unknown variable " + node->name_path[0] + " in function name");
            } else {
                TypePtr baseType = baseVar->type;
                for (size_t i = 1; i < node->name_path.size(); ++i) {
                    std::string field = node->name_path[i];
                    if (baseType->kind == Type::Kind::RECORD) {
                        auto it = baseType->record->fields.find(field);
                        if (it == baseType->record->fields.end()) {
                            TypePtr newTable = Type::make_map(Type::make_string(), Type::make_any());
                            baseType->record->fields[field] = newTable;
                            baseType = newTable;
                        } else {
                            baseType = it->second;
                        }
                    } else if (baseType->kind == Type::Kind::MAP and baseType->key->kind == Type::Kind::STRING) {
                        baseType = baseType->value;
                    } else if (baseType->kind == Type::Kind::ANY) {
                        baseType = Type::make_any();
                    } else {
                        _errors.add_error(node->line, node->column, "Cannot assign nested function to type " + baseType->to_string());
                        baseType = Type::make_any();
                    }
                }
                if (baseType->kind == Type::Kind::RECORD) {
                    baseType->record->fields[node->method_name] = funcType;
                } else if (baseType->kind == Type::Kind::MAP and baseType->key->kind == Type::Kind::STRING) {
                    if (baseType->value->kind == Type::Kind::ANY) { baseType->value = funcType; }
                }
            }
        } else {
            bool ok = _symbols.define(funcName, funcType, false, node->visibility == Visibility::GLOBAL);
            if (not ok) {
                VariableInfo *existing = _symbols.lookup(funcName);
                if (existing and not funcType->equals(existing->type)) { _errors.add_error(node->line, node->column, "Function redefinition with different type for " + funcName); }
            }
        }
    }
    _symbols.push_scope();
    if (node->is_method) { _symbols.define("self", paramTypes[0], false, false); }
    size_t startIndex = node->is_method ? 1 : 0;
    for (size_t i = startIndex; i < node->body->parameters.size() + (node->is_method ? 1 : 0); ++i) {
        size_t astIndex = node->is_method ? i - 1 : i;
        std::string paramName = node->body->parameters[astIndex].name;
        if (paramName.empty()) paramName = "_arg" + std::to_string(i);
        _symbols.define(paramName, paramTypes[i], false, false);
    }
    if (node->body->body) { check_statement(node->body->body.get()); }
    _symbols.pop_scope();
}

void TypeChecker::check_variable_declaration(VariableDeclarationStatement *node)
{
    size_t nVars = node->names.size();
    size_t nTypes = node->types.size();
    std::vector<TypePtr> valueTypes;
    for (auto &val : node->values) { valueTypes.push_back(check_expression(val.get())); }
    for (size_t i = valueTypes.size(); i < nVars; ++i) { valueTypes.push_back(Type::make_nil()); }
    for (size_t i = 0; i < nVars; ++i) {
        std::string varName = node->names[i].name;
        bool is_const = (node->names[i].attribute and node->names[i].attribute.value() == "const");
        std::optional<std::string> attr = node->names[i].attribute;
        TypePtr declaredType = (i < nTypes and node->types[i]) ? resolve_type_node(node->types[i].get()) : nullptr;
        TypePtr initType = (i < valueTypes.size() ? valueTypes[i] : Type::make_nil());
        TypePtr varType;
        if (not declaredType) {
            if (initType->kind == Type::Kind::NIL) {
                _errors.add_error(node->line, node->column, "Cannot infer type for " + varName + " from nil; please provide a type");
                varType = Type::make_any();
            } else {
                varType = initType;
            }
        } else {
            varType = declaredType;
        }
        if (initType and declaredType and not initType->is_assignable_to(declaredType)) { _errors.add_error(node->line, node->column, "Type mismatch in initialization of " + varName + ": cannot assign " + initType->to_string() + " to " + declaredType->to_string()); }
        bool ok = _symbols.define(varName, varType, is_const, node->visibility == Visibility::GLOBAL, attr);
        if (not ok) { _errors.add_error(node->line, node->column, "Duplicate definition of variable " + varName); }
        if (attr and attr.value() == "total" and varType->kind == Type::Kind::MAP) {
            if (i < node->values.size()) {
                if (auto tableLit = dynamic_cast<TableConstructorExpression *>(node->values[i].get())) {
                    if (varType->key->kind == Type::Kind::ENUM) {
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
                            if (not found) { _errors.add_error(node->line, node->column, "Table not total: missing key \"" + enumVal + "\""); }
                        }
                    } else if (varType->key->kind == Type::Kind::BOOLEAN) {
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
                        if (not hasTrue or not hasFalse) { _errors.add_error(node->line, node->column, "Table not total: missing key " + std::string(not hasTrue ? "\"true\"" : "\"false\"")); }
                    }
                }
            }
        }
    }
}

void TypeChecker::check_assignment(AssignmentStatement *node)
{
    std::vector<TypePtr> rightTypes;
    for (auto &expr : node->right) { rightTypes.push_back(check_expression(expr.get())); }
    if (rightTypes.size() < node->left.size()) {
        while (rightTypes.size() < node->left.size()) { rightTypes.push_back(Type::make_nil()); }
    }
    for (size_t i = 0; i < node->left.size(); ++i) {
        Expression *lhs = node->left[i].get();
        TypePtr rhsType = (i < rightTypes.size() ? rightTypes[i] : Type::make_nil());
        if (auto nameExp = dynamic_cast<NameExpression *>(lhs)) {
            std::string name = nameExp->name;
            VariableInfo *var = _symbols.lookup(name);
            if (not var) {
                _errors.add_error(lhs->line, lhs->column, "Undefined variable " + name);
                continue;
            }
            if (var->is_const) { _errors.add_error(lhs->line, lhs->column, "Cannot assign to constant variable " + name); }
            if (not rhsType->is_assignable_to(var->type)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch: cannot assign " + rhsType->to_string() + " to " + var->type->to_string()); }
        } else if (auto idxExp = dynamic_cast<IndexExpression *>(lhs)) {
            TypePtr tableType = check_expression(idxExp->table.get());
            TypePtr indexType = check_expression(idxExp->index.get());
            if (tableType->kind == Type::Kind::ARRAY) {
                if (indexType->kind != Type::Kind::NUMBER and indexType->kind != Type::Kind::INTEGER) { _errors.add_error(lhs->line, lhs->column, "Array index is not a number"); }
                if (not rhsType->is_assignable_to(tableType->element)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch in array assignment: expected " + tableType->element->to_string()); }
            } else if (tableType->kind == Type::Kind::MAP) {
                if (not indexType->is_assignable_to(tableType->key)) { _errors.add_error(lhs->line, lhs->column, "Map index type mismatch: cannot index " + tableType->to_string() + " with " + indexType->to_string()); }
                if (not rhsType->is_assignable_to(tableType->value)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch in map assignment: expected " + tableType->value->to_string()); }
            } else if (tableType->kind == Type::Kind::TUPLE) {
                if (indexType->kind != Type::Kind::NUMBER and indexType->kind != Type::Kind::INTEGER) { _errors.add_error(lhs->line, lhs->column, "Tuple index is not a number"); }
            } else if (tableType->kind == Type::Kind::RECORD) {
                if (tableType->record->array_element_type) {
                    if (indexType->kind != Type::Kind::NUMBER and indexType->kind != Type::Kind::INTEGER) { _errors.add_error(lhs->line, lhs->column, "Array part index is not a number"); }
                    if (not rhsType->is_assignable_to(tableType->record->array_element_type)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch in array part assignment: expected " + tableType->record->array_element_type->to_string()); }
                } else {
                    _errors.add_error(lhs->line, lhs->column, "Cannot index type " + tableType->to_string());
                }
            }
        } else if (auto fieldExp = dynamic_cast<FieldExpression *>(lhs)) {
            TypePtr objType = check_expression(fieldExp->object.get());
            std::string fieldName = fieldExp->field;
            if (objType->kind == Type::Kind::RECORD) {
                auto &fields = objType->record->fields;
                auto it = fields.find(fieldName);
                if (it != fields.end()) {
                    TypePtr fieldType = it->second;
                    if (not rhsType->is_assignable_to(fieldType)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch for field '" + fieldName + "': cannot assign " + rhsType->to_string() + " to " + fieldType->to_string()); }
                } else {
                    if (objType->record->kind != TypeSymbol::Kind::INTERFACE) {
                        _errors.add_error(lhs->line, lhs->column, "Field " + fieldName + " does not exist in record " + objType->record->name);
                    } else {
                        _errors.add_error(lhs->line, lhs->column, "Cannot assign to field of interface type");
                    }
                }
            } else if (objType->kind == Type::Kind::MAP and objType->key->kind == Type::Kind::STRING) {
                if (objType->value->kind != Type::Kind::ANY and not rhsType->is_assignable_to(objType->value)) { _errors.add_error(lhs->line, lhs->column, "Type mismatch for table field '" + fieldName + "'"); }
                if (objType->value->kind == Type::Kind::ANY) { objType->value = rhsType; }
            } else if (objType->kind != Type::Kind::ANY) {
                _errors.add_error(lhs->line, lhs->column, "Cannot access field of type " + objType->to_string());
            }
        } else {
            _errors.add_error(node->line, node->column, "Invalid assignment target");
        }
    }
}

TypePtr TypeChecker::check_expression(Expression *expr) { return check_expression(expr, nullptr); }

TypePtr TypeChecker::check_expression(Expression *expr, const TypePtr &expectedType)
{
    if (not expr) return Type::make_nil();
    if (auto num = dynamic_cast<NumberExpression *>(expr)) {
        bool isInt = true;
        for (char c : num->value) {
            if (c == '.' or c == 'e' or c == 'E') {
                isInt = false;
                break;
            }
        }
        return isInt ? Type::make_integer() : Type::make_number();
    }
    if (dynamic_cast<StringExpression *>(expr)) { return Type::make_string(); }
    if (dynamic_cast<BooleanExpression *>(expr)) { return Type::make_boolean(); }
    if (dynamic_cast<NilExpression *>(expr)) { return Type::make_nil(); }
    if (auto nameExp = dynamic_cast<NameExpression *>(expr)) {
        VariableInfo *var = _symbols.lookup(nameExp->name);
        if (not var) {
            _errors.add_error(expr->line, expr->column, "Undefined variable " + nameExp->name);
            return Type::make_any();
        }
        return var->type;
    }
    if (auto callExp = dynamic_cast<FunctionCallExpression *>(expr)) {
        TypePtr baseType = check_expression(callExp->base.get());
        std::vector<TypePtr> argTypes;
        for (auto &arg : callExp->arguments) { argTypes.push_back(check_expression(arg.get())); }
        if (baseType->kind == Type::Kind::FUNCTION) {
            auto sig = baseType->function.get();
            size_t provided = argTypes.size();
            size_t minRequired = 0;
            for (bool opt : sig->optional_parameters) {
                if (not opt) minRequired++;
            }
            if (provided < minRequired or (not sig->is_varadict and provided > sig->parameter_types.size())) {
                _errors.add_error(expr->line, expr->column, "Incorrect number of arguments in function call");
            } else {
                size_t checkCount = std::min(provided, sig->parameter_types.size());
                for (size_t i = 0; i < checkCount; ++i) {
                    if (not argTypes[i]->is_assignable_to(sig->parameter_types[i])) { _errors.add_error(expr->line, expr->column, "Argument " + std::to_string(i + 1) + " type mismatch: expected " + sig->parameter_types[i]->to_string() + ", got " + argTypes[i]->to_string()); }
                }
            }
            if (not sig->return_types.empty()) {
                return sig->return_types[0];
            } else if (sig->is_varadict_return) {
                return Type::make_any();
            } else {
                return Type::make_nil();
            }
        } else if (baseType->kind == Type::Kind::ANY) {
            return Type::make_any();
        } else {
            _errors.add_error(expr->line, expr->column, "Attempt to call non-function type " + baseType->to_string());
            return Type::make_any();
        }
    }
    if (auto binOp = dynamic_cast<BinaryOperationExpression *>(expr)) {
        TypePtr ltype = check_expression(binOp->left.get());
        TypePtr rtype = check_expression(binOp->right.get());
        switch (binOp->operation) {
        case TokenType::ADD:
        case TokenType::SUB:
        case TokenType::MUL:
        case TokenType::DIV:
        case TokenType::MOD:
        case TokenType::POW:
            if (not ltype->is_assignable_to(Type::make_number()) or not rtype->is_assignable_to(Type::make_number())) { _errors.add_error(expr->line, expr->column, "Arithmetic operator applied to non-numeric type"); }
            if (ltype->kind == Type::Kind::INTEGER and rtype->kind == Type::Kind::INTEGER and binOp->operation != TokenType::DIV) {
                return Type::make_integer();
            } else {
                return Type::make_number();
            }
        case TokenType::CONCAT:
            if (not ltype->is_assignable_to(Type::make_string()) and not ltype->is_assignable_to(Type::make_number())) { _errors.add_error(expr->line, expr->column, "Concat left operand not string or number"); }
            if (not rtype->is_assignable_to(Type::make_string()) and not rtype->is_assignable_to(Type::make_number())) { _errors.add_error(expr->line, expr->column, "Concat right operand not string or number"); }
            return Type::make_string();
        case TokenType::EQUALS:
        case TokenType::NOT_EQ:
        case TokenType::LESS:
        case TokenType::LESS_EQ:
        case TokenType::GREATER:
        case TokenType::GREATER_EQ:
            if (not ltype->equals(rtype) and ltype->kind != Type::Kind::ANY and rtype->kind != Type::Kind::ANY) { _errors.add_error(expr->line, expr->column, "Comparing incompatible types " + ltype->to_string() + " and " + rtype->to_string()); }
            return Type::make_boolean();
        case TokenType::AND: {
            TypePtr resultType;
            if (ltype->kind == Type::Kind::UNION) {
                bool hasFalsy = false;
                for (auto &opt : ltype->union_members) {
                    if (opt->kind == Type::Kind::NIL or opt->kind == Type::Kind::BOOLEAN) {
                        hasFalsy = true;
                        break;
                    }
                }
                resultType = hasFalsy ? Type::make_union({ Type::make_nil(), rtype }) : rtype;
            } else {
                resultType = (ltype->kind == Type::Kind::NIL or ltype->kind == Type::Kind::BOOLEAN) ? Type::make_union({ ltype, rtype }) : rtype;
            }
            return resultType ? resultType : rtype;
        }
        case TokenType::OR: {
            TypePtr resultType;
            if (ltype->kind == Type::Kind::UNION) {
                bool hasFalsy = false;
                std::vector<TypePtr> truthies;
                for (auto &opt : ltype->union_members) {
                    if (opt->kind == Type::Kind::NIL or opt->kind == Type::Kind::BOOLEAN) {
                        hasFalsy = true;
                    } else {
                        truthies.push_back(opt);
                    }
                }
                resultType = hasFalsy ? Type::make_union({ rtype, ltype }) : ltype;
            } else {
                resultType = (ltype->kind == Type::Kind::NIL or ltype->kind == Type::Kind::BOOLEAN) ? Type::make_union({ ltype, rtype }) : ltype;
            }
            return resultType ? resultType : Type::make_union({ ltype, rtype });
        }
        default:
            return Type::make_any();
        }
    }
    if (auto unOp = dynamic_cast<UnaryOperationExpression *>(expr)) {
        TypePtr operandType = check_expression(unOp->operand.get());
        switch (unOp->operation) {
        case TokenType::SUB:
            if (not operandType->is_assignable_to(Type::make_number())) { _errors.add_error(expr->line, expr->column, "Unary minus on non-numeric type"); }
            return operandType->kind == Type::Kind::INTEGER ? Type::make_integer() : Type::make_number();
        case TokenType::NOT:
            return Type::make_boolean();
        case TokenType::LENGTH:
            if (operandType->kind == Type::Kind::STRING or operandType->kind == Type::Kind::ARRAY or operandType->kind == Type::Kind::MAP or operandType->kind == Type::Kind::TUPLE) {
                return Type::make_number();
            } else if (operandType->kind == Type::Kind::RECORD and operandType->record->array_element_type) {
                return Type::make_number();
            } else if (operandType->kind == Type::Kind::ANY) {
                return Type::make_number();
            } else {
                _errors.add_error(expr->line, expr->column, "Length operator not supported on type " + operandType->to_string());
                return Type::make_number();
            }
        default:
            return Type::make_any();
        }
    }
    if (auto isExp = dynamic_cast<IsTypeExpression *>(expr)) {
        check_expression(isExp->expression.get());
        return Type::make_boolean();
    }
    if (auto castExp = dynamic_cast<CastExpression *>(expr)) {
        TypePtr exprType = check_expression(castExp->expression.get());
        if (not castExp->target_types.empty()) {
            TypePtr targetType = resolve_type_node(castExp->target_types[0].get());
            if (!(exprType->kind == Type::Kind::ANY or targetType->kind == Type::Kind::ANY)) {
                if (exprType->is_assignable_to(targetType)) {
                    // already assignable, cast not needed
                }
            }
            return targetType;
        }
        return Type::make_any();
    }
    if (auto tableLit = dynamic_cast<TableConstructorExpression *>(expr)) {
        if (expectedType) {
            if (expectedType->kind == Type::Kind::ARRAY or expectedType->kind == Type::Kind::TUPLE) {
                TypePtr elemType = (expectedType->kind == Type::Kind::ARRAY) ? expectedType->element : nullptr;
                std::vector<TypePtr> tupleTypes;
                if (expectedType->kind == Type::Kind::TUPLE) { tupleTypes = expectedType->tuple_types; }
                size_t index = 0;
                for (auto &field : tableLit->fields) {
                    TypePtr valType;
                    if (std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                        valType = check_expression(std::get<std::unique_ptr<Expression>>(field).get());
                        if (expectedType->kind == Type::Kind::ARRAY) {
                            if (not valType->is_assignable_to(elemType)) { _errors.add_error(expr->line, expr->column, "Array element type mismatch: expected " + elemType->to_string()); }
                        } else if (expectedType->kind == Type::Kind::TUPLE) {
                            if (index < tupleTypes.size() and not valType->is_assignable_to(tupleTypes[index])) { _errors.add_error(expr->line, expr->column, "Tuple element " + std::to_string(index + 1) + " type mismatch"); }
                        }
                    } else {
                        const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                        TypePtr keyType = std::holds_alternative<std::string>(kv.key) ? Type::make_string() : check_expression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                        TypePtr valueType = check_expression(kv.value.get());
                        if (expectedType->kind == Type::Kind::MAP) {
                            if (not keyType->is_assignable_to(expectedType->key)) { _errors.add_error(expr->line, expr->column, "Table key type mismatch: expected " + expectedType->key->to_string()); }
                            if (not valueType->is_assignable_to(expectedType->value)) { _errors.add_error(expr->line, expr->column, "Table value type mismatch: expected " + expectedType->value->to_string()); }
                        } else {
                            _errors.add_error(expr->line, expr->column, "Key-value in array literal not allowed");
                        }
                    }
                    index++;
                }
                return expectedType;
            } else if (expectedType->kind == Type::Kind::MAP) {
                TypePtr keyType = expectedType->key;
                TypePtr valType = expectedType->value;
                for (auto &field : tableLit->fields) {
                    if (std::holds_alternative<std::unique_ptr<Expression>>(field)) {
                        _errors.add_error(expr->line, expr->column, "Array entry in map literal not allowed");
                    } else {
                        const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                        TypePtr kType = std::holds_alternative<std::string>(kv.key) ? Type::make_string() : check_expression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                        TypePtr vType = check_expression(kv.value.get());
                        if (not kType->is_assignable_to(keyType)) { _errors.add_error(expr->line, expr->column, "Table key type mismatch"); }
                        if (not vType->is_assignable_to(valType)) { _errors.add_error(expr->line, expr->column, "Table value type mismatch"); }
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
                if (hasKV) { _errors.add_error(expr->line, expr->column, "Mixed array and map literal entries"); }
                TypePtr vType = check_expression(std::get<std::unique_ptr<Expression>>(field).get());
                if (not arrayElemType) arrayElemType = vType;
                else if (not vType->equals(arrayElemType)) {
                    arrayElemType = Type::make_union({ arrayElemType, vType });
                }
            } else {
                hasKV = true;
                const auto &kv = std::get<TableConstructorExpression::KeyValuePair>(field);
                TypePtr kType = std::holds_alternative<std::string>(kv.key) ? Type::make_string() : check_expression(std::get<std::unique_ptr<Expression>>(kv.key).get());
                TypePtr vType = check_expression(kv.value.get());
                if (not mapKeyType) {
                    mapKeyType = kType;
                    mapValType = vType;
                } else {
                    if (not kType->equals(mapKeyType)) { mapKeyType = Type::make_union({ mapKeyType, kType }); }
                    if (not vType->equals(mapValType)) { mapValType = Type::make_union({ mapValType, vType }); }
                }
            }
        }
        if (not hasKV) {
            if (not arrayElemType) { return Type::make_array(Type::make_any()); }
            if (arrayElemType->kind == Type::Kind::UNION) {
                return Type::make_array(arrayElemType);
            } else {
                return Type::make_array(arrayElemType);
            }
        } else if (not arrayElemType) {
            if (not mapKeyType) { return Type::make_map(Type::make_any(), Type::make_any()); }
            return Type::make_map(mapKeyType, mapValType);
        } else {
            return Type::make_any();
        }
    }
    return Type::make_any();
}

void TypeChecker::check_statement(Statement *stmt)
{
    if (not stmt) return;
    if (auto block = dynamic_cast<Block *>(stmt)) {
        _symbols.push_scope();
        for (auto &s : block->statements) { check_statement(s.get()); }
        _symbols.pop_scope();
    } else if (auto ret = dynamic_cast<ReturnStatement *>(stmt)) {
        std::vector<TypePtr> retTypes;
        for (auto &val : ret->values) { retTypes.push_back(check_expression(val.get())); }
        // Compare retTypes with current function's return types if we had context
    } else if (dynamic_cast<BreakStatement *>(stmt)) {
        if (_loop_depth <= 0) { _errors.add_error(stmt->line, stmt->column, "break statement not within a loop"); }
    } else if (auto ifs = dynamic_cast<IfStatement *>(stmt)) {
        check_if(ifs);
    } else if (auto wh = dynamic_cast<WhileStatement *>(stmt)) {
        check_while(wh);
    } else if (auto rep = dynamic_cast<RepeatStatement *>(stmt)) {
        check_repeat(rep);
    } else if (auto fornum = dynamic_cast<ForNumericStatement *>(stmt)) {
        check_numeric_for(fornum);
    } else if (auto forin = dynamic_cast<ForInStatement *>(stmt)) {
        check_for_in(forin);
    } else if (auto funcDecl = dynamic_cast<FunctionDeclarationStatement *>(stmt)) {
        check_function_declaration(funcDecl);
    } else if (auto varDecl = dynamic_cast<VariableDeclarationStatement *>(stmt)) {
        check_variable_declaration(varDecl);
    } else if (auto recordDecl = dynamic_cast<RecordDeclarationStatement *>(stmt)) {
        define_type(find_type_symbol({ recordDecl->name }));
    } else if (auto _ = dynamic_cast<EnumDeclarationStatement *>(stmt)) {
        // no additional check needed for enum beyond first pass
    } else if (auto aliasDecl = dynamic_cast<TypeAliasStatement *>(stmt)) {
        define_type(find_type_symbol({ aliasDecl->name }));
    } else if (auto assign = dynamic_cast<AssignmentStatement *>(stmt)) {
        check_assignment(assign);
    } else if (auto callStmt = dynamic_cast<CallStatement *>(stmt)) {
        if (callStmt->call) { check_expression(callStmt->call.get()); }
    }
}
