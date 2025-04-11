#include "Type.hpp"
#include <sstream>

using namespace teal::parser::typecheck;

static TypePtr s_nilType;
static TypePtr s_booleanType;
static TypePtr s_numberType;
static TypePtr s_integerType;
static TypePtr s_stringType;
static TypePtr s_anyType;

TypePtr Type::makeNil() {
    if (not s_nilType) s_nilType = std::make_shared<Type>(Kind::Nil);
    return s_nilType;
}
TypePtr Type::makeBoolean() {
    if (not s_booleanType) s_booleanType = std::make_shared<Type>(Kind::Boolean);
    return s_booleanType;
}
TypePtr Type::makeNumber() {
    if (not s_numberType) s_numberType = std::make_shared<Type>(Kind::Number);
    return s_numberType;
}
TypePtr Type::makeInteger() {
    if (not s_integerType) s_integerType = std::make_shared<Type>(Kind::Integer);
    return s_integerType;
}
TypePtr Type::makeString() {
    if (not s_stringType) s_stringType = std::make_shared<Type>(Kind::String);
    return s_stringType;
}
TypePtr Type::makeAny() {
    if (not s_anyType) s_anyType = std::make_shared<Type>(Kind::Any);
    return s_anyType;
}
TypePtr Type::makeUnknown() {
    return makeAny();
}
TypePtr Type::makeUnion(const std::vector<TypePtr>& members) {
    std::vector<TypePtr> flat;
    for (auto &t : members) {
        if (not t) continue;
        if (t->kind == Kind::Union) {
            for (auto &sub : t->union_members) flat.push_back(sub);
        } else {
            flat.push_back(t);
        }
    }
    std::vector<TypePtr> unique;
    for (auto &t : flat) {
        bool dup = false;
        for (auto &u : unique) {
            if (t->equals(u)) { dup = true; break; }
        }
        if (not dup) unique.push_back(t);
    }
    if (unique.empty()) return makeNil();
    if (unique.size() == 1) return unique.front();
    for (auto &u : unique) {
        if (u->kind == Kind::Any) {
            return makeAny();
        }
    }
    TypePtr ut = std::make_shared<Type>(Kind::Union);
    ut->union_members = unique;
    return ut;
}
TypePtr Type::makeFunction(const std::vector<TypePtr>& params, const std::vector<bool>& opt, bool varargs,
                           const std::vector<TypePtr>& rets, bool ret_varargs,
                           const std::vector<TypePtr>& type_params) {
    auto t = std::make_shared<Type>(Kind::Function);
    t->func = std::make_unique<FunctionSig>();
    t->func->param_types = params;
    t->func->param_optional = opt;
    t->func->param_varargs = varargs;
    t->func->return_types = rets;
    t->func->return_varargs = ret_varargs;
    t->func->type_params = type_params;
    return t;
}
TypePtr Type::makeRecord(std::shared_ptr<TypeSymbol> symbol, const std::vector<TypePtr>& type_args) {
    assert(symbol);
    if (type_args.empty()) {
        if (symbol->type) return symbol->type;
        TypePtr t = std::make_shared<Type>(Kind::Record);
        t->record = symbol;
        t->type_args.clear();
        symbol->type = t;
        return t;
    }
    TypePtr t = std::make_shared<Type>(Kind::Record);
    t->record = symbol;
    t->type_args = type_args;
    return t;
}
TypePtr Type::makeEnum(std::shared_ptr<TypeSymbol> symbol) {
    assert(symbol);
    if (symbol->type) return symbol->type;
    TypePtr t = std::make_shared<Type>(Kind::Enum);
    t->enum_type = symbol;
    symbol->type = t;
    return t;
}
TypePtr Type::makeArray(TypePtr element_type) {
    auto t = std::make_shared<Type>(Kind::Array);
    t->element = element_type;
    return t;
}
TypePtr Type::makeMap(TypePtr key_type, TypePtr value_type) {
    auto t = std::make_shared<Type>(Kind::Map);
    t->key = key_type;
    t->value = value_type;
    return t;
}
TypePtr Type::makeTuple(const std::vector<TypePtr>& element_types) {
    auto t = std::make_shared<Type>(Kind::Tuple);
    t->tuple_types = element_types;
    return t;
}
TypePtr Type::makeTypeVar(const std::string &name, TypePtr constraint) {
    auto info = std::make_shared<TypeVarInfo>(name, constraint);
    TypePtr t = std::make_shared<Type>(Kind::TypeVar);
    t->type_var = info;
    return t;
}

bool Type::equals(const TypePtr &other) const {
    if (not other) return false;
    if (this == other.get()) return true;
    if (kind != other->kind) return false;
    switch(kind) {
    case Kind::Nil:
    case Kind::Boolean:
    case Kind::Number:
    case Kind::Integer:
    case Kind::String:
    case Kind::Any:
    case Kind::Unknown:
        return true;
    case Kind::TypeVar:
        return type_var == other->type_var;
    case Kind::Record:
        if (record != other->record) return false;
        if (type_args.size() != other->type_args.size()) return false;
        for (size_t i = 0; i < type_args.size(); ++i) {
            if (not type_args[i]->equals(other->type_args[i])) return false;
        }
        return true;
    case Kind::Enum:
        return enum_type == other->enum_type;
    case Kind::Union: {
        if (union_members.size() != other->union_members.size()) return false;
        for (auto &m : union_members) {
            bool found = false;
            for (auto &om : other->union_members) {
                if (m->equals(om)) { found = true; break; }
            }
            if (not found) return false;
        }
        return true;
    }
    case Kind::Function: {
        auto &f1 = func;
        auto &f2 = other->func;
        if (not f1 or not f2) return false;
        if (f1->param_types.size() != f2->param_types.size()) return false;
        if (f1->param_optional != f2->param_optional) return false;
        if (f1->param_varargs != f2->param_varargs) return false;
        if (f1->return_types.size() != f2->return_types.size()) return false;
        if (f1->return_varargs != f2->return_varargs) return false;
        if (f1->type_params.size() != f2->type_params.size()) return false;
        for (size_t i = 0; i < f1->type_params.size(); ++i) {
            TypePtr tp1 = f1->type_params[i];
            TypePtr tp2 = f2->type_params[i];
            if (tp1->type_var and tp2->type_var) {
                TypePtr c1 = tp1->type_var->constraint;
                TypePtr c2 = tp2->type_var->constraint;
                if ((c1 and not c2) or (not c1 and c2)) return false;
                if (c1 and c2 and not c1->equals(c2)) return false;
            }
        }
        for (size_t i = 0; i < f1->param_types.size(); ++i) {
            if (not f1->param_types[i]->equals(f2->param_types[i])) return false;
        }
        for (size_t i = 0; i < f1->return_types.size(); ++i) {
            if (not f1->return_types[i]->equals(f2->return_types[i])) return false;
        }
        return true;
    }
    case Kind::Array:
        return element->equals(other->element);
    case Kind::Map:
        return key->equals(other->key) and value->equals(other->value);
    case Kind::Tuple:
        if (tuple_types.size() != other->tuple_types.size()) return false;
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            if (not tuple_types[i]->equals(other->tuple_types[i])) return false;
        }
        return true;
    }
    return false;
}

bool Type::isAssignableTo(const TypePtr &target) const {
    if (not target) return false;
    if (equals(target) or target->kind == Kind::Any) {
        return true;
    }
    if (kind == Kind::Any and target->kind != Kind::Any) {
        return false;
    }
    if (kind == Kind::Nil) {
        if (target->kind == Kind::Nil) return true;
        if (target->kind == Kind::Union) {
            for (auto &opt : target->union_members) {
                if (opt->kind == Kind::Nil) return true;
            }
        }
        return false;
    }
    if (target->kind == Kind::Nil) {
        return false;
    }
    if (kind == Kind::Integer and target->kind == Kind::Number) {
        return true;
    }
    if (kind == Kind::Number and target->kind == Kind::Integer) {
        return false;
    }
    if (target->kind == Kind::Union) {
        for (auto &opt : target->union_members) {
            if (isAssignableTo(opt)) {
                return true;
            }
        }
        return false;
    }
    if (kind == Kind::Union) {
        for (auto &opt : union_members) {
            if (not opt->isAssignableTo(target)) {
                return false;
            }
        }
        return true;
    }
    if (kind == Kind::Record and target->kind == Kind::Record) {
        if (record != target->record) {
            if (target->record->kind == TypeSymbol::Kind::Interface) {
                for (auto &intf : record->interfaces) {
                    if (intf->equals(target->record->type)) {
                        return true;
                    }
                }
                bool allFields = true;
                for (const auto &[fname, ftype] : target->record->fields) {
                    auto it = record->fields.find(fname);
                    if (it == record->fields.end()) { allFields = false; break; }
                    if (not it->second->isAssignableTo(ftype)) { allFields = false; break; }
                }
                return allFields;
            }
            return false;
        }
        if (type_args.size() == target->type_args.size()) {
            for (size_t i = 0; i < type_args.size(); ++i) {
                if (not type_args[i]->equals(target->type_args[i])) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }
    if (kind == Kind::Record and record->kind == TypeSymbol::Kind::Interface and
        target->kind == Kind::Record and target->record->kind == TypeSymbol::Kind::Interface) {
        return record == target->record;
    }
    if (kind == Kind::Record and target->kind == Kind::Record) {
        return false;
    }
    if (kind == Kind::Enum and target->kind == Kind::String) {
        return true;
    }
    if (kind == Kind::String and target->kind == Kind::Enum) {
        return false;
    }
    if (kind == Kind::Array and target->kind == Kind::Array) {
        return element->equals(target->element);
    }
    if (kind == Kind::Map and target->kind == Kind::Map) {
        return key->equals(target->key) and value->equals(target->value);
    }
    if (kind == Kind::Tuple and target->kind == Kind::Tuple) {
        if (tuple_types.size() != target->tuple_types.size()) return false;
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            if (not tuple_types[i]->isAssignableTo(target->tuple_types[i])) {
                return false;
            }
        }
        return true;
    }
    if (kind == Kind::Function and target->kind == Kind::Function) {
        return equals(target);
    }
    if (target->kind == Kind::TypeVar) {
        TypePtr constr = target->type_var->constraint;
        return constr ? isAssignableTo(constr) : true;
    }
    return false;
}

std::string Type::toString() const {
    std::ostringstream oss;
    switch(kind) {
    case Kind::Nil: oss << "nil"; break;
    case Kind::Boolean: oss << "boolean"; break;
    case Kind::Number: oss << "number"; break;
    case Kind::Integer: oss << "integer"; break;
    case Kind::String: oss << "string"; break;
    case Kind::Any:
    case Kind::Unknown:
        oss << "any"; break;
    case Kind::TypeVar:
        oss << type_var->name;
        if (type_var->constraint) {
            oss << " is " << type_var->constraint->toString();
        }
        break;
    case Kind::Record:
        if (record) {
            oss << record->name;
            if (not type_args.empty()) {
                oss << "<";
                for (size_t i = 0; i < type_args.size(); ++i) {
                    oss << type_args[i]->toString();
                    if (i + 1 < type_args.size()) oss << ", ";
                }
                oss << ">";
            }
        } else {
            oss << "(record)";
        }
        break;
    case Kind::Enum:
        oss << (enum_type ? enum_type->name : "(enum)");
        break;
    case Kind::Union:
        for (size_t i = 0; i < union_members.size(); ++i) {
            oss << union_members[i]->toString();
            if (i + 1 < union_members.size()) oss << " | ";
        }
        break;
    case Kind::Function:
        oss << "function";
        if (func and not func->type_params.empty()) {
            oss << "<";
            for (size_t i = 0; i < func->type_params.size(); ++i) {
                TypePtr tp = func->type_params[i];
                if (tp->kind == Kind::TypeVar) {
                    oss << tp->type_var->name;
                    if (tp->type_var->constraint) oss << " is " << tp->type_var->constraint->toString();
                } else {
                    oss << tp->toString();
                }
                if (i + 1 < func->type_params.size()) oss << ", ";
            }
            oss << ">";
        }
        if (func) {
            oss << "(";
            for (size_t i = 0; i < func->param_types.size(); ++i) {
                oss << func->param_types[i]->toString();
                if (func->param_optional[i]) oss << "?";
                if (i + 1 < func->param_types.size()) oss << ", ";
                else if (func->param_varargs) oss << "...";
            }
            oss << ")";
            if (not func->return_types.empty() or func->return_varargs) {
                oss << ": ";
                if (func->return_types.empty() and func->return_varargs) {
                    oss << "any...";
                } else {
                    for (size_t i = 0; i < func->return_types.size(); ++i) {
                        oss << func->return_types[i]->toString();
                        if (i + 1 < func->return_types.size()) oss << ", ";
                        else if (func->return_varargs) oss << ", any...";
                    }
                }
            }
        }
        break;
    case Kind::Array:
        oss << "{" << element->toString() << "}";
        break;
    case Kind::Map:
        oss << "{" << key->toString() << ":" << value->toString() << "}";
        break;
    case Kind::Tuple:
        oss << "{";
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            oss << tuple_types[i]->toString();
            if (i + 1 < tuple_types.size()) oss << ", ";
        }
        oss << "}";
        break;
    }
    return oss.str();
}
