#include "Type.hpp"
#include <sstream>


using namespace teal::parser;
using namespace typecheck;

using ast::serialisation::Value;

ast::serialisation::Object Type::serialise() const
{
    auto obj = ast::serialisation::Object();
    obj["kind"] = Value::from("Type");
    
    switch (kind) {
    case Kind::NIL:
        obj["type"] = Value::from("nil");
        break;
    case Kind::BOOLEAN:
        obj["type"] = Value::from("boolean");
        break;
    case Kind::NUMBER:
        obj["type"] = Value::from("number");
        break;
    case Kind::INTEGER:
        obj["type"] = Value::from("integer");
        break;
    case Kind::STRING:
        obj["type"] = Value::from("string");
        break;
    case Kind::ANY:
        obj["type"] = Value::from("any");
        break;
    case Kind::UNKNOWN:
        obj["type"] = Value::from("unknown");
        break;
    case Kind::UNION:
        obj["type"] = Value::from("union");
        obj["members"] = Value::from(union_members);
        break;
    case Kind::FUNCTION:
        obj["type"] = Value::from("function");
        obj["signature"] = Value::from(function->serialise());
        break;
    case Kind::RECORD:
        obj["type"] = Value::from("record");
        obj["symbol"] = Value::from(record->serialise());
        if (not type_arguments.empty()) {
            obj["type_arguments"] = Value::from(type_arguments);
        } else {
            obj["type_arguments"] = Value::from(std::monostate());
        }
        break;
    case Kind::INTERFACE:
        obj["type"] = Value::from("interface");
        obj["symbol"] = Value::from(record->serialise());
        if (not type_arguments.empty()) {
            obj["type_arguments"] = Value::from(type_arguments);
        } else {
            obj["type_arguments"] = Value::from(std::monostate());
        }
        break;
    case Kind::ENUM:
        obj["type"] = Value::from("enum");
        obj["symbol"] = Value::from(enum_type->serialise());
        break;
    case Kind::ARRAY:
        obj["type"] = Value::from("array");
        obj["element_type"] = Value::from(element);
        break;
    case Kind::MAP:
        obj["type"] = Value::from("map");
        obj["key_type"] = Value::from(key);
        obj["value_type"] = Value::from(value);
        break;

    case Kind::TUPLE:
        obj["type"] = Value::from("tuple");
        obj["element_types"] = Value::from(tuple_types);
        break;

    case Kind::TYPE_VARIABLE:
        obj["type"] = Value::from("type_variable");
        obj["name"] = Value::from(type_variable->name);
        if (type_variable->constraint) {
            obj["constraint"] = Value::from(type_variable->constraint);
        } else {
            obj["constraint"] = Value::from(std::monostate());
        }
        break;

    default:
        obj["type"] = Value::from("unknown");
        break;
    }

    return obj;
}

ast::serialisation::Object TypeSymbol::serialise() const
{
    auto obj = ast::serialisation::Object();
    obj["name"] = Value::from(name);
    obj["kind"] = Value::from(({
        std::string k;
        switch (kind) {
        case Kind::RECORD:
            k = "record";
            break;
        case Kind::ENUM:
            k = "enum";
            break;
        case Kind::INTERFACE:
            k = "interface";
            break;
        case Kind::ALIAS:
            k = "alias";
            break;
        default:
            k = "unknown";
            break;
        }
        k;
    }));
    if (type) {
        obj["type"] = Value::from(type->serialise());
    } else {
        obj["type"] = Value::from(std::monostate());
    }
    return obj;
}

ast::serialisation::Object Type::FunctionSignature::serialise() const
{
    auto obj = ast::serialisation::Object();
    obj["is_varadict"] = Value::from(is_varadict);
    obj["is_varadict_return"] = Value::from(is_varadict_return);
    obj["return_types"] = Value::from(return_types);
    obj["parameter_types"] = Value::from(parameter_types);
    obj["optional_parameters"] = Value::from(optional_parameters);
    obj["type_parameters"] = Value::from(type_parameters);
    return obj;
}

static TypePtr s_nilType;
static TypePtr s_booleanType;
static TypePtr s_numberType;
static TypePtr s_integerType;
static TypePtr s_stringType;
static TypePtr s_anyType;

TypePtr Type::make_nil()
{
    if (not s_nilType) s_nilType = std::make_shared<Type>(Kind::NIL);
    return s_nilType;
}
TypePtr Type::make_boolean()
{
    if (not s_booleanType) s_booleanType = std::make_shared<Type>(Kind::BOOLEAN);
    return s_booleanType;
}
TypePtr Type::make_number()
{
    if (not s_numberType) s_numberType = std::make_shared<Type>(Kind::NUMBER);
    return s_numberType;
}
TypePtr Type::make_integer()
{
    if (not s_integerType) s_integerType = std::make_shared<Type>(Kind::INTEGER);
    return s_integerType;
}
TypePtr Type::make_string()
{
    if (not s_stringType) s_stringType = std::make_shared<Type>(Kind::STRING);
    return s_stringType;
}
TypePtr Type::make_any()
{
    if (not s_anyType) s_anyType = std::make_shared<Type>(Kind::ANY);
    return s_anyType;
}
TypePtr Type::make_unknown() { return make_any(); }
TypePtr Type::make_union(const std::vector<TypePtr> &members)
{
    std::vector<TypePtr> flat;
    for (auto &t : members) {
        if (not t) continue;
        if (t->kind == Kind::UNION) {
            for (auto &sub : t->union_members) flat.push_back(sub);
        } else {
            flat.push_back(t);
        }
    }
    std::vector<TypePtr> unique;
    for (auto &t : flat) {
        bool dup = false;
        for (auto &u : unique) {
            if (t->equals(u)) {
                dup = true;
                break;
            }
        }
        if (not dup) unique.push_back(t);
    }
    if (unique.empty()) return make_nil();
    if (unique.size() == 1) return unique.front();
    for (auto &u : unique) {
        if (u->kind == Kind::ANY) { return make_any(); }
    }
    TypePtr ut = std::make_shared<Type>(Kind::UNION);
    ut->union_members = unique;
    return ut;
}
TypePtr Type::make_function(
    const std::vector<TypePtr> &params, const std::vector<bool> &opt, bool varargs, const std::vector<TypePtr> &rets,
    bool ret_varargs, const std::vector<TypePtr> &type_params
)
{
    auto t = std::make_shared<Type>(Kind::FUNCTION);
    t->function = std::make_unique<FunctionSignature>();
    t->function->parameter_types = params;
    t->function->optional_parameters = opt;
    t->function->is_varadict = varargs;
    t->function->return_types = rets;
    t->function->is_varadict_return = ret_varargs;
    t->function->type_parameters = type_params;
    return t;
}
TypePtr Type::make_record(std::shared_ptr<TypeSymbol> symbol, const std::vector<TypePtr> &type_args)
{
    assert(symbol);
    if (type_args.empty()) {
        if (symbol->type) return symbol->type;
        TypePtr t = std::make_shared<Type>(Kind::RECORD);
        t->record = symbol;
        t->type_arguments.clear();
        symbol->type = t;
        return t;
    }
    TypePtr t = std::make_shared<Type>(Kind::RECORD);
    t->record = symbol;
    t->type_arguments = type_args;
    return t;
}
TypePtr Type::make_enum(std::shared_ptr<TypeSymbol> symbol)
{
    assert(symbol);
    if (symbol->type) return symbol->type;
    TypePtr t = std::make_shared<Type>(Kind::ENUM);
    t->enum_type = symbol;
    symbol->type = t;
    return t;
}
TypePtr Type::make_array(TypePtr element_type)
{
    auto t = std::make_shared<Type>(Kind::ARRAY);
    t->element = element_type;
    return t;
}
TypePtr Type::make_map(TypePtr key_type, TypePtr value_type)
{
    auto t = std::make_shared<Type>(Kind::MAP);
    t->key = key_type;
    t->value = value_type;
    return t;
}
TypePtr Type::make_tuple(const std::vector<TypePtr> &element_types)
{
    auto t = std::make_shared<Type>(Kind::TUPLE);
    t->tuple_types = element_types;
    return t;
}
TypePtr Type::make_type_variable(const std::string &name, TypePtr constraint)
{
    auto info = std::make_shared<TypeVariableInfo>(name, constraint);
    TypePtr t = std::make_shared<Type>(Kind::TYPE_VARIABLE);
    t->type_variable = info;
    return t;
}

bool Type::equals(const TypePtr &other) const
{
    if (not other) return false;
    if (this == other.get()) return true;
    if (kind != other->kind) return false;
    switch (kind) {
    case Kind::NIL:
    case Kind::BOOLEAN:
    case Kind::NUMBER:
    case Kind::INTEGER:
    case Kind::STRING:
    case Kind::ANY:
    case Kind::UNKNOWN:
        return true;
    case Kind::TYPE_VARIABLE:
        return type_variable == other->type_variable;
    case Kind::INTERFACE:
    case Kind::RECORD:
        if (record != other->record) return false;
        if (type_arguments.size() != other->type_arguments.size()) return false;
        for (size_t i = 0; i < type_arguments.size(); ++i) {
            if (not type_arguments[i]->equals(other->type_arguments[i])) return false;
        }
        return true;
    case Kind::ENUM:
        return enum_type == other->enum_type;
    case Kind::UNION: {
        if (union_members.size() != other->union_members.size()) return false;
        for (auto &m : union_members) {
            bool found = false;
            for (auto &om : other->union_members) {
                if (m->equals(om)) {
                    found = true;
                    break;
                }
            }
            if (not found) return false;
        }
        return true;
    }
    case Kind::FUNCTION: {
        auto &f1 = function;
        auto &f2 = other->function;
        if (not f1 or not f2) return false;
        if (f1->parameter_types.size() != f2->parameter_types.size()) return false;
        if (f1->optional_parameters != f2->optional_parameters) return false;
        if (f1->is_varadict != f2->is_varadict) return false;
        if (f1->return_types.size() != f2->return_types.size()) return false;
        if (f1->is_varadict_return != f2->is_varadict_return) return false;
        if (f1->type_parameters.size() != f2->type_parameters.size()) return false;
        for (size_t i = 0; i < f1->type_parameters.size(); ++i) {
            TypePtr tp1 = f1->type_parameters[i];
            TypePtr tp2 = f2->type_parameters[i];
            if (tp1->type_variable and tp2->type_variable) {
                TypePtr c1 = tp1->type_variable->constraint;
                TypePtr c2 = tp2->type_variable->constraint;
                if ((c1 and not c2) or (not c1 and c2)) return false;
                if (c1 and c2 and not c1->equals(c2)) return false;
            }
        }
        for (size_t i = 0; i < f1->parameter_types.size(); ++i) {
            if (not f1->parameter_types[i]->equals(f2->parameter_types[i])) return false;
        }
        for (size_t i = 0; i < f1->return_types.size(); ++i) {
            if (not f1->return_types[i]->equals(f2->return_types[i])) return false;
        }
        return true;
    }
    case Kind::ARRAY:
        return element->equals(other->element);
    case Kind::MAP:
        return key->equals(other->key) and value->equals(other->value);
    case Kind::TUPLE:
        if (tuple_types.size() != other->tuple_types.size()) return false;
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            if (not tuple_types[i]->equals(other->tuple_types[i])) return false;
        }
        return true;
    }
    return false;
}

bool Type::is_assignable_to(const TypePtr &target) const
{
    if (not target) return false;
    if (equals(target) or target->kind == Kind::ANY) { return true; }
    if (kind == Kind::ANY and target->kind != Kind::ANY) { return false; }
    if (kind == Kind::NIL) {
        if (target->kind == Kind::NIL) return true;
        if (target->kind == Kind::UNION) {
            for (auto &opt : target->union_members) {
                if (opt->kind == Kind::NIL) return true;
            }
        }
        return false;
    }
    if (target->kind == Kind::NIL) { return false; }
    if (kind == Kind::INTEGER and target->kind == Kind::NUMBER) { return true; }
    if (kind == Kind::NUMBER and target->kind == Kind::INTEGER) { return false; }
    if (target->kind == Kind::UNION) {
        for (auto &opt : target->union_members) {
            if (is_assignable_to(opt)) { return true; }
        }
        return false;
    }
    if (kind == Kind::UNION) {
        for (auto &opt : union_members) {
            if (not opt->is_assignable_to(target)) { return false; }
        }
        return true;
    }
    if (kind == Kind::RECORD and target->kind == Kind::RECORD) {
        if (record != target->record) {
            if (target->record->kind == TypeSymbol::Kind::INTERFACE) {
                for (auto &intf : record->interfaces) {
                    if (intf->equals(target->record->type)) { return true; }
                }
                bool allFields = true;
                for (const auto &[fname, ftype] : target->record->fields) {
                    auto it = record->fields.find(fname);
                    if (it == record->fields.end()) {
                        allFields = false;
                        break;
                    }
                    if (not it->second->is_assignable_to(ftype)) {
                        allFields = false;
                        break;
                    }
                }
                return allFields;
            }
            return false;
        }
        if (type_arguments.size() == target->type_arguments.size()) {
            for (size_t i = 0; i < type_arguments.size(); ++i) {
                if (not type_arguments[i]->equals(target->type_arguments[i])) { return false; }
            }
            return true;
        }
        return false;
    }
    if (kind == Kind::RECORD and record->kind == TypeSymbol::Kind::INTERFACE and target->kind == Kind::RECORD
        and target->record->kind == TypeSymbol::Kind::INTERFACE) {
        return record == target->record;
    }
    if (kind == Kind::RECORD and target->kind == Kind::RECORD) { return false; }
    if (kind == Kind::ENUM and target->kind == Kind::STRING) { return true; }
    if (kind == Kind::STRING and target->kind == Kind::ENUM) { return false; }
    if (kind == Kind::ARRAY and target->kind == Kind::ARRAY) { return element->equals(target->element); }
    if (kind == Kind::MAP and target->kind == Kind::MAP) {
        return key->equals(target->key) and value->equals(target->value);
    }
    if (kind == Kind::TUPLE and target->kind == Kind::TUPLE) {
        if (tuple_types.size() != target->tuple_types.size()) return false;
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            if (not tuple_types[i]->is_assignable_to(target->tuple_types[i])) { return false; }
        }
        return true;
    }
    if (kind == Kind::FUNCTION and target->kind == Kind::FUNCTION) { return equals(target); }
    if (target->kind == Kind::TYPE_VARIABLE) {
        TypePtr constr = target->type_variable->constraint;
        return constr ? is_assignable_to(constr) : true;
    }
    return false;
}

std::string Type::to_string() const
{
    std::ostringstream oss;
    switch (kind) {
    case Kind::NIL:
        oss << "nil";
        break;
    case Kind::BOOLEAN:
        oss << "boolean";
        break;
    case Kind::NUMBER:
        oss << "number";
        break;
    case Kind::INTEGER:
        oss << "integer";
        break;
    case Kind::STRING:
        oss << "string";
        break;
    case Kind::ANY:
    case Kind::UNKNOWN:
        oss << "any";
        break;
    case Kind::TYPE_VARIABLE:
        oss << type_variable->name;
        if (type_variable->constraint) { oss << " is " << type_variable->constraint->to_string(); }
        break;
    case Kind::INTERFACE:
    case Kind::RECORD:
        if (record) {
            oss << record->name;
            if (not type_arguments.empty()) {
                oss << "<";
                for (size_t i = 0; i < type_arguments.size(); ++i) {
                    oss << type_arguments[i]->to_string();
                    if (i + 1 < type_arguments.size()) oss << ", ";
                }
                oss << ">";
            }
        } else {
            oss << "(record)";
        }
        break;
    case Kind::ENUM:
        oss << (enum_type ? enum_type->name : "(enum)");
        break;
    case Kind::UNION:
        for (size_t i = 0; i < union_members.size(); ++i) {
            oss << union_members[i]->to_string();
            if (i + 1 < union_members.size()) oss << " | ";
        }
        break;
    case Kind::FUNCTION:
        oss << "function";
        if (function and not function->type_parameters.empty()) {
            oss << "<";
            for (size_t i = 0; i < function->type_parameters.size(); ++i) {
                TypePtr tp = function->type_parameters[i];
                if (tp->kind == Kind::TYPE_VARIABLE) {
                    oss << tp->type_variable->name;
                    if (tp->type_variable->constraint) oss << " is " << tp->type_variable->constraint->to_string();
                } else {
                    oss << tp->to_string();
                }
                if (i + 1 < function->type_parameters.size()) oss << ", ";
            }
            oss << ">";
        }
        if (function) {
            oss << "(";
            for (size_t i = 0; i < function->parameter_types.size(); ++i) {
                oss << function->parameter_types[i]->to_string();
                if (function->optional_parameters[i]) oss << "?";
                if (i + 1 < function->parameter_types.size()) oss << ", ";
                else if (function->is_varadict) oss << "...";
            }
            oss << ")";
            if (not function->return_types.empty() or function->is_varadict_return) {
                oss << ": ";
                if (function->return_types.empty() and function->is_varadict_return) {
                    oss << "any...";
                } else {
                    for (size_t i = 0; i < function->return_types.size(); ++i) {
                        oss << function->return_types[i]->to_string();
                        if (i + 1 < function->return_types.size()) oss << ", ";
                        else if (function->is_varadict_return) oss << ", any...";
                    }
                }
            }
        }
        break;
    case Kind::ARRAY:
        oss << "{" << element->to_string() << "}";
        break;
    case Kind::MAP:
        oss << "{" << key->to_string() << ":" << value->to_string() << "}";
        break;
    case Kind::TUPLE:
        oss << "{";
        for (size_t i = 0; i < tuple_types.size(); ++i) {
            oss << tuple_types[i]->to_string();
            if (i + 1 < tuple_types.size()) oss << ", ";
        }
        oss << "}";
        break;
    }
    return oss.str();
}
