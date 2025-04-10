// Copyright (C) 2025 Amrit Bhogal
// 
// teal-parser-test is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// teal-parser-test is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with teal-parser-test. If not, see <https://www.gnu.org/licenses/>.

#include "AST.hpp"

using namespace teal::parser::ast;
using namespace teal::parser::ast::serialisation;

using namespace std::string_literals;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-value"

// #define $field(val, ...) std::pair<std::string, std::unique_ptr<Value>> { #val, Value::from((val __VA_OPT__(, __VA_ARGS__))) }
#define $field(val, ...) obj[#val] = Value::from((val __VA_OPT__(, __VA_ARGS__)))

#define $get_macro(_1, _2, NAME, ...) NAME

// Macro when no extra serialisation is provided; call the base version.
#define $serialisation0(N) \
    Object N::serialise() const {         \
        auto obj = ASTNode::serialise();  \
        obj["kind"] = Value::from(#N);     \
        $_SER

// Macro when an extra expression is provided; use it instead.
#define $serialisation1(N, EXTRA)       \
    Object N::serialise() const {       \
        auto obj = (EXTRA);             \
        obj["kind"] = Value::from(#N);   \
        $_SER

// Dispatcher macro that selects between $serialisation0 and $serialisation1.
#define $serialisation(...) $get_macro(__VA_ARGS__, $serialisation1, $serialisation0)(__VA_ARGS__)

// #define $serialisation(N, ...) Object N::serialise() const {\
//     auto obj = (ASTNode::serialise() __VA_OPT__(, __VA_ARGS__)); \
//     obj["kind"] = Value::from(#N); \
//     $_SER
#define $_SER(...)\
    /*__VA_OPT__(insert_many(&obj, {__VA_ARGS__}));*/\
    __VA_ARGS__\
    return obj;\
}

Object ASTNode::serialise() const
{
    auto obj = Object();

    obj["kind"] = Value::from("ASTNode");
    obj["line"] = Value::from((long long)line);
    obj["column"] = Value::from((long long)column);
    
    return obj;
}


$serialisation(NameExpression) (
    $field(name);
);

$serialisation(NumberExpression) (
    $field(value, ({
        auto v = Number();
        if (value.find('.') != std::string::npos)
            v = std::stod(value);
        else
            v = std::stoll(value);
        v;
    }));
);

$serialisation(StringExpression) (
    $field(value);
);

$serialisation(BooleanExpression) (
    $field(value);
);

$serialisation(NilExpression) ();

$serialisation(VarargExpression) ();

$serialisation(FunctionCallExpression) (
    $field(base);
    $field(method_name);
    $field(arguments);
);

$serialisation(IndexExpression) (
    $field(table);
    $field(index);
);

$serialisation(FieldExpression) (
    $field(object);
    $field(field);
);

$serialisation(OperationExpression) (
    $field(operation, Token::type_to_string(operation)) ; 
);

$serialisation(BinaryOperationExpression) (
    $field(left);
    $field(right);
);

$serialisation(UnaryOperationExpression) (
    $field(operand);
);

$serialisation(Block) (
    $field(statements);
);

$serialisation(GenericTypeParameter, Object()) (
    $field(name);
    $field(is);
);

$serialisation(FunctionBody::Parameter, Object()) (
    $field(name);
    $field(is_varadict);
    $field(is_optional);
    $field(type);
);

$serialisation(FunctionBody) (
    $field(type_parameters);
    $field(parameters);
    $field(return_types);
    $field(varadict_return);
    $field(body);
);

$serialisation(FunctionDefinitionExpression) (
    $field(body);
);

$serialisation(CastExpression) (
    $field(expression);
    $field(target_types);
);

$serialisation(IsTypeExpression) (
    $field(expression);
    $field(type);
);

// Object TableConstructorExpression::KeyValuePair::serialise() const
// {
//     auto o = Object();

//     o["kind"] = Value::from("TableConstructorExpression::KeyValuePair");
//     o["key"] = Value::from(({
//         auto v = Value();
//         if (std::holds_alternative<std::string>(key))
//             v = std::get<std::string>(key);
//         else
//             v = std::get<std::unique_ptr<Expression>>(key)->serialise();
//         std::move(v);
//     }));
//     o["value"] = Value::from(value);
//     o["type"] = Value::from(type);

//     return o;
// }

$serialisation(TableConstructorExpression::KeyValuePair, Object()) (
    $field(key, ({
        auto v = Value();
        if (std::holds_alternative<std::string>(key))
            v = std::get<std::string>(key);
        else
            v = std::get<std::unique_ptr<Expression>>(key)->serialise();
        std::move(v);
    }));
    $field(value);
    $field(type);
);

$serialisation(TableConstructorExpression) (
    $field(fields, ({
        auto arr = Array();
        arr.reserve(fields.size());

        for (auto &v : fields) {
            if (std::holds_alternative<std::unique_ptr<Expression>>(v)) {
                arr.push_back(Value::from(std::get<std::unique_ptr<Expression>>(v)->serialise()));
            } else {
                arr.push_back(Value::from(std::get<KeyValuePair>(v).serialise()));
            }
        }

        Value::from(std::move(arr));
    }));
);

$serialisation(BasicTypeNode) (
    $field(name);
);

$serialisation(NominalTypeNode) (
    $field(name_parts);
    $field(type_arguments);
);

$serialisation(TableTypeNode) (
    $field(element_types);
    $field(key_type);
    $field(is_map);
);

$serialisation(FunctionTypeNode::ParameterType, Object()) (
    $field(name);
    $field(is_optional);
    $field(type);
);

$serialisation(FunctionTypeNode) (
    $field(type_parameters);
    $field(parameters);
    $field(return_types);
    $field(varadict_return); 
);

$serialisation(UnionTypeNode) (
    $field(options);
);

$serialisation(RecordBody::Entry, Object()) (
    $field(entry_kind, ({
        std::string k;
        switch (entry_kind) {
            case Entry::Kind::FIELD: k = "FIELD"; break;
            case Entry::Kind::USERDATA: k = "USERDATA"; break;
            case Entry::Kind::TYPE_ALIAS: k = "TYPE_ALIAS"; break;
            case Entry::Kind::RECORD: k = "RECORD"; break;
            case Entry::Kind::ENUM: k = "ENUM"; break;
            case Entry::Kind::INTERFACE: k = "INTERFACE"; break;
        }
        k;
    }));
    $field(is_metamethod);
    $field(name);
    $field(key_literal);
    $field(type);
    $field(type_name);
    $field(type_value);
    $field(nested_name);
    $field(nested_body);
);

$serialisation(RecordBody) (
    $field(type_parameters);
    $field(structural_ext);
    $field(interface_ext);
    $field(where_clause);
    $field(entries);
);



$serialisation(TypeRecordNode) (
    $field(body);
);

$serialisation(TypeEnumNode) (
    $field(elements);
);

$serialisation(RequireTypeNode) (
    $field(module_name);
    $field(type_names);
);

$serialisation(ReturnStatement) (
    $field(values);
);

$serialisation(BreakStatement) ();

$serialisation(GotoStatement) (
    $field(label);
);

$serialisation(LabelStatement) (
    $field(name);
);

$serialisation(DoStatement) (
    $field(body);
);

$serialisation(IfStatement::IfBranch, Object()) (
    $field(condition);
    $field(block);
);

$serialisation(IfStatement) (
    $field(if_branches);
    $field(else_block);
);

$serialisation(WhileStatement) (
    $field(condition);
    $field(body);
);

$serialisation(RepeatStatement) (
    $field(body);
    $field(condition);
);

$serialisation(ForNumericStatement) (
    $field(variable_name);
    auto exprs = Object();
    exprs["start"] = Value::from(expressions.start);
    exprs["end"] = Value::from(expressions.end);
    exprs["step"] = Value::from(expressions.step);
    $field(expressions, std::move(exprs));
    $field(body);
);

$serialisation(ForInStatement) (
    $field(names);
    $field(exprs);
    $field(body);
);

$serialisation(FunctionDeclarationStatement) (
    $field(visibility, std::string(visibility_to_string(visibility)));
    $field(name_path);
    $field(method_name);
    $field(is_method);
    $field(is_macro);
    $field(body);
);

$serialisation(VariableDeclarationStatement::Name, Object()) (
    $field(name);
    $field(attribute);
);

$serialisation(VariableDeclarationStatement) (
    $field(visibility, std::string(visibility_to_string(visibility)));
    $field(names);
    $field(types);
    $field(values);
);

$serialisation(RecordDeclarationStatement) (
    $field(is_interface);
    $field(visibility, std::string(visibility_to_string(visibility)));
    $field(name);
    $field(body);
);

$serialisation(EnumBody) (
    $field(elements);
);

$serialisation(EnumDeclarationStatement) (
    $field(visibility, std::string(visibility_to_string(visibility)));
    $field(name);
    $field(body);
);

$serialisation(TypeAliasStatement) (
    $field(visibility, std::string(visibility_to_string(visibility)));
    $field(name);
    $field(type_parameters);
    $field(type);
);

$serialisation(AssignmentStatement) (
    $field(left);
    $field(right);
);

$serialisation(CallStatement) (
    $field(call);
);

#pragma clang diagnostic pop

template<class ...Ts>
struct Overload : Ts... {
    using Ts::operator()...;
};
template<class ...Ts>
Overload(Ts...) -> Overload<Ts...>;

template<typename Variant>
static constexpr auto match(Variant &&var)
{
    return [&var]<typename... T>(T &&...matchers) { return std::visit(Overload { std::forward<T>(matchers)... }, std::forward<Variant>(var)); };
}

template<typename T>
constexpr T replace_all(T s, std::string_view from, std::string_view to) {
    size_t pos = 0;
    while ((pos = s.find(from, pos)) != std::string::npos) {
        s.replace(pos, from.size(), to);
        pos += to.size();
    }
    return s;
}

struct ReplaceAdaptor {
    std::string_view from;
    std::string_view to;
};

template<typename Range>
constexpr inline auto operator|(Range &&range, const ReplaceAdaptor &adaptor)
{ return replace_all(std::string(std::begin(range), std::end(range)), adaptor.from, adaptor.to); }
constexpr inline auto replace(std::string_view from, std::string_view to)
{ return ReplaceAdaptor{from, to}; }

//Properly escape the string so it can be properly used for JSON
std::string escape_string(const std::string &str)
{
    return str
        | replace("\\", "\\\\")
        | replace("\"", "\\\"")
        | replace("\b", "\\b")
        | replace("\f", "\\f")
        | replace("\n", "\\n")
        | replace("\r", "\\r")
        | replace("\t", "\\t");
}

std::string Value::to_json()
{
    return match(*this) (
        [](const std::string &v) {
            return std::format("\"{}\"", escape_string(v));
        },
        [](bool v) { return v ? "true"s : "false"s; },
        [](const Number &n) {
            if (std::holds_alternative<double>(n)) {
                return std::to_string(std::get<double>(n));
            } else {
                return std::to_string(std::get<long long>(n));
            }
        },
        [](std::monostate) { return "null"s; },
        [](const Object &v) {
            auto obj = "{"s;
            for (const auto &[k, v] : v) {
                obj += std::format("\"{}\": {}, ", k, v->to_json());
            }
            if (obj.size() > 1) {
                obj.resize(obj.size() - 2); // remove last comma and space
            }
            obj += "}";
            return obj;
        },
        [](const Array &v) {
            auto arr = "["s;
            for (const auto &v : v) {
                arr += std::format("{}, ", v->to_json());
            }
            if (arr.size() > 1) {
                arr.resize(arr.size() - 2);
            }
            arr += "]";
            return arr;
        },
        [](const auto &x) { return std::format("Unknown node of type {}", typeid(x).name()); }
    );
}
