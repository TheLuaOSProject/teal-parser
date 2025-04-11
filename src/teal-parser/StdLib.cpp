#include "TypeChecker.hpp" // Access to TypeChecker and its members like symbol_table, Type classes

using namespace teal::parser;;
using namespace teal::parser::typechecker;

// Helper function to create a simple function type
std::shared_ptr<FunctionType> make_func(const ast::ASTNode& node,
                                       std::vector<FunctionParameter> params,
                                       std::vector<std::shared_ptr<Type>> rets) {
    auto return_tuple = std::make_shared<TupleType>(node, std::move(rets));
    return std::make_shared<FunctionType>(node, std::move(params), return_tuple);
}
std::shared_ptr<FunctionType> make_func_va_in(const ast::ASTNode& node,
                                       std::string param_name, std::shared_ptr<Type> param_type,
                                       std::vector<std::shared_ptr<Type>> rets) {
    auto return_tuple = std::make_shared<TupleType>(node, std::move(rets));
    std::vector<FunctionParameter> params = {{param_name, param_type, false, true}};
    return std::make_shared<FunctionType>(node, std::move(params), return_tuple);
}
std::shared_ptr<FunctionType> make_func_va_out(const ast::ASTNode& node,
                                       std::vector<FunctionParameter> params,
                                       std::shared_ptr<Type> ret_type) {
    auto return_tuple = std::make_shared<TupleType>(node, std::vector<std::shared_ptr<Type>>{ret_type}, true);
    return std::make_shared<FunctionType>(node, std::move(params), return_tuple);
}

void TypeChecker::load_stdlib() {
    auto global_scope = symbol_table.get_global_scope();
    if (!global_scope) return;

    ast::ASTNode builtin_loc(0,0); // Dummy location for built-ins
    auto any_t = get_any_type(builtin_loc);
    auto string_t = std::make_shared<StringType>(builtin_loc);
    auto number_t = std::make_shared<NumberType>(builtin_loc);
    auto integer_t = std::make_shared<IntegerType>(builtin_loc);
    auto boolean_t = std::make_shared<BooleanType>(builtin_loc);
    auto nil_t = get_nil_type(builtin_loc);
    auto thread_t = std::make_shared<ThreadType>(builtin_loc);
    auto userdata_t = std::make_shared<UserdataType>(builtin_loc);
    auto func_t = std::make_shared<FunctionType>(builtin_loc, std::vector<FunctionParameter>{}, std::make_shared<TupleType>(builtin_loc, std::vector<std::shared_ptr<Type>>{})); // Basic function type placeholder

    // --- Global Functions ---
    global_scope->add_symbol({"print", make_func_va_in(builtin_loc, "...", any_t, {}), true});
    global_scope->add_symbol({"type", make_func(builtin_loc, {{"v", any_t, false, false}}, {string_t}), true});
    global_scope->add_symbol({"tostring", make_func(builtin_loc, {{"v", any_t, false, false}}, {string_t}), true});
    global_scope->add_symbol({"tonumber", make_func(builtin_loc, {{"e", any_t, false, false}, {"base", integer_t, true, false}}, {UnionType::create_union(builtin_loc, {number_t, nil_t})}), true}); // Returns number? in Lua 5.1+
    global_scope->add_symbol({"error", make_func(builtin_loc, {{"message", any_t, false, false}, {"level", integer_t, true, false}}, {}), true}); // Returns nothing (never returns)
    global_scope->add_symbol({"assert", make_func_va_out(builtin_loc, {{"v", any_t, false, false}, {"message", any_t, true, true}}, any_t), true}); // Returns v and potentially message... -> any...

    // pairs/ipairs iterators (simplified - real iterators return func, state, init)
    // auto iterator_func = make_func(builtin_loc, std::vector<FunctionParameter> {*any_t, *any_t}, {any_t, any_t}); // Simplified iterator type
    // auto pairs_returns = std::make_shared<TupleType>(builtin_loc, std::vector<std::shared_ptr<Type>>{iterator_func, any_t, nil_t});
    // global_scope->add_symbol({"pairs", make_func(builtin_loc, {{"t", any_t, false, false}}, {iterator_func, any_t, nil_t}), true});
    // global_scope->add_symbol({"ipairs", make_func(builtin_loc, {{"t", any_t, false, false}}, {iterator_func, any_t, integer_t}), true}); // ipairs state is number
    // global_scope->add_symbol({"next", make_func(builtin_loc, {{"t", any_t, false, false}, {"k", any_t, true, false}}, {any_t, any_t}), true});


    // --- Math Table ---
    auto math_rec = std::make_shared<RecordType>(builtin_loc, "math");
    math_rec->is_global = true;
    auto num_num = make_func(builtin_loc, {{"x", number_t, false, false}}, {number_t});
    auto num_int = make_func(builtin_loc, {{"x", number_t, false, false}}, {integer_t});
    auto num_num_num = make_func(builtin_loc, {{"x", number_t, false, false}, {"y", number_t, false, false}}, {number_t});
    auto int_int_int = make_func(builtin_loc, {{"m", integer_t, false, false}, {"n", integer_t, true, false}}, {integer_t}); // random

    math_rec->fields["abs"] = {"abs", num_num};
    math_rec->fields["acos"] = {"acos", num_num};
    math_rec->fields["asin"] = {"asin", num_num};
    math_rec->fields["atan"] = {"atan", num_num}; // Overload needed for atan2
    math_rec->fields["ceil"] = {"ceil", num_int};
    math_rec->fields["floor"] = {"floor", num_int};
    math_rec->fields["cos"] = {"cos", num_num};
    math_rec->fields["sin"] = {"sin", num_num};
    math_rec->fields["tan"] = {"tan", num_num};
    math_rec->fields["exp"] = {"exp", num_num};
    math_rec->fields["log"] = {"log", num_num}; // Overload needed for log(x, base)
    math_rec->fields["sqrt"] = {"sqrt", num_num};
    math_rec->fields["pow"] = {"pow", num_num_num};
    math_rec->fields["fmod"] = {"fmod", num_num_num}; // Lua 5.1 returns number, 5.3+ depends
    math_rec->fields["random"] = {"random", int_int_int}; // Overload needed for no args or 1 arg
    math_rec->fields["randomseed"] = {"randomseed", make_func(builtin_loc, {{"x", number_t, false, false}}, {})}; // Lua 5.1 style
    math_rec->fields["pi"] = {"pi", number_t};
    math_rec->fields["huge"] = {"huge", number_t};
    // Add others: deg, rad, max, min, modf, ldexp, frexp, type (5.3+),ult (5.3+)
     global_scope->add_symbol({"math", math_rec, true});


    // --- String Table ---
     auto string_rec = std::make_shared<RecordType>(builtin_loc, "string");
     string_rec->is_global = true;
     auto str_int_int_ints = make_func_va_out(builtin_loc, {{"s", string_t, false, false}, {"i", integer_t, true, false}, {"j", integer_t, true, false}}, integer_t);
     string_rec->fields["byte"] = {"byte", str_int_int_ints};
     string_rec->fields["char"] = {"char", make_func_va_in(builtin_loc, "...", integer_t, {string_t})};
     string_rec->fields["len"] = {"len", make_func(builtin_loc, {{"s", string_t, false, false}}, {integer_t})};
     string_rec->fields["sub"] = {"sub", make_func(builtin_loc, {{"s", string_t, false, false}, {"i", integer_t, false, false}, {"j", integer_t, true, false}}, {string_t})};
     string_rec->fields["upper"] = {"upper", make_func(builtin_loc, {{"s", string_t, false, false}}, {string_t})};
     string_rec->fields["lower"] = {"lower", make_func(builtin_loc, {{"s", string_t, false, false}}, {string_t})};
     string_rec->fields["reverse"] = {"reverse", make_func(builtin_loc, {{"s", string_t, false, false}}, {string_t})};
     string_rec->fields["rep"] = {"rep", make_func(builtin_loc, {{"s", string_t, false, false}, {"n", integer_t, false, false}, {"sep", string_t, true, false}}, {string_t})};
     // format, find, gmatch, gsub, match need more complex types (often returning unions or multiple values)
     // Simplified format:
     string_rec->fields["format"] = {"format", make_func_va_in(builtin_loc, "...", any_t, {string_t})}; // First arg is string fmt
      // Simplified find:
      auto find_ret = std::make_shared<TupleType>(builtin_loc, std::vector<std::shared_ptr<Type>>{UnionType::create_union(builtin_loc, {integer_t, nil_t})}, true); // int|nil, int|nil, ... captures
     string_rec->fields["find"] = {"find", make_func(builtin_loc, {{"s", string_t, false, false}, {"pattern", string_t, false, false}, {"init", integer_t, true, false}, {"plain", boolean_t, true, false}}, {find_ret->element_types[0] /*int|nil*/, find_ret->element_types[0] /*int|nil*/ })}; // Simplified return
      // Add pack, unpack, packsize (5.3+)
      global_scope->add_symbol({"string", string_rec, true});


    // --- Table Table ---
     auto table_rec = std::make_shared<RecordType>(builtin_loc, "table");
     table_rec->is_global = true;
     // insert, remove, sort modify the table - represented as void return for now
     table_rec->fields["insert"] = {"insert", make_func(builtin_loc, {{"t", any_t, false, false}, {"pos", any_t, false, false}, {"value", any_t, true, false}}, {})}; // Types depend on table
     table_rec->fields["remove"] = {"remove", make_func(builtin_loc, {{"t", any_t, false, false}, {"pos", integer_t, true, false}}, {any_t})}; // Returns removed element type
     table_rec->fields["concat"] = {"concat", make_func(builtin_loc, {{"list", any_t, false, false}, {"sep", string_t, true, false}, {"i", integer_t, true, false}, {"j", integer_t, true, false}}, {string_t})};
     // sort needs function type
     auto sort_cmp_func = make_func(builtin_loc, {{"a", any_t, false, false}, {"b", any_t, false, false}}, {boolean_t});
     table_rec->fields["sort"] = {"sort", make_func(builtin_loc, {{"list", any_t, false, false}, {"comp", sort_cmp_func, true, false}}, {})};
     // pack, unpack (5.2+)
     table_rec->fields["pack"] = {"pack", make_func_va_in(builtin_loc, "...", any_t, {any_t})}; // Returns a table {n=..., ...}
     table_rec->fields["unpack"] = {"unpack", make_func_va_out(builtin_loc, {{"list", any_t, false, false}, {"i", integer_t, true, false}, {"j", integer_t, true, false}}, any_t)};
     global_scope->add_symbol({"table", table_rec, true});

     // --- Add io, os, coroutine, debug, utf8, package ... ---
     // These require careful definition of their functions and nested types/records (like FILE)

}

// --- END OF FILE ---
