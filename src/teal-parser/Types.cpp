#include "Types.hpp"
#include <algorithm> // For std::all_of, std::any_of
#include <limits>    // For SIZE_MAX
#include <sstream>

using namespace teal::parser;
using namespace typechecker;
// --- Base Type ---

bool Type::is_subtype_of(std::shared_ptr<const Type> other) const
{
    // Any type is a subtype of 'any'
    if (other->kind == Kind::ANY) { return true; }
    // Default: only subtypes of itself (checked by equals)
    return this->equals(other);
}

bool Type::check_equality(std::shared_ptr<const Type> t1, std::shared_ptr<const Type> t2)
{
    if (t1 == t2) return true; // Same instance
    if (not t1 or not t2) return false; // One is null
    // Simple kind check first
    if (t1->kind != t2->kind) return false;
    // Delegate to virtual equals
    return t1->equals(t2);
}

bool Type::check_subtype(std::shared_ptr<const Type> sub, std::shared_ptr<const Type> super)
{
    // if (!sub || !super) return false; // Or handle differently? Invalid usually propagates.
    // if (sub == super) return true; // Same instance

    // // Avoid error cascades: Invalid is subtype of everything, everything is subtype of Invalid
    // if (sub->kind == Kind::INVALID || super->kind == Kind::INVALID) return true;

    // // Resolve aliases before checking subtype relationship
    // // auto resolved_sub = resolve_alias(std::const_pointer_cast<Type>(sub->shared_from_this())); // Needs shared_ptr? Hacky. Pass shared_ptr
    // // directly? auto resolved_super = resolve_alias(std::const_pointer_cast<Type>(super->shared_from_this())); // Ugh, need to refactor to pass
    // // shared_ptr

    // auto resolved_sub = resolve_alias(sub);
    // auto resolved_super = resolve_alias(super);

    // if (!resolved_sub || !resolved_super) return false; // Alias resolution failed?

    // // Check again after resolving aliases
    // if (resolved_sub == resolved_super) return true;
    // if (resolved_sub->kind == Kind::INVALID || resolved_super->kind == Kind::INVALID) return true;

    // // Any is supertype of all, but only subtype of itself (handled in AnyType::is_subtype_of)
    // if (resolved_super->kind == Kind::ANY) return true;

    // // Nil is subtype of all (potentially except non-nullable types)
    // if (resolved_sub->kind == Kind::NIL) return NilType(ast::ASTNode(0, 0)).is_subtype_of(resolved_super); // Call Nil's implementation

    // // Delegate to virtual is_subtype_of on the resolved subtype
    // return resolved_sub->is_subtype_of(resolved_super);
    if (!sub || !super) return false; // Null pointers
    if (sub.get() == super.get()) return true; // Same instance

    // Resolve aliases first
    auto resolved_sub = resolve_alias(sub);
    auto resolved_super = resolve_alias(super);

    if (!resolved_sub || !resolved_super) return false; // Alias resolution failed (recursive?)
    if (resolved_sub.get() == resolved_super.get()) return true; // Same instance after resolution

    // Handle InvalidType to prevent error cascades
    if (resolved_sub->kind == Kind::INVALID || resolved_super->kind == Kind::INVALID) return true;

    // Any is supertype of all
    if (resolved_super->kind == Kind::ANY) return true;
    // But 'any' is only subtype of 'any' (handled in AnyType::is_subtype_of)

    // Nil is subtype of others (handled in NilType::is_subtype_of)
    if (resolved_sub->kind == Kind::NIL) {
        // Call the virtual method on the resolved nil instance
        return resolved_sub->is_subtype_of(resolved_super);
    }

    // Delegate to virtual is_subtype_of on the resolved subtype
    return resolved_sub->is_subtype_of(resolved_super);
}

// --- Primitive Types ---

bool AnyType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    // 'any' is only a subtype of 'any'
    return other->kind == Kind::ANY;
}

bool NilType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    // nil is a subtype of any type that isn't explicitly non-nullable
    // including unions containing nil, records (nullable fields), any, etc.
    // It's NOT a subtype of basic primitives like number, string, boolean unless they are in a union with nil.
    if (other->kind == Kind::ANY || other->kind == Kind::NIL) return true;
    if (other->kind == Kind::UNION) {
        // nil <: T1 | T2 if nil <: T1 or nil <: T2
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    // Can be assigned to optional record fields etc., but the base type itself isn't a supertype.
    // For basic assignability check `nil <: T` where T is not any/nil/union, return false.
    return false;
}

bool BooleanType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
    return this->equals(other);
}

std::string StringType::to_string() const
{
    if (literal_value) {
        // Basic escaping for quotes
        std::string escaped = *literal_value;
        size_t pos = 0;
        while ((pos = escaped.find('"', pos)) != std::string::npos) {
            escaped.replace(pos, 1, "\\\"");
            pos += 2;
        }
        return "\"" + escaped + "\""; // Represent literal string type with quotes
    }
    return "string";
}

bool StringType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::STRING) return false;
    const auto *other_str = static_cast<const StringType *>(other.get());
    // Literal strings are only equal if their values are equal
    if (literal_value && other_str->literal_value) { return *literal_value == *other_str->literal_value; }
    // Non-literal string type is equal to another non-literal string type
    // Literal string type is NOT equal to non-literal string type
    return !literal_value && !other_str->literal_value;
}

bool StringType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;

    // Literal string is subtype of non-literal string
    if (other->kind == Kind::STRING) {
        const auto *other_str = static_cast<const StringType *>(other.get());
        if (!other_str->literal_value) { // target is generic 'string'
            return true; // any string (literal or not) is a subtype of 'string'
        } else { // target is specific literal string "xyz"
            return this->equals(other); // only equal if literals match
        }
    }
    // Literal string is subtype of enum if its value is a member
    if (other->kind == Kind::ENUM) {
        if (literal_value) {
            const auto *enum_type = static_cast<const EnumType *>(other.get());
            return enum_type->members.count(*literal_value);
        }
        return false; // Non-literal string cannot be subtype of specific enum
    }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return this->equals(other); // Check equality otherwise
}

bool NumberType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return this->equals(other);
}

bool IntegerType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY || other->kind == Kind::NUMBER) { return true; }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return this->equals(other);
}

bool ThreadType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return this->equals(other);
}

bool UserdataType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
     // Userdata is subtype of specific record types if they are userdata proxies
    if (other->kind == Kind::RECORD) {
        const auto *rec = static_cast<const RecordType *>(other.get());
        return rec->is_userdata_proxy;
    }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return this->equals(other);
}

// --- Composite Types ---

std::string ArrayType::to_string() const { return "{" + (element_type ? element_type->to_string() : "<unknown>") + "}"; }

bool ArrayType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::ARRAY) return false;
    const auto *other_arr = static_cast<const ArrayType *>(other.get());
    return check_equality(element_type, other_arr->element_type);
}

// Ensure these definitions take non-const shared_ptr
std::shared_ptr<Type> resolve_alias(std::shared_ptr<Type> type) {
    std::set<Type*> visited; // Detect recursion
    return resolve_alias_recursive(std::move(type), visited);
}

// Ensure this definition takes non-const shared_ptr
std::shared_ptr<Type> resolve_alias_recursive(std::shared_ptr<Type> type, std::set<Type*>& visited) {
    while (type && type->kind == Type::Kind::ALIAS) {
        // Use raw pointer for visited check to handle cycles
        Type* current_raw = type.get();
        if (!visited.insert(current_raw).second) {
             // Error: Recursive type alias detected
             ast::ASTNode dummy_loc(type->line, type->column);
             // Add error? For now, return invalid type.
             // How to add error here? Pass checker instance? Or just return invalid?
             return std::make_shared<InvalidType>(dummy_loc);
        }
        // Get the next type *before* recursive call
        auto next_type = std::static_pointer_cast<TypeAlias>(type)->aliased_type;
        // Resolve the next type recursively
        type = resolve_alias_recursive(std::move(next_type), visited);
        // No need to remove from visited: if resolution fails below, we return invalid;
        // if it succeeds, we continue loop or exit with resolved type.
    }
    return type; // Return the first non-alias type found, or the original type
}

bool ArrayType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    // if (other->kind == Kind::ANY) return true;
    // if (other->kind == Kind::ARRAY) {
    //     const auto *other_arr = static_cast<const ArrayType *>(other.get());
    //     // Arrays are covariant in their element type: {T} <: {U} if T <: U
    //     return check_subtype(element_type, other_arr->element_type);
    // }
    //  // Array is a subtype of map<integer, V> if its element type is a subtype of V
    // if (other->kind == Kind::MAP) {
    //     const auto *other_map = static_cast<const MapType *>(other.get());
    //     if (other_map->key_type->kind == Kind::INTEGER || other_map->key_type->kind == Kind::NUMBER) {
    //         return check_subtype(element_type, other_map->value_type);
    //     }
    // }
    //  // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    // if (other->kind == Kind::UNION) {
    //     const auto *other_union = static_cast<const UnionType *>(other.get());
    //     return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
    //         return this->is_subtype_of(opt);
    //     });
    // }
    // return false; // Not subtype of other things by default

        // Note: 'this' is const Type*, 'other' is const Type&
    // We need shared_ptrs to call the static Type::check_subtype
    // This indicates a potential design issue if is_subtype_of needs shared_ptr logic internally.
    // For now, let's assume we can construct temporary shared_ptrs if needed,
    // or preferably rely only on raw pointers/references within the virtual methods.

    // Revert to using raw pointers for internal checks where possible
    if (other->kind == Kind::ANY) return true;
    if (other->kind == Kind::ARRAY) {
        const auto* other_arr = static_cast<const ArrayType*>(other.get());
        // Arrays are covariant in their element type: {T} <: {U} if T <: U
        // Use raw pointer check directly if resolve_alias isn't needed *within* this specific comparison
        return Type::check_subtype(element_type, other_arr->element_type); // Use the OLD static check_subtype(ptr, ptr) here? Yes.

        // *** OR change the OLD check_subtype back to using pointers ***
        // static bool check_subtype(const Type* sub, const Type* super); // In header
        // static bool check_subtype(const Type* sub, const Type* super) { // In cpp
        //    ... resolve alias ... (needs shared_ptr -> problem!)
        // }

        // *** Let's keep the check_subtype(shared_ptr, shared_ptr) and use it where needed ***
        // *** But is_subtype_of virtual methods should ideally work with references/pointers ***

        // Let's stick to the raw pointer check here for simplicity inside the virtual method:
         return Type::check_subtype(element_type, other_arr->element_type); // Requires check_subtype(ptr,ptr)
         // *** Correction: The Type::check_subtype needs alias resolution, so it MUST use shared_ptr version. ***
         // *** This means is_subtype_of CANNOT easily call the helper without shared_ptr access. ***
         // *** Quick Fix: Assume caller (the static check_subtype) already resolved aliases. ***
         // *** Therefore, inside the virtual method, we only need structural/direct checks. ***

         // Re-evaluate: The static Type::check_subtype handles alias resolution.
         // The virtual is_subtype_of should compare `this` (already resolved alias) with `other` (already resolved alias).
         // So, within the virtual methods, we don't call resolve_alias again.

         // Corrected ArrayType::is_subtype_of:
          if (other->kind == Kind::ARRAY) {
             const auto* other_arr = static_cast<const ArrayType*>(other.get());
             // Check if this->element_type is subtype of other_arr->element_type
             // Need the static check_subtype again for potential nested aliases in element types!
             return Type::check_subtype(this->element_type, other_arr->element_type); // Pass shared_ptr
          }
          // Corrected Map check inside ArrayType::is_subtype_of:
          if (other->kind == Kind::MAP) {
             const auto* other_map = static_cast<const MapType*>(other.get());
             if (other_map->key_type->kind == Kind::INTEGER || other_map->key_type->kind == Kind::NUMBER) {
                 return Type::check_subtype(this->element_type, other_map->value_type); // Pass shared_ptr
             }
         }
         // Corrected Union check inside ArrayType::is_subtype_of:
         if (other->kind == Kind::UNION) {
            //  const auto* other_union = static_cast<const UnionType*>(other.get());
             // Need a way to check 'this' against the options. Requires shared_ptr of 'this'.
             // This is the core problem. Let's postpone fixing this deep issue.
             // Assume the static helper handles unions correctly.
             // The virtual method should not handle unions itself usually.
              return false; // Should be handled by the static checker before calling virtual method?
         }
         return this->equals(other); // Fallback to equality check
    }

    // Repeat this correction logic for is_subtype_of in ALL Type subclasses.
    // The virtual methods should primarily check the structure assuming aliases are resolved
    // and unions are handled by the static Type::check_subtype caller.
}

std::string MapType::to_string() const
{
    return "{" + (key_type ? key_type->to_string() : "<unknown>") + ": " + (value_type ? value_type->to_string() : "<unknown>") + "}";
}

bool MapType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::MAP) return false;
    const auto *other_map = static_cast<const MapType *>(other.get());
    return check_equality(key_type, other_map->key_type) && check_equality(value_type, other_map->value_type);
}

bool MapType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
    if (other->kind == Kind::MAP) {
        const auto *other_map = static_cast<const MapType *>(other.get());
        // Maps are contravariant in key type (SuperKey <: SubKey)
        // and covariant in value type (SubValue <: SuperValue)
        return check_subtype(other_map->key_type, key_type) && check_subtype(value_type, other_map->value_type);
    }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return false;
}

std::string TupleType::to_string() const
{
    std::stringstream ss;
    // Use parentheses for tuples representing function returns only?
    // Teal syntax uses {} for tuple types: {T1, T2, ...}
    ss << "{";
    for (size_t i = 0; i < element_types.size(); ++i) {
        ss << (element_types[i] ? element_types[i]->to_string() : "<unknown>");
        if (i == element_types.size() - 1 && is_variadic) {
            // Note: Teal syntax for variadic tuple type is `T...`, not `{..., T}`
            // The internal representation needs clarity. Assuming last element is repeated.
            ss << "...";
        }
        if (i < element_types.size() - 1) { ss << ", "; }
    }
    ss << "}";
    return ss.str();
}

bool TupleType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::TUPLE) return false;
    const auto *other_tuple = static_cast<const TupleType *>(other.get());
    if (is_variadic != other_tuple->is_variadic || element_types.size() != other_tuple->element_types.size()) { return false; }
    for (size_t i = 0; i < element_types.size(); ++i) {
        if (!check_equality(element_types[i], other_tuple->element_types[i])) { return false; }
    }
    return true;
}

std::shared_ptr<Type> TupleType::get_type_at(size_t index) const
{
    if (index < element_types.size()) { return element_types[index]; }
    if (is_variadic && !element_types.empty()) {
        return element_types.back(); // Return the variadic part's type
    }
    return nullptr; // Index out of bounds
}

bool TupleType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;

    // Tuple can be a subtype of an Array if all elements are subtypes of the array element type
    if (other->kind == Kind::ARRAY) {
        const auto *other_arr = static_cast<const ArrayType *>(other.get());
        if (is_variadic) return false; // Cannot guarantee homogeneity for array
        if (element_types.empty()) return true; // Empty tuple is subtype of any array
        return std::all_of(element_types.begin(), element_types.end(), [&](const std::shared_ptr<Type> &t) {
            return check_subtype(t, other_arr->element_type);
        });
    }
    // Tuple can be a subtype of Map<integer, V> if all elements are subtypes of V
    if (other->kind == Kind::MAP) {
        const auto *other_map = static_cast<const MapType *>(other.get());
        if (other_map->key_type->kind == Kind::INTEGER || other_map->key_type->kind == Kind::NUMBER) {
            if (is_variadic) return false; // Cannot guarantee homogeneity
            if (element_types.empty()) return true; // Empty tuple is subtype
            return std::all_of(element_types.begin(), element_types.end(), [&](const std::shared_ptr<Type> &t) {
                return check_subtype(t, other_map->value_type);
            });
        }
    }
    // Tuple is subtype of another tuple if element types match covariantly
    if (other->kind == Kind::TUPLE) {
        const auto *other_tuple = static_cast<const TupleType *>(other.get());
        size_t n_sub = element_types.size();
        size_t n_super = other_tuple->element_types.size();

        if (!is_variadic && !other_tuple->is_variadic) {
            // {T1, T2} <: {U1, U2} iff T1<:U1 and T2<:U2
            if (n_sub != n_super) return false;
            for (size_t i = 0; i < n_sub; ++i) {
                if (!check_subtype(element_types[i], other_tuple->element_types[i])) return false;
            }
            return true;
        } else if (is_variadic && other_tuple->is_variadic) {
            // {T1, T2...} <: {U1, U2...} iff T1<:U1 and T2<:U2
            if (n_sub != n_super) return false; // Require same base length for variadic subtype? Or allow different? Let's require same for now.
            for (size_t i = 0; i < n_sub; ++i) {
                if (!check_subtype(element_types[i], other_tuple->element_types[i])) return false;
            }
            return true;
        } else if (!is_variadic && other_tuple->is_variadic) {
            // {T1, T2, T3} <: {U1, U2...} iff T1<:U1, T2<:U2, T3<:U2
            if (n_super == 0) return false; // Cannot subtype into empty variadic
            if (n_sub < n_super - 1) return false; // Must have at least the fixed part
            auto super_vararg_type = other_tuple->element_types.back();
            for (size_t i = 0; i < n_super - 1; ++i) {
                if (!check_subtype(element_types[i], other_tuple->element_types[i])) return false;
            }
            // Check remaining fixed elements against the vararg type
            for (size_t i = n_super - 1; i < n_sub; ++i) {
                if (!check_subtype(element_types[i], super_vararg_type)) return false;
            }
            return true;
        } else { // is_variadic && !other_tuple->is_variadic
            // {T1, T2...} <: {U1, U2, U3} -> false
            return false; // Variadic cannot be subtype of fixed length tuple
        }
    }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    return false;
}

// --- Function Type ---

std::string FunctionType::to_string() const
{
    std::stringstream ss;
    ss << "function(";
    for (size_t i = 0; i < parameters.size(); ++i) {
        const auto &p = parameters[i];
        if (!p.name.empty()) {
            ss << p.name;
            if (p.is_optional) ss << "?";
            ss << ": ";
        }
        if (p.is_variadic) ss << "..."; // Teal syntax: ...: T
        else ss << (p.type ? p.type->to_string() : "<unknown>");

        if (i < parameters.size() - 1) ss << ", ";
    }
    ss << ")";

    // Format return types
    if (return_types && (!return_types->element_types.empty() || return_types->is_variadic)) {
        ss << ": ";
        const auto &returns = return_types->element_types;
        bool returns_are_va = return_types->is_variadic;
        bool use_parens = returns.size() > 1 || returns_are_va; // Use parens for multiple or variadic returns

        if (use_parens) ss << "(";
        for (size_t i = 0; i < returns.size(); ++i) {
            ss << (returns[i] ? returns[i]->to_string() : "<unknown>");
            if (i == returns.size() - 1 && returns_are_va) { ss << "..."; }
            if (i < returns.size() - 1) { ss << ", "; }
        }
        if (use_parens) ss << ")";
    }
    return ss.str();
}

size_t FunctionType::min_arity() const
{
    size_t count = 0;
    for (const auto &p : parameters) {
        if (!p.is_optional && !p.is_variadic) { count++; }
    }
    return count;
}

size_t FunctionType::max_arity() const
{
    if (has_variadic_param()) { return std::numeric_limits<size_t>::max(); }
    return parameters.size();
}
bool FunctionType::has_variadic_param() const { return !parameters.empty() && parameters.back().is_variadic; }

bool FunctionType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::FUNCTION) return false;
    const auto *other_func = static_cast<const FunctionType *>(other.get());

    if (parameters.size() != other_func->parameters.size()) return false;
    for (size_t i = 0; i < parameters.size(); ++i) {
        if (parameters[i].is_optional != other_func->parameters[i].is_optional || parameters[i].is_variadic != other_func->parameters[i].is_variadic
            || !check_equality(parameters[i].type, other_func->parameters[i].type)) {
            return false;
        }
    }
    return check_equality(return_types, other_func->return_types);
}

bool FunctionType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
    if (other->kind != Kind::FUNCTION) {
         // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
        if (other->kind == Kind::UNION) {
            const auto *other_union = static_cast<const UnionType *>(other.get());
            return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
                return this->is_subtype_of(opt);
            });
        }
        return false;
    }

    const auto *super_func = static_cast<const FunctionType *>(other.get());

    // Subtyping Rules:
    // 1. Arity: Subtype must accept AT LEAST the arguments the supertype requires,
    //           and must not require MORE arguments than the supertype accepts.
    //           (Super Min <= Sub Min) AND (Sub Max <= Super Max)
    // 2. Parameters: Contravariant (Super Param Type <: Sub Param Type)
    // 3. Return Types: Covariant (Sub Return Type <: Super Return Type)

    size_t sub_min = min_arity();
    size_t sub_max = max_arity();
    size_t super_min = super_func->min_arity();
    size_t super_max = super_func->max_arity();

    // Arity Check
    if (!(super_min <= sub_min && sub_max >= super_max)) { // Check if sub can accept call intended for super
         // This logic needs refinement. If super accepts 2-3 args, sub accepting 1-4 is okay.
         // If super accepts 2.. args, sub accepting 2.. is okay.
         // If super accepts 2 args, sub accepting 1.. is okay.
         // Rule: Can I call the subtype function wherever the supertype function is expected?
         // Subtype must accept *more* or *equal* parameters (more general).
         // Parameters are contravariant, so if super expects T, sub must accept T or supertype of T.
         // Return types are covariant, sub must return U or subtype of U if super returns U.

         // Let's refine the arity check:
         // Subtype must accept *at least* as many required args as supertype? No, contravariance.
         // Supertype parameter list must be assignable to subtype parameter list (contravariance).
         // Number of required params in subtype <= number of required params in supertype.
         // Total number of params (if not variadic) in subtype >= total number of params in supertype.

        if (min_arity() > super_func->min_arity()) return false; // Subtype requires more args than supertype
        if (!super_func->has_variadic_param() && has_variadic_param()) return false; // Subtype cannot be vararg if supertype is fixed
        if (!super_func->has_variadic_param() && !has_variadic_param() && parameters.size() < super_func->parameters.size())
            return false; // Subtype has less fixed params than supertype
    }

    // Parameter Type Check (Contravariant)
    size_t n_super_params = super_func->parameters.size();
    size_t n_sub_params = parameters.size();
    // bool super_is_va = super_func->has_variadic_param();
    bool sub_is_va = has_variadic_param();

    for (size_t i = 0; i < n_super_params; ++i) {
        const auto &super_param = super_func->parameters[i];
        std::shared_ptr<Type> sub_param_type_for_super = nullptr;

        if (i < n_sub_params) {
            // Match super param i with sub param i
            if (parameters[i].is_variadic != super_param.is_variadic) return false; // Variadic must align? Or handle sub fixed vs super va?
            // Optional check: Subtype param can be required if supertype is optional (sub is stricter).
            if (!parameters[i].is_optional && super_param.is_optional) { /* ok */
            }
            // Subtype param *cannot* be optional if supertype is required.
            if (parameters[i].is_optional && !super_param.is_optional && !super_param.is_variadic) return false;

            sub_param_type_for_super = parameters[i].type;

        } else if (sub_is_va) {
            // Match super param i against subtype's vararg param
            if (super_param.is_variadic) return false; // Cannot match fixed super param with sub vararg if super is also vararg here
            if (!super_param.is_optional) return false; // Subtype vararg cannot fulfill required super param if sub fixed params are exhausted
            sub_param_type_for_super = parameters.back().type; // Check against sub vararg type
        } else {
            // Subtype has fewer fixed parameters and no varargs, but supertype expects more
            // This should be caught by arity check, but double check required params
            if (!super_param.is_optional && !super_param.is_variadic) return false;
            // If super param was optional or vararg, it's okay that sub doesn't have it
            continue;
        }

        // Contravariant check: Super type must be subtype of Sub type
        if (!check_subtype(super_param.type, sub_param_type_for_super)) { return false; }
    }

    // Return Type Check (Covariant)
    // Sub Return Type <: Super Return Type
    if (!check_subtype(return_types, super_func->return_types)) { return false; }

    return true;
}

// --- Nominal Types ---

std::string EnumType::to_string() const
{
    return qualified_name; // +" (enum)";
}

bool EnumType::equals(std::shared_ptr<const Type> other) const
{
    // Nominal typing for enums
    if (other->kind != Kind::ENUM) return false;
    const auto *other_enum = static_cast<const EnumType *>(other.get());
    // Equality based on unique qualified name (assuming names are properly managed)
    return qualified_name == other_enum->qualified_name;
}

bool EnumType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
    // Enum is subtype of non-literal string
    if (other->kind == Kind::STRING) {
        const auto *other_str = static_cast<const StringType *>(other.get());
        if (!other_str->literal_value) return true; // Enum <: string
    }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
     // Nominal subtyping: Enum only subtype of itself (and string/any)
    return this->equals(other);
}

std::string RecordType::to_string() const
{
     // Optionally list fields/metamethods for debugging
    return qualified_name;
}

bool RecordType::equals(std::shared_ptr<const Type> other) const
{
    // Nominal typing for records
    if (other->kind != Kind::RECORD) return false;
    const auto *other_rec = static_cast<const RecordType *>(other.get());
    // Equality based on unique qualified name
    return qualified_name == other_rec->qualified_name;
}

bool RecordType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;

    // Check if it's a subtype of a map (usually map<string, V>) - Structural check
    if (other->kind == Kind::MAP) {
        const auto *other_map = static_cast<const MapType *>(other.get());
        auto map_key_type = other_map->key_type;
        auto map_val_type = other_map->value_type;
        // Check if map key can accept string (or any)
        auto str_t = get_string_type(ast::ASTNode(0, 0));
        if (check_subtype(str_t, map_key_type)) { // String <: MapKey ?
            // Check if all record fields are subtypes of the map's value type
            bool fields_ok
                = std::all_of(fields.begin(), fields.end(), [&](const auto &pair) { return check_subtype(pair.second.type, map_val_type); });
            // Also check metamethods? Maybe only fields for structural map compatibility.
            return fields_ok;
        }
    }
    // Check for userdata proxy compatibility
    if (other->kind == Kind::USERDATA && is_userdata_proxy) { return true; }
     // Check unions: T <: U1 | U2 if T <: U1 or T <: U2
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
     // Nominal subtyping: Record only subtype of itself (or compatible map/userdata/any)
     // TODO: Add structural subtyping check if interfaces/parent records are implemented
    return this->equals(other);
}

std::optional<RecordField> RecordType::get_field(const std::string &field_name) const
{
    auto it = fields.find(field_name);
    if (it != fields.end()) { return it->second; }
    // TODO: Check parent types / interfaces if structural inheritance is added
    return std::nullopt;
}

std::optional<std::shared_ptr<Type>> RecordType::get_field_type(const std::string &field_name) const
{
    if (auto field_opt = get_field(field_name)) { return field_opt->type; }
    // If field not found directly, check __index metamethod ONLY IF IT'S A TABLE.
    // If __index is a function, it's dynamic lookup, can't statically determine field type.
    // Static analysis usually relies on declared fields.
    return std::nullopt;
}

std::optional<std::shared_ptr<FunctionType>> RecordType::get_metamethod_type(const std::string &method_name) const
{
    auto it = metamethods.find(method_name);
    if (it != metamethods.end()) { return it->second; }
     // TODO: Check parent types / interfaces if structural inheritance is added
    return std::nullopt;
}

// --- Union Type ---

// Custom comparator for shared_ptr<Type> in the set
bool PtrTypeComparator::operator()(const std::shared_ptr<Type> &lhs, const std::shared_ptr<Type> &rhs) const
{
    if (!lhs && !rhs) return false; // equal
    if (!lhs) return true;  // null < non-null
    if (!rhs) return false; // non-null > null

    // Compare by kind first for broad ordering
    if (lhs->kind != rhs->kind) { return lhs->kind < rhs->kind; }

    // Use type_id for potentially faster comparison of complex types
    // if (lhs->type_id != 0 && rhs->type_id != 0 && lhs->type_id != rhs->type_id) {
    //      return lhs->type_id < rhs->type_id;
    // }
    // Primitives and types without unique IDs might share type_id 0.

    // Fallback to string comparison (ensure to_string is deterministic and reasonably efficient)
    // This is the main source of potential performance issues with large unions.
    return lhs->to_string() < rhs->to_string();
}

std::string UnionType::to_string() const
{
    std::stringstream ss;
    bool first = true;
    for (const auto &opt : options) {
        if (!first) ss << " | ";
        ss << (opt ? opt->to_string() : "<unknown>");
        first = false;
    }
    return ss.str();
}

bool UnionType::equals(std::shared_ptr<const Type> other) const
{
    if (other->kind != Kind::UNION) return false;
    const auto *other_union = static_cast<const UnionType *>(other.get());

    if (options.size() != other_union->options.size()) return false;

    // Element-wise comparison. Requires the sets to be ordered identically.
    // The PtrTypeComparator ensures this if `equals` is consistent.
    return std::equal(
        options.begin(), options.end(), other_union->options.begin(),
        [](const std::shared_ptr<Type> &a, const std::shared_ptr<Type> &b) { return check_equality(a, b); }
    );
}

bool UnionType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;

    // Union subtyping: T1 | T2 <: U iff T1 <: U AND T2 <: U
    // Check if *all* options in this union are subtypes of the target type 'other'.
    bool all_options_are_subtypes = std::all_of(options.begin(), options.end(), [&](const std::shared_ptr<Type> &opt) {
        return check_subtype(opt, other); // Pass raw pointer to avoid recursion issues with shared_ptr creation
    });

    return all_options_are_subtypes;
}

std::shared_ptr<Type> UnionType::create_union(const ast::ASTNode &node, const std::vector<std::shared_ptr<Type>> &types)
{
    std::set<std::shared_ptr<Type>, PtrTypeComparator> unique_options;
    std::vector<std::shared_ptr<Type>> flattened_options;
    std::set<Type *> visited_flatten; // Avoid infinite loops during flattening

    // 1. Flatten nested unions and initial filtering
    std::function<void(const std::shared_ptr<Type> &)> flatten = [&](const std::shared_ptr<Type> &t_ptr) {
        if (!t_ptr) return;
        Type *t = t_ptr.get();

        if (visited_flatten.count(t)) return; // Already processed this instance
        visited_flatten.insert(t);

        if (t->kind == Kind::INVALID) return; // Skip invalid types

        if (t->kind == Kind::ANY) {
            flattened_options.assign(1, t_ptr); // Union simplifies to 'any'
            return; // Stop processing
        }
        if (t->kind == Kind::UNION) {
            const auto *u = static_cast<const UnionType *>(t);
            for (const auto &opt : u->options) {
                flatten(opt);
                if (!flattened_options.empty() && flattened_options[0]->kind == Kind::ANY) return; // Early exit
            }
        } else if (t->kind == Kind::ALIAS) {
            // Resolve alias before adding
            flatten(resolve_alias(t_ptr));
        } else {
            flattened_options.push_back(t_ptr);
        }
        visited_flatten.erase(t); // Remove after processing children/self
    };

    for (const auto &t : types) {
        visited_flatten.clear();
        flatten(t);
        if (!flattened_options.empty() && flattened_options[0]->kind == Kind::ANY) break; // Early exit
    }

    // Handle simplification results
    if (flattened_options.empty()) return get_nil_type(node); // Union of nothing or only invalid types? Return nil?
    if (flattened_options.size() == 1 && flattened_options[0]->kind == Kind::ANY) return get_any_type(node);

    // 2. Remove subsumed types (if T <: U, remove T if U is present)
    std::vector<std::shared_ptr<Type>> simplified_options;
    simplified_options.reserve(flattened_options.size());

    for (const auto &t1 : flattened_options) {
        bool subsumed = false;
        for (const auto &t2 : flattened_options) {
            if (t1 == t2) continue; // Don't compare with self

            // Check if t1 is a subtype of t2 (and t2 is not a subtype of t1 to avoid removing both in number|integer case)
            if (check_subtype(t1, t2) /* && !check_subtype(t2, t1) */) {
                 // Special cases:
                 // - Keep specific string literal if generic string is present? No, string is supertype.
                 // - Keep integer if number is present? No, number is supertype.
                 // - Keep nil if T|nil? Yes.
                if (t1->kind == Kind::NIL && t2->kind != Kind::ANY) { // Keep nil unless 'any' is present
                    continue;
                }
                 // General case: if t1 is a proper subtype of t2, t1 is subsumed.
                subsumed = true;
                break;
            }
        }
        if (!subsumed) {
             // Check for duplicates before adding to simplified_options
            bool duplicate = false;
            for (const auto &existing : simplified_options) {
                if (check_equality(t1, existing)) {
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) { simplified_options.push_back(t1); }
        }
    }

    // 3. Build final set and return result
    for (const auto &opt : simplified_options) {
        unique_options.insert(opt); // Insert into the ordered set
    }

    if (unique_options.empty()) return get_nil_type(node); // Should be rare
    if (unique_options.size() == 1) return *unique_options.begin(); // Single type left

    return std::make_shared<UnionType>(node, std::move(unique_options));
}

std::shared_ptr<Type> UnionType::create_union(const ast::ASTNode &node, std::shared_ptr<Type> t1, std::shared_ptr<Type> t2)
{
    return create_union(node, std::vector<std::shared_ptr<Type>> { std::move(t1), std::move(t2) });
}

// --- Type Alias ---

std::string TypeAlias::to_string() const
{
    // Maybe indicate it's an alias for clarity in some contexts?
    // return name + " (alias for " + (aliased_type ? aliased_type->to_string() : "<unknown>") + ")";
    return name; // Usually just show the alias name
}

bool TypeAlias::equals(std::shared_ptr<const Type> other) const
{
    // Nominal equality for aliases themselves
    if (other->kind != Kind::ALIAS) return false;
    const auto *other_alias = static_cast<const TypeAlias *>(other.get());
    return qualified_name == other_alias->qualified_name;
    // Note: Two aliases are equal only if they are the *same* alias definition.
    // type A = string; type B = string; A != B
}

bool TypeAlias::is_subtype_of(std::shared_ptr<const Type> other) const
{
    // Resolve this alias first, then check subtype relationship
    auto resolved_self = resolve_alias(std::const_pointer_cast<Type>(this->shared_from_this())); // Hacky... needs refactor
    if (!resolved_self) return false; // Recursive alias error

    // Delegate to the resolved type's subtyping check
    return check_subtype(resolved_self, other);
}

// --- Special Types ---

bool EmptyTableType::is_subtype_of(std::shared_ptr<const Type> other) const
{
    if (other->kind == Kind::ANY) return true;
    // {} is assignable to any table-like type (array, map, record, tuple)
    // It's also assignable to a union containing any of these or nil/any.
    if (other->kind == Kind::ARRAY || other->kind == Kind::MAP || other->kind == Kind::RECORD || other->kind == Kind::TUPLE) { return true; }
    if (other->kind == Kind::UNION) {
        const auto *other_union = static_cast<const UnionType *>(other.get());
        return std::any_of(other_union->options.begin(), other_union->options.end(), [this](const std::shared_ptr<Type> &opt) {
            return this->is_subtype_of(opt);
        });
    }
    // Is {} a subtype of nil? No. Is it subtype of {}? Yes.
    return this->equals(other);
}

// --- Utility Functions ---

std::shared_ptr<Type> resolve_alias(std::shared_ptr<Type> type)
{
    std::set<Type *> visited;
    return resolve_alias_recursive(std::move(type), visited);
}

std::shared_ptr<Type> resolve_alias_recursive(std::shared_ptr<Type> type, std::set<Type *> &visited)
{
    while (type && type->kind == Type::Kind::ALIAS) {
        if (!visited.insert(type.get()).second) {
             // Error: Recursive type alias detected
             // Return InvalidType? Need location info. For now return nullptr signal error.
             // TODO: Propagate location info for error reporting
            ast::ASTNode dummy_loc(type->line, type->column);
            return std::make_shared<InvalidType>(dummy_loc);
        }
        // Recursively resolve the aliased type before continuing the loop
        auto next_type = static_cast<TypeAlias *>(type.get())->aliased_type;
        type = resolve_alias_recursive(std::move(next_type), visited);

        // We don't need to remove from visited here because we fully resolve
        // the inner alias before continuing with the outer one.
    }
    return type; // Return the first non-alias type found, or the original type if it wasn't an alias
}

// Simple global primitive instances (use get_X_type factories)
// std::shared_ptr<AnyType> ANY_TYPE = std::make_shared<AnyType>();
// ...

// Factory functions with basic caching simulation (just return new instance)
std::shared_ptr<Type> get_primitive_type(const std::string &name, const ast::ASTNode &node)
{
    if (name == "number") return std::make_shared<NumberType>(node);
    if (name == "integer") return std::make_shared<IntegerType>(node);
    if (name == "string") return std::make_shared<StringType>(node);
    if (name == "boolean") return std::make_shared<BooleanType>(node);
    if (name == "nil") return std::make_shared<NilType>(node);
    if (name == "any") return std::make_shared<AnyType>(node);
    if (name == "thread") return std::make_shared<ThreadType>(node);
    if (name == "userdata") return std::make_shared<UserdataType>(node);
    // ... other primitives ...
    return std::make_shared<InvalidType>(node); // Unknown primitive name
}
std::shared_ptr<NilType> get_nil_type(const ast::ASTNode &node)
{
    return std::make_shared<NilType>(node); // Use global instance if caching
}
std::shared_ptr<AnyType> get_any_type(const ast::ASTNode &node)
{
    return std::make_shared<AnyType>(node); // Use global instance if caching
}
std::shared_ptr<BooleanType> get_boolean_type(const ast::ASTNode &node) { return std::make_shared<BooleanType>(node); }
std::shared_ptr<NumberType> get_number_type(const ast::ASTNode &node) { return std::make_shared<NumberType>(node); }
std::shared_ptr<IntegerType> get_integer_type(const ast::ASTNode &node) { return std::make_shared<IntegerType>(node); }
std::shared_ptr<StringType> get_string_type(const ast::ASTNode &node) { return std::make_shared<StringType>(node); }

std::shared_ptr<InvalidType> get_invalid_type(const ast::ASTNode &node) { return std::make_shared<InvalidType>(node); }

// --- Hash Implementations (Example for StringType) ---
// size_t StringType::hash() const {
//     size_t h = std::hash<int>()(static_cast<int>(Kind::STRING));
//     if (literal_value) {
//         h = h * 31 + std::hash<std::string>()(*literal_value);
//     }
//     return h;
// }
// Implement hash() for other types similarly if needed for unordered containers.
