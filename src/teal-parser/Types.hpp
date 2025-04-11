#pragma once

#include <functional> // For std::hash with shared_ptr
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

// Assume AST.hpp defines ast::ASTNode with line/column
#include "AST.hpp" // Need ASTNode for location info

namespace teal::parser::typechecker
{

// Forward declarations
    struct Type;
    struct FunctionType;
    struct RecordType;
    struct EnumType;
    struct ArrayType;
    struct MapType;
    struct TupleType;
    struct UnionType;
    struct NominalType;
    struct AnyType;
    struct NilType;
    struct BooleanType;
    struct StringType;
    struct NumberType;
    struct IntegerType;
    struct ThreadType;
    struct UserdataType;
    struct InvalidType; // Represents a type error
    struct EmptyTableType; // Special type for initial {} inference
    struct TypeAlias; // Forward declare TypeAlias

// --- Base Type ---
    struct Type : std::enable_shared_from_this<Type> {
        size_t line = 0;
        size_t column = 0;
        std::string filename = ""; // Source location for error reporting

    // Basic type identifier for faster checks than dynamic_cast in some cases
        enum class Kind {
            ANY,
            NIL,
            BOOLEAN,
            STRING,
            NUMBER,
            INTEGER,
            THREAD,
            USERDATA,
            INVALID,
            EMPTY_TABLE,
            ARRAY,
            MAP,
            TUPLE,
            FUNCTION,
            ENUM,
            RECORD,
            UNION,
            ALIAS
        } kind;

    // Unique ID generation (simple counter for now, could be more sophisticated)
        inline static size_t next_type_id = 1;
        size_t type_id = 0; // 0 indicates unassigned or primitive that doesn't need unique ID

        Type(Kind k, size_t l, size_t c, std::string f) : line(l), column(c), filename(std::move(f)), kind(k) { }
        Type(Kind k, const ast::ASTNode &node) :
            line(node.line), column(node.column), filename("@internal"), kind(k)
        { /*TODO: Pass filename properly*/ } // Use filename from node if available
        virtual ~Type() = default;

    // RTTI helpers
        template <typename T>
        bool is() const
        {
            return dynamic_cast<const T *>(this) != nullptr;
        }
        template <typename T>
        T *as()
        {
            return dynamic_cast<T *>(this);
        }
        template <typename T>
        const T *as() const
        {
            return dynamic_cast<const T *>(this);
        }

        virtual std::string to_string() const = 0;
        virtual bool is_subtype_of(std::shared_ptr<const Type> other) const; // Default implementation (strict equality or any)
        virtual bool equals(std::shared_ptr<const Type> other) const = 0; // Pure virtual for strict equality check

    // Helper for comparison (implement in .cpp)
        static bool check_equality(std::shared_ptr<const Type> sub, std::shared_ptr<const Type> super);
        static bool check_subtype(std::shared_ptr<const Type> sub, std::shared_ptr<const Type> super);

    // Hash support for using shared_ptr<Type> in unordered containers if needed
    // virtual size_t hash() const = 0;
    };

// --- Primitive Types ---
    struct AnyType : Type {
        AnyType(const ast::ASTNode &node) : Type(Kind::ANY, node) { }
        std::string to_string() const override { return "any"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::ANY; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::ANY)); }
    };

    struct NilType : Type {
        NilType(const ast::ASTNode &node) : Type(Kind::NIL, node) { }
        std::string to_string() const override { return "nil"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::NIL; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // nil is subtype of any other type (except maybe non-null)
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::NIL)); }
    };

    struct BooleanType : Type {
        BooleanType(const ast::ASTNode &node) : Type(Kind::BOOLEAN, node) { }
        std::string to_string() const override { return "boolean"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::BOOLEAN; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::BOOLEAN)); }
    };

    struct StringType : Type {
        std::optional<std::string> literal_value; // For enum/literal checks
        StringType(const ast::ASTNode &node, std::optional<std::string> lit = std::nullopt) : Type(Kind::STRING, node), literal_value(std::move(lit))
        {
        }
        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
     // size_t hash() const override;
    };

    struct NumberType : Type {
        NumberType(const ast::ASTNode &node) : Type(Kind::NUMBER, node) { }
        std::string to_string() const override { return "number"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::NUMBER; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::NUMBER)); }
    };

    struct IntegerType : Type {
        IntegerType(const ast::ASTNode &node) : Type(Kind::INTEGER, node) { }
        std::string to_string() const override { return "integer"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::INTEGER; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // integer is subtype of number
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::INTEGER)); }
    };

    struct ThreadType : Type {
        ThreadType(const ast::ASTNode &node) : Type(Kind::THREAD, node) { }
        std::string to_string() const override { return "thread"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::THREAD; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::THREAD)); }
    };

    struct UserdataType : Type {
        UserdataType(const ast::ASTNode &node) : Type(Kind::USERDATA, node) { }
        std::string to_string() const override { return "userdata"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::USERDATA; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::USERDATA)); }
    };

    struct InvalidType : Type { // Used to signal errors without stopping entirely
        InvalidType(const ast::ASTNode &node) : Type(Kind::INVALID, node) { }
        InvalidType(size_t l, size_t c, std::string f) : Type(Kind::INVALID, l, c, std::move(f)) { }
        std::string to_string() const override { return "<invalid type>"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::INVALID; }
        bool is_subtype_of(std::shared_ptr<const Type> ) const override { return true; } // Avoid cascading errors
    // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::INVALID)); }
    };

// --- Composite Types ---

    struct ArrayType : Type {
        std::shared_ptr<Type> element_type;
        ArrayType(const ast::ASTNode &node, std::shared_ptr<Type> et) : Type(Kind::ARRAY, node), element_type(std::move(et))
        {
            type_id = next_type_id++;
        }
        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
     // size_t hash() const override;
    };

    struct MapType : Type {
        std::shared_ptr<Type> key_type;
        std::shared_ptr<Type> value_type;
        MapType(const ast::ASTNode &node, std::shared_ptr<Type> kt, std::shared_ptr<Type> vt) :
            Type(Kind::MAP, node), key_type(std::move(kt)), value_type(std::move(vt))
        {
            type_id = next_type_id++;
        }
        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // Covariant values, Contravariant keys
     // size_t hash() const override;
    };

// Represents fixed-size heterogeneous lists OR function return values
    struct TupleType : Type {
        std::vector<std::shared_ptr<Type>> element_types;
        bool is_variadic = false; // If the last type represents varargs (...)
        TupleType(const ast::ASTNode &node, std::vector<std::shared_ptr<Type>> et, bool is_va = false) :
            Type(Kind::TUPLE, node), element_types(std::move(et)), is_variadic(is_va)
        {
            type_id = next_type_id++;
        }
        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
    // Helper to get type at a specific index, handling variadics
        std::shared_ptr<Type> get_type_at(size_t index) const;
    // size_t hash() const override;
    };

// --- Function Type ---
    struct FunctionParameter {
        std::string name; // Optional
        std::shared_ptr<Type> type;
        bool is_optional = false;
        bool is_variadic = false; // Corresponds to '...'
    };

    struct FunctionType : Type {
        std::vector<FunctionParameter> parameters;
        std::shared_ptr<TupleType> return_types; // Use TupleType for multiple/variadic returns

        FunctionType(const ast::ASTNode &node, std::vector<FunctionParameter> params, std::shared_ptr<TupleType> rets) :
            Type(Kind::FUNCTION, node), parameters(std::move(params)), return_types(std::move(rets))
        {
            type_id = next_type_id++;
        }

        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // Covariant returns, Contravariant params

        size_t min_arity() const;
        size_t max_arity() const; // Returns SIZE_MAX if variadic
        bool has_variadic_param() const;
    // size_t hash() const override;
    };

// --- Nominal Types ---

// Base for named types (record, enum, alias)
    struct NamedType : Type {
        std::string name;
        std::string qualified_name; // e.g., mymodule.MyRecord
        bool is_global = false; // Affects qualified name generation / lookup?

        NamedType(Kind k, const ast::ASTNode &node, std::string n) : Type(k, node), name(std::move(n)), qualified_name(name)
        {
             // TODO: Fix qualified name later based on module context
            type_id = next_type_id++; // Named types need unique IDs
        }
        std::string to_string() const override { return qualified_name; }
    };

    struct EnumMember {
        std::string name;
    // Potentially store associated value if enums support them later
    };

    struct EnumType : NamedType {
        std::set<std::string> members; // Store member names for validation

        EnumType(const ast::ASTNode &node, std::string n, std::set<std::string> m) : NamedType(Kind::ENUM, node, std::move(n)), members(std::move(m))
        {
        }

        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // Enum is subtype of string
                                                              // size_t hash() const override { return std::hash<std::string>()(qualified_name); }
    };

    struct RecordField {
        std::string name;
        std::shared_ptr<Type> type;
        size_t line = 0; // Location of field definition
        size_t column = 0;
        // Add visibility, constness etc. if needed
    };

    struct RecordType : NamedType {
        // Use ordered map if field order matters for iteration/subtyping (structural)
        // For nominal, unordered is fine.
        std::unordered_map<std::string, RecordField> fields;
        // Keep track of metamethods separately
        std::unordered_map<std::string, std::shared_ptr<FunctionType>> metamethods;
        bool is_userdata_proxy = false; // For `record Name is userdata`
        // TODO: Add support for parent types / interfaces for structural subtyping if needed

        RecordType(const ast::ASTNode &node, std::string n) : NamedType(Kind::RECORD, node, std::move(n)) { }

        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override; // Nominal typing
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // Nominal typing (unless structural interfaces added)

        // Helpers
        std::optional<std::shared_ptr<Type>> get_field_type(const std::string &field_name) const;
        std::optional<RecordField> get_field(const std::string &field_name) const;
        std::optional<std::shared_ptr<FunctionType>> get_metamethod_type(const std::string &method_name) const;

        // size_t hash() const override { return std::hash<std::string>()(qualified_name); }
    };

    // --- Union Type ---
    // Comparator for shared_ptr<Type> in the set
    struct PtrTypeComparator {
        bool operator()(const std::shared_ptr<Type> &lhs, const std::shared_ptr<Type> &rhs) const;
    };

    struct UnionType : Type {
        std::set<std::shared_ptr<Type>, PtrTypeComparator> options;

        // Constructor now internal, use create_union factory
        UnionType(const ast::ASTNode &node, std::set<std::shared_ptr<Type>, PtrTypeComparator> opts) :
            Type(Kind::UNION, node), options(std::move(opts))
        {
            type_id = next_type_id++;
        }

        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override;
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // T1|T2 <: U if T1 <: U and T2 <: U

        // Factory function to create and simplify unions
        static std::shared_ptr<Type> create_union(const ast::ASTNode &node, const std::vector<std::shared_ptr<Type>> &types);
        static std::shared_ptr<Type> create_union(const ast::ASTNode &node, std::shared_ptr<Type> t1, std::shared_ptr<Type> t2);

        // size_t hash() const override; // Complex hash needed over options
    };

    // --- Special Types ---
    struct EmptyTableType : Type { // For initial {} inference
        EmptyTableType(const ast::ASTNode &node) : Type(Kind::EMPTY_TABLE, node) { }
        std::string to_string() const override { return "{}"; }
        bool equals(std::shared_ptr<const Type> other) const override { return other->kind == Kind::EMPTY_TABLE; }
        bool is_subtype_of(std::shared_ptr<const Type> other) const override;
        // size_t hash() const override { return std::hash<int>()(static_cast<int>(Kind::EMPTY_TABLE)); }
    };

    // --- Type Alias ---
    // Not a distinct runtime type, but holds the definition for resolution
    struct TypeAlias : NamedType {
        std::shared_ptr<Type> aliased_type;
        // Store generic parameters if generics are added

        TypeAlias(const ast::ASTNode &node, std::string n, std::shared_ptr<Type> aliased) :
            NamedType(Kind::ALIAS, node, std::move(n)), aliased_type(std::move(aliased))
        {
        }

        // Delegates to aliased_type for most properties, but equality is nominal
        std::string to_string() const override;
        bool equals(std::shared_ptr<const Type> other) const override; // Nominal equality
        bool is_subtype_of(std::shared_ptr<const Type> other) const override; // Delegate to aliased type after resolving 'other'

        // size_t hash() const override { return std::hash<std::string>()(qualified_name); }
    };

    // --- Type Hash Specialization for shared_ptr ---
    // Required if using std::unordered_set<std::shared_ptr<Type>>
    // struct SharedTypePtrHash {
    //     std::size_t operator()(const std::shared_ptr<Type>& t) const {
    //         return t ? t->hash() : 0;
    //     }
    // };
    // struct SharedTypePtrEqual {
    //      bool operator()(const std::shared_ptr<Type>& lhs, const std::shared_ptr<Type>& rhs) const {
    //          return Type::check_equality(lhs.get(), rhs.get());
    //      }
    // };

    // --- Utility Functions ---
    std::shared_ptr<Type> resolve_alias(std::shared_ptr<const Type> type); // Follows alias chains
    std::shared_ptr<Type> resolve_alias_recursive(std::shared_ptr<const Type> type, std::set<Type *> &visited); // Internal helper

    // Factory/cache for primitive types
    std::shared_ptr<Type> get_primitive_type(const std::string &name, const ast::ASTNode &node);
    std::shared_ptr<NilType> get_nil_type(const ast::ASTNode &node);
    std::shared_ptr<AnyType> get_any_type(const ast::ASTNode &node);
    std::shared_ptr<BooleanType> get_boolean_type(const ast::ASTNode &node);
    std::shared_ptr<NumberType> get_number_type(const ast::ASTNode &node);
    std::shared_ptr<IntegerType> get_integer_type(const ast::ASTNode &node);
    std::shared_ptr<StringType> get_string_type(const ast::ASTNode &node);
    std::shared_ptr<InvalidType> get_invalid_type(const ast::ASTNode &node);

    // Common instances (optional, for performance) - manage lifetime carefully
    // extern std::shared_ptr<AnyType> ANY_TYPE;
    // extern std::shared_ptr<NilType> NIL_TYPE;
    // ...

} // namespace teal::checker