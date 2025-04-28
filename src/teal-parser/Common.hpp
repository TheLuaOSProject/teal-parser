// Copyright (C) 2025 Amrit Bhogal
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#pragma once

#include <format>
#include <source_location>
#include <string_view>
#include <variant>
#include <typeinfo>

namespace teal::parser
{
#ifndef NDEBUG
#define $on_debug(...) __VA_ARGS__
#define $on_release(...)
#else
#define $on_debug(...)
#define $on_release(...) __VA_ARGS__
#endif
    // concept Stringable = requires(const std::string &s) {
    //     { s.to_string() } -> std::convertible_to<std::string>;
    // };

    // For std::visit overloads
    template <typename... T>
    struct Overload : T... {
        using T::operator()...;
    };

    template <typename... T>
    Overload(T...) -> Overload<T...>;

    template <typename TVariant>
    static constexpr inline auto typematch(TVariant &&v) -> decltype(auto)
    {
        return [v]<typename... TFn>(TFn &&...fn) constexpr { return std::visit(Overload { std::forward<TFn>(fn)... }, std::forward<TVariant>(v)); };
    }

    constexpr uint64_t bit_str(std::string_view data)
    {
        auto result = std::array<char, 8>();

        for (size_t i = 0; i < std::min(data.size(), result.max_size()); i++) result[i] = data[i];

        return std::bit_cast<uint64_t>(result);
    }

    template <typename... T> // requires Stringable<T>
    struct Error {
        using Kind_t = std::variant<T...>;
        Kind_t kind;
        size_t line, column;
        $on_debug(std::source_location location);

        Error(Kind_t kind, size_t line, size_t column $on_debug(, std::source_location location = std::source_location::current())) :
            kind(kind), line(line), column(column) $on_debug(, location(location))
        {
        }

        constexpr inline std::string to_string() const
        {
            return typematch(kind)([this](const auto &e) {
                if constexpr (requires { e.to_string(); }) {
                    return e.to_string();
                } else {
                    $on_debug(
                        return std::format("[{}:{}] Error at {}:{} - {}", location.file_name(), location.line(), line, column, typeid(e).name());
                    ) return std::format("Error at {}:{} - {}", line, column, typeid(e).name());
                }
            });
        }
    };

    struct UnionAccessException : public std::exception {
        const char *what() const noexcept override { return "Union access"; }
    };

    template <typename T, typename U, typename... Others>
    consteval std::size_t find_index_in_pack(std::size_t idx = 0)
    {
        if constexpr (std::is_same_v<T, U>) {
            return idx;
        } else {
            if constexpr (sizeof...(Others) == 0) {
                return static_cast<std::size_t>(-1);
            } else {
                return find_index_in_pack<T, Others...>(idx + 1);
            }
        }
    }

    template <typename X, typename... Xs>
    inline constexpr bool is_one_of = (std::is_same_v<X, Xs> or ...);

    template <std::size_t A, std::size_t B>
    struct static_max {
        static constexpr std::size_t value = A > B ? A : B;
    };
    template <std::size_t... Ns>
    struct static_max_of;
    template <std::size_t N>
    struct static_max_of<N> {
        static constexpr std::size_t value = N;
    };
    template <std::size_t N1, std::size_t N2, std::size_t... Ns>
    struct static_max_of<N1, N2, Ns...> {
        static constexpr std::size_t value = static_max_of<(N1 > N2 ? N1 : N2), Ns...>::value;
    };

    template <typename T, typename... Ts>
    concept is_in_type_list = (std::is_same_v<std::decay_t<T>, Ts> or ...);

    template <size_t I, typename... Ts>
    concept is_in_range = (I < sizeof...(Ts));

    template <typename... SubTs>
    class UnionSlice;

    template <typename... Ts>
        requires(sizeof...(Ts) > 0)
    class Union {
        static constexpr std::size_t DATA_SIZE = static_max_of<sizeof(Ts)...>::value;
        static constexpr std::size_t DATA_ALIGN = static_max_of<alignof(Ts)...>::value;
        alignas(DATA_ALIGN) std::byte _data[DATA_SIZE];
        std::size_t _active_idx;  // index of currently held type in Ts... (npos if none)

        static constexpr bool ALL_TRIVIAL_DTOR = (std::is_trivially_destructible_v<Ts> and ...);

        // Destroy the current object (called before switching types)
        template <std::size_t I = 0>
        constexpr void destroy_at_index() noexcept
        {
            if constexpr (I < sizeof...(Ts)) {
                if (_active_idx == I) {
                    using AltType = std::tuple_element_t<I, std::tuple<Ts...>>;
                    reinterpret_cast<AltType *>(&_data)->~AltType();
                } else {
                    destroy_at_index<I + 1>();
                }
            }
        }
        constexpr void destroy_current() noexcept
        {
            if constexpr (not ALL_TRIVIAL_DTOR) {
                if (not valueless_by_exception()) { destroy_at_index<0>(); }
            }
        }

    public:
        static constexpr std::size_t INVALID_INDEX = static_cast<std::size_t>(-1);

        using First_t = std::tuple_element_t<0, std::tuple<Ts...>>;
        // Default constructor (only if first alternative is default-constructible)
        constexpr Union() noexcept(std::is_nothrow_default_constructible_v<First_t>)
            requires(std::is_default_constructible_v<First_t>)
        {
            new (&_data) First_t();
            _active_idx = 0;
        }
        constexpr Union()
            requires(not std::is_default_constructible_v<First_t>)
        = delete;

        // Construct from one of the allowed types
        template <typename T>
            requires is_in_type_list<std::decay_t<T>, Ts...>
        constexpr Union(T &&value) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>, T &&>)
        {
            using U = std::decay_t<T>;
            constexpr std::size_t idx = find_index_in_pack<U, Ts...>();
            new (&_data) U(std::forward<T>(value));
            _active_idx = idx;
        }

        Union(const Union &other)
        {
            if (not other.valueless_by_exception()) {
                _active_idx = other._active_idx;
                // Copy-construct the stored value from the other union
                other.visit([this](const auto &val) {
                    using ValT = std::decay_t<decltype(val)>;
                    new (&_data) ValT(val);
                });
            } else {
                _active_idx = INVALID_INDEX;
            }
        }

        Union(Union &&other) noexcept(((std::is_nothrow_move_constructible_v<Ts> or std::is_nothrow_copy_constructible_v<Ts>) and ...))
        {
            if (not other.valueless_by_exception()) {
                _active_idx = other._active_idx;
                // Move-construct the stored value from the other union
                other.visit([this](auto &val) {
                    using ValT = std::decay_t<decltype(val)>;
                    new (&_data) ValT(std::move(val));
                });
            } else {
                _active_idx = INVALID_INDEX;
            }
        }

        ~Union() { destroy_current(); }

        Union &operator=(const Union &other)
        {
            if (this != &other) {
                if (not valueless_by_exception()) { destroy_current(); }
                if (not other.valueless_by_exception()) {
                    // Copy the other's value into this union
                    other.visit([this](const auto &val) {
                        using ValT = std::decay_t<decltype(val)>;
                        new (&_data) ValT(val);
                        _active_idx = find_index_in_pack<ValT, Ts...>();
                    });
                } else {
                    _active_idx = INVALID_INDEX;
                }
            }
            return *this;
        }

        Union &operator=(Union &&other) noexcept(((std::is_nothrow_move_constructible_v<Ts> or std::is_nothrow_copy_constructible_v<Ts>) and ...))
        {
            if (this != &other) {
                if (not valueless_by_exception()) { destroy_current(); }
                if (not other.valueless_by_exception()) {
                    // Move the other's value into this union
                    other.visit([this](auto &val) {
                        using ValT = std::decay_t<decltype(val)>;
                        new (&_data) ValT(std::move(val));
                        _active_idx = find_index_in_pack<ValT, Ts...>();
                    });
                } else {
                    _active_idx = INVALID_INDEX;
                }
            }
            return *this;
        }

        // Assign a value of one of the alternative types
        template <typename T>
            requires is_in_type_list<std::decay_t<T>, Ts...>
        Union &operator=(T &&value)
        {
            using U = std::decay_t<T>;
            if (not valueless_by_exception()) {
                if (holds_alternative<U>()) {
                    // Same type active: assign into it
                    *reinterpret_cast<U *>(&_data) = std::forward<T>(value);
                } else {
                    // Different type: destroy current and construct new
                    destroy_current();
                    new (&_data) U(std::forward<T>(value));
                }
            } else {
                // Union currently valueless: just construct new value
                new (&_data) U(std::forward<T>(value));
            }
            _active_idx = find_index_in_pack<U, Ts...>();
            return *this;
        }

        // In-place construct a new alternative (destructs current value first)
        template <typename T, typename... Args>
            requires is_in_type_list<T, Ts...>
        T &emplace(Args &&...args)
        {
            if (not valueless_by_exception()) { destroy_current(); }
            new (&_data) T(std::forward<Args>(args)...);
            _active_idx = find_index_in_pack<T, Ts...>();
            return *reinterpret_cast<T *>(&_data);
        }

        // **Safe access by type **– returns reference if type matches, otherwise throws
        template <typename T>
            requires is_in_type_list<T, Ts...>
        T &get() &
        {
            if (not holds_alternative<T>()) { throw UnionAccessException(); }
            return *reinterpret_cast<T *>(&_data);
        }
        template <typename T>
            requires is_in_type_list<T, Ts...>
        const T &get() const &
        {
            if (not holds_alternative<T>()) { throw UnionAccessException(); }
            return *reinterpret_cast<const T *>(&_data);
        }
        template <typename T>
            requires is_in_type_list<T, Ts...>
        T &&get() &&
        {
            if (not holds_alternative<T>()) { throw UnionAccessException(); }
            return std::move(*reinterpret_cast<T *>(&_data));
        }

        // **Safe access by index **(template parameter I)
        template <std::size_t I>
            requires is_in_range<I, Ts...>
        std::tuple_element_t<I, std::tuple<Ts...>> &get_index() &
        {
            using ElemT = std::tuple_element_t<I, std::tuple<Ts...>>;
            if (_active_idx != I) { throw UnionAccessException(); }
            return *reinterpret_cast<ElemT *>(&_data);
        }
        template <std::size_t I>
            requires is_in_range<I, Ts...>
        const std::tuple_element_t<I, std::tuple<Ts...>> &get_index() const &
        {
            using ElemT = std::tuple_element_t<I, std::tuple<Ts...>>;
            if (_active_idx != I) { throw UnionAccessException(); }
            return *reinterpret_cast<const ElemT *>(&_data);
        }

        // **Unsafe access by type **– no runtime check (caller must ensure correctness)
        template <typename T>
            requires is_in_type_list<T, Ts...>
        T &get_unsafe() &
        {
            return *reinterpret_cast<T *>(&_data);
        }
        template <typename T>
            requires is_in_type_list<T, Ts...>
        const T &get_unsafe() const &
        {
            return *reinterpret_cast<const T *>(&_data);
        }
        template <typename T>
            requires is_in_type_list<T, Ts...>
        T &&get_unsafe() &&
        {
            return std::move(*reinterpret_cast<T *>(&_data));
        }

        // **Unsafe access by index **
        template <std::size_t I>
            requires is_in_range<I, Ts...>
        std::tuple_element_t<I, std::tuple<Ts...>> &get_index_unsafe() &
        {
            using ElemT = std::tuple_element_t<I, std::tuple<Ts...>>;
            return *reinterpret_cast<ElemT *>(&_data);
        }
        template <std::size_t I>
            requires is_in_range<I, Ts...>
        const std::tuple_element_t<I, std::tuple<Ts...>> &get_index_unsafe() const &
        {
            using ElemT = std::tuple_element_t<I, std::tuple<Ts...>>;
            return *reinterpret_cast<const ElemT *>(&_data);
        }

        // Query the current state
        template <typename T>
            requires is_in_type_list<T, Ts...>
        bool holds_alternative() const noexcept
        {
            return (not valueless_by_exception() and _active_idx == find_index_in_pack<T, Ts...>());
        }
        std::size_t index() const noexcept { return _active_idx; }
        bool valueless_by_exception() const noexcept { return _active_idx == INVALID_INDEX; }

        // Static compile-time index lookup for a type in this Union
        template <typename T>
            requires is_in_type_list<T, Ts...>
        static constexpr std::size_t index_of()
        {
            return find_index_in_pack<T, Ts...>();
        }

        // Apply a visitor functor to the active value
        template <typename Visitor>
        decltype(auto) visit(Visitor &&vis)
        {
            if (valueless_by_exception()) { throw UnionAccessException(); }
            return visit_helper<0>(std::forward<Visitor>(vis));
        }
        template <typename Visitor>
        decltype(auto) visit(Visitor &&vis) const
        {
            if (valueless_by_exception()) { throw UnionAccessException(); }
            return visit_helper_const<0>(std::forward<Visitor>(vis));
        }

        // Create a UnionSlice that references this Union (allowed types SubTs...)
        template <typename... SubTs>
            requires((is_one_of<SubTs, Ts...> and ...))
        UnionSlice<SubTs...> slice() &
        {
            return UnionSlice<SubTs...>(this);
        }

        template <typename TSlice>
            requires requires { TSlice::slice; }
        UnionSlice<TSlice> slice() &
        {
            return UnionSliceOf_t<TSlice>(this);
        }

        // Provide internal data pointers for UnionSlice (advanced usage)
        void *data_pointer() noexcept { return static_cast<void *>(&_data); }
        std::size_t *index_pointer() noexcept { return &_active_idx; }

    private:
        // Recursive helper for visit (non-const)
        template <std::size_t I, typename Visitor>
        decltype(auto) visit_helper(Visitor &&vis)
        {
            if constexpr (I < sizeof...(Ts)) {
                if (_active_idx == I) {
                    using AltType = std::tuple_element_t<I, std::tuple<Ts...>>;
                    return std::forward<Visitor>(vis)(*reinterpret_cast<AltType *>(&_data));
                } else {
                    return visit_helper<I + 1>(std::forward<Visitor>(vis));
                }
            }
            // (Should never happen as active_index is valid)
            throw UnionAccessException();
        }
        // Recursive helper for visit (const)
        template <std::size_t I, typename Visitor>
        decltype(auto) visit_helper_const(Visitor &&vis) const
        {
            if constexpr (I < sizeof...(Ts)) {
                if (_active_idx == I) {
                    using AltType = std::tuple_element_t<I, std::tuple<Ts...>>;
                    return std::forward<Visitor>(vis)(*reinterpret_cast<const AltType *>(&_data));
                } else {
                    return visit_helper_const<I + 1>(std::forward<Visitor>(vis));
                }
            }
            throw UnionAccessException();
        }
    };

    // UnionSlice class template – references a Union's storage for a subset of types
    template <typename... SubTs>
    class UnionSlice {
    private:
        void *_data_ptr;               // pointer to the Union's data buffer
        std::size_t *_index_ptr;       // pointer to the Union's active index
        std::array<std::size_t, sizeof...(SubTs)> _orig_indices; // original indices of each SubT in the Union

        static constexpr std::size_t COUNT = sizeof...(SubTs);
        template <typename T>
        static constexpr std::size_t allowed_index_of()
        {
            return find_index_in_pack<T, SubTs...>();
        }
        template <std::size_t I>
        using allowed_type = std::tuple_element_t<I, std::tuple<SubTs...>>;

        // Destroy the current object in the Union if it is one of the allowed types
        template <std::size_t I = 0>
        void destroy_current_allowed()
        {
            if constexpr (I < COUNT) {
                if (*_index_ptr == _orig_indices[I]) {
                    using CurT = allowed_type<I>;
                    reinterpret_cast<CurT *>(_data_ptr)->~CurT();
                } else {
                    destroy_current_allowed<I + 1>();
                }
            }
        }

        std::size_t (*_ti_lookup)(const std::type_info &) noexcept;

    public:
        template <typename... AllTs>
        static std::size_t type_index_lookup(const std::type_info &ti) noexcept
        {
            std::size_t i = 0;
            // fold-expression over the pack: first match wins
            ((ti == typeid(AllTs) ? 0 : ++i), ...);
            return i;                    // if no match you'll get sizeof...(AllTs)
        }
        // Construct a UnionSlice from a Union<TAll...> &(SubTs... must be subset of TAll...)
        template <typename... AllTs>
            requires((is_one_of<SubTs, AllTs...> and ...))
        explicit UnionSlice(Union<AllTs...> *u) :
            _data_ptr(u->data_pointer()),
            _index_ptr(u->index_pointer()), 
            _orig_indices { Union<AllTs...>::template index_of<SubTs>()... },
            _ti_lookup(&type_index_lookup<AllTs...>)
        {
            
        }

        template <typename... OtherSubTs>
            requires((is_one_of<SubTs, OtherSubTs...> || ...))   // same Union, any subset/superset
        UnionSlice(const UnionSlice<OtherSubTs...> &other) : _data_ptr(other.data_pointer()), _index_ptr(other.index_pointer()), _ti_lookup(other.type_info_lookup_pointer())
        {
        // fill every entry we care about by querying the underlying Union
            ((_orig_indices[allowed_index_of<SubTs>()] = _ti_lookup(typeid(SubTs))), ...);
        }

        constexpr inline void *data_pointer() const noexcept { return _data_ptr; }
        constexpr inline std::size_t *index_pointer() const noexcept { return _index_ptr; }
        constexpr inline std::size_t *original_indices_pointer() const noexcept { return _orig_indices.data(); }
        constexpr inline auto type_info_lookup_pointer() const noexcept { return _ti_lookup; }
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        constexpr inline std::size_t original_index_of() const noexcept
        {
            return _orig_indices[allowed_index_of<T>()];
        }

        // **Safe access by type **(subset types only)
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        T &get()
        {
            std::size_t orig_idx = _orig_indices[allowed_index_of<T>()];
            if (*_index_ptr != orig_idx) { throw UnionAccessException(); }
            return *reinterpret_cast<T *>(_data_ptr);
        }
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        const T &get() const
        {
            std::size_t orig_idx = _orig_indices[allowed_index_of<T>()];
            if (*_index_ptr != orig_idx) { throw UnionAccessException(); }
            return *reinterpret_cast<const T *>(_data_ptr);
        }

        // **Safe access by index **(allowed type index 0..count-1)
        template <std::size_t I>
            requires is_in_range<I, SubTs...>
        allowed_type<I> &get_index()
        {
            if (*_index_ptr != _orig_indices[I]) { throw UnionAccessException(); }
            return *reinterpret_cast<allowed_type<I> *>(_data_ptr);
        }
        template <std::size_t I>
            requires is_in_range<I, SubTs...>
        const allowed_type<I> &get_index() const
        {
            if (*_index_ptr != _orig_indices[I]) { throw UnionAccessException(); }
            return *reinterpret_cast<const allowed_type<I> *>(_data_ptr);
        }

        // **Unsafe access by type **(allowed types only)
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        T &get_unsafe()
        {
            return *reinterpret_cast<T *>(_data_ptr);
        }
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        const T &get_unsafe() const
        {
            return *reinterpret_cast<const T *>(_data_ptr);
        }

        // **Unsafe access by index **
        template <std::size_t I>
            requires is_in_range<I, SubTs...>
        allowed_type<I> &get_index_unsafe()
        {
            return *reinterpret_cast<allowed_type<I> *>(_data_ptr);
        }
        template <std::size_t I>
            requires is_in_range<I, SubTs...>
        const allowed_type<I> &get_index_unsafe() const
        {
            return *reinterpret_cast<const allowed_type<I> *>(_data_ptr);
        }

        // Assign a new value of an allowed type to the underlying Union
        template <typename T>
            requires is_in_type_list<std::decay_t<T>, SubTs...>
        UnionSlice &operator=(T &&value)
        {
            using U = std::decay_t<T>;
            std::size_t new_orig_idx = _orig_indices[allowed_index_of<U>()];
            if (not valid()) {
                // Cannot assign through slice if current Union type is outside allowed set
                throw UnionAccessException();
            }
            if (*_index_ptr == new_orig_idx) {
                // Same allowed type currently active: assign directly
                *reinterpret_cast<U *>(_data_ptr) = std::forward<T>(value);
            } else {
                // Different type: destroy current allowed object, then construct new one
                destroy_current_allowed<0>();
                new (_data_ptr) U(std::forward<T>(value));
                *_index_ptr = new_orig_idx;
            }
            return *this;
        }

        // Check current type state
        template <typename T>
            requires is_in_type_list<T, SubTs...>
        bool holds_alternative() const noexcept
        {
            return *_index_ptr == _orig_indices[allowed_index_of<T>()];
        }
        bool valid() const noexcept
        {
            // True if underlying Union's active type is one of SubTs...
            for (std::size_t i = 0; i < COUNT; ++i) {
                if (*_index_ptr == _orig_indices[i]) return true;
            }
            return false;
        }

        // Visit the allowed types (calls visitor if the underlying type is in SubTs, else throws)
        template <typename Visitor>
        decltype(auto) visit(Visitor &&vis)
        {
            if (not valid()) { throw UnionAccessException(); }
            return visit_allowed<0>(std::forward<Visitor>(vis));
        }
        template <std::size_t I, typename Visitor>
        decltype(auto) visit_allowed(Visitor &&vis)
        {
            if constexpr (I < COUNT) {
                if (*_index_ptr == _orig_indices[I]) {
                    using CurT = allowed_type<I>;
                    return std::forward<Visitor>(vis)(*reinterpret_cast<CurT *>(_data_ptr));
                } else {
                    return visit_allowed<I + 1>(std::forward<Visitor>(vis));
                }
            }
            throw UnionAccessException();
        }

        constexpr inline auto operator->() const noexcept
        {
            static_assert(COUNT == 1, "UnionSlice::operator->() only valid for single-type slices");
            return reinterpret_cast<allowed_type<0> *>(_data_ptr);
        }
    };

    template <typename... Ts>
    struct type_list { };

    template <typename T, typename List>
    struct contains;

    template <typename T, typename... Ts>
    struct contains<T, type_list<Ts...>> : std::bool_constant<(std::is_same_v<T, Ts> || ...)> { };

    template <typename List, typename... NewTs>
    struct append_unique;

    template <typename... Ts>
    struct append_unique<type_list<Ts...>>          // 0 new types -> done
    {
        using type = type_list<Ts...>;
    };

    template <typename... Ts, typename U, typename... Us>
    struct append_unique<type_list<Ts...>, U, Us...> {
        using maybe_added = std::conditional_t<
            contains<U, type_list<Ts...>>::value,
            type_list<Ts...>,           // already present
            type_list<Ts..., U>>;      // append U

        using type = typename append_unique<maybe_added, Us...>::type;
    };

    template <typename Lhs, typename Rhs>
    struct list_union;

    template <typename... LhsTs, typename... RhsTs>
    struct list_union<type_list<LhsTs...>, type_list<RhsTs...>> {
        using type = typename append_unique<type_list<LhsTs...>, RhsTs...>::type;
    };

    template <typename List>
    struct list_to_union;

    template <typename... Ts>
    struct list_to_union<type_list<Ts...>> {
        using type = Union<Ts...>;
    };

    template <typename List>
    struct list_to_union_slice;

    template <typename... Ts>
    struct list_to_union_slice<type_list<Ts...>> {
        using type = UnionSlice<Ts...>;
    };

    template <typename U>
    struct extract_types;

    template <typename... Ts>
    struct extract_types<Union<Ts...>> {
        using type = type_list<Ts...>;
    };

    template <typename... Ts>
    struct extract_types<UnionSlice<Ts...>> {
        using type = type_list<Ts...>;
    };

    template <typename... Us>           // Us may each be Union<…> or UnionSlice<…>
    struct UnionOf;

    template <typename First>
    struct UnionOf<First> {              // base-case – just strip to typelist -> Union
        using type = typename list_to_union<typename extract_types<First>::type>::type;
    };

    template <typename First, typename Second, typename... Rest>
    struct UnionOf<First, Second, Rest...> {
        using lhs = typename extract_types<First>::type;
        using rhs = typename extract_types<Second>::type;
        using merged = typename list_union<lhs, rhs>::type;
        using type = typename UnionOf<typename list_to_union<merged>::type, Rest...>::type;
    };

    template <typename... Us>
    using UnionOf_t = typename UnionOf<Us...>::type;

    template <typename... Us>
    struct UnionSliceOf;
    template <typename First>
    struct UnionSliceOf<First> {
        using type = typename list_to_union_slice<typename extract_types<First>::type>::type;
    };
    template <typename First, typename Second, typename... Rest>
    struct UnionSliceOf<First, Second, Rest...> {
        using lhs = typename extract_types<First>::type;
        using rhs = typename extract_types<Second>::type;
        using merged = typename list_union<lhs, rhs>::type;
        using type = typename UnionSliceOf<typename list_to_union_slice<merged>::type, Rest...>::type;
    };
    template <typename... Us>
    using UnionSliceOf_t = typename UnionSliceOf<Us...>::type;

    template <typename T>
    class IndexPointer {
    private:
        size_t _idx;

    public:
        static constexpr inline auto INVALID_INDEX = std::numeric_limits<std::size_t>::max();
        using Target_t = T;

        constexpr IndexPointer(nullptr_t) : _idx(INVALID_INDEX) { }

        constexpr IndexPointer(size_t idx) : _idx(idx) { }

        constexpr inline size_t index() const { return _idx; }

        template <template <typename> class TContainer>
        constexpr inline bool has_value(const TContainer<T> &container) const
        {
            return _idx < container.size();
        }

        template <template <typename> class TContainer>
        constexpr inline T &get(const TContainer<T> &container)
        {
            return container.get(_idx);
        }

        template <template <typename> class TContainer>
        constexpr inline const T &get(const TContainer<T> &container) const
        {
            return container.get(_idx);
        }

        template <template <typename> class TContainer>
        constexpr inline T &get_unchecked(const TContainer<T> &container)
        {
            return container[_idx];
        }

        template <template <typename> class TContainer>
        constexpr inline const T &get_unchecked(const TContainer<T> &container) const
        {
            return container[_idx];
        }
    };

    template <typename T, template <typename> class TContainer>
    class ContainerPointer {
    private:
        TContainer<T> *_data;
        size_t _idx;

    public:
        static constexpr inline auto INVALID_INDEX = std::numeric_limits<std::size_t>::max();
        using Target_t = T;
        using Container_t = TContainer<T>;

        constexpr ContainerPointer(nullptr_t) : _data(nullptr), _idx(INVALID_INDEX) { }

        constexpr ContainerPointer(Container_t *u, size_t idx) : _data(u), _idx(idx)
        {
            $on_debug(
                if (not _data) throw std::runtime_error("Container is null"); if (u->size() <= idx) throw std::out_of_range("Index out of range");
            )
        }

        constexpr inline Container_t *data() const { return _data; }

        constexpr inline bool has_value() const { return _data != nullptr; }

        constexpr inline operator bool() const { return has_value(); }

        constexpr inline T &operator*() { return (*_data)[_idx]; }
        constexpr inline const T &operator*() const { return (*_data)[_idx]; }

        constexpr inline T *operator->() { return &(*_data)[_idx]; }
        constexpr inline const T *operator->() const { return &(*_data)[_idx]; }

        constexpr inline T *operator&() { return &(*_data)[_idx]; }
        constexpr inline const T *operator&() const { return &(*_data)[_idx]; }

        constexpr inline T &get() { return _data->get(_idx); }
        constexpr inline const T &get() const { return _data->get(_idx); }
    };

    // static void f()
    // {
    //     auto u = Union<int, double, const char *, std::string>(32);
    //     auto sl = u.slice<int, double>();

    //     UnionSlice<int, double, const char *> sl2 = sl;
    // }
}

template <typename TExpected, typename... T>
    requires std::disjunction_v<std::is_same<T, TExpected>...>
static constexpr bool operator->*(const std::variant<T...> &variant, TExpected *(&expected))
{
    try {
        *expected = std::get<TExpected>(variant);
        return true;
    } catch (std::bad_variant_access) {
        expected = nullptr;
        return false;
    }
}
