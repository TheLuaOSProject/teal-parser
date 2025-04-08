#include <memory_resource>
#include <functional>
#include <string>
#include <vector>


namespace teal::parser
{
    using Allocator = std::pmr::polymorphic_allocator<std::byte>;
    using String = std::pmr::string;
    template<typename T>
    using Vector = std::pmr::vector<T>;

    struct PointerDeleter {
        template <typename T>
        void operator()(T *p) const {
            if (p) {
                p->~T();

                //the arena will dealloc the actual memory
            }
        }
    };

    template<typename T>
    using Pointer = std::unique_ptr<T, PointerDeleter>;

    template<typename T, typename ...TArgs>
    constexpr inline Pointer<T> allocate(Allocator alloc, TArgs &&...args)
    { return Pointer<T>(alloc.new_object<T>(std::forward<TArgs>(args)...), PointerDeleter{}); }

}
