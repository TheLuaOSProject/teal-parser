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

#include <memory_resource>

_LIBCPP_BEGIN_NAMESPACE_STD

namespace pmr
{
    constexpr size_t DEFAULT_SIZE = 1024 * 1024;
    // MacOS doesn't have this implemented, so this is a temporary hack
    memory_resource *get_default_resource() noexcept
    {
        thread_local static memory_resource *mem = new monotonic_buffer_resource(DEFAULT_SIZE);
        return mem;
    }
}

_LIBCPP_END_NAMESPACE_STD


