
#pragma once

#include "Environment.hpp"

namespace teal::parser::typechecker::builtins
{
    // Function to populate the global environment with builtin types and functions
    void populateGlobalEnvironment(Environment &env);

} // namespace teal::typechecker::builtins
