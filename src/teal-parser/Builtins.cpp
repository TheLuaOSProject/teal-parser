
#include "Builtins.hpp"
#include "TypeSystem.hpp"

namespace teal::parser::typechecker::builtins
{
    using namespace teal::parser::typechecker::types; // Access predefined types::Any, etc.

    void populateGlobalEnvironment(Environment &env)
    {
        // --- Builtin Types ---
        // These are implicitly available via types:: namespace, but we can define them
        // explicitly in the global type scope if needed for lookup consistency.
        env.defineType("any", Any);
        env.defineType("nil", Nil);
        env.defineType("boolean", Boolean);
        env.defineType("integer", Integer);
        env.defineType("number", Number);
        env.defineType("string", String);
        env.defineType("thread", Thread);

        // --- Builtin Functions (Examples) ---
        env.defineValue("print", makeFunction({{std::nullopt, Any, false/*, true*/}}, {}), false); // Varargs later
        env.defineValue("tostring", makeFunction({Any}, {String}), false);
        env.defineValue("tonumber", makeFunction({Any}, {Number}), false); // Actually returns number | nil
        env.defineValue("type", makeFunction({Any}, {String}), false);
        env.defineValue("assert", makeFunction({Any, String}, {Any}), false); // Returns first arg if truthy, TODO: varargs later

        // --- Table Functions ---
        auto tableType = std::make_shared<RecordType>("table"); // Represent 'table' library as a record-like object

        // table.insert(list, [pos,] value)
         auto tableInsertType = makeFunction(
             {
                {std::nullopt, std::make_shared<ArrayType>(Any), false}, // list: {any} for now
                {std::nullopt, Integer, true}, // pos?: integer
                {std::nullopt, Any, false} // value: any
             },
             {} // Returns nothing
         );
         tableType->fields["insert"] = {tableInsertType};

         // table.concat(list [, sep [, i [, j]]])
         auto tableConcatType = makeFunction(
              {
                 {std::nullopt, std::make_shared<ArrayType>(String), false}, // list: {string} (or number)
                 {std::nullopt, String, true}, // sep?: string
                 {std::nullopt, Integer, true}, // i?: integer
                 {std::nullopt, Integer, true} // j?: integer
              },
              {String}
         );
         tableType->fields["concat"] = {tableConcatType};

         // table.unpack(list [, i [, j]]) -> any...
          auto tableUnpackType = makeFunction(
              {
                 {std::nullopt, std::make_shared<ArrayType>(Any), false}, // list: {any}
                 {std::nullopt, Integer, true}, // i?: integer
                 {std::nullopt, Integer, true} // j?: integer
              },
              {Any/*...*/} // Returns varargs any -> Represent as MultiReturn? Or just Any? Let's use Any for now.
         );
         tableType->fields["unpack"] = {tableUnpackType};

         // Define the 'table' global value
         env.defineValue("table", tableType, false);

         // --- String Functions ---
         // TODO: Add string library functions (string.sub, string.find, etc.)

         // --- Math Functions ---
          // TODO: Add math library functions (math.random, math.floor, etc.)

         // --- IO Functions ---
          // TODO: Add io library functions (io.read, io.write, io.open etc.)
          // Need File userdata type
          // Example: io.open(filename [, mode]) -> file | nil, string
            // local record File is userdata ... end
            // global io: record open: function(string, string?): (File | nil), string end

        // --- Coroutine Functions ---
        // TODO: Add coroutine library functions

        // --- Global Variables ---
        env.defineValue("_VERSION", String, false);

        // Add more builtins as needed...
    }

} // namespace teal::typechecker::builtins
