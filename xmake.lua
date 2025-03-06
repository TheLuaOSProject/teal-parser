add_rules("mode.debug", "mode.release")

set_languages("gnuxx23")

target("teal-parser")
    set_kind(is_kind("shared") and "shared" or "static")
    add_files("src/**.cpp")
    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++"
    )
target_end()
