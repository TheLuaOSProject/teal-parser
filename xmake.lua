add_rules("mode.debug", "mode.release")

set_languages("gnuxx23")

target("teal-parser")
    set_kind(is_kind("shared") and "shared" or "static")
    add_files("src/**.cpp")
    add_headerfiles("src/(teal-parser/**.hpp)", { public = true })
    add_includedirs("src/", { public = true })

    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++",
        "-fexperimental-library"
    )

    add_ldflags("-stdlib=libc++", "-fexperimental-library")
target_end()

target("test")
    set_kind("binary")
    add_files("tests/**.cpp")
    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++",
        "-fexperimental-library"
    )
    add_ldflags("-stdlib=libc++", "-fexperimental-library")

    add_deps("teal-parser")
