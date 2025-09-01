add_rules("mode.debug", "mode.release")

set_languages("gnuxx23")
add_requires("libc++")

target("teal-parser")
    set_kind(is_kind("shared") and "shared" or "static")
    add_files("src/**.cpp")
    add_headerfiles("src/(teal-parser/**.hpp)", { public = true })
    add_includedirs("src/", { public = true })

    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-Wno-c99-designator",
        "-stdlib=libc++",
        "-fexperimental-library"
    )

    add_packages("libc++")
target_end()

target("test")
    set_kind("binary")
    add_files("test.cpp")
    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++",
        "-fexperimental-library"
    )

    add_packages("libc++")

    add_deps("teal-parser")

target_end()

target("unit_tests")
    set_kind("binary")
    add_files("tests/test_main.cpp")
    add_includedirs("tests", {public = false})
    add_cxxflags(
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++",
        "-fexperimental-library"
    )
    add_packages("libc++")
    add_deps("teal-parser")
target_end()
