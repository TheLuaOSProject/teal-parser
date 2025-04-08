# Ghost in the Macro: Dual-Parser Bicameral Macro System for Teal

## Personal Information

- **Name:** Amrit Bhogal
- **Email:** [ambhogal01@gmail.com](mailto\:ambhogal01@gmail.com)
- **GitHub:** [https://github.com/frityet](https://github.com/frityet)
- **University:** Undergraduate student at the University of Birmingham
- **Possible Time Commitments:** University exams during May; fully dedicated to the project from June onwards.

I have extensive programming experience in Teal, Lua, C, C++, Objective-C, C#, Java, and F#, with Visual Studio Code as my primary development tool. My passion for Lua is profound, I love Lua for its simplicity, flexibility, and elegance. Over the years, I've developed several significant Lua projects, including my ongoing ambitious project, LuaOS, which demonstrates my deep commitment to the Lua ecosystem, and with relation to teal, a standalone compiler for teal which I plan to rewrite my kernel in. Additionally, my enthusiasm extends into the open-source community, where I'm an active contributor and maintainer, notably with the xmake build system, significantly enhancing its Lua module rules, and with various other projects. Professionally, I’ve gained valuable experience from internships at Coast Capital Savings, working effectively in collaborative team environments and contributing to high-quality software solutions.

## Synopsis

This project aims to create a robust, high-performance parser combined with an easy-to-use macro system for the Teal programming language. Leveraging a bicameral macro model, the project clearly distinguishes the parsing and macro expansion phases whilst still keeping the syntax streamlined. This enables flexible, powerful compile-time code transformations while maintaining clarity and efficiency in compiler design and performance.

## Benefits to the Community

The introduction of a high-performance parser and a flexible macro system will significantly enhance Teal's capabilities. It empowers developers to implement complex code transformations at transpile-time easily, dramatically improving the expressiveness, maintainability, and scalability of Teal codebases. Moreover, it can address longstanding community requests for advanced code generation and automation features (issues such as [class support in teal](https://github.com/teal-language/tl/issues/861)).

<!-- ## Detailed Deliverables

- Optimised and performant parser for Teal, capable of handling large codebases efficiently.
- Bicameral macro system.
- Full integration of the macro system with both existing Lua-based and newly developed C++ parsers.
- Extensive documentation, clearly detailing architecture, API usage, and examples.
- Rigorous testing suite covering unit tests, integration tests, and macro-specific test cases.
- Submission of fully functional and well-tested parser and macro implementations for community review and integration into the official Teal repository. -->

## Example Usage of macros

```lua
---#pragma macro statement
local macroexp unless(cond: tl.ASTNode, body: tl.ASTNode): tl.ASTNode
    return {
        kind = "if",
        condition = {
            kind = "not",
            expression = cond,
        },
        thenBranch = {
            kind = "block",
            statements = body,
        },
    }
end

local x = 4

unless x > 3 do
    print("x is not greater than 3")
end
```

## Comprehensive Project Timeline

### Community Bonding (May)

- Thoroughly finalise macro system specifications, clearly defining functionality, API structures, and usage guidelines.
- Confirm adherence to the bicameral (syntax-semantics separation) model through extensive research and design documentation.
- Actively engage with Teal maintainers and community members for constructive feedback and ensure alignment with the project's broader goals.

### Weeks 1–2 (June)

- Implement the reader phase, transforming parsed Teal source code AST into a highly flexible intermediate representation tailored for macro expansions.
- Design preliminary tests that validate intermediate representation correctness and ensure support for comprehensive macro expansion scenarios.

### Weeks 3–4 (June)

- Begin development and integration of a macro expansion engine within the pipeline, emphasising seamless interaction between macro definitions and intermediate representation.
- Expand and document extensive test suites, specifically targeting macro expansion correctness, edge cases, and interactions with existing Teal features.

### Week 5 (Midterm, Early July)

- Fully operational macro expansion pipeline, clearly demonstrable with several representative examples.
- Conduct structured midterm evaluations, documenting detailed feedback and necessary project adjustments.

### Weeks 6–7 (July)

- Adapt the fully functional macro system to integrate seamlessly with the new high-performance C++ parser.
- Ensure intermediate representation compatibility, detailed interoperability documentation, and cross-parser validation tests.

### Weeks 8–9 (July)

- Implement parser lowering passes, effectively translating macro-expanded intermediate representation back into a finalised AST.
- Confirm full compatibility and interoperability between Lua-based and C++ parsers and integrate both with the Teal transpiler toolchain.

### Week 10 (August)

- Conduct thorough final testing and performance benchmarking, addressing any remaining performance bottlenecks or bugs identified.
- Perform extensive optimisation, thorough code cleanup, and detailed documentation to facilitate future maintenance and enhancements.
- Prepare comprehensive and professional pull requests for submission and collaborative review by Teal maintainers, ensuring readiness for integration into the main project repository.

## Detailed Technical Approach

- **Bicameral Syntax Model:** Clearly separated parsing into reader (generic intermediate representation) and macro expansion phases.
- **Macro Expansion Pipeline:** Intermediate representation designed for maximum compatibility, flexibility, and ease of transformation.
- **Parser Compatibility:** Dual parser implementation strategy covering Lua-based and C++ parser implementations.
- **Testing and Optimisation:** Rigorous, continuous testing and iterative optimisations with systematic community feedback.

## Commitment

Fully committed to the project with availability from June onwards. Limited availability in May due to university examinations, factored into project planning.

## Midterm and Final Deliverables

### By Midterm:

- The macro system’s reader phase will be fully implemented and validated.

- Intermediate representation will be functional and tested.

- Macro expansion engine will be integrated into the Lua-based parser.

- Basic macro examples will compile and expand correctly, with test coverage in place.

- Parser optimisations for performance enhancement will be largely complete.

### By Final Submission:

- Macro system will be fully integrated into both the Lua-based and new C++ parsers.

- Complete lowering pass will translate macro-expanded intermediate representation into valid Teal AST.

- Comprehensive testing suite will cover all supported macro scenarios and edge cases.

- The macro system will be compatible with the Teal transpiler and fully documented.

- Pull requests containing the full system will be submitted for integration into the upstream Teal repository.
