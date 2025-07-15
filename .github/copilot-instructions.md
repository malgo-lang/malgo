# Malgo project coding conventions

## Naming conventions

- Use descriptive names for all identifiers. Do not use single-letter names.
  - If the variable does not have a meaningful name, use the type name replaced with lowercase.
  - Codebase sometimes uses single-letter names for variables, but this is not recommended.
    - If you encounter such a name, consider renaming it to something more descriptive.

## Converting data to strings

In order to produce highly readable output, data should be converted to strings using the following three methods as appropriate.

1. `pShow` function of the `Text.Pretty.Simple` module

- Use for output of any type that has a `Show` instance.
- In principle, use only for debugging output or log output.

2. `pretty` function of the `Prettyprinter` module

- Use for output of any type where a `Pretty` instance exists.
- Use when outputting to the user.

3. `sShow` function of the `Malgo.SExpr` module

- Use for output of ASTs and intermediate representations.

## Tools

- Use `mise run build` to build the project.
- Use `mise run test` to run tests.
- Use `mise run exec` to run `malgo` executable.
