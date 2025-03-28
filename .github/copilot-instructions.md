# Malgo project coding conventions

## Naming conventions

- Use descriptive names for all identifiers. Do not use single-letter names except:
  - Well-known names like `f`, `g` for function parameters in higher-order functions.
  - If the variable does not have a meaningful name, use the type name replaced with lowercase.

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