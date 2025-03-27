## Malgo project coding conventions

### Converting data to strings

In order to produce highly readable output, data should be converted to strings using the following three methods as appropriate.

1. `pShow` function of the `Text.Pretty.Simple` module
  - Use for output of any type that has a `Show` instance.
  - In principle, use only for debugging output or log output.
2. `pretty` function of the `Prettyprinter` module
  - Use for output of any type where a `Pretty` instance exists.
  - Use when outputting to the user.
3. `sShow` function of the `Malgo.SExpr` module
  - Use for output of ASTs and intermediate representations.