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

## Instructions for GitHub Copilot

- If you notice anything while using Copilot Edits, add it to the last of ‘GitHub Copilot comments’ section of this file (`.github/copilot-instructions.md`).
- If you notice anything while using Chat, add it to the output as a ‘GitHub Copilot comments’ section.
- DO NOT EDIT or REMOVE any comments in 'GitHub Copilot comments' section.

## GitHub Copilot comments

DO NOT EDIT or REMOVE this section. Just ADD your comments here.

- Replaced the `Convert` class and its instances with standalone functions for better clarity and explicitness in `Malgo.Sequent.ToFun` module.