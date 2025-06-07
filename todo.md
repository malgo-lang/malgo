# Roadmap

- [x] Update documentations.
  - [x] tour.md : Malgo language tour.
  - [x] reference.md : Malgo reference.
  - [x] architecture.md : Malgo compiler architecture.
- [ ] Change tuple syntax to use `{}` instead of `()`.
- [ ] Add C-like function call syntax.
  - [ ] `f(x)` is equivalent to `f x`.
  - [ ] `f(x, y, z)` is equivalent to `f x y z`.
  - [ ] `f()` is equivalent to `f {}`.
  - [ ] `{ (x, y) -> ... }` is equivalent to `{ x y -> ... }`.
- [ ] Add `forall` and `exists` quantifiers.
  - [ ] Add Bidirectional type inference.
  - [ ] Add new IR based on System Fω.
- [ ] Add record polymorphism.
  - [ ] Row polymorphism?
  - [ ] Record kinds like SML#?
- [ ] Add module system based on polymorphic records.
  - [ ] F-ing modules?
- [ ] Add copatterns.
  - [ ] Q: How to integrate copatterns with the module system?
- [ ] Add delimited continuations.
- [ ] Add effects.
  - [ ] As a library?

## Change tuple syntax to use `{}` instead of `()`

To implement a flag-based opt-in feature (like `--enable-cstyle-call`) in your Haskell compiler frontend, follow these steps. This approach is idiomatic, keeps existing behavior unchanged, and is easy to test and extend.

---

## 1. Add the Flag to the CLI Parser

Assuming you use [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative):

- Add a new field to your global compiler config (e.g., `CompilerOptions`).
- Update the CLI parser to recognize `--enable-cstyle-call`.

**Example:**

```haskell
-- src/Malgo/Driver.hs

data CompilerOptions = CompilerOptions
  { enableCStyleCall :: Bool
  -- ...existing fields...
  }

parseCompilerOptions :: Parser CompilerOptions
parseCompilerOptions = CompilerOptions
  <$> switch
      ( long "enable-cstyle-call"
     <> help "Enable C-style function call syntax (f(x, y))" )
  -- ...existing options...
```

---

## 2. Thread the Flag Through the Compiler

- Pass `CompilerOptions` (or just the flag) to all relevant modules, especially the parser.

**Example:**

```haskell
-- src/Malgo/Driver.hs

main :: IO ()
main = do
  options <- execParser opts
  runCompiler options

runCompiler :: CompilerOptions -> IO ()
runCompiler options = do
  -- ...existing code...
  parseResult <- Malgo.Parser.parseFile options inputFile
  -- ...existing code...
```

---

## 3. Use the Flag in the Parser

- Pass the flag to the parser.
- Conditionally enable/disable grammar rules.

**Example:**

```haskell
-- src/Malgo/Parser.hs

parseFile :: CompilerOptions -> FilePath -> IO (Either ParseError AST)
parseFile options file = do
  source <- readFile file
  runParser (parser (enableCStyleCall options)) file source

parser :: Bool -> Parsec String () AST
parser enableCStyleCall = do
  -- ...existing grammar...
  if enableCStyleCall
    then cStyleCall <|> normalCall
    else normalCall

cStyleCall :: Parsec String () Expr
cStyleCall = do
  -- ...parse f(x, y)...
  -- If not enabled, this branch is unreachable
```

Or, if you want to provide a helpful error when the syntax is used without the flag:

```haskell
cStyleCall :: Bool -> Parsec String () Expr
cStyleCall enableCStyleCall = do
  if enableCStyleCall
    then -- ...parse f(x, y)...
    else unexpected "C-style calls are not enabled. Use --enable-cstyle-call to enable this syntax."
```

---

## 4. Keep Existing Behavior Unchanged

- The default for `enableCStyleCall` should be `False`.
- Only enable the new syntax when the flag is present.

---

## 5. Add Unit Tests

- Test both with and without the flag.
- Use your test framework (e.g., Hspec).

**Example:**

```haskell
-- test/Malgo/ParserSpec.hs

spec :: Spec
spec = do
  describe "C-style call syntax" $ do
    it "parses f(x, y) when enabled" $ do
      let options = CompilerOptions { enableCStyleCall = True, ... }
      parseFile options "test.mlg" `shouldParse` expectedAST

    it "fails on f(x, y) when not enabled" $ do
      let options = CompilerOptions { enableCStyleCall = False, ... }
      parseFile options "test.mlg" `shouldFailWith` "C-style calls are not enabled"
```

---

## 6. Best Practices

- Use descriptive names for flags and config fields.
- Document the flag in help text and user docs.
- Keep the flag’s default value `False` to avoid breaking changes.
- Thread the flag through only where needed (avoid global mutable state).

---

**Summary:**  
Add a `--enable-cstyle-call` flag to your CLI parser, thread it through your config, and use it in the parser to conditionally allow new syntax. Provide clear error messages and write tests for both modes. This pattern is robust and idiomatic for Haskell compilers.
