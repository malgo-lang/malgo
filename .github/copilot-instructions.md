## Development Commands

### Build and Setup

```bash
# Initial setup (installs GHC, cabal, and tools)
mise run setup

# Build the project (includes auto-formatting)
mise run build

# Format code only
mise run format

# Setup Haskell Language Server
mise run setup-hls
```

### Running Tests

```bash
# Run all tests
mise run test

# Run specific test by pattern
mise run test --option match="Parser"

# Run a specific test file
cabal test --test-show-details=direct --test-options=--match --test-options="Malgo.ParserSpec"
```

### Running the Compiler

```bash
# Run the malgo executable
mise run exec

# Evaluate a Malgo program
malgo eval examples/malgo/Hello.mlg

# With debug output
malgo eval --debug-mode examples/malgo/Hello.mlg

# Without optimizations
malgo eval --no-opt examples/malgo/Hello.mlg
```

## Compiler Architecture

The Malgo compiler follows a multi-stage pipeline with intermediate representations (IRs):

### Compilation Pipeline

```
Source (.mlg) → Parse → Rename → ToFun → ToCore → Flat → Join → Eval
```

### Key Compiler Phases

1. **Parser** (`Malgo.Parser`): Converts source text to AST

   - Entry: `Malgo.Parser.Pass.ParserPass`
   - Output: `Module (Malgo Parse)`

2. **Renamer** (`Malgo.Rename`): Resolves names and desugars

   - Entry: `Malgo.Rename.Pass.RenamePass`
   - Output: `Module (Malgo Rename)`
   - Creates module interfaces (.mlgi files)

3. **IR Transformations**:

   - **ToFun** (`Malgo.Sequent.ToFun`): AST → functional IR
   - **ToCore** (`Malgo.Sequent.ToCore`): Fun → sequent calculus Core
   - **Flat** (`Malgo.Sequent.Core.Flat`): Flattens nested computations
   - **Join** (`Malgo.Sequent.Core.Join`): Normalizes control flow

4. **Evaluator** (`Malgo.Sequent.Eval`): Interprets the Join IR

### Pass System

All compiler phases implement the `Pass` typeclass from `Malgo.Pass`:

```haskell
class Pass p where
  type PassInput p
  type PassOutput p
  runPass :: ... => p -> PassInput p -> Eff es (PassOutput p)
```

### Module System

- Modules are tracked in `.malgo-works/` directory
- Interface files (.mlgi) enable separate compilation
- Dependencies are linked during the Join phase

## Testing Infrastructure

Tests use HSpec with golden testing:

- Test files: `test/Malgo/*Spec.hs`
- Test cases: `test/testcases/malgo/`
- Golden outputs stored alongside test cases
- Use `Malgo.TestUtils` for common test utilities

### Test Patterns

```haskell
-- Running a single phase test
runPass ParserPass (filepath, source)

-- Golden test pattern
golden "test description" $ do
  -- test action returning String
```

## Important Files and Directories

### Core Compiler

- `src/Malgo/Driver.hs`: Main compiler driver and pipeline orchestration
- `src/Malgo/Syntax.hs`: AST definition with phase indexing
- `src/Malgo/Pass.hs`: Pass abstraction and error handling
- `src/Malgo/Monad.hs`: Compiler monad stack setup

### Runtime

- `runtime/malgo/Builtin.mlg`: Built-in primitive operations
- `runtime/malgo/Prelude.mlg`: Standard library

### Build Files

- `mise.toml`: Development task definitions
- `package.yaml`: Hpack configuration (generates malgo.cabal)
- `malgo.cabal`: Generated cabal file (do not edit directly)

## Language Features

### Syntax Example

```malgo
module {..} = import "../../runtime/malgo/Builtin.mlg"

data List a = Nil | Cons a (List a)

def map : (a -> b) -> List a -> List b
def map = { _ Nil -> Nil,
            f (Cons x xs) -> Cons (f x) (map f xs) }

def main = {
  putStrLn "Hello, Malgo!"
}
```

### Key Language Constructs

- ML-style syntax with curly braces
- Pattern matching with multiple clauses
- Explicit type annotations (no type inference)
- Module imports/exports
- Foreign function interface for runtime primitives
- Infix operators with precedence declarations

## Development Workflow

1. Make changes to source files
2. Run `mise run build` to format and compile
3. Test changes with `mise run test`
4. For compiler changes, use debug mode to see intermediate representations
5. Golden tests will show diffs for output changes

## Debugging Tips

- Use `--debug-mode` flag to see all compiler phases
- Check `.malgo-works/` for cached module interfaces
- IR dumps are available via `withDump` in Driver.hs
- S-expression output available for ASTs and IRs
