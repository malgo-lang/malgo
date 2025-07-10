# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System and Commands

Malgo uses `cabal` for package management and `mise` for development task management.

### Essential Commands
- `mise run build` - Build the project (runs `cabal build`)
- `mise run test` - Run test suite with detailed output
- `mise run test --match=<pattern>` - Run specific tests matching pattern
- `cabal exec malgo` - Run the malgo executable directly
- `cabal exec malgo -- eval <file.mlg>` - Evaluate a Malgo program
- `cabal exec malgo -- eval --debug-mode --no-opt <file.mlg>` - Debug mode with optimizations disabled

### Setup Commands
- `mise run setup` - Install GHC, cabal, and dependencies
- `mise run setup-hls` - Build Haskell Language Server
- `mise run format` - Format code with ormolu

## Architecture Overview

Malgo is a statically typed functional programming language with a sophisticated multi-stage compiler pipeline.

### Core Pipeline Flow
```
Source (.mlg) → Parser → Rename → [Infer] → [Refine] → ToFun → ToCore → Flat → Join → Eval
```

**Key architectural points:**
- **Dual Parser System**: Traditional ML syntax (default) vs C-style syntax (with `#new-syntax` pragma)
- **Optional Type Checking**: InferPass and RefinePass can be skipped for faster evaluation
- **Multi-IR Pipeline**: Four intermediate representations for different optimization stages
- **Phase-Indexed AST**: Uses type families for extensibility across compilation phases

### Module Structure
- `src/Malgo/Driver.hs` - Main pipeline orchestrator
- `src/Malgo/Parser.hs` - Traditional syntax parser
- `src/Malgo/NewParser.hs` - C-style syntax parser (activated by `#new-syntax`)
- `src/Malgo/Rename/` - Name resolution and desugaring
- `src/Malgo/Sequent/` - Intermediate representations and transformations
- `src/Malgo/Sequent/Eval.hs` - Final interpreter

### Intermediate Representations
1. **Fun IR** (`Sequent/Fun.hs`) - Functional, close to AST
2. **Core IR** (`Sequent/Core/Full.hs`) - Sequent calculus with explicit control
3. **Flat IR** (`Sequent/Core/Flat.hs`) - Flattened, no nested computations
4. **Join IR** (`Sequent/Core/Join.hs`) - Normalized with explicit join points (final)

### Language Features
- Statically typed with type inference
- First-class functions and pattern matching
- Algebraic data types and polymorphism
- Module system with qualified imports
- Both traditional ML syntax and C-style syntax support

## Development Workflow

### Testing
- Test files are in `test/` directory with golden tests
- Use `mise run test` to run all tests
- Use `mise run test --match="Parser"` to run specific test groups

### File Structure
- `examples/malgo/` - Example programs
- `runtime/malgo/` - Standard library (Builtin.mlg, Prelude.mlg)
- `test/testcases/malgo/` - Test cases for the interpreter

### Common Development Tasks
- Build before testing: `mise run build && mise run test`
- Format code: `mise run format`
- Debug compilation: use `--debug-mode` flag with malgo eval
- Generate dependency graph: `mise run graph`

## Important Notes

- Both parsers produce identical AST structure (`Module (Malgo Parse)`)
- Type checking can be bypassed for faster evaluation during development
- The effectful monad stack uses the `Effectful` library
- IR transformations are structure-preserving until flattening stages
- Uses GHC 9.12.2 with extensive language extensions enabled
- **Do not modify files in `test/testcases/**` - these are human-written test cases**

