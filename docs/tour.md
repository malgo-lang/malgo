# Malgo Language Tour

Welcome to the Malgo Language Tour! This document is a comprehensive, example-driven onboarding guide for new developers, language enthusiasts, and OSS contributors interested in Malgo. It covers language features, type system, runtime, and the compiler pipeline, with references to the codebase for deeper understanding.

---

## 1. Introduction and Philosophy

Malgo is a statically-typed, functional programming language inspired by Haskell and ML-family languages. It aims to provide a modern, expressive type system, ergonomic syntax, and a modular compiler architecture. Malgo is designed for language research, systems programming, and as a platform for experimenting with advanced type system features.

- **Codebase:** See `src/` for compiler implementation, `runtime/malgo/` for runtime, and `examples/malgo/` for sample programs.
- **Philosophy:** Simplicity, composability, and extensibility.

## 2. Basic Syntax and Constructs

### Hello, World!

```malgo
module {..} = import Builtin
module {..} = import Prelude

def main = {
  putStrLn "Hello, world!"
}
```

### Comments and Literals

- Single-line: `-- comment`
- Multi-line: `{- comment -}`
- Literals: `42`, `3.14`, `'a'`, `"hello"`

### Variables and Functions

```malgo
def add = { x y -> x + y }
def id = { x -> x }
```

## 3. Type System

Malgo features a Hindley-Milner-style type system with type inference, type annotations, algebraic data types, and type synonyms.

- **Type definitions:**
  ```malgo
  data List a = Nil | Cons a (List a)
  type Pair a b = (a, b)
  ```
- **Type annotations:**
  ```malgo
  def add : Int32 -> Int32 -> Int32
  def add = { x y -> x + y }
  ```
- **Type inference:** Most types can be omitted; the compiler infers them.
- **Implementation:** See `src/Malgo/Syntax.hs` (Type, Expr), `src/Malgo/Infer.hs`.

## 4. Pattern Matching and Data Types

Pattern matching is a core feature, supporting algebraic data types and destructuring.

```malgo
data Maybe a = Nothing | Just a

def fromMaybe = { default m ->
  case m {
    Nothing -> default,
    Just x -> x
  }
}
```

- **Pattern syntax:** Supports variables, constructors, tuples, records, lists, and literals.
- **See also:** `src/Malgo/Syntax.hs` (Pat, Clause)

## 5. Records, Tuples, and Lists

### Records

```malgo
type Person = { name: String, age: Int32 }
def personAge = { { name = _, age = age } -> age }
```

### Tuples

```malgo
def swap = { (a, b) -> (b, a) }
```

### Lists

```malgo
def sum = { Nil -> 0, Cons x xs -> x + sum xs }
```

## 6. Modules and Imports

Malgo supports modular programming with explicit imports.

```malgo
module {..} = import Builtin
module {foo, bar} = import SomeModule
```

- **Import all:** `module {..} = import Builtin`
- **Selective import:** `module {foo, bar} = import SomeModule`
- **Implementation:** See `src/Malgo/Syntax.hs` (Module, Import)

## 7. Operator Definitions and Infix Notation

Operators can be defined and given fixity.

```malgo
infixl 6 (+)
def (+) = { x y -> addInt32 x y }
```

- **Custom operators:** Use `infixl`, `infixr`, or `infix` for associativity and precedence.
- **See:** `examples/malgo/Infixing.mlg`

## 8. Foreign Function Interface (FFI)

Malgo can call foreign functions, enabling system-level programming.

```malgo
foreign import print_string : String# -> ()
```

- **See:** `runtime/malgo/Builtin.mlg` for FFI examples.

## 9. Control Flow: If, With, and Sequencing

### If Expressions

```malgo
def abs = { x -> if (x < 0) { -x } { x } }
```

### With Statement

```malgo
def printAndReturn = { str k ->
  printString str;
  k str
}

def main = {
  with x = printAndReturn "foo";
  printString x
}
```

### Sequencing

```malgo
def main = {
  printString "A";
  printString "B"
}
```

## 10. Compiler Pipeline

Malgo's compiler is modular, with distinct passes:

- **Parsing:** `src/Malgo/Parser.hs` (produces AST)
- **Renaming:** `src/Malgo/Rename.hs` (resolves names)
- **Type Inference:** `src/Malgo/Infer.hs` (infers types)
- **Refinement:** `src/Malgo/Refine.hs` (elaborates types)
- **Code Generation:** `src/Malgo/Sequent/`
- **Pass Management:** `src/Malgo/Pass.hs`

Each pass is composable and testable in isolation.

## 11. Runtime Model and Backend

Malgo's runtime is implemented in Malgo itself, with primitives and IO provided by `runtime/malgo/Builtin.mlg` and `runtime/malgo/Prelude.mlg`.

- **Primitive types:** `Int32`, `Int64`, `Float`, `Double`, `Char`, `String`, `Bool`
- **Boxed/unboxed representation:** See `Builtin.mlg`
- **IO and system functions:** `printString`, `getChar`, `exitFailure`, etc.
- **Data structures:** `Maybe`, `List`, etc.

## 12. Advanced Features

### Delimited Continuations (Experimental)

```malgo
-- Conceptual example
def callCC = { f -> ... }
def example = {
  callCC { exit ->
    printString "before";
    exit ();
    printString "after"  -- Not executed
  }
}
```

- **Note:** API may change; see `src/Malgo/Sequent/` for backend details.

## 13. Building, Testing, and Contributing

- **Build:**
  ```zsh
  mise run build
  ```
- **Test:**
  ```zsh
  mise run test
  ```
- **Explore examples:** See `examples/malgo/`
- **Read the code:** Start with `src/Malgo/Syntax.hs` and `src/Malgo/Pass.hs`
- **Contribute:** See [CONTRIBUTING.md] if available, or open issues/PRs.

---

> This tour is a living document. Please contribute improvements or report issues as the language evolves.
