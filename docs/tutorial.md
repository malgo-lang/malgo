# Malgo Tutorial

Welcome to the Malgo tutorial! This guide will help you get started with the Malgo programming language, covering installation, basic syntax, and core language features. For detailed language reference, see [reference.md](./reference.md).

## Table of Contents

1. Introduction
2. Installation
3. Your First Malgo Program
4. Basic Syntax
5. Functions and Pattern Matching
6. Data Types and Type Synonyms
7. Records
8. The `with` Statement
9. Modules and Imports
10. Foreign Function Interface (FFI)
11. Operator Definitions
12. Exercises
13. Next Steps

---

## 1. Introduction

Malgo is a statically-typed functional programming language with features such as type inference, pattern matching, currying, and algebraic data types.

## 2. Installation

See the [reference.md](./reference.md) or [docs/reference/index.adoc](./reference/index.adoc) for up-to-date installation instructions.

## 3. Your First Malgo Program

```malgo
module Hello = {
  import Builtin;
  import Prelude;

  main = {
    putStrLn "Hello, world!"
  }
}
```

Save this as `Hello.mlg` and compile it using the Malgo compiler.

## 4. Basic Syntax

- **Comments:**
  - Single-line: `-- comment`
  - Multi-line: `{- comment -}`
- **Literals:**
  - Numbers: `42`, `3.14`, `1L`, `2.0F`
  - Strings: `"hello"`
  - Chars: `'a'`
- **Variables and Functions:**
  ```malgo
  def add = { x y -> x + y }
  def id = { x -> x }
  ```

## 5. Functions and Pattern Matching

```malgo
def fib = {
  0 -> 1,
  1 -> 1,
  n -> fib (n - 1) + fib (n - 2),
}
```

## 6. Data Types and Type Synonyms

```malgo
data List a = Nil | Cons a (List a)
type Pair a b = (a, b)
```

## 7. Records

```malgo
type Person = { name: String, age: Int32 }
def makePerson = { name age -> { name: name, age: age } }
```

## 8. The `with` Statement

The `with` statement in Malgo is used to sequence computations, often for resource management or to ensure certain actions are performed. It can be used in two forms:

- `with expr;` — evaluates `expr` for its effect.
- `with name = expr;` — evaluates `expr`, binds the result to `name`, and makes it available in subsequent code.

### Example: Using `with` for Resource Management

```malgo
def finally : (a -> r) -> {a} -> r
def finally = { finalizer k ->
  let x = k ();
  finalizer x
}

def printAndReturn = { str k ->
  printString str;
  k str
}

def main = {
  with finally { () -> printString "end" };
  with x = printAndReturn "foo";
  printString x
}
```

In this example:

- `with finally { ... }` ensures that `printString "end"` is called after the main computation.
- `with x = printAndReturn "foo";` binds the result of `printAndReturn` to `x` for use in the next statement.

## 9. Modules and Imports

```malgo
module {..} = import Builtin
module {foo, bar} = import SomeModule
```

## 10. Foreign Function Interface (FFI)

```malgo
foreign import print_string : String# -> ()
```

## 11. Operator Definitions

```malgo
infixl 6 (+)
def (+) = { x y -> addInt32 x y }
```

## 12. Exercises

1. Write a function to compute the factorial of a number.
2. Define a data type for binary trees and write a function to compute their size.
3. Use pattern matching to write a safe head function for lists.

## 13. Next Steps

- Explore more examples in the `examples/malgo/` directory.
- Read the [reference.md](./reference.md) for advanced features and full syntax.
- Join the Malgo community for questions and contributions!

---

> This tutorial is a living document. Please contribute improvements or report issues as the language evolves.
