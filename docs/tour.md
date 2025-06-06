# Malgo Language Tour

Welcome to the Malgo Language Tour! This document provides a concise, example-driven overview of Malgo's most important features and idioms. For a step-by-step guide, see [tutorial.md](./tutorial.md). For full details, see [reference.md](./reference.md).

## Table of Contents

1. Hello, World!
2. Basic Syntax
3. Functions and Pattern Matching
4. Data Types and Type Synonyms
5. Records
6. The `with` Statement
7. Modules and Imports
8. Foreign Function Interface (FFI)
9. Operator Definitions
10. Delimited Continuations
11. More Examples

---

## 1. Hello, World!

```malgo
module {..} = import Builtin
module {..} = import Prelude

def main = {
  putStrLn "Hello, world!"
}
```

## 2. Basic Syntax

- Comments: `-- comment` or `{- comment -}`
- Literals: `42`, `3.14`, `"hello"`, `'a'`
- Variables and Functions:
  ```malgo
  def add = { x y -> x + y }
  def id = { x -> x }
  ```

## 3. Functions and Pattern Matching

```malgo
def fib = {
  0 -> 1,
  1 -> 1,
  n -> fib (n - 1) + fib (n - 2),
}
```

## 4. Data Types and Type Synonyms

```malgo
data List a = Nil | Cons a (List a)
type Pair a b = (a, b)
```

## 5. Records

```malgo
type Person = { name: String, age: Int32 }
def makePerson = { name age -> { name: name, age: age } }
```

## 6. The `with` Statement

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

## 7. Modules and Imports

```malgo
module {..} = import Builtin
module {foo, bar} = import SomeModule
```

## 8. Foreign Function Interface (FFI)

```malgo
foreign import print_string : String# -> ()
```

## 9. Operator Definitions

```malgo
infixl 6 (+)
def (+) = { x y -> addInt32 x y }
```

## 10. Delimited Continuations

```malgo
-- Conceptual example; actual API may differ
def callCC = { f -> ... }
def example = {
  callCC { exit ->
    printString "before";
    exit ();
    printString "after";  -- Not executed
  }
}
```

## 11. More Examples

- See the `examples/malgo/` directory for more sample programs.
- For advanced features and full syntax, see [reference.md](./reference.md).

---

> This tour is a living document. Please contribute improvements or report issues as the language evolves.
