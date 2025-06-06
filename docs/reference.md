# Malgo Language Reference

This document describes the syntax and semantics of the Malgo programming language, based on the current parser implementation and test suite as of June 2025.

## Lexical Structure

### Comments

- Single-line: `--` to end of line
- Multi-line: `{- ... -}` (nestable)

### Identifiers

- Identifiers: `[a-zA-Z_][a-zA-Z0-9_#']*`
- Operators: One or more of `+-*/\%=><:;|&!#.`
- Uppercase identifiers: Types, constructors, modules (e.g., `Int32`, `List`)
- Lowercase identifiers: Variables, functions (e.g., `main`, `putStrLn`)

### Reserved Words

`class`, `def`, `data`, `exists`, `forall`, `foreign`, `impl`, `import`, `infix`, `infixl`, `infixr`, `let`, `type`, `module`, `with`

### Literals

- Integer: `42`, `42L`, `42#`, `42L#`
- Float/Double: `3.14`, `3.14F`, `3.14#`, `3.14F#`
- Char: `'a'`, `'a'#`
- String: `"hello"`, `"hello"#`
- Boxed literals: No trailing `#` (e.g., `42`)
- Unboxed literals: Trailing `#` (e.g., `42#`)

## Modules and Imports

### Module Definition

```
module = many decl
```

### Import Declaration

```
module {..} = import "path/to/module.mlg"
module {foo, bar} = import SomeModule
module Foo = import SomeModule
```

- `{..}`: Import all
- `{foo, bar}`: Import selected
- `Foo`: Import as alias

## Declarations

### Data Type Declaration

```
data List a = Nil | Cons a (List a)
```

### Type Synonym

```
type MyTuple a = (a, a)
type MyString = String
```

### Infix Declaration

```
infixl 6 (+)
def (+) = { x y -> addInt32 x y }
```

### Foreign Declaration

```
foreign import malgo_print_string : String# -> ()
```

### Value Signature and Definition

```
def f : Int32 -> Int32
def f = { x -> x }
```

## Expressions

### Literals, Variables, Application

```
42
x
f x y
```

### Function (Lambda) and Pattern Matching

```
def even = {
  0 -> True,
  n -> odd (subInt32 n 1),
}
```

### Let Binding

```
let x = 1;
let y = 2;
addInt32 x y
```

### With Statement

```
with x = expr;
with expr;
```

### Records

```
{ foo = 1, bar = 2 }
```

### Lists

```
[1, 2, 3]
```

### Tuples

```
(x, y)
```

## Patterns

- Variable: `x`
- Literal: `42`, `'a'`, etc.
- Constructor: `Cons x xs`
- Tuple: `(x, y)`
- Record: `{ foo = x, bar = y }`
- List: `[x, y, z]`

## Types

- Function: `Int32 -> Int32`
- Type application: `Maybe Int32`
- Tuple: `(Int32, String)`
- Record: `{ foo: Int32, bar: String }`
- Block: `{ Int32 }`
- Type variable: `a`

## Operator Precedence and Associativity

- Declared with `infix`, `infixl`, `infixr`
- Example: `infixr 2 (<|>)`

## Example: Factorial

```
def fact = { n -> factAcc n 1L }
def factAcc = { n acc -> if (n == 0L) { acc } { factAcc (n - 1L) (n * acc) } }
def main = { fact 5L |> toStringInt64 |> putStrLn }
```

## Example: Data and Foreign

```
data Int = Int# Int64#
foreign import malgo_int64_t_to_string : Int64# -> String#
def main = { {(Int# x) -> malgo_print_string (malgo_int64_t_to_string x) } (Int# 1L#) }
```

## Example: Record

```
def r = { foo = 1, bar = 2 }
```

## Example: With

```
with x = printAndReturn "foo";
printString x
```

## Example: Pattern Matching

```
def even = {
  0 -> True,
  n -> odd (subInt32 n 1),
}
```

## Example: Type Synonym

```
type MyTuple a = (a, a)
def hello : MyTuple String
def hello = ("hello", "world")
```

---

This document is generated from the parser and test suite. For the most up-to-date syntax, see the implementation in `src/Malgo/Parser.hs` and the testcases in `test/testcases/malgo/`.
