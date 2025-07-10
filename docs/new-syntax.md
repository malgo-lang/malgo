# New Syntax for Malgo

The new syntax is an optional syntax extension that changes how function application and tuples are written in Malgo. When enabled, it makes the syntax more familiar to programmers coming from C-like languages.

## Enabling New Syntax

Add the following pragma at the top of your Malgo file:

```malgo
#new-syntax
```

## Syntax Changes

### Function Application

**Current Syntax:**

```malgo
f x y z       -- Multiple arguments
f x           -- Single argument
f             -- No arguments (just the function value)
```

**New Syntax:**

```malgo
f(x, y, z)    -- Multiple arguments with commas
f(x)          -- Single argument in parentheses
f()           -- Explicit no-argument call
f             -- Still allowed for function values
```

### Tuple Syntax

**Current Syntax:**

```malgo
()            -- Unit/empty tuple
(x)           -- Parenthesized expression (NOT a tuple)
(x, y)        -- 2-tuple
(x, y, z)     -- 3-tuple
(Int, Int)    -- Tuple type
```

**New Syntax:**

```malgo
{}            -- Unit/empty tuple
(x)           -- Parenthesized expression (same as regular)
{x, y}        -- 2-tuple with braces
{x, y, z}     -- 3-tuple with braces
{Int, Int}    -- Tuple type with braces
```

Note: Single-element tuples are not supported in either style. `{x}` is a syntax error.

### Pattern Matching in Functions

**Current Syntax:**

```malgo
def map = {
  _ Nil -> Nil,
  f (Cons x xs) -> Cons (f x) (map f xs)
}
```

**New Syntax:**

```malgo
def map = {
  (_, Nil) -> Nil,
  (f, Cons(x, xs)) -> Cons(f(x), map(f, xs))
}

-- Or without parentheses (both are allowed):
def map = {
  _, Nil -> Nil,
  f, Cons(x, xs) -> Cons(f(x), map(f, xs))
}
```

### Tuple Patterns

**Current Syntax:**

```malgo
def fst = { (x, y) -> x }
def isEmpty = { () -> True, _ -> False }
```

**New Syntax:**

```malgo
def fst = { {x, y} -> x }
def isEmpty = { {} -> True, _ -> False }
```

### Data Constructors

**Current Syntax:**

```malgo
data List a = Nil | Cons a (List a)
```

**New Syntax:**

```malgo
data List(a) = Nil | Cons(a, List(a))
```

## What Doesn't Change

The following syntax remains the same regardless of the C-style apply feature:

3. **Record syntax** - Always uses braces with `field = value`:

   ```malgo
   { x = 1, y = 2 }
   ```

4. **List syntax** - Always uses square brackets:
   ```malgo
   [1, 2, 3]
   ```

## Complete Examples

### Example 1: List Operations

**Current Syntax:**

```malgo
module {..} = import "List.mlg"

def length : List a -> Int
def length = {
  Nil -> 0,
  Cons _ xs -> 1 + length xs
}

def zip : List a -> List b -> List (a, b)
def zip = {
  Nil _ -> Nil,
  _ Nil -> Nil,
  (Cons x xs) (Cons y ys) -> Cons (x, y) (zip xs ys)
}
```

**New Syntax:**

```malgo
#new-syntax
module {..} = import "List.mlg"

def length : List(a) -> Int
def length = {
  Nil -> 0,
  Cons(_, xs) -> 1 + length(xs)
}

def zip : List(a) -> List(b) -> List({a, b})
def zip = {
  (Nil, _) -> Nil,
  (_, Nil) -> Nil,
  (Cons(x, xs), Cons(y, ys)) -> Cons({x, y}, zip(xs, ys))
}
```

### Example 2: Higher-Order Functions

**Current Syntax:**

```malgo
def compose : (b -> c) -> (a -> b) -> a -> c
def compose = { f g x -> f (g x) }

def flip : (a -> b -> c) -> b -> a -> c
def flip = { f x y -> f y x }

def main = {
  let inc = { x -> x + 1 };
  let double = { x -> x * 2 };
  compose inc double 5  -- Returns 11
}
```

**New Syntax:**

```malgo
#new-syntax
def compose : (b -> c) -> (a -> b) -> a -> c
def compose = { (f, g, x) -> f(g(x)) }

def flip : (a -> b -> c) -> b -> a -> c
def flip = { (f, x, y) -> f(y, x) }

def main = {
  let inc = { x -> x + 1 };
  let double = { x -> x * 2 };
  compose(inc, double, 5)  -- Returns 11
}
```

## Design Rationale

The C-style apply feature makes Malgo more approachable for developers familiar with mainstream languages while preserving the functional programming model. Key benefits:

1. **Familiar function call syntax** - `f(x, y)` is instantly recognizable
2. **Clear tuple syntax** - `{x, y}` visually distinguishes tuples from parenthesized expressions
3. **Explicit no-arg calls** - `f()` makes it clear when a function is being called vs passed as a value
4. **Consistent with records** - Both records and tuples use braces (though with different internal syntax)

The feature is entirely optional and can be enabled on a per-file basis, allowing gradual adoption or mixed codebases.

## EBNF

```ebnf
toplevel = pragma | decl ;

pragma = "#" [any character except newline]* ;

decl = dataDecl
     | typeDecl
     | infixDecl
     | foreignDecl
     | importDecl
     | signatureDecl
     | variableDecl
     ;

dataDecl = "data" ident ident* "=" constructorList ;

constructorList = constructor ( "|" constructor )* ;

constructor = ident atomType* ;

typeDecl = "type" ident ident* "=" type ;

infixDecl = "infixl" decimal "(" operator ")"
          | "infixr" decimal "(" operator ")"
          | "infix" decimal "(" operator ")" ;
operator = ident ;

foreignDecl = "foreign" "import" ident ":" type ;

importDecl = "module" modulePattern "=" "import" modulePath ;
modulePattern = "{" ".." "}"
              | "{" ident ("," ident)* "}"
              | moduleName
              ;
moduleName = ident ;
modulePath = string ;

signatureDecl = "def" ident ":" type
              | "def" "(" operator ")" ":" type ;

variableDecl = "def" ident "=" expr
             | "def" "(" operator ")" "=" expr ;

expr = opApp (":" type)? ;

opApp = apply (operator apply)* ;

apply = atom
      | apply "." ident
      | apply "(" ")"
      | apply "(" expr ("," expr)* ")"
      ;

atom = literal | variable | tuple | parens | record | function | list | sequence ;

literal = boxed "#"? ;
boxed = float | int | char | string ;

float = FLOAT ;

int = DECIMAL ;

char = "'" CHAR "'" ;

string = "\"" CHAR* "\"" ;

variable = ident ;

tuple = "{" "}"
      | "{" expr "," expr ("," expr)* "}" ;

parens = "(" expr ")" ;

record = "{" field ("," field)* "}" ;

field = ident "=" expr ;

function = "{" clause (";" clause)* "}" ;

clause = "(" ")" "->" statements
       | "(" pattern ("," pattern)* ")" "->" statements
       | statements
       ;

pattern = atomPattern
        | pattern "." ident
        | pattern "(" ")"
        | pattern "(" pattern ("," pattern)* ")"
        ;

atomPattern = this | variable | literal | tuplePattern | parensPattern | recordPattern | listPattern ;

this = "#" ;

tuplePattern = "{" "}"
              | "{" pattern "," pattern ("," pattern)* "}" ;

parensPattern = "(" pattern ")" ;

recordPattern = "{" fieldPattern ("," fieldPattern)* "}" ;
fieldPattern = ident "=" pattern ;

listPattern = "[" "]"
             | "[" pattern ("," pattern)* "]" ;

list = "[" "]"
     | "[" expr ("," expr)* "]" ;

sequence = "(" statements ")" ;

statements = statement (";" statement)* ;

statement = letStatement
          | withStatement
          | expr
          ;

letStatement = "let" ident "=" expr ;

withStatement = "with" ident "=" expr
              | "with" expr
              ;

type = applyType ("->" applyType)* ;

applyType = atomType
          | atomType "(" ")"
          | atomType "(" type ("," type)* ")"
          ;

atomType = variable
         | tupleType
         | parensType
         | recordType
         ;

tupleType = "{" "}"
           | "{" type "," type ("," type)* "}" ;

parensType = "(" type ")" ;

recordType = "{" fieldType ("," fieldType)* "}" ;
fieldType = ident ":" type ;
```
