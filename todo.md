# Roadmap

- [x] Update documentations.
  - [x] tour.md : Malgo language tour.
  - [x] reference.md : Malgo reference.
  - [x] architecture.md : Malgo compiler architecture.
- [x] Change tuple syntax to use `{}` instead of `()`.
- [x] Add C-like function call syntax.
  - [x] `f(x)` is equivalent to `f x`.
  - [x] `f(x, y, z)` is equivalent to `f x y z`.
  - [x] `f()` is equivalent to `f {}`.
  - [x] `{ (x, y) -> ... }` is equivalent to `{ x y -> ... }`.
- [x] Canceled: Add `forall` and `exists` quantifiers.
  - [ ] Add Bidirectional type inference.
  - [ ] Add new IR based on System Fω.
- [x] Canceled: Add record polymorphism.
  - [ ] Row polymorphism?
  - [ ] Record kinds like SML#?
- [x] Canceled: Add module system based on polymorphic records.
  - [ ] F-ing modules?
- [ ] Replace function application syntax with C-style function calls.
  - [x] Enable C-style function calls by default.
  - [ ] Replace function application and pattern matching syntax with C-style function calls and tuple syntax on `**.mlg` files.
  - [ ] Delete old function application syntax from the parser.
  - [ ] Delete unused pragma `#c-style-apply`.
- [ ] Add copatterns.
  - [ ] Q: How to integrate copatterns with the module system?
- [ ] Add delimited continuations.
- [ ] Add effects.
  - [ ] As a library?

## Implementation Notes

### C-Style Function Calls and Tuple Syntax (Completed ✅)

C-style function call syntax and curly brace tuples are now the **default syntax** in Malgo.

**Default Syntax:**

- **Function calls:** `f(x, y)` - C-style parentheses with comma separation
- **Tuple syntax:** `{x, y}` - Curly braces instead of parentheses
- **Tuple patterns:** `{x, y}` - In pattern matching and function parameters
- **Function parameters:** Optional parentheses `f(x, y) -> ...`

**Breaking Change:**

- Old syntax `f x y` and `(x, y)` is **no longer supported**
- All Malgo code must use the new C-style syntax
- The `#c-style-apply` pragma has been removed

**Implementation:**

- Parser completely converted to C-style syntax (`src/Malgo/Parser.hs`)
- Feature system cleaned up (`src/Malgo/Features.hs`)
- Pragma parsing removed - no longer needed
- Test files updated to use new syntax

**Example:**

```malgo
def fact : Int -> Int -> Int
def fact = { {n, acc} ->
  if(n == 0L, { acc }, { fact({n - 1L, n * acc}) }) }

def main = { putStrLn("Hello, world!") }
```

### Copatterns (TODO 📝)
