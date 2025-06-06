# Roadmap

- [ ] Update documentations.
  - [ ] tour.md : Malgo language tour.
  - [ ] tutorial.md : Malgo tutorial.
  - [ ] reference.md : Malgo reference.
  - [ ] design.md : Malgo language and compiler design.
  - [ ] architecture.md : Malgo compiler architecture.
- [ ] Change function application syntax to `f(x, y)(z)` instead of `f x y z`.
  - [ ] `f(x, y)` is a syntactic sugar for `f(x)(y)`.
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

```
type EQ = {
  type t
  def eq : t -> t -> Bool
}

type MAP = {
  type key
  type map[a]
  def empty[a] : map[a]
  def add[a] : key -> a -> map[a] -> map[a]
  def lookup[a] : key -> map[a] -> Maybe[a]
}

def Map[Key : EQ] :> MAP where { type key = Key.t } = {
  type key = Key.t
  type map[a] = key -> Maybe[a]
  def empty[a](k) = Nothing[a]
  def lookup[a](k, m) = m(k)
  def add[a](k, v, m)(x) =
    match Key.eq(k, x) {
      True -> Just[a](v),
      False -> m(x)
    }
}
```
