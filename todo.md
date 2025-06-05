# Implement kind-based polymorphic record types

## Add `forall` syntax

- [x] Move `HasType` instances in Malgo.Syntax to Malgo.Infer.TypeRep
- [x] Move `Malgo.Syntax.getTyVars` to Malgo.Infer.Pass
- [ ] Introduce `Scheme` syntax type and replace `Type` with `Scheme` in `Decl`
- [ ] Parse `forall` syntax
- [ ] Resolve `Scheme` in Malgo.Rename
- [ ] Resolve `Scheme` in Malgo.Infer

Do not add `Forall` to `TypeRep` or `Type`. We don't want to take care about higher-rank polymorphism yet.

## More clear module dependencies

- Modules in `Malgo.Foo.**` should not be imported from modules that are not in `Malgo.Foo.**`.
- All exposed items should exported from `Malgo.Foo` module.

## Design choices regarding positions that allow `forall`

1. `forall` can only be used in the head of a type signature.
2. Rank 2 polymorphism to support module signatures.

- TODO: Read one-ml paper to understand how to implement module level polymorphism.
