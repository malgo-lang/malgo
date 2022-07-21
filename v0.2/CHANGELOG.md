# Changelog

## [Unreleased](https://github.com/takoeight0821/malgo/tree/HEAD)

[Full Changelog](https://github.com/takoeight0821/malgo/compare/v0.1.0...HEAD)

**Implemented enhancements:**

- Replace type checker [\#41](https://github.com/takoeight0821/malgo/pull/41) ([takoeight0821](https://github.com/takoeight0821))
- Griff to Malgo \(close \#22\) [\#24](https://github.com/takoeight0821/malgo/pull/24) ([takoeight0821](https://github.com/takoeight0821))

**Closed issues:**

- id :: a -\> aにunboxedな値を渡せてしまう [\#44](https://github.com/takoeight0821/malgo/issues/44)
- Griff is Malgo 1.0.0 [\#22](https://github.com/takoeight0821/malgo/issues/22)

**Merged pull requests:**

- Remove --via-binding option [\#51](https://github.com/takoeight0821/malgo/pull/51) ([takoeight0821](https://github.com/takoeight0821))
- Record patterns [\#49](https://github.com/takoeight0821/malgo/pull/49) ([takoeight0821](https://github.com/takoeight0821))
- Add record [\#48](https://github.com/takoeight0821/malgo/pull/48) ([takoeight0821](https://github.com/takoeight0821))
- Refactoring [\#47](https://github.com/takoeight0821/malgo/pull/47) ([takoeight0821](https://github.com/takoeight0821))
- Better compile error messages [\#46](https://github.com/takoeight0821/malgo/pull/46) ([takoeight0821](https://github.com/takoeight0821))
- Fix Kind check [\#45](https://github.com/takoeight0821/malgo/pull/45) ([takoeight0821](https://github.com/takoeight0821))
- Better code generation [\#43](https://github.com/takoeight0821/malgo/pull/43) ([takoeight0821](https://github.com/takoeight0821))
- Rename NewTypeCheck to TypeCheck [\#42](https://github.com/takoeight0821/malgo/pull/42) ([takoeight0821](https://github.com/takoeight0821))
- Type synonym [\#40](https://github.com/takoeight0821/malgo/pull/40) ([takoeight0821](https://github.com/takoeight0821))
- Refactoring [\#39](https://github.com/takoeight0821/malgo/pull/39) ([takoeight0821](https://github.com/takoeight0821))
- Better error message [\#38](https://github.com/takoeight0821/malgo/pull/38) ([takoeight0821](https://github.com/takoeight0821))
- Refactor new typechecker [\#37](https://github.com/takoeight0821/malgo/pull/37) ([takoeight0821](https://github.com/takoeight0821))
- Fix type scheme equiv check [\#36](https://github.com/takoeight0821/malgo/pull/36) ([takoeight0821](https://github.com/takoeight0821))
- Use unordered containers [\#35](https://github.com/takoeight0821/malgo/pull/35) ([takoeight0821](https://github.com/takoeight0821))
- Add new typechecker [\#34](https://github.com/takoeight0821/malgo/pull/34) ([takoeight0821](https://github.com/takoeight0821))
- New build system [\#32](https://github.com/takoeight0821/malgo/pull/32) ([takoeight0821](https://github.com/takoeight0821))
- Dump AST as JSON [\#31](https://github.com/takoeight0821/malgo/pull/31) ([takoeight0821](https://github.com/takoeight0821))
- Add new ir syntax [\#29](https://github.com/takoeight0821/malgo/pull/29) ([takoeight0821](https://github.com/takoeight0821))
- Add documents [\#27](https://github.com/takoeight0821/malgo/pull/27) ([takoeight0821](https://github.com/takoeight0821))
- Change syntax: definitions must be separeted with semicolon [\#26](https://github.com/takoeight0821/malgo/pull/26) ([takoeight0821](https://github.com/takoeight0821))
- Make syntax more restrictive and understandable [\#25](https://github.com/takoeight0821/malgo/pull/25) ([takoeight0821](https://github.com/takoeight0821))

## [v0.1.0](https://github.com/takoeight0821/malgo/tree/v0.1.0) (2021-01-23)

[Full Changelog](https://github.com/takoeight0821/malgo/compare/3ddbc4fc79f9e1ba05caecb4db619c24e5c37ba1...v0.1.0)

**Implemented enhancements:**

- 真偽値を表すUnboxedな値を追加する [\#8](https://github.com/takoeight0821/malgo/issues/8)
- Add boxed literals. [\#21](https://github.com/takoeight0821/malgo/pull/21) ([takoeight0821](https://github.com/takoeight0821))

**Fixed bugs:**

- 複数の要素を持つタプルのUnpackが壊れている [\#6](https://github.com/takoeight0821/malgo/issues/6)
- 多相関数の型検査が健全でない [\#5](https://github.com/takoeight0821/malgo/issues/5)

**Closed issues:**

- miscompile of `id = {x -> x}; id 42#` [\#14](https://github.com/takoeight0821/malgo/issues/14)
- 自分自身を値として使う関数のLLVM IR生成が間違っている [\#7](https://github.com/takoeight0821/malgo/issues/7)

**Merged pull requests:**

- Add pointer type [\#23](https://github.com/takoeight0821/malgo/pull/23) ([takoeight0821](https://github.com/takoeight0821))
- Impove module lookup [\#20](https://github.com/takoeight0821/malgo/pull/20) ([takoeight0821](https://github.com/takoeight0821))
- Add Parens "\( e \)" to Syntax and fix infix parser [\#19](https://github.com/takoeight0821/malgo/pull/19) ([takoeight0821](https://github.com/takoeight0821))
- Zonk constraints before solving [\#18](https://github.com/takoeight0821/malgo/pull/18) ([takoeight0821](https://github.com/takoeight0821))
- Fix module handling [\#17](https://github.com/takoeight0821/malgo/pull/17) ([takoeight0821](https://github.com/takoeight0821))
- Primitive functions [\#16](https://github.com/takoeight0821/malgo/pull/16) ([takoeight0821](https://github.com/takoeight0821))
- Add unboxed kind \(Fix \#14\) [\#15](https://github.com/takoeight0821/malgo/pull/15) ([takoeight0821](https://github.com/takoeight0821))
- Myownprelude [\#4](https://github.com/takoeight0821/malgo/pull/4) ([takoeight0821](https://github.com/takoeight0821))
- Lambda [\#3](https://github.com/takoeight0821/malgo/pull/3) ([takoeight0821](https://github.com/takoeight0821))
- Malgoモナドの設計を見直し [\#2](https://github.com/takoeight0821/malgo/pull/2) ([takoeight0821](https://github.com/takoeight0821))
- Rewrite [\#1](https://github.com/takoeight0821/malgo/pull/1) ([takoeight0821](https://github.com/takoeight0821))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
