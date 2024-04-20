# Changelog

All notable changes to this project will be documented in this file.

## [unreleased]

### 🚀 Features

- Improve Koriel pretty printer
- .kor -> .kor.bin
- Implement Koriel Parser
- Annotate Koriel Program
- Add new runtime
- Add rust runtime
- Imported module's functions are not external
- Add `=` expression
- Replace `bind` with `=` in several places.
- Koriel.Core.Flat uses `destruct`
- Add default case to `Switch`
- Generate no-optimized .kor file
- Add lint rule 'all cases have same type of pattern'
- Convert `match` to `switch` if it is possible
- Convert `match` to `destruct-record` if it is possible
- Add `switch-unboxed`
- Convert `Exact` to `SwitchUnboxed`
- `malgo build` builds each files concurrently
- Update llvm-hs to 15
- Remove noop `destruct`
- Generate global function's closure in global scope
- More descriptive label
- Implement allocaresult
- Inline gblcls
- Add helper function malgo_print_tag
- Unique string value
- New normalization pass
- Match to Switch conversion

### 🐛 Bug Fixes

- Remove 'noName'
- Update malgo.cabal
- Fix 'Main'
- Fix error cases filepath
- Enable tests
- Fix tests
- Reduce lambdalifiting time
- Use `alpha` conversion in inline expanding
- Use Control.Exception.assert
- Fix CI
- Fix getClangCommand
- Generate kor.bin
- Flatten switch and switch-unboxed correctly
- Fix lint

### 🚜 Refactor

- Simplify constraints in Malgo.Rename
- Simplify constraints in Malgo.Refine
- Simplify constraints in Malgo.Lsp
- Simplify constraints in Malgo.Infer
- Simplify constraints in Malgo.Desugar
- Remove unused import
- Remove 'noName' and 'newNoNameId'
- Simplify constraints in Koriel
- Remove WIP Scheme Backend
- Rename TypeVar to MetaVar
- `type TypeVar = Id Kind`
- Flatten type representations
- Add 'insertKind' and 'askKind'
- Explicit export lists (Syntax, Monad, Link)
- Explicit export lists (Interface)
- Explicit export lists
- Explicit export lists
- Explicit export lists
- Remove unused code
- Remove unused lens classes
- HasUniqSupply becomes alias of HasField
- Simplify CodeGenEnv
- Simplify AlphaEnv and OptimizeEnv
- Simplifiy LambdaLIftState
- Simplify LocalDef
- Remove `object`
- Simplify Index
- Simplify IndexEnv
- Simplify TypeDef
- Small change
- Simplify DsState
- Make type informations explicitly
- Use list
- Internal Id's postfix is now hexadecimal
- Ids introduced in LambdaLift respects original Id's module
- External and Native Id doesn't care `uniq` field
- Remove `uniq` field
- Remove `newIdOnName` and specialize `cloneId` for `Koriel.Core.Alpha`
- `newExternalId` requires `MonadReader`
- `newNativeId` requires `MonadReader`
- `HasModuleName` is a special case of `HasField`
- Small change
- Small change
- `unboundFreevars` ignores kind variables.
- Simplify `toBound`
- Simplify `generalize`
- Small change
- Remove comments
- Use c_char
- Replace `Switch` with `Exact`
- Improve internal representation
- Small change
- Add `moduleName` field to `MalgoEnv`
- Add flatMatch
- Make the inline function size a constant
- Traverse all constructors explicitly
- Rename optimization passes
- Check term size in checkInlineable
- Simplify inlineFunciton
- Don't optimize after lambda-lifting
- Check if optimization preserves flat form
- Make tests more robust
- Exp to expr
- Remove 'appObj'
- Remove appCase and appProgram
- Use Plated
- Remove old codes
- Rewrite Plated (Expr a) instance
- INLINE plate
- Simplify imports
- Remove redundant bitcast
- Use ContT
- Use withContT
- WithTerminator
- Small refactoring
- Add comments
- Use shiftT
- Use shiftT
- Remove allocaResult strategy
- Remove comments

### 🧪 Testing

- Add koriel parser test
- Test all combinations of test options
- Add golden tests for malgo passes

### ⚙️ Miscellaneous Tasks

- Update ghc to 9.2.5
- Update CI
- Add flake.nix
- Update flake.nix
- Update flake.nix
- Add default.nix and shell.nix
- Don't check ListLike
- Format flake.nix
- Add stack.yaml
- Update haskell-language-server
- Add comments
- Update cabal.project.freeze
- Add rust
- Add cmake to dev container and update bdwgc-alloc
- Add flags for bdw-gc
- Mv testcases/ test/testcases/
- Don't print error messages in testError
- Update hls
- Add unliftio
- Add benchmarks
- Update ghc
- Update malgo_ci.yml
- Add some comments
- Update Dockerfile
- Update ghc 9.2.7
- Small fix on malgo.cabal
- Diagnose 2.4.0
- Use fourmole
- Fix CI
- Pass -opaque-pointers to clang
- Fix CI
- Update Dockerfile
- Cabal freeze
- Update CI
- Update Dockerfile
- Small changes
- Small change
- Update GHC 9.6.1
- Small change
- Update Dockerfile
- Remove comments
- Remove comments
- Remove griff
- Update stack.yaml
- Fix test output
- Update Dockerfile
- Add commitizen
- Reconfigure devcontainer
- Add git cliff

### FIX

- Convert `Call` to `CallDirect` if necessary.

### WIP

- Add Annotate
- Use Plated
- Use Plated in inlineFunciton
- Add PrimtLLVM
- Add test for PrintLLVM
- Update PrintLLVM
- PrintLLVM
- PrintLLVM
- PrintLLVM
- PrintLLVM
- Rewrite with effectful
- Effectful

### Wip

- Add Koriel.Core.Parser
- PrintLLVM (global string)
- Add isAllocaResult flag

## [0.3.0] - 2022-11-30

### 🚀 Features

- Change default inline size
- Disable lambda-lifiting
- More effective inline-expansion and uncurrying
- Simplify compile options
- `malgo build` uses JSON configuration file
- Dump Core IR as JSON

### 🐛 Bug Fixes

- Fix concatenation of the result of pkg-config

### 🚜 Refactor

- Use MalgoEnv in Koriel.Core.CodeGen.LLVM
- Rename srcName to srcPath
- Rename srcName to srcPath (app/Main.hs)
- NewCodeGenEnv
- Fix curryFun
- Remove unused field
- Rename _srcPath to srcPath
- Rename _srcPath to srcPath
- Rename _dstPath to dstPath
- Remove storeInterface
- Rename some fields of the compile option
- Remove pretty-simple
- Use DerivingAnyClass and DervingVia
- Remove unused LANGUAGE pragma
- Use DerivingAnyClass
- Remove used import
- Split MalgoM, RnState and DsState into modules

### 🎨 Styling

- Formatting

### 🧪 Testing

- Add testcases/malgo/Factorial.mlg
- Rewrite pretest.sh in Haskell
- Rewrite test.sh in Haskell
- Rewrite test-nolift.sh in Haskell
- Rewrite test scripts in Haskell

### ⚙️ Miscellaneous Tasks

- Remove unnecessary import
- Build on Docker
- Move to app/malgo
- Remove unused scripts
- Delete unused scripts
- Remove app/Main.hs
- Delete old script
- Update CI
- Compile hls
- Update Dockerfile
- Add /root/.cabal/bin to PATH
- Update Dockerfile
- Move Dockerfile
- Fix Dockerfile
- Small change
- Update devcontainer.json
- Add .github/release.yml

### Error

- Module name and source name are mismatched

## [0.2.0] - 2022-07-05

### 🚀 Features

- Implement Refine pass for class and impl
- Add --dump-refine option
- Add `eqString` and `substring`
- Type annotation for expressions
- Use `;` outside `{}`
- ProgramBuilderT
- `Builtin.panic : String -> a`
- With statement
- Do not generalize `let` bindings
- Improve readability of type errors
- Boxed literal pattern
- `{ | A -> ... | B -> ... }` is also available
- Check that numbers of patterns are same
- Error on string literal pattern
- `{ a }` is the syntax sugar for `() -> a`
- Inline trivial function application
- Compile.sh now recognize options to malgo
- `mlgToCore` supports Level1_Int.mlg
- Try to run new desugar-pass always
- Add some functions to Builtin.mlg and Prelude.mlg

### 🐛 Bug Fixes

- Desugar groups of `ScDef` correctly
- Prettyprint `bug $ Unreachable reason`
- Generate initialization of toplevel variables correctly
- Change llvm-hs version to 9.0.1
- Fix `groupTuple`
- Resolve the parser performance issue about `expr : type`
- Resolve fprintf warning
- Handle right-associative operators correctly
- Add missing `cast`
- Support `()` pattern
- Prettyprint TyVar
- Add space to pPrint TyConApp
- Fix `malgo_string_append`
- Malgo_panic returns void*
- Use Control.Monad.Trans.Writer.CPS
- Improve performance of `checkInlineable`
- `[]` pattern
- Change the key of `interned` to `(Int, ModuleName)`
- Update testcases
- External global variable
- Missing os field

### 🚜 Refactor

- Simplify AST for `impl` and `class`
- Rename `TypeCheck` to `Infer`
- Move Malgo/{UTerm.hs, Unify.hs} to Malgo/Infer/
- Fix the behavior of UTerm.viewTyConApp
- Replace `NonEmpty (Stmt x)` with `Seq`
- Merge `liftUnify` and `unify`
- Minor change
- TyConApp
- BuildTyArr
- WalkOn is Traversal
- Organize type classes
- Simplify `unify`
- Rename `newLocalId` and `newGlobalId` with `newInternalId` and `newExternalId`
- Remove duplicate codes
- Rename `newLocalId` and `newGlobalId` with `newInternalId` and `newExternalId`
- Rename `defs` with `defaults`
- Add new ir
- MonadExpBuilder
- `MonadCodeGen` and `runCodeGenT`
- Remove `asumMap` and `<<$>>`
- Use `error` instead of `bug`
- Remove `foldMapA`
- New design of `With` and some minor changes
- Simplify `With`
- Use `Text`
- Newtype `MalgoM`
- Reduce usage of `TemplateHaskell`
- Refactor TcEnv
- Use relude
- Hlint
- With -> Annotated
- Add some comments and small changes
- Merge `Static` and `UTerm`
- Hand-written Plated instance
- `TyLazy` -> `TyBlock`
- More readable paramter names
- Point-free
- Hand-written zonk
- Add signatures
- Add comments
- Rename `Infer` to `TypeCheck`
- `DsEnv` is superset of `TcEnv`
- Split `ExtCall` to `RawCall` and `ExtFunc`
- Remove unused parameter
- Remove `class`, `impl`, `=>`
- Remove TyDArr
- Split `tcScDefs` to some subunits
- Fix Pretty instances
- Move `lookup` functions to Rename.Env
- Merge `freevars*`

### 📚 Documentation

- Update scripts/README.md
- Add comments
- Add comments

### 🎨 Styling

- Format

### 🧪 Testing

- Update testcase
- Update testcase
- Add test script
- Add test cases
- Add test cases
- Add test case
- Update InvalidPattern.mlg
- Add test case
- Add boxed literal pattern test
- Add malgo-test package
- Add `Punctuate`
- Add `Spec.hs`
- Add test case
- Add test cases
- Add testcase
- `stack test` runs tests as test_malgo_parallel.sh

### ⚙️ Miscellaneous Tasks

- Add `pretty-simple` to dependencies
- Update cabal.project.freeze
- Update stack.yaml
- Remove cabal.project.freeze
- Use stack
- Update hie.yaml
- Update malgo_ci.yml
- Update hie.yaml
- Use stack
- Update malgo_ci.yml
- Remove cabal.project.freeze and use cabal in hls instead of stack
- Move malgo/* to root
- Update hie.yaml
- Update resolver
- Update templates/runtime.c.mustache
- Update examples
- Add Fib.mlg and NQueen.mlg
- 10-queen
- Add scripts/bench.sh
- Update README.md
- Update README.md
- Update stack.yaml
- Remove templates
- Update malgo_ci.yml
- Update stack.yaml
- Update stack.yaml

### Example

- Update List.mlg

### Wip

- Add Malgo.Core.Syntax
- Add Malgo.Core.MlgToCore and rename `id` to `identity`
- Merge TypeRep
- MlgToCore
- Disable warnings
- Implement core-to-js pass
- Support Level3_Func.mlg
- Support Level5_ExtFunc.mlg
- Implement MlgToCore for Level6_Pair.mlg and Level7_List.mlg
- Disable warnings for Malgo.Core

## [0.1.0] - 2021-01-23

### Typeに関数型を表す

- ->を追加

### Default-extensions

- StrictData, Strict

### 定義の

- =を削除し、配列への書き込みを:=に変更

<!-- generated by git-cliff -->
