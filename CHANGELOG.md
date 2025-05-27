<a name="unreleased"></a>
## [Unreleased]


<a name="v2.0.0"></a>
## [v2.0.0] - 2025-05-27
### Chore
- update cachix-action to version 16 in GitHub Actions workflow
- update GitHub Actions to use specific versions of actions
- add commit message template
- add plan.md
- Update dependabot configuration for weekly updates
- Update GitHub Actions versions in malgo_ci.yml
- update README.md
- update copilot-instructions.md for clarity and additional guidelines
- remove update-flake-lock workflow and clean up nix.yml
- update project dependencies and add Hoogle script
- fix formatting in nix.yml workflow
- emphasize the importance of preserving comments in 'GitHub Copilot comments' section
- clarify instructions regarding comments in 'GitHub Copilot comments' section
- **dependencies:** update lens to version 5.3.4
- **deps:** bump cachix/install-nix-action from 31.0.0 to 31.3.0
- **deps:** bump DeterminateSystems/update-flake-lock from 24 to 25
- **deps:** bump DeterminateSystems/nix-installer-action from 16 to 17
- **test:** update Fib.mlg
- **test:** replace (.) operator with (<<)
- **test:** add script to run tests with custom options
- **vscode:** add Copilot commit message generation instructions

### Ci
- add workflow to update flake.lock automatically
- fix malgo_ci.yml to run all test cases

### Doc
- update coding conventions and naming guidelines
- **copilot-instructions:** clarify naming conventions for identifiers

### Docs
- update GitHub Copilot instructions for clarity and brevity
- introduce new syntax for function application and field access

### Feat
- add pragma support and update related tests
- **infer:** add error handling for invalid type applications
- **parser:** enhance handling of pragma

### Fix
- update imports for Prettyprinter in multiple files
- handle MainNotFound error in evalProgram and improve error reporting
- handle Record capture correctly
- use Map and Set instead of HashMap and HashSet
- remove absolute path from Show instance of ArtifactPath
- **parser:** fix unary operator parsing and type variable handling
- **parser:** update parsing output format in NewParserSpec
- **parser:** add '.' to reserved operators list
- **prelude:** improve pretty printing of Range instances
- **rename:** fix constructor pattern handling in rnClause

### Refactor
- simplify conversion functions in ToFun module
- simplify Consumer representation and evaluation logic
- simple interface
- exportedIdentList and exportedTypeIdentList as function
- split meta informations in Id to wrapper type Meta
- change Pretty instance for Malgo.Syntax
- delete Malgo.Core and Malgo.Desugar
- combine Malgo and Koriel modules
- use makeFieldsId instead of makeFieldsNoPrefix
- rename NewRename to Rename
- new internal resource management system
- use new resource management system in Malgo.Driver
- update ConsumerRepr and related functions to return unit type
- update parser and rename pass
- update conversion functions in ToCore module for clarity and correctness
- rename conversion functions for consistency and clarity in ToFun module
- implement new parser
- **Interface:** update buildInterface to use HasField
- **driver:** rename exitIfError to failIfError for clarity
- **flake:** remove icu package
- **infer:** simplify access to fields in TcEnv and related modules
- **infer:** split KindCtx from TcEnv
- **infer:** make RnEnv polymorphic
- **interface:** clarify kindCtx usage in Interface data type
- **io:** remove io-streams dependency and simplify IO handling
- **parser:** rename stripPragmas to extractPragmas for clarity
- **parser:** replace old parser with NewParser and clean up
- **parser:** rename NewParser with Parser
- **range:** move HasRange to Prelude
- **refine:** replace old parser and rename with new versions
- **rename:** remove old RnState and RnEnv modules
- **rename:** enhance Project expression handling
- **sequent:** remove unused indentStrategy function
- **sequent:** remove unused HasRange import from modules

### Test
- add error case tests for Infer and Refine
- add SameImport.mlg
- add sexpr golden tests
- golden tests for generated llvm IRs
- rewrite DriverSpec as Golden Tests
- add golden tests for malgo passes
- **errors:** add error cases for DuplicateDef and UndefinedVariable
- **infer:** use NewParser and NewRename

### WIP
- combine Malgo and Koriel

### Wip
- mask unique
- add pragma
- refactor Consumer evaluation
- update flake.nix
- implement eval
- Add initial implementation of evaluation module with environment handling
- implement new compiler
- add vm
- merge Malgo.Core.Syntax
- implement eval
- **rename:** handle Project expressions in rnExpr
- **rename:** add NewRename pass

### Reverts
- Add Windows testing job to Nix workflow
- generate changelog

### Pull Requests
- Merge pull request [#188](https://github.com/malgo-lang/malgo/issues/188) from malgo-lang/update_flake_lock_action
- Merge pull request [#190](https://github.com/malgo-lang/malgo/issues/190) from malgo-lang/dependabot/github_actions/DeterminateSystems/nix-installer-action-17
- Merge pull request [#191](https://github.com/malgo-lang/malgo/issues/191) from malgo-lang/dependabot/github_actions/cachix/install-nix-action-31.3.0
- Merge pull request [#192](https://github.com/malgo-lang/malgo/issues/192) from malgo-lang/dependabot/github_actions/DeterminateSystems/update-flake-lock-25
- Merge pull request [#186](https://github.com/malgo-lang/malgo/issues/186) from malgo-lang/remove-old-core
- Merge pull request [#185](https://github.com/malgo-lang/malgo/issues/185) from malgo-lang/update_flake_lock_action
- Merge pull request [#183](https://github.com/malgo-lang/malgo/issues/183) from malgo-lang/dependabot/github_actions/cachix/install-nix-action-31
- Merge pull request [#184](https://github.com/malgo-lang/malgo/issues/184) from malgo-lang/use-unicode-identifier
- Merge pull request [#182](https://github.com/malgo-lang/malgo/issues/182) from malgo-lang/sequent-core
- Merge pull request [#181](https://github.com/malgo-lang/malgo/issues/181) from malgo-lang/interpreter
- Merge pull request [#180](https://github.com/malgo-lang/malgo/issues/180) from malgo-lang:dependabot/github_actions/orhun/git-cliff-action-4


<a name="v1.0.0"></a>
## [v1.0.0] - 2024-04-20
### Chore
- add git cliff
- reconfigure devcontainer
- add commitizen
- update Dockerfile
- fix test output
- update stack.yaml
- remove griff
- remove comments
- remove comments
- Update Dockerfile
- small change
- update GHC 9.6.1
- small change
- small changes
- update Dockerfile
- update CI
- cabal freeze
- update Dockerfile
- fix CI
- pass -opaque-pointers to clang
- fix CI
- Use fourmole
- diagnose 2.4.0
- small fix on malgo.cabal
- update ghc 9.2.7
- update Dockerfile
- add some comments
- update malgo_ci.yml
- update ghc
- add benchmarks
- add unliftio
- update hls
- don't print error messages in testError
- mv testcases/ test/testcases/
- Add flags for bdw-gc
- Add cmake to dev container and update bdwgc-alloc
- add rust
- update cabal.project.freeze
- add comments
- update haskell-language-server
- add stack.yaml
- format flake.nix
- don't check ListLike
- add default.nix and shell.nix
- update flake.nix
- update flake.nix
- add flake.nix
- update CI
- update ghc to 9.2.5

### FIX
- Convert `Call` to `CallDirect` if necessary.

### Feat
- Match to Switch conversion
- new normalization pass
- unique string value
- add helper function malgo_print_tag
- inline gblcls
- implement allocaresult
- more descriptive label
- Generate global function's closure in global scope
- remove noop `destruct`
- update llvm-hs to 15
- `malgo build` builds each files concurrently
- convert `Exact` to `SwitchUnboxed`
- add `switch-unboxed`
- convert `match` to `destruct-record` if it is possible
- convert `match` to `switch` if it is possible
- Add lint rule 'all cases have same type of pattern'
- Generate no-optimized .kor file
- Add default case to `Switch`
- Koriel.Core.Flat uses `destruct`
- Replace `bind` with `=` in several places.
- Add `=` expression
- imported module's functions are not external
- add rust runtime
- add new runtime
- Annotate Koriel Program
- Implement Koriel Parser
- .kor -> .kor.bin
- improve Koriel pretty printer

### Fix
- fix lint
- flatten switch and switch-unboxed correctly
- generate kor.bin
- fix getClangCommand
- fix CI
- Use Control.Exception.assert
- use `alpha` conversion in inline expanding
- reduce lambdalifiting time
- fix tests
- enable tests
- fix error cases filepath
- fix 'Main'
- Update malgo.cabal
- remove 'noName'

### Refactor
- remove comments
- remove allocaResult strategy
- use shiftT
- use shiftT
- add comments
- small refactoring
- withTerminator
- use withContT
- use ContT
- remove redundant bitcast
- Simplify imports
- INLINE plate
- Rewrite Plated (Expr a) instance
- Remove old codes
- Use Plated
- remove appCase and appProgram
- remove 'appObj'
- exp to expr
- make tests more robust
- Check if optimization preserves flat form
- Don't optimize after lambda-lifting
- simplify inlineFunciton
- check term size in checkInlineable
- rename optimization passes
- traverse all constructors explicitly
- Make the inline function size a constant
- Add flatMatch
- Add `moduleName` field to `MalgoEnv`
- small change
- improve internal representation
- Replace `Switch` with `Exact`
- use c_char
- remove comments
- small change
- simplify `generalize`
- simplify `toBound`
- `unboundFreevars` ignores kind variables.
- small change
- small change
- `HasModuleName` is a special case of `HasField`
- `newNativeId` requires `MonadReader`
- `newExternalId` requires `MonadReader`
- remove `newIdOnName` and specialize `cloneId` for `Koriel.Core.Alpha`
- remove `uniq` field
- External and Native Id doesn't care `uniq` field
- Ids introduced in LambdaLift respects original Id's module
- Internal Id's postfix is now hexadecimal
- use list
- make type informations explicitly
- simplify DsState
- small change
- simplify TypeDef
- simplify IndexEnv
- simplify Index
- remove `object`
- simplify LocalDef
- simplifiy LambdaLIftState
- simplify AlphaEnv and OptimizeEnv
- Simplify CodeGenEnv
- HasUniqSupply becomes alias of HasField
- remove unused lens classes
- remove unused code
- explicit export lists
- explicit export lists
- explicit export lists
- explicit export lists (Interface)
- explicit export lists (Syntax, Monad, Link)
- add 'insertKind' and 'askKind'
- Flatten type representations
- `type TypeVar = Id Kind`
- Rename TypeVar to MetaVar
- Remove WIP Scheme Backend
- simplify constraints in Koriel
- remove 'noName' and 'newNoNameId'
- remove unused import
- simplify constraints in Malgo.Desugar
- simplify constraints in Malgo.Infer
- simplify constraints in Malgo.Lsp
- simplify constraints in Malgo.Refine
- simplify constraints in Malgo.Rename

### Test
- Test all combinations of test options
- Add koriel parser test

### WIP
- effectful
- Rewrite with effectful
- PrintLLVM
- PrintLLVM
- PrintLLVM
- PrintLLVM
- update PrintLLVM
- Add test for PrintLLVM
- add PrimtLLVM
- use Plated in inlineFunciton
- use Plated
- Add Annotate

### Wip
- add isAllocaResult flag
- PrintLLVM (global string)
- add Koriel.Core.Parser

### Reverts
- Add Statement (`Stmt`) to `Koriel.Core.Syntax`.
- refactor: Simplify CodeGenEnv
- refactor: simplify DsState

### Pull Requests
- Merge pull request [#176](https://github.com/malgo-lang/malgo/issues/176) from malgo-lang:consistent-seq
- Merge pull request [#173](https://github.com/malgo-lang/malgo/issues/173) from malgo-lang:effectful
- Merge pull request [#172](https://github.com/malgo-lang/malgo/issues/172) from malgo-lang:prettyprinter
- Merge pull request [#171](https://github.com/malgo-lang/malgo/issues/171) from malgo-lang:remove-unused-operator
- Merge pull request [#170](https://github.com/malgo-lang/malgo/issues/170) from malgo-lang:call-graph
- Merge pull request [#169](https://github.com/malgo-lang/malgo/issues/169) from malgo-lang:refactor-id
- Merge pull request [#168](https://github.com/malgo-lang/malgo/issues/168) from malgo-lang:new-call-semantics
- Merge pull request [#167](https://github.com/malgo-lang/malgo/issues/167) from malgo-lang/update-ghc9.6.2
- Merge pull request [#166](https://github.com/malgo-lang/malgo/issues/166) from malgo-lang:effectful
- Merge pull request [#165](https://github.com/malgo-lang/malgo/issues/165) from malgo-lang:fix-codesize
- Merge pull request [#163](https://github.com/malgo-lang/malgo/issues/163) from malgo-lang:new-llvm-backend
- Merge pull request [#162](https://github.com/malgo-lang/malgo/issues/162) from malgo-lang:detect-bottleneck
- Merge pull request [#161](https://github.com/malgo-lang/malgo/issues/161) from malgo-lang/ghc-9.6.1
- Merge pull request [#160](https://github.com/malgo-lang/malgo/issues/160) from malgo-lang:ghc-9.4.4
- Merge pull request [#158](https://github.com/malgo-lang/malgo/issues/158) from malgo-lang/bench
- Merge pull request [#157](https://github.com/malgo-lang/malgo/issues/157) from malgo-lang/flat-koriel
- Merge pull request [#156](https://github.com/malgo-lang/malgo/issues/156) from malgo-lang:rust-runtime
- Merge pull request [#155](https://github.com/malgo-lang/malgo/issues/155) from malgo-lang:refactor-typecheck
- Merge pull request [#154](https://github.com/malgo-lang/malgo/issues/154) from malgo-lang:koriel-typecheck
- Merge pull request [#153](https://github.com/malgo-lang/malgo/issues/153) from malgo-lang/koriel-parser
- Merge pull request [#152](https://github.com/malgo-lang/malgo/issues/152) from malgo-lang/flat-typerep
- Merge pull request [#151](https://github.com/malgo-lang/malgo/issues/151) from malgo-lang/remove-scheme
- Merge pull request [#150](https://github.com/malgo-lang/malgo/issues/150) from malgo-lang/nix-flakes
- Merge pull request [#149](https://github.com/malgo-lang/malgo/issues/149) from malgo-lang/simplify-constraints


<a name="v0.3.0"></a>
## [v0.3.0] - 2022-11-30
### Chore
- add .github/release.yml
- update devcontainer.json
- small change
- fix Dockerfile
- move Dockerfile
- update Dockerfile
- add /root/.cabal/bin to PATH
- update Dockerfile
- compile hls
- Update CI
- Delete old script
- remove app/Main.hs
- Delete unused scripts
- remove unused scripts
- move to app/malgo
- Build on Docker
- remove unnecessary import

### Error
- module name and source name are mismatched

### Feat
- dump Core IR as JSON
- `malgo build` uses JSON configuration file
- simplify compile options
- more effective inline-expansion and uncurrying
- disable lambda-lifiting
- change default inline size

### Fix
- fix concatenation of the result of pkg-config

### Refactor
- split MalgoM, RnState and DsState into modules
- remove used import
- use DerivingAnyClass
- remove unused LANGUAGE pragma
- use DerivingAnyClass and DervingVia
- remove pretty-simple
- rename some fields of the compile option
- remove storeInterface
- rename _dstPath to dstPath
- rename _srcPath to srcPath
- rename _srcPath to srcPath
- remove unused field
- fix curryFun
- newCodeGenEnv
- rename srcName to srcPath (app/Main.hs)
- rename srcName to srcPath
- Use MalgoEnv in Koriel.Core.CodeGen.LLVM

### Style
- formatting

### Test
- Rewrite test scripts in Haskell
- Rewrite test-nolift.sh in Haskell
- Rewrite test.sh in Haskell
- Rewrite pretest.sh in Haskell
- add testcases/malgo/Factorial.mlg

### Reverts
- Move source codes and others to malgo-compiler

### Pull Requests
- Merge pull request [#148](https://github.com/malgo-lang/malgo/issues/148) from malgo-lang:refactor-cache
- Merge pull request [#147](https://github.com/malgo-lang/malgo/issues/147) from malgo-lang:serialise-json
- Merge pull request [#146](https://github.com/malgo-lang/malgo/issues/146) from malgo-lang:no-script-test
- Merge pull request [#145](https://github.com/malgo-lang/malgo/issues/145) from malgo-lang:no-script-test
- Merge pull request [#144](https://github.com/malgo-lang/malgo/issues/144) from malgo-lang/new-compile-command
- Merge pull request [#143](https://github.com/malgo-lang/malgo/issues/143) from malgo-lang:refactor-codegen
- Merge pull request [#142](https://github.com/malgo-lang/malgo/issues/142) from malgo-lang/linker
- Merge pull request [#141](https://github.com/malgo-lang/malgo/issues/141) from malgo-lang:codegen-scheme
- Merge pull request [#140](https://github.com/malgo-lang/malgo/issues/140) from malgo-lang:split-language-server
- Merge pull request [#139](https://github.com/malgo-lang/malgo/issues/139) from malgo-lang:codegen-scheme
- Merge pull request [#138](https://github.com/malgo-lang/malgo/issues/138) from malgo-lang/def-keyword
- Merge pull request [#137](https://github.com/malgo-lang/malgo/issues/137) from malgo-lang/no-field-selectors
- Merge pull request [#136](https://github.com/malgo-lang/malgo/issues/136) from malgo-lang:match-one
- Merge pull request [#135](https://github.com/malgo-lang/malgo/issues/135) from malgo-lang/hash-table
- Merge pull request [#134](https://github.com/malgo-lang/malgo/issues/134) from malgo-lang/annotated
- Merge pull request [#133](https://github.com/malgo-lang/malgo/issues/133) from malgo-lang/bidir
- Merge pull request [#132](https://github.com/malgo-lang/malgo/issues/132) from malgo-lang/new-unboxed-checker


<a name="v0.2.0"></a>
## [v0.2.0] - 2022-07-05
### Chore
- update stack.yaml
- update stack.yaml
- update malgo_ci.yml
- remove templates
- update README.md
- update README.md
- update stack.yaml
- add scripts/bench.sh
- 10-queen
- add Fib.mlg and NQueen.mlg
- update examples
- update templates/runtime.c.mustache
- update resolver
- remove cabal.project.freeze
- update hie.yaml
- move malgo/* to root
- remove cabal.project.freeze and use cabal in hls instead of stack
- update malgo_ci.yml
- update malgo_ci.yml
- update hie.yaml
- use stack
- use stack
- update hie.yaml
- update stack.yaml
- update cabal.project.freeze
- add `pretty-simple` to dependencies

### Doc
- add comments

### Docs
- add comments
- update scripts/README.md

### Example
- update List.mlg

### Feat
- add some functions to Builtin.mlg and Prelude.mlg
- try to run new desugar-pass always
- `mlgToCore` supports Level1_Int.mlg
- compile.sh now recognize options to malgo
- inline trivial function application
- `{ a }` is the syntax sugar for `() -> a`
- error on string literal pattern
- check that numbers of patterns are same
- `{ | A -> ... | B -> ... }` is also available
- boxed literal pattern
- improve readability of type errors
- do not generalize `let` bindings
- with statement
- `Builtin.panic : String -> a`
- ProgramBuilderT
- use `;` outside `{}`
- type annotation for expressions
- add `eqString` and `substring`
- add --dump-refine option
- Implement Refine pass for class and impl

### Fix
- missing os field
- external global variable
- update testcases
- change the key of `interned` to `(Int, ModuleName)`
- `[]` pattern
- improve performance of `checkInlineable`
- use Control.Monad.Trans.Writer.CPS
- malgo_panic returns void*
- fix `malgo_string_append`
- add space to pPrint TyConApp
- prettyprint TyVar
- support `()` pattern
- add missing `cast`
- handle right-associative operators correctly
- resolve fprintf warning
- resolve the parser performance issue about `expr : type`
- fix `groupTuple`
- change llvm-hs version to 9.0.1
- generate initialization of toplevel variables correctly
- prettyprint `bug $ Unreachable reason`
- desugar groups of `ScDef` correctly

### Refactor
- merge `freevars*`
- move `lookup` functions to Rename.Env
- fix Pretty instances
- split `tcScDefs` to some subunits
- remove TyDArr
- remove `class`, `impl`, `=>`
- remove unused parameter
- split `ExtCall` to `RawCall` and `ExtFunc`
- `DsEnv` is superset of `TcEnv`
- rename `Infer` to `TypeCheck`
- add comments
- add signatures
- hand-written zonk
- point-free
- more readable paramter names
- `TyLazy` -> `TyBlock`
- hand-written Plated instance
- merge `Static` and `UTerm`
- add some comments and small changes
- With -> Annotated
- hlint
- use relude
- rename `newLocalId` and `newGlobalId` with `newInternalId` and `newExternalId`
- refactor TcEnv
- reduce usage of `TemplateHaskell`
- newtype `MalgoM`
- use `Text`
- simplify `With`
- new design of `With` and some minor changes
- remove `foldMapA`
- use `error` instead of `bug`
- remove `asumMap` and `<<$>>`
- `MonadCodeGen` and `runCodeGenT`
- remove duplicate codes
- MonadExpBuilder
- add new ir
- rename `defs` with `defaults`
- rename `newLocalId` and `newGlobalId` with `newInternalId` and `newExternalId`
- simplify `unify`
- organize type classes
- walkOn is Traversal
- buildTyArr
- TyConApp
- minor change
- merge `liftUnify` and `unify`
- replace `NonEmpty (Stmt x)` with `Seq`
- fix the behavior of UTerm.viewTyConApp
- move Malgo/{UTerm.hs, Unify.hs} to Malgo/Infer/
- rename `TypeCheck` to `Infer`
- simplify AST for `impl` and `class`

### Style
- format

### Test
- `stack test` runs tests as test_malgo_parallel.sh
- add testcase
- add test cases
- add test case
- add `Spec.hs`
- add `Punctuate`
- add malgo-test package
- add boxed literal pattern test
- add test case
- update InvalidPattern.mlg
- add test case
- add test cases
- add test cases
- add test script
- update testcase
- update testcase

### Wip
- disable warnings for Malgo.Core
- Implement MlgToCore for Level6_Pair.mlg and Level7_List.mlg
- Support Level5_ExtFunc.mlg
- Support Level3_Func.mlg
- implement core-to-js pass
- disable warnings
- mlgToCore
- merge TypeRep
- add Malgo.Core.MlgToCore and rename `id` to `identity`
- add Malgo.Core.Syntax

### Reverts
- [WIP] makeFieldsNoPrefix Id
- [WIP] Define typelevel function (type constructor), TyAbs

### Pull Requests
- Merge pull request [#130](https://github.com/malgo-lang/malgo/issues/130) from malgo-lang:refactor-test
- Merge pull request [#129](https://github.com/malgo-lang/malgo/issues/129) from malgo-lang:datadef-bug-fix
- Merge pull request [#125](https://github.com/malgo-lang/malgo/issues/125) from malgo-lang:document-symbols
- Merge pull request [#126](https://github.com/malgo-lang/malgo/issues/126) from malgo-lang:parser-improve
- Merge pull request [#124](https://github.com/malgo-lang/malgo/issues/124) from malgo-lang:temporal-id
- Merge pull request [#123](https://github.com/malgo-lang/malgo/issues/123) from malgo-lang/lsp-opt
- Merge pull request [#122](https://github.com/malgo-lang/malgo/issues/122) from malgo-lang/refactor-opt
- Merge pull request [#121](https://github.com/malgo-lang/malgo/issues/121) from malgo-lang:lsp-definitions
- Merge pull request [#120](https://github.com/malgo-lang/malgo/issues/120) from malgo-lang:improve-hover
- Merge pull request [#119](https://github.com/malgo-lang/malgo/issues/119) from malgo-lang:language-server-hover
- Merge pull request [#118](https://github.com/malgo-lang/malgo/issues/118) from malgo-lang:update-packages
- Merge pull request [#117](https://github.com/malgo-lang/malgo/issues/117) from malgo-lang:lsp-server
- Merge pull request [#116](https://github.com/malgo-lang/malgo/issues/116) from malgo-lang:refactor-fields
- Merge pull request [#114](https://github.com/malgo-lang/malgo/issues/114) from malgo-lang/make-fields
- Merge pull request [#113](https://github.com/malgo-lang/malgo/issues/113) from malgo-lang/remove-malgo-core
- Merge pull request [#109](https://github.com/malgo-lang/malgo/issues/109) from malgo-lang/add-document-comments
- Merge pull request [#108](https://github.com/malgo-lang/malgo/issues/108) from malgo-lang/test-in-haskell
- Merge pull request [#107](https://github.com/malgo-lang/malgo/issues/107) from malgo-lang/update-prelude
- Merge pull request [#106](https://github.com/malgo-lang/malgo/issues/106) from malgo-lang/simplify-code
- Merge pull request [#105](https://github.com/malgo-lang/malgo/issues/105) from malgo-lang/mlg-core
- Merge pull request [#103](https://github.com/malgo-lang/malgo/issues/103) from malgo-lang/improve-zonk
- Merge pull request [#104](https://github.com/malgo-lang/malgo/issues/104) from malgo-lang/benchmark
- Merge pull request [#102](https://github.com/malgo-lang/malgo/issues/102) from malgo-lang/refactor-desugar
- Merge pull request [#101](https://github.com/malgo-lang/malgo/issues/101) from malgo-lang/simple-block
- Merge pull request [#100](https://github.com/malgo-lang/malgo/issues/100) from malgo-lang/fix-pattern
- Merge pull request [#99](https://github.com/malgo-lang/malgo/issues/99) from malgo-lang/boxed-pattern
- Merge pull request [#96](https://github.com/malgo-lang/malgo/issues/96) from malgo-lang/with
- Merge pull request [#95](https://github.com/malgo-lang/malgo/issues/95) from malgo-lang/use-relude
- Merge pull request [#88](https://github.com/malgo-lang/malgo/issues/88) from malgo-lang/refactor-infer
- Merge pull request [#94](https://github.com/malgo-lang/malgo/issues/94) from malgo-lang/merge_koriel_malgo
- Merge pull request [#90](https://github.com/malgo-lang/malgo/issues/90) from malgo-lang/refactor-koriel
- Merge pull request [#92](https://github.com/malgo-lang/malgo/issues/92) from malgo-lang/refactor-match
- Merge pull request [#91](https://github.com/malgo-lang/malgo/issues/91) from malgo-lang/fix-match
- Merge pull request [#87](https://github.com/malgo-lang/malgo/issues/87) from malgo-lang/sequence-expr
- Merge pull request [#86](https://github.com/malgo-lang/malgo/issues/86) from malgo-lang/type-annotation
- Merge pull request [#85](https://github.com/malgo-lang/malgo/issues/85) from malgo-lang/refactor-infer
- Merge pull request [#84](https://github.com/malgo-lang/malgo/issues/84) from malgo-lang/fix-toplevel-variable
- Merge pull request [#83](https://github.com/malgo-lang/malgo/issues/83) from malgo-lang/typecheck-infer
- Merge pull request [#82](https://github.com/malgo-lang/malgo/issues/82) from malgo-lang/refactor-unreachable
- Merge pull request [#79](https://github.com/malgo-lang/malgo/issues/79) from malgo-lang/eq-class
- Merge pull request [#81](https://github.com/malgo-lang/malgo/issues/81) from malgo-lang/fix-layout
- Merge pull request [#80](https://github.com/malgo-lang/malgo/issues/80) from malgo-lang/fix-toplevel-application
- Merge pull request [#78](https://github.com/malgo-lang/malgo/issues/78) from malgo-lang/record-semicolon
- Merge pull request [#77](https://github.com/malgo-lang/malgo/issues/77) from malgo-lang/toplevel-variable
- Merge pull request [#75](https://github.com/malgo-lang/malgo/issues/75) from malgo-lang/refactor-core
- Merge pull request [#74](https://github.com/malgo-lang/malgo/issues/74) from malgo-lang/type-as-prefix
- Merge pull request [#73](https://github.com/malgo-lang/malgo/issues/73) from malgo-lang/qualified-type
- Merge pull request [#72](https://github.com/malgo-lang/malgo/issues/72) from malgo-lang/space
- Merge pull request [#71](https://github.com/malgo-lang/malgo/issues/71) from malgo-lang/unreachable
- Merge pull request [#70](https://github.com/malgo-lang/malgo/issues/70) from malgo-lang/improve-message
- Merge pull request [#68](https://github.com/malgo-lang/malgo/issues/68) from malgo-lang/list-syntax
- Merge pull request [#67](https://github.com/malgo-lang/malgo/issues/67) from malgo-lang/new-runtime
- Merge pull request [#65](https://github.com/malgo-lang/malgo/issues/65) from malgo-lang/split-testcases
- Merge pull request [#64](https://github.com/malgo-lang/malgo/issues/64) from malgo-lang/new-module-syntax
- Merge pull request [#63](https://github.com/malgo-lang/malgo/issues/63) from malgo-lang/refactor-typechecker
- Merge pull request [#62](https://github.com/malgo-lang/malgo/issues/62) from malgo-lang/fix-type-checker
- Merge pull request [#60](https://github.com/malgo-lang/malgo/issues/60) from malgo-lang/fix-multiple-parameterized-types
- Merge pull request [#58](https://github.com/malgo-lang/malgo/issues/58) from malgo-lang/refactor-unify
- Merge pull request [#57](https://github.com/malgo-lang/malgo/issues/57) from malgo-lang/remove_language_prefix
- Merge pull request [#55](https://github.com/malgo-lang/malgo/issues/55) from takoeight0821/new-module-syntax
- Merge pull request [#56](https://github.com/malgo-lang/malgo/issues/56) from takoeight0821/add_architecture_doc
- Merge pull request [#54](https://github.com/malgo-lang/malgo/issues/54) from takoeight0821/improve-ci
- Merge pull request [#53](https://github.com/malgo-lang/malgo/issues/53) from takoeight0821/new-driver
- Merge pull request [#52](https://github.com/malgo-lang/malgo/issues/52) from takoeight0821/experiment-syntax
- Merge pull request [#51](https://github.com/malgo-lang/malgo/issues/51) from takoeight0821/remove-llvm-hs-pretty
- Merge pull request [#49](https://github.com/malgo-lang/malgo/issues/49) from takoeight0821/record-pattern
- Merge pull request [#48](https://github.com/malgo-lang/malgo/issues/48) from takoeight0821/add-record
- Merge pull request [#47](https://github.com/malgo-lang/malgo/issues/47) from takoeight0821/refactoring
- Merge pull request [#46](https://github.com/malgo-lang/malgo/issues/46) from takoeight0821/better-error-message
- Merge pull request [#45](https://github.com/malgo-lang/malgo/issues/45) from takoeight0821/fix-kind-check
- Merge pull request [#43](https://github.com/malgo-lang/malgo/issues/43) from takoeight0821/better-code-generation
- Merge pull request [#42](https://github.com/malgo-lang/malgo/issues/42) from takoeight0821/clean-code
- Merge pull request [#41](https://github.com/malgo-lang/malgo/issues/41) from takoeight0821/simple-typechecker
- Merge pull request [#40](https://github.com/malgo-lang/malgo/issues/40) from takoeight0821/type-synonym
- Merge pull request [#39](https://github.com/malgo-lang/malgo/issues/39) from takoeight0821/refactoring
- Merge pull request [#38](https://github.com/malgo-lang/malgo/issues/38) from takoeight0821/better-error-message
- Merge pull request [#37](https://github.com/malgo-lang/malgo/issues/37) from takoeight0821/refactor-new-typechecker
- Merge pull request [#36](https://github.com/malgo-lang/malgo/issues/36) from takoeight0821/fix-scheme-equiv-check
- Merge pull request [#35](https://github.com/malgo-lang/malgo/issues/35) from takoeight0821/use-unordered-containers
- Merge pull request [#24](https://github.com/malgo-lang/malgo/issues/24) from takoeight0821/griff-to-malgo
- Merge pull request [#34](https://github.com/malgo-lang/malgo/issues/34) from takoeight0821/add-new-typechecker
- Merge pull request [#32](https://github.com/malgo-lang/malgo/issues/32) from takoeight0821/new-build-system
- Merge pull request [#31](https://github.com/malgo-lang/malgo/issues/31) from takoeight0821/ast_json
- Merge pull request [#29](https://github.com/malgo-lang/malgo/issues/29) from takoeight0821/add-new-ir-syntax
- Merge pull request [#27](https://github.com/malgo-lang/malgo/issues/27) from takoeight0821/add-documents
- Merge pull request [#26](https://github.com/malgo-lang/malgo/issues/26) from takoeight0821/more-restricted-syntax
- Merge pull request [#25](https://github.com/malgo-lang/malgo/issues/25) from takoeight0821/more-restricted-syntax


<a name="v0.1.0"></a>
## v0.1.0 - 2021-01-23
### Reverts
- typedを実装
- delete Kind
- 自由変数に自分自身を含めないように変更
- ヴァリアントをparseまで実装
- 組み込み型を追加
- Preludeを自前で作成
- outputableを一旦合併
- ランク1多相完全理解
- insertLetの機能を変更
- 設計の見直し
- カリー化した関数をデフォルトに
- 古いexamplesを移動
- 一旦push
- リファクタリング!!!!!

### Pull Requests
- Merge pull request [#23](https://github.com/malgo-lang/malgo/issues/23) from takoeight0821/add-pointer-type
- Merge pull request [#21](https://github.com/malgo-lang/malgo/issues/21) from takoeight0821/add_boxed_literal
- Merge pull request [#20](https://github.com/malgo-lang/malgo/issues/20) from takoeight0821/improve-module-path
- Merge pull request [#19](https://github.com/malgo-lang/malgo/issues/19) from takoeight0821/fix-parse-parens
- Merge pull request [#18](https://github.com/malgo-lang/malgo/issues/18) from takoeight0821/fix-typecheck-nested-let
- Merge pull request [#17](https://github.com/malgo-lang/malgo/issues/17) from takoeight0821/fix-module-handling
- Merge pull request [#16](https://github.com/malgo-lang/malgo/issues/16) from takoeight0821/primitive-functions
- Merge pull request [#15](https://github.com/malgo-lang/malgo/issues/15) from takoeight0821/add-unboxed-kind
- Merge pull request [#4](https://github.com/malgo-lang/malgo/issues/4) from takoeight0821/myownprelude
- Merge pull request [#3](https://github.com/malgo-lang/malgo/issues/3) from takoeight0821/lambda
- Merge pull request [#2](https://github.com/malgo-lang/malgo/issues/2) from takoeight0821/rewrite_monad
- Merge pull request [#1](https://github.com/malgo-lang/malgo/issues/1) from takoeight0821/rewrite


[Unreleased]: https://github.com/malgo-lang/malgo/compare/v2.0.0...HEAD
[v2.0.0]: https://github.com/malgo-lang/malgo/compare/v1.0.0...v2.0.0
[v1.0.0]: https://github.com/malgo-lang/malgo/compare/v0.3.0...v1.0.0
[v0.3.0]: https://github.com/malgo-lang/malgo/compare/v0.2.0...v0.3.0
[v0.2.0]: https://github.com/malgo-lang/malgo/compare/v0.1.0...v0.2.0
