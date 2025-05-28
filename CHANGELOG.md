## [](https://github.com/malgo-lang/malgo/compare/v2.0.0...v) (2025-05-28)

### ⚠ BREAKING CHANGES

* **build:** remove unsupported GHC versions from matrix
* **package:** bump version to 2.0.0 and update copyright year

### Miscellaneous Chores

* **package:** bump version to 2.0.0 and update copyright year ([fefa0a1](https://github.com/malgo-lang/malgo/commit/fefa0a19dc4f9e7455e518e6b1fc340adfd76eb3))

### Continuous Integration

* **build:** remove unsupported GHC versions from matrix ([28a7348](https://github.com/malgo-lang/malgo/commit/28a7348ab8741c3c61add7b62c9c8bba35ebd9d3))
## [2.0.0](https://github.com/malgo-lang/malgo/compare/v1.0.0...v2.0.0) (2025-05-27)

### ⚠ BREAKING CHANGES

* **changelog:** restructure changelog for clarity and organization
* **changelog:** add changelog configuration and template files
* **cabal:** update dependency versions
* **infer:** update type synonym handling with kind checks
* **infer:** introduce Kind module and integrate into TypeRep
* **errors:** add error cases for parser
* **Main:** simplify EvalOpt and remove unused code
* **Driver:** implement generateSequent and linkSequent functions
* **Program:** add dependencies field to Program data structure
* **Eval, Interface, Fun:** remove unused GHC options
* **test:** remove .malgo-work directory if it exists
* add path-based import
* change workspace directory structure
* remove (old) LSP server and build system
* delete syntax for module with specified name

### Features

* add path-based import ([9f357c0](https://github.com/malgo-lang/malgo/commit/9f357c0904b4d00515c4795339fcf66124e70a84))
* add pragma support and update related tests ([8122ff3](https://github.com/malgo-lang/malgo/commit/8122ff34b63ca4ce1679cb8a0af18e6c4c2dd5cc))
* delete syntax for module with specified name ([723393e](https://github.com/malgo-lang/malgo/commit/723393e3541bae4100c77e47d272837da0d17fd6))
* **Driver:** implement generateSequent and linkSequent functions ([2fc4f2e](https://github.com/malgo-lang/malgo/commit/2fc4f2ecb5bab0278db8555cf3f9193a6c51175e))
* **infer:** add error handling for invalid type applications ([1a81eac](https://github.com/malgo-lang/malgo/commit/1a81eac7016056a88ef82f5fea84790d32a5a63c))
* **infer:** introduce Kind module and integrate into TypeRep ([5eb17b0](https://github.com/malgo-lang/malgo/commit/5eb17b0f87d5ed11558487b7a3ebc3946a955797))
* **parser:** enhance handling of pragma ([c330f2e](https://github.com/malgo-lang/malgo/commit/c330f2edce8b8e7aed8ce111a64ec9a93eeb5702))
* **Program:** add dependencies field to Program data structure ([f295a8f](https://github.com/malgo-lang/malgo/commit/f295a8fd405eb57ee343a4290065a43dc9ca73af))
* remove (old) LSP server and build system ([db969c7](https://github.com/malgo-lang/malgo/commit/db969c70b191b878f202feb614e0a36836354eaf))

### Bug Fixes

* handle MainNotFound error in evalProgram and improve error reporting ([d8ac92a](https://github.com/malgo-lang/malgo/commit/d8ac92a7a1105af6598fecbdb0b867f9e3b75efa))
* handle Record capture correctly ([974997d](https://github.com/malgo-lang/malgo/commit/974997d81091c99d15899dbdbfd10ce38cb13c84))
* **Main:** simplify EvalOpt and remove unused code ([41afce9](https://github.com/malgo-lang/malgo/commit/41afce90e3a71fbbcd9973777485d8977b9359d4))
* **parser:** add '.' to reserved operators list ([efda8a3](https://github.com/malgo-lang/malgo/commit/efda8a3e836de385779d7b401d74087175b36bed))
* **parser:** fix unary operator parsing and type variable handling ([0f14ccb](https://github.com/malgo-lang/malgo/commit/0f14ccb9bb7a95520981ca69de9418e0829035da))
* **parser:** update parsing output format in NewParserSpec ([fe3026a](https://github.com/malgo-lang/malgo/commit/fe3026afa3ca34fa5660e3e1db742dfcf0f0a87a))
* **prelude:** improve pretty printing of Range instances ([80f06d4](https://github.com/malgo-lang/malgo/commit/80f06d49a3a1789e77c08cba4cd3d4dd38a2376e))
* remove absolute path from Show instance of ArtifactPath ([785df32](https://github.com/malgo-lang/malgo/commit/785df3272faddfb1384bbc7601746e39521f202d))
* **rename:** fix constructor pattern handling in rnClause ([a606937](https://github.com/malgo-lang/malgo/commit/a60693744b7d15ae41f38b9b450dd11fc53de401))
* update imports for Prettyprinter in multiple files ([a984084](https://github.com/malgo-lang/malgo/commit/a984084d1db4f65b3e74269e9b2d3a6a1d0fc8d2))
* use Map and Set instead of HashMap and HashSet ([4d009b5](https://github.com/malgo-lang/malgo/commit/4d009b5857761b8844dc9c27566dc2dd7c454d24))

### Reverts

* Revert "Add Windows testing job to Nix workflow" ([e43d548](https://github.com/malgo-lang/malgo/commit/e43d5482c23859fcbaf93102c55ca752722df1c5))
* Revert "generate changelog" ([3c0443c](https://github.com/malgo-lang/malgo/commit/3c0443cdcf23a6da9ea2b88d040ed917d9620d16))

### Styles

* **Eval, Interface, Fun:** remove unused GHC options ([eeca0b0](https://github.com/malgo-lang/malgo/commit/eeca0b0afc8e27913892eaf035d807ba7c653fb7))

### Miscellaneous Chores

* **cabal:** update dependency versions ([96ed655](https://github.com/malgo-lang/malgo/commit/96ed6551776c1e9bd7d345365ae0c9ea06f28cf0))
* **changelog:** add changelog configuration and template files ([4987760](https://github.com/malgo-lang/malgo/commit/49877609756b6e4c58b04a0fffa57aa90fafa137))
* **changelog:** restructure changelog for clarity and organization ([1989ae5](https://github.com/malgo-lang/malgo/commit/1989ae59594681a133da1e46e6c8c295fff57c25))
* **test:** remove .malgo-work directory if it exists ([a256f73](https://github.com/malgo-lang/malgo/commit/a256f73df89a20792820c1aa49f8724a3b9fe332))

### Code Refactoring

* change workspace directory structure ([ebca7ec](https://github.com/malgo-lang/malgo/commit/ebca7ecae41bf3eab6f1e0fad04de9c2f0068150))
* **infer:** update type synonym handling with kind checks ([713137d](https://github.com/malgo-lang/malgo/commit/713137deb772fe46effab59ede6dd9503310b598))

### Tests

* **errors:** add error cases for parser ([1bfb6fe](https://github.com/malgo-lang/malgo/commit/1bfb6fec6b9aff8fe983de44a26232ee8fa69004))
## [1.0.0](https://github.com/malgo-lang/malgo/compare/v0.3.0...v1.0.0) (2024-04-20)

### Features

* .kor -> .kor.bin ([0ae4537](https://github.com/malgo-lang/malgo/commit/0ae45370fec64bbbe3d84941e533298df7297b22))
* `malgo build` builds each files concurrently ([1f96e7e](https://github.com/malgo-lang/malgo/commit/1f96e7e46780e95388b2b710e7f620fa0d8a743e))
* Add `=` expression ([8f8f07a](https://github.com/malgo-lang/malgo/commit/8f8f07ae06c570a523c53eaf5c45f66aa3079572))
* add `switch-unboxed` ([dd04fb7](https://github.com/malgo-lang/malgo/commit/dd04fb7ccf375ee4825f0e5ee75b1438344424f0))
* Add default case to `Switch` ([d55aa08](https://github.com/malgo-lang/malgo/commit/d55aa0854ff874a3ee6671d4ec508d6673d18ba3))
* add helper function malgo_print_tag ([1e2f39a](https://github.com/malgo-lang/malgo/commit/1e2f39a868d7c2c8e0cf08dc976ee15865cacf65))
* Add lint rule 'all cases have same type of pattern' ([b1206fd](https://github.com/malgo-lang/malgo/commit/b1206fddce10c1112f907b838a8f7ad834702e58))
* add new runtime ([aa0eb0f](https://github.com/malgo-lang/malgo/commit/aa0eb0ff736868ed67eb5f2d0515e5c7f1636155))
* add rust runtime ([dd0a10d](https://github.com/malgo-lang/malgo/commit/dd0a10dc0cde078e3aabea95151732cde3634f02))
* Annotate Koriel Program ([723819f](https://github.com/malgo-lang/malgo/commit/723819f5e89870470737c8bb99a1552f6d7a048a))
* convert `Exact` to `SwitchUnboxed` ([e7094ec](https://github.com/malgo-lang/malgo/commit/e7094ec354ef5e47c310710874280ddd5c8ccf21))
* convert `match` to `destruct-record` if it is possible ([3519d44](https://github.com/malgo-lang/malgo/commit/3519d44479764ab98ed618ccc281743a6925c05f))
* convert `match` to `switch` if it is possible ([05edb0c](https://github.com/malgo-lang/malgo/commit/05edb0c2b746aa19eeddba181d4532495933eb43))
* Generate global function's closure in global scope ([301bc23](https://github.com/malgo-lang/malgo/commit/301bc23473063cbc7defc512bb037f1dcc9c870f))
* Generate no-optimized .kor file ([47fc781](https://github.com/malgo-lang/malgo/commit/47fc781cd4713090e5f07629ab507978d66e470a))
* implement allocaresult ([3b0b891](https://github.com/malgo-lang/malgo/commit/3b0b8919c5659b807f4525ea0bfe3a7fcc08e3e9))
* Implement Koriel Parser ([dd2b69b](https://github.com/malgo-lang/malgo/commit/dd2b69b4dc1ad9e4f5b9c6fd07e40fb667b70987))
* imported module's functions are not external ([1409107](https://github.com/malgo-lang/malgo/commit/1409107054b66bc8209dc4480ca6643962d838bd))
* improve Koriel pretty printer ([e99144f](https://github.com/malgo-lang/malgo/commit/e99144f174103f31703203e45bfa1bf42996d7e2))
* inline gblcls ([b8e806e](https://github.com/malgo-lang/malgo/commit/b8e806e90d5740d025e8f241477b32b0e8a16e6e))
* Koriel.Core.Flat uses `destruct` ([8484c50](https://github.com/malgo-lang/malgo/commit/8484c502ddbd1782ca0ec0668fcbf045c22700fa))
* Match to Switch conversion ([7693df2](https://github.com/malgo-lang/malgo/commit/7693df2175cbd922050fe95283a7f594cea09487))
* more descriptive label ([25ec138](https://github.com/malgo-lang/malgo/commit/25ec138e04c1b7d6dab0d4934878eb4ddacf56de))
* new normalization pass ([10b3ddc](https://github.com/malgo-lang/malgo/commit/10b3ddccbe3523ce4c109eff227fc11ea8725243))
* remove noop `destruct` ([8942072](https://github.com/malgo-lang/malgo/commit/8942072af7c2d4ac5c42ff0a99ca0d334e935cc7))
* Replace `bind` with `=` in several places. ([9cba13a](https://github.com/malgo-lang/malgo/commit/9cba13a0933fd9f888aab46ecff7320741d67088))
* unique string value ([9d62a80](https://github.com/malgo-lang/malgo/commit/9d62a80909dce1ebdf05cef4eedac4b1f5a78280))
* update llvm-hs to 15 ([4a7ae93](https://github.com/malgo-lang/malgo/commit/4a7ae93f61e678448023e9ed3f9b82d201ab1c0b))

### Bug Fixes

* Convert `Call` to `CallDirect` if necessary. ([b5d00a9](https://github.com/malgo-lang/malgo/commit/b5d00a93a675e6401426fa3b1f8c17fcc0677e1a))
* enable tests ([1866ffb](https://github.com/malgo-lang/malgo/commit/1866ffb2c27ab3ac20bdd6ada140a568227c4213))
* fix 'Main' ([057aab1](https://github.com/malgo-lang/malgo/commit/057aab10c55ec540487a2745aa6e79f00accd160))
* fix CI ([7d22bfa](https://github.com/malgo-lang/malgo/commit/7d22bfaa73555a917e2dee97a0f34a27f9c40188))
* fix error cases filepath ([e221d33](https://github.com/malgo-lang/malgo/commit/e221d33c3de1851f53590d0db4dcf61c5264fcc9))
* fix getClangCommand ([2629dd6](https://github.com/malgo-lang/malgo/commit/2629dd6462c90563fcec3fa0dba7daca301ed2eb))
* fix lint ([bc638d8](https://github.com/malgo-lang/malgo/commit/bc638d88c483cf5767519ce50de7a06162c96627))
* fix tests ([2e7d022](https://github.com/malgo-lang/malgo/commit/2e7d022d989d552458694373bbdcb5fafb2a4dca))
* flatten switch and switch-unboxed correctly ([7f4b5cc](https://github.com/malgo-lang/malgo/commit/7f4b5cc0200d43af5b025de21d5b15915a60c353))
* generate kor.bin ([8fc4fbc](https://github.com/malgo-lang/malgo/commit/8fc4fbce354f2d22bd8b146ba16ec9868555433b))
* reduce lambdalifiting time ([06795f0](https://github.com/malgo-lang/malgo/commit/06795f01341fe2b5e60eebe32b818290eb5ed8a7))
* remove 'noName' ([e322df4](https://github.com/malgo-lang/malgo/commit/e322df4cebde3985a9ec6fff2d874c01ae6b2595))
* Update malgo.cabal ([0805f92](https://github.com/malgo-lang/malgo/commit/0805f920544e9e8ba5f471ffdad1d34f1ccab3f3))
* use `alpha` conversion in inline expanding ([8163945](https://github.com/malgo-lang/malgo/commit/81639452a4a7e0221bba290471c76a534dc61078))
* Use Control.Exception.assert ([8606be7](https://github.com/malgo-lang/malgo/commit/8606be706a88c25ebd5a28d1b5d4ba8bee04cf19))

### Reverts

* Revert "Add Statement (`Stmt`) to `Koriel.Core.Syntax`." ([b69474b](https://github.com/malgo-lang/malgo/commit/b69474b90df0ed576c3bb88ec9d8e655ed0ab3e0))
* Revert "refactor: Simplify CodeGenEnv" ([2726397](https://github.com/malgo-lang/malgo/commit/27263972586cfa4f3161407dcebe99a232f1e79a))
* Revert "refactor: simplify DsState" ([ab67a64](https://github.com/malgo-lang/malgo/commit/ab67a64bb6e43d2e5b85bd114de92921522696c2))
## [0.3.0](https://github.com/malgo-lang/malgo/compare/v0.2.0...v0.3.0) (2022-11-30)

### Features

* `malgo build` uses JSON configuration file ([e71027b](https://github.com/malgo-lang/malgo/commit/e71027b36fc86d14b5af71829bed113d8ccca40c))
* change default inline size ([d1f3094](https://github.com/malgo-lang/malgo/commit/d1f309444c759169431331b30aada688b62edbbf))
* disable lambda-lifiting ([e74e0b7](https://github.com/malgo-lang/malgo/commit/e74e0b7db10d4cbc00cbb08862ba735add6b3cc4))
* dump Core IR as JSON ([6b4419e](https://github.com/malgo-lang/malgo/commit/6b4419e0dc190ca0ae072887597a4644a8498baf))
* more effective inline-expansion and uncurrying ([cd7ac23](https://github.com/malgo-lang/malgo/commit/cd7ac23e42e85a42ca29ba307914db60cb4d3a96))
* simplify compile options ([190216a](https://github.com/malgo-lang/malgo/commit/190216a7da7ca720d00c986935fdc6a19f82a836))

### Bug Fixes

* fix concatenation of the result of pkg-config ([4842c2d](https://github.com/malgo-lang/malgo/commit/4842c2d6c67202b137929b3fd918abdbcffc0f08))

### Reverts

* Revert "Move source codes and others to malgo-compiler" ([9bd8839](https://github.com/malgo-lang/malgo/commit/9bd8839d665e5c3fabe4b1f4a6e9f71ebf539de5))
## [0.2.0](https://github.com/malgo-lang/malgo/compare/v0.1.0...v0.2.0) (2022-07-05)

### Features

* `{ | A -> ... | B -> ... }` is also available ([fb758b3](https://github.com/malgo-lang/malgo/commit/fb758b36028f70576e032f3c26ec217103898437))
* `{ a }` is the syntax sugar for `() -> a` ([1a64392](https://github.com/malgo-lang/malgo/commit/1a6439217bbdaccbe103a06e5fbd67b583df6547))
* `Builtin.panic : String -> a` ([c719abf](https://github.com/malgo-lang/malgo/commit/c719abf4f2312c63672f37e0e4b9894eac17c1b0))
* `mlgToCore` supports Level1_Int.mlg ([984189d](https://github.com/malgo-lang/malgo/commit/984189d918d1fb6482107d27ac7d2e693db00fda))
* add --dump-refine option ([f670c96](https://github.com/malgo-lang/malgo/commit/f670c9657d3bce28ab55ee61e5c6b501572b73d4))
* add `eqString` and `substring` ([6164f1e](https://github.com/malgo-lang/malgo/commit/6164f1ec88783792879e874f2d42c973095c0d71))
* add some functions to Builtin.mlg and Prelude.mlg ([c7b1e7b](https://github.com/malgo-lang/malgo/commit/c7b1e7b9e72fc00910e53aef51c31e3ae405cf32))
* boxed literal pattern ([faac1d1](https://github.com/malgo-lang/malgo/commit/faac1d19d6dd25638db86fdd976581349ed9e528))
* check that numbers of patterns are same ([936f470](https://github.com/malgo-lang/malgo/commit/936f470c033d732078492537ced89c33f50d479e))
* compile.sh now recognize options to malgo ([34599a7](https://github.com/malgo-lang/malgo/commit/34599a7261ea65dc4917564d7d03c269f70c3790))
* do not generalize `let` bindings ([699cca2](https://github.com/malgo-lang/malgo/commit/699cca2498f9ddcaabf572684b3be735b24055da))
* error on string literal pattern ([2cbd1b9](https://github.com/malgo-lang/malgo/commit/2cbd1b9c6acb6414d1c5f46b7d3bab2a849e341c))
* Implement Refine pass for class and impl ([a2d920d](https://github.com/malgo-lang/malgo/commit/a2d920d11f99a223cddd7ef492a11c81f6708372))
* improve readability of type errors ([ba4dc11](https://github.com/malgo-lang/malgo/commit/ba4dc111844990243d188284a3467549fda290e7))
* inline trivial function application ([bd85ac7](https://github.com/malgo-lang/malgo/commit/bd85ac7b6ab7e7d601497573c809bf9febb0949d))
* ProgramBuilderT ([f6c5a37](https://github.com/malgo-lang/malgo/commit/f6c5a375373be3851a3bec21c504230bc9d25f39))
* try to run new desugar-pass always ([2baafd4](https://github.com/malgo-lang/malgo/commit/2baafd455c4cf50e262778bc73381db8d67fd53d))
* type annotation for expressions ([456fb1a](https://github.com/malgo-lang/malgo/commit/456fb1abb2f2328e86f92a4a524f0ca3e9f43078))
* use `;` outside `{}` ([6252fe9](https://github.com/malgo-lang/malgo/commit/6252fe9035abfebb7175f408a7da2c2a2575c1cc))
* with statement ([7c27687](https://github.com/malgo-lang/malgo/commit/7c27687ffa0cc49446c4c00c9a59392d01476ed7))

### Bug Fixes

* `[]` pattern ([cc3c4b5](https://github.com/malgo-lang/malgo/commit/cc3c4b5e42dea288406ee7b0e04a608a01b44526))
* add missing `cast` ([300d516](https://github.com/malgo-lang/malgo/commit/300d516677d460741804ee3f523476f0f21d786b))
* add space to pPrint TyConApp ([eb14845](https://github.com/malgo-lang/malgo/commit/eb14845fa8cf4522d2534b3ba6680afcf14d1121))
* change llvm-hs version to 9.0.1 ([04b4e1a](https://github.com/malgo-lang/malgo/commit/04b4e1a21afd64632a5528f62dd2f014d3438b69))
* change the key of `interned` to `(Int, ModuleName)` ([ab130d2](https://github.com/malgo-lang/malgo/commit/ab130d2acc56034d658bf141c660b18f2a0ee5e7))
* desugar groups of `ScDef` correctly ([3ccd7f7](https://github.com/malgo-lang/malgo/commit/3ccd7f7638085eae7106542a20d3b7025d32346c))
* external global variable ([c452b4b](https://github.com/malgo-lang/malgo/commit/c452b4b1979299641db67ca498f891907c757be7))
* fix `groupTuple` ([87915e1](https://github.com/malgo-lang/malgo/commit/87915e1a63355c45a3e41b654c12d3b9e1dfd97e))
* fix `malgo_string_append` ([86c179a](https://github.com/malgo-lang/malgo/commit/86c179a9795500424441572724f54c22d9d892b4))
* generate initialization of toplevel variables correctly ([0845d9c](https://github.com/malgo-lang/malgo/commit/0845d9cd57acb6029353490da56657aa7eea3209))
* handle right-associative operators correctly ([d5654e3](https://github.com/malgo-lang/malgo/commit/d5654e3a4516db8ef4166f163b418469c0ea17d0))
* improve performance of `checkInlineable` ([f605972](https://github.com/malgo-lang/malgo/commit/f605972c5e1af3690bb40d6035393d6fac5321eb))
* malgo_panic returns void* ([ca9e836](https://github.com/malgo-lang/malgo/commit/ca9e836b3a5d80804fed994a6bd86716e23bd5f3))
* missing os field ([f670e3a](https://github.com/malgo-lang/malgo/commit/f670e3ade4b03dcebbcad0424485a1c876d81f66))
* prettyprint `bug $ Unreachable reason` ([2f78f76](https://github.com/malgo-lang/malgo/commit/2f78f7613f88fbb30dbbbeb37dac7d237e290e2a))
* prettyprint TyVar ([43b8f24](https://github.com/malgo-lang/malgo/commit/43b8f24374e30c3dd2e5388efb682357f13630f7))
* resolve fprintf warning ([172fb25](https://github.com/malgo-lang/malgo/commit/172fb25a6e9a1a83003a80af80aa99b478c4988a))
* resolve the parser performance issue about `expr : type` ([b499a89](https://github.com/malgo-lang/malgo/commit/b499a897271ea32ffe55d5e15aaa0e3138479f58))
* support `()` pattern ([23c8b18](https://github.com/malgo-lang/malgo/commit/23c8b18288013001abf26fac08cad448df64f9fb))
* update testcases ([caf6ee5](https://github.com/malgo-lang/malgo/commit/caf6ee59ab3ab9625e87e6cd3258925264accb61))
* use Control.Monad.Trans.Writer.CPS ([f9a5a7f](https://github.com/malgo-lang/malgo/commit/f9a5a7f410bbe5830741d88ae59567d5581ab393))

### Reverts

* Revert "[WIP] makeFieldsNoPrefix Id" ([015c958](https://github.com/malgo-lang/malgo/commit/015c958cc64f68719f0591db40fa7557280571c5))
* Revert "[WIP] Define typelevel function (type constructor), TyAbs" ([b410ecb](https://github.com/malgo-lang/malgo/commit/b410ecb3ada2f1fd65bb5a365812aa41a80186b4))
* Revert "typedを実装" ([683a3fd](https://github.com/malgo-lang/malgo/commit/683a3fd193b7ccae61216d54d1fb055e65fc9a43))
* Revert "delete Kind" ([f012531](https://github.com/malgo-lang/malgo/commit/f012531e435eb88229ae9f66f6dfbd8957518d1a))
* Revert "自由変数に自分自身を含めないように変更" ([2067292](https://github.com/malgo-lang/malgo/commit/2067292d8843cf6124acaa87fb33ea414816e18d))
* Revert "ヴァリアントをparseまで実装" ([ff58dea](https://github.com/malgo-lang/malgo/commit/ff58deae43131638a59da3de8b797ba7389f640e))
* Revert "組み込み型を追加" ([13067fb](https://github.com/malgo-lang/malgo/commit/13067fb685c55f7fbbd7a1caee552c1b796bdc6e))
* Revert "Preludeを自前で作成" ([c8f3ffa](https://github.com/malgo-lang/malgo/commit/c8f3ffa76521b2c57b9cbabde7a3128bb964c75a))
* Revert "outputableを一旦合併" ([cf16626](https://github.com/malgo-lang/malgo/commit/cf1662638985ecb5a78fa84776f112ae94744ec9))
* Revert "ランク1多相完全理解" ([fc83bee](https://github.com/malgo-lang/malgo/commit/fc83bee7b1c80d8af990ac916a572917bd7feeb4))
* Revert "insertLetの機能を変更" ([e1e5a3e](https://github.com/malgo-lang/malgo/commit/e1e5a3ef87265e16d2f29eea8c3e47b0663ff94e))
* Revert "設計の見直し" ([7d61afa](https://github.com/malgo-lang/malgo/commit/7d61afaa07e3b1b27e4abfb21f9af243c191ad8c))
* Revert "カリー化した関数をデフォルトに" ([8ed41cd](https://github.com/malgo-lang/malgo/commit/8ed41cd75e43409f28d70ae752ba64d13e8c0961))
* Revert "古いexamplesを移動" ([2559441](https://github.com/malgo-lang/malgo/commit/255944194b82d5652128497a017e7187f05e64ab))
* Revert "一旦push" ([1ff8934](https://github.com/malgo-lang/malgo/commit/1ff8934de8fefa4f327611d1ccb76a30f347241a))
* Revert "リファクタリング!!!!!" ([7aae9d1](https://github.com/malgo-lang/malgo/commit/7aae9d1bbc3fb8503a02db5ff04424d4f6748213))
