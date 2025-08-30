# Changelog

## [2.1.0](https://github.com/malgo-lang/malgo/compare/v2.0.0...v2.1.0) (2025-08-30)


### Features

* **parser:** update float parsing to use f32 and f64 suffixes ([8ee2315](https://github.com/malgo-lang/malgo/commit/8ee2315ed45af7bf52fbb37dc7d39acb614590c3))
* **parser:** update integer parsing to support i32 and i64 suffixes ([fe65f1b](https://github.com/malgo-lang/malgo/commit/fe65f1bde23f108a80da77a1ac5b6446cacd8c8b))

## [2.0.0](https://github.com/malgo-lang/malgo/compare/v1.0.0...v2.0.0) (2025-08-30)


### ⚠ BREAKING CHANGES

* **changelog:** remove CHANGELOG.md file
* add copatterns
* **prompts:** add auto-implementing small issues prompt
* **build:** remove unsupported GHC versions from matrix
* **package:** bump version to 2.0.0 and update copyright year
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

* add copatterns ([f484e0f](https://github.com/malgo-lang/malgo/commit/f484e0f2f5d65de72af3675a14b3cb90ba61167a))
* add path-based import ([9f357c0](https://github.com/malgo-lang/malgo/commit/9f357c0904b4d00515c4795339fcf66124e70a84))
* add pragma support and update related tests ([8122ff3](https://github.com/malgo-lang/malgo/commit/8122ff34b63ca4ce1679cb8a0af18e6c4c2dd5cc))
* add Project expression handling in refineExpr ([36b17d6](https://github.com/malgo-lang/malgo/commit/36b17d6d9b6d3fd9b27f031fc0daca6a56f98cac))
* delete syntax for module with specified name ([723393e](https://github.com/malgo-lang/malgo/commit/723393e3541bae4100c77e47d272837da0d17fd6))
* **Driver:** implement generateSequent and linkSequent functions ([2fc4f2e](https://github.com/malgo-lang/malgo/commit/2fc4f2ecb5bab0278db8555cf3f9193a6c51175e))
* **infer:** add error handling for invalid type applications ([1a81eac](https://github.com/malgo-lang/malgo/commit/1a81eac7016056a88ef82f5fea84790d32a5a63c))
* **infer:** introduce Kind module and integrate into TypeRep ([5eb17b0](https://github.com/malgo-lang/malgo/commit/5eb17b0f87d5ed11558487b7a3ebc3946a955797))
* modularize parser architecture with consistent naming ([1319c52](https://github.com/malgo-lang/malgo/commit/1319c5219f236c83e4b239db22f1bc88cb255ff9))
* **parser:** add C-style data definitions and type synonyms ([9cdabe5](https://github.com/malgo-lang/malgo/commit/9cdabe5d7103fbbba4d3c8eaaf6e8827e463d8a7))
* **parser:** enhance handling of pragma ([c330f2e](https://github.com/malgo-lang/malgo/commit/c330f2edce8b8e7aed8ce111a64ec9a93eeb5702))
* **Program:** add dependencies field to Program data structure ([f295a8f](https://github.com/malgo-lang/malgo/commit/f295a8fd405eb57ee343a4290065a43dc9ca73af))
* **prompts:** add auto-implementing small issues prompt ([4a8f898](https://github.com/malgo-lang/malgo/commit/4a8f898dde2572591cf83cf239a1c42404dcbc80))
* remove (old) LSP server and build system ([db969c7](https://github.com/malgo-lang/malgo/commit/db969c70b191b878f202feb614e0a36836354eaf))


### Bug Fixes

* 214: Preserve Parens expressions through rename phase ([2faf6d6](https://github.com/malgo-lang/malgo/commit/2faf6d6e0ffd564279a0a587eeae9faa6b2fcc59))
* correct comment formatting in Wrapper.hs ([abc08a6](https://github.com/malgo-lang/malgo/commit/abc08a690910bbd452db20641efafb4595578c93))
* escape quotes in mise.toml format task ([229e0b0](https://github.com/malgo-lang/malgo/commit/229e0b0d32e594094f156f2f275dfc99a6508046))
* handle MainNotFound error in evalProgram and improve error reporting ([d8ac92a](https://github.com/malgo-lang/malgo/commit/d8ac92a7a1105af6598fecbdb0b867f9e3b75efa))
* handle Record capture correctly ([974997d](https://github.com/malgo-lang/malgo/commit/974997d81091c99d15899dbdbfd10ce38cb13c84))
* **Main:** simplify EvalOpt and remove unused code ([41afce9](https://github.com/malgo-lang/malgo/commit/41afce90e3a71fbbcd9973777485d8977b9359d4))
* Parser correctly uses Parens expression for single-expression parentheses ([4bc77c5](https://github.com/malgo-lang/malgo/commit/4bc77c5c26ecf03c70445fc730df53809156df20))
* Parser now correctly uses Parens expression for single-expression parentheses ([fe6a803](https://github.com/malgo-lang/malgo/commit/fe6a803e6e3cdc48ad5a66013bf070c61210e379)), closes [#212](https://github.com/malgo-lang/malgo/issues/212)
* **parser:** add '.' to reserved operators list ([efda8a3](https://github.com/malgo-lang/malgo/commit/efda8a3e836de385779d7b401d74087175b36bed))
* **parser:** fix unary operator parsing and type variable handling ([0f14ccb](https://github.com/malgo-lang/malgo/commit/0f14ccb9bb7a95520981ca69de9418e0829035da))
* **parser:** update parsing output format in NewParserSpec ([fe3026a](https://github.com/malgo-lang/malgo/commit/fe3026afa3ca34fa5660e3e1db742dfcf0f0a87a))
* **pass:** correct CompileError handling in tests ([3f07d59](https://github.com/malgo-lang/malgo/commit/3f07d591db4395701c9ba373d5942ef3f22c0a05))
* **prelude:** improve pretty printing of Range instances ([80f06d4](https://github.com/malgo-lang/malgo/commit/80f06d49a3a1789e77c08cba4cd3d4dd38a2376e))
* **prompts:** update base branch from main to master ([1d8ac08](https://github.com/malgo-lang/malgo/commit/1d8ac08440a13a05fe69beada054d1728e4187fc))
* remove absolute path from Show instance of ArtifactPath ([785df32](https://github.com/malgo-lang/malgo/commit/785df3272faddfb1384bbc7601746e39521f202d))
* **rename:** fix constructor pattern handling in rnClause ([a606937](https://github.com/malgo-lang/malgo/commit/a60693744b7d15ae41f38b9b450dd11fc53de401))
* update imports for Prettyprinter in multiple files ([a984084](https://github.com/malgo-lang/malgo/commit/a984084d1db4f65b3e74269e9b2d3a6a1d0fc8d2))
* use Map and Set instead of HashMap and HashSet ([4d009b5](https://github.com/malgo-lang/malgo/commit/4d009b5857761b8844dc9c27566dc2dd7c454d24))


### Styles

* **Eval, Interface, Fun:** remove unused GHC options ([eeca0b0](https://github.com/malgo-lang/malgo/commit/eeca0b0afc8e27913892eaf035d807ba7c653fb7))


### Miscellaneous Chores

* **cabal:** update dependency versions ([96ed655](https://github.com/malgo-lang/malgo/commit/96ed6551776c1e9bd7d345365ae0c9ea06f28cf0))
* **changelog:** add changelog configuration and template files ([4987760](https://github.com/malgo-lang/malgo/commit/49877609756b6e4c58b04a0fffa57aa90fafa137))
* **changelog:** remove CHANGELOG.md file ([7717ec3](https://github.com/malgo-lang/malgo/commit/7717ec3b03804e56e19b0a87c97b2473776cd1b5))
* **changelog:** restructure changelog for clarity and organization ([1989ae5](https://github.com/malgo-lang/malgo/commit/1989ae59594681a133da1e46e6c8c295fff57c25))
* **package:** bump version to 2.0.0 and update copyright year ([fefa0a1](https://github.com/malgo-lang/malgo/commit/fefa0a19dc4f9e7455e518e6b1fc340adfd76eb3))
* **test:** remove .malgo-work directory if it exists ([a256f73](https://github.com/malgo-lang/malgo/commit/a256f73df89a20792820c1aa49f8724a3b9fe332))


### Code Refactoring

* change workspace directory structure ([ebca7ec](https://github.com/malgo-lang/malgo/commit/ebca7ecae41bf3eab6f1e0fad04de9c2f0068150))
* **infer:** update type synonym handling with kind checks ([713137d](https://github.com/malgo-lang/malgo/commit/713137deb772fe46effab59ede6dd9503310b598))


### Tests

* **errors:** add error cases for parser ([1bfb6fe](https://github.com/malgo-lang/malgo/commit/1bfb6fec6b9aff8fe983de44a26232ee8fa69004))


### Continuous Integration

* **build:** remove unsupported GHC versions from matrix ([28a7348](https://github.com/malgo-lang/malgo/commit/28a7348ab8741c3c61add7b62c9c8bba35ebd9d3))
