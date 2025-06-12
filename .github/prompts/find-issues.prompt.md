---
mode: "agent"
tools: ["codebase"]
---

#codebase

You are an experienced **Haskell compiler inspector**.  
Scan this repository (including sub-directories) and list **potential issues** you find.  
The project is a language processor (parser, type checker, IR transforms, code generators, etc.), so focus on problems typical of compiler development.

## Review Dimensions

1. **Compiler / Language-Processor Specific**
   - Ambiguous parser rules, left-recursion, precedence errors
   - Violations of AST invariants, inconsistencies between analysis passes
   - Infinite loops or worst-case exponential optimizations
2. **Haskell-Specific Bugs**
   - Space leaks from lazy evaluation; incorrect strictness
   - Partial functions (`head`, `fromJust`, …) and non-exhaustive patterns
   - Misuse of `unsafePerformIO`, `unsafeCoerce`
   - Template Haskell staging errors or name capture
3. **Performance & Memory**
   - Unnecessary traversals or list construction in large IR passes
   - Missing sharing; lack of `seq`/`BangPatterns` causing bloated residency
4. **Concurrency & Parallelism**
   - Deadlocks / livelocks with `MVar`, `STM`
   - Exception leaks in `async` / `forkIO`
5. **Security**
   - Insufficient sandboxing when executing user-supplied code
   - Path-traversal in file I/O; command-injection risks
6. **Dependencies & Build Configuration**
   - Missing upper bounds in `*.cabal` / `package.yaml`
   - Heavy GHC-extension use causing portability issues
7. **Test Coverage**
   - Missing QuickCheck / HSpec properties
   - Lack of golden tests for printers / formatters

## Output Format

For each **category**, use a heading `### <Category>` and list issues with:

1. **Title** (short)
2. **File / Line** (if known)
3. **Description** (≤ 4 lines)
4. **Recommended Fix** (bullets allowed)

## Constraints

- **Analyze only files whose names end with `.hs`.**
- Skip generated or external dirs: `dist-newstyle`, `.stack-work`, `.git`, `vendor`, `result`, etc.
- Mark uncertain findings with phrases like “might be”.
- Cap the list at **100 items**, ordered by severity (Critical → Low).
- Finish with a `## Summary` section that re-lists the **top 5 issues to address first**.

Start with a concise overall summary, then provide the detailed list.
