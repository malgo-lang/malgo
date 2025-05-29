## 📝 Analysis of User Input

- **Objective:** Propose three distinct plans for removing `KindCtx` from the codebase.
- **Background:**
  - `KindCtx` is currently a mapping from type variables to kinds, used throughout the type inference and checking phases (notably in `Malgo.Infer.Kind`, `Malgo.Infer.Pass`, and `Malgo.Interface`).
  - It is threaded through the type checker and stored in interfaces for cross-module type information.
  - Removing `KindCtx` would require a fundamental change in how kind information is managed, propagated, or inferred.

## 🔍 Research Items

- Identify all usages of `KindCtx` in the codebase (type signatures, state, interface, etc.).
- Understand how kinds are currently inferred, checked, and stored.
- Investigate alternative approaches for kind management (e.g., local inference, type-level encoding, or embedding in other structures).
- Assess the impact on interface files and cross-module type sharing.
- Consider backward compatibility and migration strategies.

## 🛠️ Change Plan

### Plan 1: Inline Kind Inference (On-the-fly, No Global Context)
- **Target Files:**
  - `src/Malgo/Infer/Kind.hs`
  - `src/Malgo/Infer/Pass.hs`
  - `src/Malgo/Infer/TcEnv.hs`
  - `src/Malgo/Interface.hs`
- **Proposed Changes:**
  - Remove `KindCtx` as a global or threaded state.
  - Refactor `kindOf` and related functions to infer kinds locally from type structure, without consulting a context.
  - For type variables, infer kind from usage or require explicit kind annotations.
  - Remove `kindCtx` from `Interface` and related serialization.
- **Impact & Considerations:**
  - May require more explicit kind annotations in user code.
  - Could increase inference complexity or ambiguity in some cases.
  - Simplifies state management and interface files.

### Plan 2: Embed Kind Information in Type Definitions
- **Target Files:**
  - `src/Malgo/Infer/TypeRep.hs`
  - `src/Malgo/Infer/Kind.hs`
  - `src/Malgo/Infer/Pass.hs`
  - `src/Malgo/Interface.hs`
- **Proposed Changes:**
  - Extend type and type variable representations to carry their kind as a field.
  - Remove `KindCtx` and update all code to use the embedded kind information.
  - Update type constructors, type synonyms, and data definitions to include kind information at definition time.
  - Adjust interface serialization to store kinded types directly.
- **Impact & Considerations:**
  - Increases memory usage for type representations.
  - Makes kind information always available, simplifying kind checking.
  - Requires a migration of all type-related code to the new representation.

### Plan 3: Integrate Kind Information into TcEnv (Type Environment)
- **Target Files:**
  - `src/Malgo/Infer/TcEnv.hs`
  - `src/Malgo/Infer/Kind.hs`
  - `src/Malgo/Infer/Pass.hs`
  - `src/Malgo/Interface.hs`
- **Proposed Changes:**
  - Move kind information from `KindCtx` into `TcEnv` as part of the type environment.
  - Refactor all code to access kind information via `TcEnv` lookups, not a separate context.
  - Remove `KindCtx` and update interface files to reflect the new structure.
- **Impact & Considerations:**
  - Keeps kind information centralized but avoids a separate context.
  - May simplify or complicate environment management depending on code structure.
  - Easier migration path if `TcEnv` is already widely used.

## ✅ Next Steps

- Review and discuss the three plans with stakeholders.
- Select a plan based on project goals (simplicity, performance, compatibility).
- Prototype the chosen approach in a branch.
- Update documentation and tests to reflect the new kind management strategy.
