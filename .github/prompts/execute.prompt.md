---
mode: 'agent'
tools: ['fetch']
---

# 🧭 Prompt: Execute Change Plan from `plan.md`

**Goal:**
Read and interpret the research & change plan in `plan.md`, then apply the specified modifications directly to the source code files.

## 📋 Instructions

1. **Load and Parse `plan.md`**
   - Open `plan.md` and parse its sections:
     - **Analysis of User Input**
     - **Project Context Awareness** (if present)
     - **Research Items**
     - **Change Plan**
     - **Next Steps**
   - Identify the type and complexity of the change (simple vs. complex; parser, type system, runtime, docs, etc.).
   - Note any Malgo-specific context (language features, compiler passes, runtime behavior).

2. **Context Awareness & Preparation**
   - Review project background, affected subsystems, dependencies, and related ongoing work.
   - Check for architectural constraints or coding conventions (see Malgo project guidelines).
   - If the plan references domain-specific templates, follow them closely.

3. **Iterate Over “Change Plan” Entries**
   For each item under the **Change Plan** section:
   - **Target Files/Modules**: Open each listed file or module.
   - **Proposed Changes**:
     - Locate the lines, functions, or blocks described.
     - Apply the modifications exactly as specified, following Malgo coding conventions.
     - For complex changes, update dependencies, tests, and documentation as indicated.
   - **Dependencies**: Update any related code, libraries, or docs in tandem.
   - **Testing Strategy**: Prepare to run or update tests as described.
   - **Performance Impact**: If relevant, measure and document any performance changes.
   - **Risk Assessment**: Consider and mitigate risks (breaking changes, migration, rollback).
   - **Before/After State**: Document the current and intended future state if required.
   - **Migration/Rollback**: Implement migration or rollback steps if specified.
   - **Impact & Considerations**:
     - Check for related code that might also need updating (imports, tests, docs).
     - Ensure no unintended side-effects are introduced.

4. **Validation & Testing**
   - After applying each change, run all relevant automated tests, linters, and manual checks as described in the plan.
   - If a test fails, diagnose whether it’s expected (update the plan) or indicates a mis-application (fix the code).
   - For performance-sensitive changes, benchmark and record results.
   - If the plan is missing validation steps, append them to **Next Steps** in `plan.md`.

5. **Commit & Document**
   - Stage and commit each set of related changes with a clear commit message referencing the `plan.md` section (e.g., “feat: implement X as per plan.md – Change Plan #2”).
   - For complex changes, include before/after state and migration notes in the commit message if relevant.
   - If any change diverges from the plan (e.g., requires deeper refactoring), update `plan.md` to reflect the new approach.
   - If new research items or impacts are uncovered, append those to the **Research Items** or **Next Steps** sections in `plan.md`.

6. **Documentation & Communication**
   - Update user-facing documentation, code comments, and architecture docs as required by the plan.
   - If the change is user-visible or breaking, ensure release notes or migration guides are updated.

## ⚠️ Notes

- **Accuracy First**: Don’t guess – follow the text in `plan.md` exactly. If unclear, update the plan or seek clarification.
- **One Logical Change per Commit**: Keep commits small and focused.
- **Keep `plan.md` in Sync**: If a change uncovers new research items or impacts, append those to the **Research Items** or **Next Steps** sections.
- **Scale Appropriately**: For simple changes, a minimal process is acceptable; for complex changes, follow all steps in detail.
- **Quality Checklist**: Before finalizing, ensure all relevant sections are addressed and risks are considered.