---
mode: 'agent'
---

# 🧭 Prompt: Document Research & Change Plan in `plan.md`

**Goal:** Based on the user’s input, analyze what’s needed and document your research findings and proposed changes in the `plan.md` file. Do **not** directly edit source code—focus solely on writing the plan.

## 📋 Instructions

1. **Analyze the Input**
   * Examine the user’s input in detail. Clarify their goals and requirements.
   * Identify the type of change (e.g., parser, type system, runtime, documentation).
   * Note any Malgo-specific context (language features, compiler passes, runtime behavior).

2. **Project Context Awareness**
   * Summarize relevant project background, affected subsystems, and dependencies.
   * Note any related ongoing work or architectural constraints.

3. **Identify Research Items**
   * List all necessary research topics and points that need verification.
   * For each, specify:
     - What needs to be investigated
     - Where to look (code, docs, tests)
     - Why it matters for this change

4. **Draft the Change Plan**
   * For each change, specify:
     - **Target Files/Modules:** List all affected files/modules.
     - **Proposed Changes:** Detail the specific changes, referencing Malgo coding conventions where relevant.
     - **Dependencies:** List any code, libraries, or documentation that must be updated in tandem.
     - **Testing Strategy:** Describe how changes will be validated (unit tests, integration tests, manual checks).
     - **Performance Impact:** Note any expected performance implications and how they will be measured.
     - **Risk Assessment:** Identify risks (e.g., breaking changes, backward compatibility, migration needs) and mitigation strategies.
     - **Before/After State:** Briefly describe the current and intended future state.
     - **Migration/Rollback:** If applicable, outline migration steps and rollback plan.

5. **Append to `plan.md`**
   * Add the above information to `plan.md`. If the file already contains content, respect the existing structure and append your new sections.
   * Use the templates below as a guide. For simple changes, a minimal version is acceptable; for complex changes, fill out all sections.

## 📁 Output Format (example for `plan.md`)

```markdown
## 📝 Analysis of User Input

- **Objective:** Clearly state the user’s goal.
- **Background:** Project context, affected subsystems, and prerequisites.
- **Change Type:** (e.g., parser, type system, runtime, docs)
- **Malgo-Specific Context:** (e.g., language feature, compiler pass, runtime behavior)

## 🔍 Research Items

- [ ] What needs to be investigated, where, and why.
- [ ] ...

## 🛠️ Change Plan

- **Target Files/Modules:**
- **Proposed Changes:**
- **Dependencies:**
- **Testing Strategy:**
- **Performance Impact:**
- **Risk Assessment:**
- **Before/After State:**
- **Migration/Rollback:**

## ✅ Next Steps

- [ ] Implementation timeline
- [ ] Resource requirements
- [ ] Validation and testing checkpoints
- [ ] Documentation updates
- [ ] ...
```

## 🧩 Domain-Specific Templates

### For Parser/Type System/Runtime Changes
- What language feature or compiler pass is affected?
- Are there Malgo coding conventions or pretty-printing requirements to follow?
- What tests or examples must be updated?

### For Documentation-Only Changes
- What user-facing docs or references are affected?
- Is there a need for code comments or architecture updates?

## ⚠️ Notes

* **Do Not Edit Code Directly:** All planning and investigation must go into `plan.md`, not into source files.
* **Respect Existing Content:** If `plan.md` already exists, build on it rather than overwriting.
* **Scale Appropriately:** Use the detailed template for complex changes, and a simplified version for minor edits. Indicate which is used.
* **Quality Checklist:** Before finalizing, ensure all relevant sections are filled and risks are considered.