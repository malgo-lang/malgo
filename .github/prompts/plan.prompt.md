---
mode: 'agent'
---

# 🧭 Prompt: Document Research & Change Plan in `plan.md`

**Goal:** Based on the user’s input, analyze what’s needed and document your research findings and proposed changes in the `plan.md` file. Do **not** directly edit source code—focus solely on writing the plan.

## 📋 Instructions

1. **Analyze the Input**

   * Examine the user’s input in detail. Clarify their goals and requirements.

2. **Identify Research Items**

   * List all necessary research topics and points that need verification.

3. **Draft the Change Plan**

   * Based on your research, outline concrete steps and modifications.

4. **Append to `plan.md`**

   * Add the above information to `plan.md`. If the file already contains content, respect the existing structure and append your new sections.

## 📁 Output Format (example for `plan.md`)

```markdown
## 📝 Analysis of User Input

- **Objective:** Describe clearly what the user wants to achieve.
- **Background:** Summarize any relevant context or prerequisites.

## 🔍 Research Items

- List each item or check point that needs investigation.

## 🛠️ Change Plan

- **Target Files:** Enumerate filenames that will require changes.
- **Proposed Changes:** Detail the specific changes to be made.
- **Impact & Considerations:** Outline any ripple effects or risks.

## ✅ Next Steps

- List follow-up actions or verifications.
```

## ⚠️ Notes

* **Do Not Edit Code Directly:** All planning and investigation must go into `plan.md`, not into source files.
* **Respect Existing Content:** If `plan.md` already exists, build on it rather than overwriting.