---
mode: "agent"
description: "Prompt for auto-implementing small issues and creating pull requests in the Malgo project"
---

## Steps

Follow **Planning** → **Action**. Obtain user approval between Planning and Action.

### Planning

1. Fetch the issue body and discussion from GitHub:
   - Use `gh issue view ${issue_number}`.
2. Propose a branch name prefixed with `fix/${issue_number}/` based on the issue summary.
3. (Optional) Search codebase to locate relevant implementation points and draft an implementation plan.
4. Present the branch name and implementation plan to the user for approval.

### Action

5. After approval, verify the working directory is clean (`git status`).
6. Create the new branch and switch to it:
   ```bash
   git checkout -b fix/${issue_number}/<short-description>
   ```
7. Implement the changes according to the plan.
8. Run project checks:
   ```bash
   mise run build    # compile the project
   mise run test     # execute test suite
   ```
9. If checks pass, stage changes:
   - Use `git add -u` for modified files.
   - Use `git add <new-file>` for any added files.
10. Commit the changes with a concise message summarizing the fix.
11. Push the branch and create a draft pull request:
    ```bash
    git push --set-upstream origin fix/${issue_number}/<short-description>
    gh pr create --draft --base main --title "<concise summary>" --body "fix: #${issue_number} Auto-generated PR\n\n- Description of changes\n- References or notes\n- Questions or concerns"
    ```

## Pull Request Format

- Title: concise summary of the fix.
- Body: must start with `fix: #${issue_number} Auto-generated PR` and include:
  - A brief description of the changes
  - Any relevant references or special considerations
  - Any questions or concerns
- Base branch: `main`
