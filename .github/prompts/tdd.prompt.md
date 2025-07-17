---
mode: "agent"
description: "Follow TDD workflow to resolve an issue"
---

# 🚦 TDD Pull Request Workflow (t‑wada style)

## Context

- Repository: <REPO_URL>
- Issue: #<ISSUE_NUMBER> – <ISSUE_TITLE>
- Goal: Resolve the above issue entirely via test‑driven development.

## Global Rules

1. Follow Takuto Wada’s Outside‑In Lean TDD (Red → Green → Refactor).
2. Baby‑step cycles: ≤ 5 minutes or ≤ 15 LOC per cycle.
3. One git commit per cycle.
4. Commit‑message conventions
   - Red : `Add failing test: <behaviour>`
   - Green: `Make test pass: <behaviour>`
   - Refactor only: `Refactor: <summary>`
5. Work on branch `feature/issue-<ISSUE_NUMBER>-<slug>`.

## Tasks for the Coding Agent

1. Read the issue body & linked discussion; extract acceptance criteria.
2. List all test scenarios (Given–When–Then) in the PR description as a TODO list.
3. Write failing test(s) first.
4. Add minimal production code to go green.
5. Refactor for readability, duplication, and performance with all tests green.
6. Iterate until every acceptance criterion is satisfied.

## Git Workflow Skeleton

```bash
git switch -c feature/issue-<ISSUE_NUMBER>-<slug>

# --- Cycle N ---
# RED
# 1. write failing test
git add .
git commit -m "Add failing test: <behaviour>"

# GREEN
# 2. implement just enough code
git add .
git commit -m "Make test pass: <behaviour>"

# REFACTOR
# 3. improve design without changing behaviour
git add .
git commit -m "Refactor: <detail>"

# Push after each cycle (or every few cycles)
git push -u origin feature/issue-<ISSUE_NUMBER>-<slug>
```

## Pull Request Template (auto‑fill)

```markdown
### What & Why

Fixes #<ISSUE_NUMBER>. Implements <feature/bug‑fix> via TDD.

### Acceptance Criteria

- [x] <criterion 1>
- [x] <criterion 2>
      …

### How

Key design decisions, algorithms, and trade‑offs.

### Tests

List of new tests with intention‑revealing names.

### Checklist

- [ ] All tests green (`npm test` / `go test ./...` / etc.)
- [ ] Code formatted & linted
- [ ] No TODOs in production code
- [ ] Docs & changelog updated
```

## Done = Merged

The PR is ready when CI is green, review comments are addressed, and the checklist is complete.

## Additional Context

- Use the GitHub MCP server to manage the issue and PR if available.
- Use the `gh` CLI to manage the PR and comments if MCP is not available.
- If the issue is not clear, ask for clarification in the issue comments before starting.