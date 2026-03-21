---
name: review-code
description: Self-review code changes before shipping. Reviews the diff against the implementation plan for correctness, completeness, and quality. Use when the user says "review my changes", "check the code", "self-review", or as part of the implement-plan flow before marking a PR as ready.
argument-hint: (optional) plan file path or issue ID for context
---

# Review Code Changes

Review the current branch's changes for quality and completeness before shipping.

## 1. Gather context

Determine what to review against:

- **If $ARGUMENTS is provided** — load the referenced plan (file path or Linear
  issue ID) as the spec to review against.
- **If no argument** — look for the plan in `docs/implementation-plans/` matching
  the current branch or PR, or check the PR description for plan context.
- **If no plan exists** — review the diff on its own merits without a plan
  reference.

## 2. Load the diff

Get the full diff of changes on the current branch compared to the base branch:

```bash
git diff $(git merge-base HEAD main)..HEAD
```

If the diff is large, review it file by file rather than all at once.

Also check `CLAUDE.md` for project-specific conventions, patterns, and quality
standards.

## 3. Review checklist

Evaluate the changes against each of these categories:

### Completeness (if reviewing against a plan)
- Are all implementation steps from the plan addressed?
- Are there any gaps between what the plan specifies and what was implemented?

### Correctness
- Does the logic correctly implement the intended behavior?
- Are edge cases handled appropriately?
- Are there any off-by-one errors, null pointer risks, or race conditions?

### Tests
- Are all new code paths covered by tests?
- Do tests follow the project's existing testing patterns (check `CLAUDE.md`)?
- Are there any missing test cases for edge cases or error paths?

### Code quality
- Does the code follow existing patterns in the codebase?
- Are there any leftover TODOs, debug statements, or commented-out code?
- Are naming conventions consistent with the rest of the project?

### Security
- Are there any injection risks (SQL, command, XSS)?
- Is user input properly validated at system boundaries?
- Are secrets or sensitive data handled appropriately?

### Unintended changes
- Are there any changes to files that shouldn't have been modified?
- Are there any unrelated formatting or refactoring changes mixed in?

## 4. Report findings

Present findings as a structured list grouped by category. For each issue:

- **File and line** — where the issue is
- **Severity** — critical (must fix), warning (should fix), or note (consider)
- **Description** — what's wrong and how to fix it

If there are no issues, say so clearly.

## 5. Fix issues (when called by implement-plan)

When invoked as part of the implementation flow (not standalone), fix any
critical and warning issues found. For each fix:

1. Make the change
2. Run the project's test suite to verify no regressions
3. Commit the fix separately: `fix(<scope>): <description>`

For standalone invocations, report findings and let the user decide what to fix.
