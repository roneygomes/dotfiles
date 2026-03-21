---
name: implement-plan
description: Implement a task according to its implementation plan, run quality gates, commit, ship, and monitor. Use this skill whenever the user wants to implement, execute, or build out a planned task — even if they just say "implement this", "build it", "implement PE-XXXX", or "implement the plan in <file>".
argument-hint: issue-id or plan file path
---

# Implement Plan

Implement the work described by $ARGUMENTS following its implementation plan.

## 1. Load the plan

Determine the input type and find the plan:

- **Linear issue ID** (matches pattern like `XX-1234`):
  1. Check `docs/implementation-plans/` for a file whose frontmatter `issue`
     field matches.
  2. If no local file, read the issue description from Linear via the Linear
     MCP.
- **File path** — read the plan directly from the referenced file.
- **No argument** — ask the user for an issue ID or plan file path.

If no plan is found, stop and tell the user. Suggest running `/plan-task` first.

Use a sub-agent to parse the plan and return a structured summary:
- List of implementation steps with files to create/modify
- Testing strategy
- Any risks or prerequisites noted in the plan

## 2. Verify preconditions

- Confirm we're on the correct feature branch (listed in the plan or in the
  Linear issue's branch field). If not, check it out.
- Ensure the branch is up to date with the base branch. Check `CLAUDE.md` for
  the project's base branch convention. If not specified, use `main`.
  ```bash
  git fetch origin <base-branch>
  git rebase origin/<base-branch>
  ```
  If the rebase has conflicts, stop and ask the user.
- Confirm a draft PR exists for this branch. If not, warn the user — the
  planning skill should have created one.

## 3. Update Linear issue status (if applicable)

If working from a Linear issue, move it to "In Progress" via the Linear MCP.

## 4. Implement

Work through the plan's implementation steps in order.

Read canonical example files referenced in the plan or in `CLAUDE.md` before
writing code — match existing patterns exactly rather than inventing new ones.

### Plan deviations

If you discover something in the plan won't work as written (e.g., an API
changed, a dependency is missing, a type doesn't exist), stop and ask the user
before adapting. Explain what's different and propose alternatives. The plan is
the contract — don't silently deviate from it.

### Commit discipline

Each commit must leave the codebase in a releasable state — all quality gates
passing. This means:

- **Bundle tests with the code they cover** in the same commit. Never commit
  production code in one commit and its tests in another.
- **Group by logical unit.** A good commit boundary is one module, one service,
  or one schema — together with its tests and any config files it needs.
- **Validate before every commit** (see section 5).

If the plan has explicit implementation steps, use them as a guide for commit
boundaries, but merge steps when splitting them would break quality gates.

## 5. Validate and commit

Run the project's quality gates before each commit. Check `CLAUDE.md` for
project-specific test commands, lint commands, and other validation steps.

After validation passes:

1. Stage the relevant files for this logical unit.
2. Commit using conventional commits format: `<type>(<scope>): <description>`
3. Push to the remote branch.

Repeat sections 4-5 for each logical unit until the full plan is implemented.

## 6. Self-review

Run `/review-code` to self-review the complete implementation against the plan.
Fix any critical or warning issues found, run tests, and commit the fixes.

## 7. Update the PR description

Review the current PR description. If the implementation introduced anything
worth noting beyond what the original description covers (design decisions,
deviations from the plan, additional reviewer context), update the PR body.

Keep the existing structure and any issue links — add to it rather than
replacing it wholesale.

## 8. Mark PR as ready

Once all implementation is done, reviewed, and pushed:

```bash
gh pr ready
```

## 9. Watch the PR

Run `/watch-pr` to start the combined CI + review comment monitoring loop.
This will:

- Monitor CI checks and fix failures automatically
- Triage and respond to review comments
- Update the Linear issue status to "In Review" once everything is green
- Escalate to the user if it can't resolve something after retries

The implementation is complete when `/watch-pr` reports the PR is ready for
human review.
