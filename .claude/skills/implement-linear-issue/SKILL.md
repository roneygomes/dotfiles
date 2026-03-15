---
name: implement-linear-issue
description: Implement an issue according to its implementation plan, run quality gates, commit, mark the PR as ready, and monitor CI. Use this skill whenever the user wants to implement, execute, or build out a planned Linear issue — even if they just say "implement PE-XXXX" or "build out the plan for PE-XXXX".
disable-model-invocation: true
argument-hint: issue-id
---

# Implement Plan

Implement $ARGUMENTS by following its implementation plan. The plan was
previously written by `plan-linear-issue` and lives either as a markdown file
or in the Linear issue description.

## 1. Load the plan

Look for the plan in this order:

1. **Local file** — search `docs/implementation-plans/` for a file whose
   frontmatter `issue` field matches $ARGUMENTS (e.g., `issue: PE-2200`).
2. **Linear issue** — if no local file is found, read the issue description
   from Linear via the Linear MCP, which should contain the plan content.

If neither source has a plan, stop and tell the user.

## 2. Verify preconditions

- Confirm we're on the feature branch listed in the plan (or in the Linear
  issue's branch field). If not, check it out.
- **Rebase on devnet** to ensure the branch is up to date. This is important
  because `nx affected --base=devnet` produces wrong results on stale branches.
  ```bash
  git fetch origin devnet
  git rebase origin/devnet
  ```
  If the rebase has conflicts, stop and ask the user.
- Confirm a draft PR exists for this branch. If not, warn the user — the
  planning skill should have created one.

## 3. Update Linear issue status

Move the issue to "In Progress" using the Linear MCP so the team has
visibility that implementation has started.

## 4. Implement

Work through the plan's implementation steps in order. The plan typically
specifies files to create, files to modify, and the sequence to follow.

Read canonical example files referenced in the plan or in `CLAUDE.md`'s
testing patterns catalog before writing code — match the existing patterns
exactly rather than inventing new ones.

### Plan deviations

If you discover something in the plan won't work as written (e.g., an API
changed since planning, a dependency is missing, a type doesn't exist), stop
and ask the user before adapting. Explain what's different and propose
alternatives. The plan is the contract — don't silently deviate from it.

### Commit discipline

Each commit must leave the codebase in a releasable state — all quality gates
passing, including 100% test coverage. This means:

- **Bundle tests with the code they cover** in the same commit. Never commit
  production code in one commit and its tests in another, because the first
  commit would fail the coverage gate.
- **Group by logical unit.** A good commit boundary is one module, one service,
  or one schema — together with its tests and any config files it needs.
- **Validate before every commit** (see section 5).

If the plan has explicit implementation steps, use them as a guide for commit
boundaries, but merge steps when splitting them would break the coverage gate.

## 5. Validate and commit

Claude hooks automatically run **format** (on every file edit) and **lint +
type-check** (after every response), so those gates are already covered by
the time you're ready to commit. The remaining gate is **tests**:

```bash
./scripts/nx-mod.sh -t test -c ci
```

After tests pass:

1. Stage the relevant files for this logical unit.
2. Commit using conventional commits format: `<type>(<scope>): <description>`
3. Push to the remote branch.

Repeat sections 4–5 for each logical unit until the full plan is implemented.

## 6. Update the PR description

Review the current PR description. If the implementation introduced anything
worth noting beyond what the original description covers (design decisions,
deviations from the plan, or additional reviewer context), update the PR body:

```bash
gh pr edit --body-file /tmp/pr-body.md
```

Keep the Linear issue link and the existing structure — add to it rather than
replacing it wholesale.

## 7. Mark PR as ready

Once all implementation is done and pushed:

```bash
gh pr ready
```

## 8. Monitor CI and address review comments

After marking the PR as ready, actively monitor CI:

```bash
gh pr checks --watch --fail-fast
```

### While waiting for CI (and after it completes)

Check for review comments left by automated review bots or other reviewers:

```bash
gh pr view --json number,url --jq '.number' | xargs -I{} gh api repos/:owner/:repo/pulls/{}/comments
```

For each comment:

1. **Read and evaluate** the feedback on its merits. Automated reviewers
   sometimes flag things that are intentional or acceptable in context.
2. **If the feedback is valid** — fix the issue, run tests, commit, and push.
   Then reply to the comment explaining what was changed.
3. **If the feedback is not applicable** — reply to the comment explaining
   why (e.g., "This pattern matches the existing convention in
   `assets.service.ts`" or "Intentional — the plan calls for this approach
   because ...").
4. **Never resolve/close the comments** — leave that to the reviewer.

### If CI passes (and no pending review comments need code changes)

Update the Linear issue status to "In Review" via the Linear MCP and tell
the user the implementation is complete and the PR is ready for human review.

### If CI fails

1. Read the failure details:
   ```bash
   gh pr checks --json name,state,description,link --jq '.[] | select(.state == "FAILURE")'
   ```
2. Investigate the failing check — fetch logs if needed:
   ```bash
   gh run view <run-id> --log-failed
   ```
3. Fix the issue locally.
4. Run tests, commit the fix: `fix(<scope>): <description of what broke>`
5. Push and resume monitoring: `gh pr checks --watch --fail-fast`
6. Repeat until CI is green.

If after 3 attempts CI still fails on the same check, stop and ask the user
for guidance — the failure may be environmental or require manual intervention.
