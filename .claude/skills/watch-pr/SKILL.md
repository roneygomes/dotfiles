---
name: watch-pr
description: Monitor a PR's CI pipeline and review comments in a combined polling loop. Automatically triages CI failures and review feedback, fixes issues, and pushes. Use when the user says "watch the PR", "monitor CI", "babysit the PR", or as the post-ship phase of implement-plan.
argument-hint: (optional) PR number or branch name
---

# Watch PR

Monitor the current PR for CI status and review comments in a combined polling
loop. Act on failures and feedback automatically.

## 1. Identify the PR

- **If $ARGUMENTS is a PR number** — use it directly.
- **If $ARGUMENTS is a branch name** — find the PR for that branch.
- **If no argument** — use the PR for the current branch.

```bash
gh pr view --json number,url,headRefName
```

If no PR is found, stop and tell the user.

## 2. Start the polling loop

Use `/poll` with a 3-minute interval. Each tick runs the checks below.

### Check CI status

```bash
gh pr checks --json name,state,description,link
```

Categorize the results:
- **All passing** — CI is green, note it and continue to review comments check.
- **Pending** — still running, nothing to do this tick.
- **Failure detected** — move to failure triage (section 3).

### Check review comments

```bash
gh pr view --json number --jq '.number' | xargs -I{} gh api repos/{owner}/{repo}/pulls/{}/comments --jq '.[] | {id, path, line, body, created_at, user: .user.login}'
```

Also check for top-level PR review comments:

```bash
gh pr view --json reviews --jq '.reviews[] | {author: .author.login, state: .state, body: .body}'
```

For any **new comments since last tick** (track by timestamp or ID):
- Use a sub-agent to read and categorize each comment as:
  - **Actionable** — requires a code change
  - **Question** — needs a response but no code change
  - **Not applicable** — the reviewer's feedback doesn't apply (explain why)
- For actionable comments: fix the issue, run tests, commit, push, and reply
  to the comment explaining the change.
- For questions: reply with an explanation.
- For not-applicable: reply explaining why respectfully.
- **Never resolve/dismiss comments** — leave that to the reviewer.

## 3. CI failure triage

When a CI check fails:

1. Identify the failing check:
   ```bash
   gh pr checks --json name,state,link --jq '.[] | select(.state == "FAILURE")'
   ```

2. Use a sub-agent to read the failure logs and diagnose the issue:
   ```bash
   gh run view <run-id> --log-failed
   ```

3. If the fix is clear: make the change, run tests locally, commit with
   `fix(<scope>): <description>`, and push. Resume polling.

4. Track fix attempts per check name. If the **same check fails 3 times** after
   fix attempts, stop and escalate to the user — the failure may be
   environmental or require manual intervention.

## 4. Exit conditions

Stop the polling loop when ANY of these are met:

- **CI green + no unaddressed comments** — the PR is ready. If working from a
  Linear issue, update its status to "In Review" via Linear MCP. Tell the user
  the PR is ready for human review.
- **Fix attempt cap reached** — a CI check failed 3 times on the same issue.
  Summarize what was tried and ask the user for guidance.
- **Polling timeout** — after 30 minutes with no progress (no new CI runs, no
  new comments, no state changes), summarize the current status and hand off
  to the user.

## 5. Status updates

At each tick, if there's something noteworthy (state change, new comment, fix
applied), briefly update the user. Don't spam on no-change ticks.
