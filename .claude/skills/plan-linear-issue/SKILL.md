---
name: plan-linear-issue
description: Write implementation plan for a Linear issue.
disable-model-invocation: true
argument-hint: issue-id
---

# Plan Linear Issue Implementation

Write an implementation plan for $ARGUMENTS taking into account:
- The issue description.
- Comments added to the issue.
- The description of the project the issue is a part of.
- The resources attached to the project.

Ask the user clarifying questions with the AskUserQuestion tool for things
you can't infer on your own, that are ambiguous or haven't been considered in
the design documents/task description. When asking questions, provide options
whenever possible.

## Plan file location

Save the plan as a single markdown file at the root of `docs/implementation-plans/`,
even if other plans in that directory use subdirectories. The plan is a single file,
not a folder.

Filename pattern: `<YYYY-MM-dd>-<short-plan-description>.md`

Examples:
```
docs/implementation-plans/2025-01-27-comprehensive-agents-file.md
docs/implementation-plans/2025-10-07-update-renegotiated-repayments.md
```

## After the user accepts the plan

Once the user confirms the plan is accepted, do all of the following:

### 1. Update the Linear issue description

Update the issue description with the plan content, but skip frontmatter and
metadata that's redundant in the issue context — things like the issue title,
the link back to the issue itself, branch name, date, spec links, and execution
flow checklists. The issue already has its own title and metadata; repeating them
adds noise.

Start the issue description from the first substantive section — typically
**Summary**, **Context**, or **Technical Context**. Everything from that point
onwards goes into the issue description.

### 2. Write the plan file

Save the full plan (including any frontmatter/metadata) as the markdown file
described above.

### 3. Create feature branch

If not already on a feature branch following the branch name pattern established
in Linear, create one and switch to it.

### 4. Commit the plan

Stage and commit the plan file.

### 5. Open a draft pull request

The pull request represents the upcoming implementation work, not the plan
document itself. Even though the only commit being pushed right now contains
documentation, the PR title and description should reflect the feature that will
be built.

- **Title**: Use conventional commits format describing the feature work
  (e.g., `feat(assets): add disbursement tracking to asset sales`), not
  something like `docs(plans): add implementation plan for ...`.
- **Description**: Summarize what the implementation will deliver and link to
  the Linear issue. The plan content in the issue description serves as the
  detailed spec.
- Push the branch and open the PR as a **draft**.
