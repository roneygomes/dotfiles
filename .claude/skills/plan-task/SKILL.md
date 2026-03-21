---
name: plan-task
description: Write an implementation plan for a task. Accepts a Linear issue ID, a file/URL reference, or a plain description. Use when the user wants to plan, spec out, or design the implementation for a piece of work — even if they just say "plan this" or "write a plan for X".
disable-model-invocation: true
argument-hint: issue-id, file path, or description
---

# Plan Task Implementation

Write an implementation plan for the work described by $ARGUMENTS.

## 1. Gather context

Determine the input type and load context accordingly:

- **Linear issue ID** (matches pattern like `XX-1234`) — fetch the issue via the
  Linear MCP. Read the issue description, comments, parent project description,
  and any attached resources.
- **File path** — read the referenced file for requirements/spec content.
- **Plain description** — use $ARGUMENTS directly as the task description.
- **No argument** — ask the user what they'd like to plan.

If working from a Linear issue, also check for linked documents, parent project
context, and related issues that might inform the plan.

## 2. Understand the codebase

Before writing the plan, understand the parts of the codebase that will be
affected. Read relevant files, understand existing patterns, and identify
dependencies. Check `CLAUDE.md` for project conventions, testing patterns,
and architectural guidance.

## 3. Ask clarifying questions

Use the AskUserQuestion tool for things you can't infer on your own, that are
ambiguous, or haven't been considered in the design documents/task description.
When asking questions, provide options whenever possible.

## 4. Write the plan

The plan should include:

- **Summary** — what's being built and why
- **Technical context** — relevant architecture, patterns, dependencies
- **Implementation steps** — ordered steps with files to create/modify, broken
  into logical units that each leave the codebase in a releasable state
- **Testing strategy** — what to test and how, following the project's existing
  testing patterns
- **Risks and open questions** — anything that might need attention during
  implementation

## 5. Plan file location

Save the plan as a single markdown file at the root of `docs/implementation-plans/`,
even if other plans in that directory use subdirectories.

Filename pattern: `<YYYY-MM-dd>-<short-plan-description>.md`

Include frontmatter with:
```yaml
---
title: <plan title>
date: <YYYY-MM-dd>
issue: <issue ID, if applicable>
---
```

## 6. After the user accepts the plan

Once the user confirms the plan is accepted, do all of the following:

### Update Linear issue (if applicable)

If the input was a Linear issue ID, update the issue description with the plan
content. Skip frontmatter and metadata that's redundant in the issue context
(issue title, link back to the issue itself, branch name, date). Start from the
first substantive section — typically **Summary** or **Technical Context**.

### Write the plan file

Save the full plan (including frontmatter) as the markdown file described above.

### Create feature branch

If not already on a feature branch, create one and switch to it. If working from
a Linear issue, follow the branch naming convention established in the project.

### Commit the plan

Stage and commit the plan file.

### Open a draft pull request

The PR represents the upcoming implementation work, not the plan document itself.

- **Title**: Use conventional commits format describing the feature work
  (e.g., `feat(assets): add disbursement tracking`), not something like
  `docs(plans): add implementation plan for ...`.
- **Description**: Summarize what the implementation will deliver. Link to the
  Linear issue if applicable. The plan content in the issue description or plan
  file serves as the detailed spec.
- Push the branch and open the PR as a **draft**.
