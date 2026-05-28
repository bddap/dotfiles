---
name: agent-docs
description: >
  Anthropic's best practices for writing agent operating docs — CLAUDE.md /
  AGENTS.md memory files, Agent Skills (SKILL.md), and system prompts. Use when
  creating, editing, reviewing, or pruning a CLAUDE.md/AGENTS.md, authoring a
  skill, or tuning an agent's instructions/system prompt. Trigger phrases:
  "write/improve CLAUDE.md", "add a skill", "agent docs", "operating manual",
  "system prompt", "why is the agent ignoring its instructions".
---

# Writing agent docs (Anthropic guidance)

Distilled from Anthropic's *Claude Code best practices* and *Effective context
engineering for AI agents*. Apply these when writing any file the agent reads as
standing context.

## The core constraint: context is a finite attention budget

Everything in CLAUDE.md is loaded **every** session and every token spends the
model's attention. Performance *degrades* as context fills. So the goal is **the
minimal set of information that fully specifies the expected behavior** — minimal
≠ short, but never padded.

## CLAUDE.md / AGENTS.md

The litmus test for every line: **"Would removing this cause the agent to make a
mistake?"** If no, cut it. A bloated file makes the agent *ignore* real
instructions because they're lost in noise — that's the #1 failure mode.

| ✅ Include | ❌ Exclude |
|-----------|-----------|
| Commands the agent can't guess (exact invocations, env quirks) | Anything inferable by reading code/files |
| Conventions that differ from defaults | Standard conventions the model already knows |
| Workflow/repo etiquette (branch naming, PR flow, test runner) | Detailed API docs — link instead |
| Project-specific architectural decisions | Info that changes frequently |
| Non-obvious gotchas | File-by-file codebase descriptions |
| The security/safety boundary | Self-evident advice ("write clean code") |

- **Right altitude.** Don't hardcode brittle if-else trees; don't give vague
  hand-waving. Aim for specific-enough-to-guide, flexible-enough-to-be-heuristics.
  Exception: a *deterministic, repeated mechanical step* (e.g. an exact poll
  command) is fine to pin precisely — or better, factor it into a script the doc
  references, so it isn't re-derived each run.
- **Emphasis for adherence.** Mark the few rules that must never be violated with
  **IMPORTANT** / **YOU MUST**. Use sparingly — if everything is emphasized,
  nothing is.
- **Structure** with clear Markdown headers (or XML tags for system prompts) so
  the model can parse sections fast.
- **Examples > prose** for behavior: a couple of diverse, canonical examples beat
  a paragraph of description. Don't enumerate every edge case.
- **Keep it human-readable and concise.** No tutorials, no long explanations.

## When to use a skill instead

If a section of CLAUDE.md is **only relevant sometimes**, or has grown from a
*fact* into a *procedure/checklist*, move it to a skill. Skill bodies load **on
demand**, so long reference material costs ~nothing until needed — the opposite
of CLAUDE.md. Keep CLAUDE.md for what applies broadly and every session.

## Writing a skill (SKILL.md)

- Location: `~/.claude/skills/<name>/SKILL.md` (personal) or `.claude/skills/`
  (project). Directory name becomes the `/command`.
- Frontmatter: `name` (label) + **`description` (the most important field — it's
  how the model decides to auto-trigger the skill).** Lead with the key use case
  and concrete trigger phrases; the description is truncated ~1,536 chars in the
  listing, so front-load it. Optional: `when_to_use`, `allowed-tools`,
  `disable-model-invocation: true` (manual-only / side-effecting workflows).
- **Keep SKILL.md under ~500 lines.** Push detailed reference into sibling files
  in the skill dir and link to them (progressive disclosure).
- Two flavors: *reference* (facts/conventions the model applies when relevant) and
  *task* (numbered steps you usually invoke directly with `/name`).

## Iterate like code

Treat these files as code: review when behavior goes wrong, **prune regularly**,
and test a change by checking whether the agent's behavior *actually* shifts.
- Agent keeps doing the thing you forbade → the file is probably too long; the
  rule got lost. Prune, or enforce deterministically with a **hook**.
- Agent asks what's already documented → the phrasing is ambiguous; rewrite it.
- For guarantees that must hold every time with zero exceptions, prefer a **hook**
  (deterministic) over an advisory instruction.

## Verification mindset

Where the agent produces work, give it a way to *check* itself (a test, a build,
an API status code, a screenshot) and have it show evidence rather than asserting
success. Bake the check into the instructions so the loop can close on its own.
