#!/usr/bin/env bash
# Thin keep-alive for the bddap-bot assistant. All intelligence lives in the
# agent + ~/CLAUDE.md; this just launches one long-lived Claude Code session and
# lets systemd restart it if it ever exits. The agent's own monitor loop (driven
# by ~/CLAUDE.md, using the bot-agent-poll helper) is the real event loop.
set -uo pipefail
cd "$HOME"

# Self-heal the agent's editable assets: install the known-good default from the
# nix store only if missing, so the agent's own edits are preserved but a fresh
# machine (or wiped home) still comes up working.
mkdir -p "$HOME/.local/bin" "$HOME/.claude/skills/agent-docs"
[ -f "$HOME/.local/bin/bot-agent-poll" ] || [ -z "${BOT_AGENT_POLL_SRC:-}" ] || \
  install -m 0755 "$BOT_AGENT_POLL_SRC" "$HOME/.local/bin/bot-agent-poll"
[ -f "$HOME/.local/bin/run-untrusted" ] || [ -z "${BOT_AGENT_RUNUNTRUSTED_SRC:-}" ] || \
  install -m 0755 "$BOT_AGENT_RUNUNTRUSTED_SRC" "$HOME/.local/bin/run-untrusted"
[ -f "$HOME/CLAUDE.md" ] || [ -z "${BOT_AGENT_CLAUDEMD_SRC:-}" ] || \
  { install -m 0644 "$BOT_AGENT_CLAUDEMD_SRC" "$HOME/CLAUDE.md"; ln -sf CLAUDE.md "$HOME/AGENTS.md"; }
[ -f "$HOME/.claude/skills/agent-docs/SKILL.md" ] || [ -z "${BOT_AGENT_SKILL_SRC:-}" ] || \
  install -m 0644 "$BOT_AGENT_SKILL_SRC" "$HOME/.claude/skills/agent-docs/SKILL.md"

BOOT='You are the bddap-bot personal-assistant daemon. Your operating manual is your CLAUDE.md (already in context). Enter your persistent event-monitor loop now and never exit it: wait for the next inbound event with `bot-agent-poll --wait` (run it in the background), handle whatever it reports, then wait again. Begin.'

# Resume prior context across restarts when a session exists; else start fresh.
exec claude -p "$BOOT" --continue --dangerously-skip-permissions \
  || exec claude -p "$BOOT" --dangerously-skip-permissions
