#!/usr/bin/env bash
# Thin keep-alive for the bddap-bot assistant. All intelligence lives in the
# agent + ~/CLAUDE.md; this just launches one long-lived Claude Code session and
# lets systemd restart it if it ever exits. The agent's own monitor loop (driven
# by ~/CLAUDE.md, using the bot-agent-poll helper) is the real event loop.
set -uo pipefail
cd "$HOME"

# Self-heal: ensure the editable event-detection helper exists on the agent's
# PATH. Installed only if missing, so the agent's own edits are preserved; a
# fresh machine still gets the known-good default from the nix store.
mkdir -p "$HOME/.local/bin"
if [ ! -f "$HOME/.local/bin/bot-agent-poll" ] && [ -n "${BOT_AGENT_POLL_SRC:-}" ]; then
  install -m 0755 "$BOT_AGENT_POLL_SRC" "$HOME/.local/bin/bot-agent-poll"
fi

BOOT='You are the bddap-bot personal-assistant daemon. Your operating manual is your CLAUDE.md (already in context). Enter your persistent event-monitor loop now and never exit it: wait for the next inbound event with `bot-agent-poll --wait` (run it in the background), handle whatever it reports, then wait again. Begin.'

# Resume prior context across restarts when a session exists; else start fresh.
exec claude -p "$BOOT" --continue --dangerously-skip-permissions \
  || exec claude -p "$BOOT" --dangerously-skip-permissions
