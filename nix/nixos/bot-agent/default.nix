# Long-running personal-assistant agent for the `bot` user.
#
# Deliberately minimal: one systemd service keeps a single Claude Code session
# alive as `bot`. The agent itself monitors Telegram + GitHub and acts, using
# bash + gh + curl + the Monitor tool — driven entirely by ~/CLAUDE.md. No custom
# daemon, no bespoke tools (the "bitter lesson": give the agent context and
# general tools, not a hand-rolled harness).
#
# Secrets / allowlists come from /home/bot/.secrets/bot-agent.env (out of git and
# the nix store), loaded via systemd EnvironmentFile (optional via "-").
{ pkgs, lib, ... }:
let
  # Generous PATH so the agent can do real work. /run/wrappers/bin first for
  # sudo; bot's native-installer claude lives in ~/.local/bin.
  toolPath = lib.makeBinPath [
    pkgs.nodejs pkgs.gh pkgs.git pkgs.coreutils pkgs.gnugrep pkgs.gnused
    pkgs.gawk pkgs.curl pkgs.jq pkgs.bash pkgs.util-linux pkgs.openssh pkgs.nix
  ];
  fullPath = "/run/wrappers/bin:/home/bot/.local/bin:${toolPath}:/run/current-system/sw/bin";
in {
  systemd.services.bot-agent = {
    description = "bddap-bot personal-assistant agent (Telegram + GitHub)";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      HOME = "/home/bot";
      PATH = lib.mkForce fullPath;
      AGENT_STATE_DIR = "/home/bot/.local/state/bot-agent";
      GITHUB_ALLOWED_LOGINS = "bddap";
      # Known-good default of the editable event-detection helper; the launcher
      # installs it to ~/.local/bin/bot-agent-poll if absent (agent may edit).
      BOT_AGENT_POLL_SRC = "${./bot-agent-poll}";
    };
    serviceConfig = {
      User = "bot";
      Group = "users";
      WorkingDirectory = "/home/bot";
      EnvironmentFile = "-/home/bot/.secrets/bot-agent.env";
      ExecStart = "${pkgs.bash}/bin/bash ${./bot-agent-loop.sh}";
      Restart = "always";
      RestartSec = 15;
    };
    startLimitIntervalSec = 0;
  };
}
