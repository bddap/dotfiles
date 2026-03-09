# Entry point for install-to-disk.
# HOSTNAME env var is read at eval time.
{ ... }:
let
  hostname = builtins.getEnv "HOSTNAME";
in {
  imports = [
    ./common.nix
    ./server-hardware.nix
  ];

  networking.hostName =
    if hostname != "" then hostname
    else throw "HOSTNAME env var must be set";

  users.users.bot = {
    isNormalUser = true;
    description = "bot";
  };
}
