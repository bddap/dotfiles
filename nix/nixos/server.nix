# Entry point for install-to-disk.
# HOSTNAME and INITIAL_PASSWORD_HASH env vars are read at eval time.
{ ... }:
let
  hostname = builtins.getEnv "HOSTNAME";
  initialPasswordHash = builtins.getEnv "INITIAL_PASSWORD_HASH";
in {
  imports = [
    ./common.nix
    ./server-hardware.nix
  ];

  networking.hostName =
    if hostname != "" then hostname
    else throw "HOSTNAME env var must be set";

  users.users.a.initialHashedPassword = initialPasswordHash;

  users.users.bot = {
    isNormalUser = true;
    description = "bot";
  };
}
