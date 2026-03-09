# Entry point for install-to-disk.
# HOSTNAME and SSH_PUBKEY env vars are read at eval time.
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
}
