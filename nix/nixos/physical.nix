# Entry point for physical machines.
# Hostname is set in hardware-configuration.nix (not checked in).
{ ... }: {
  imports = [
    ./common.nix
    ./hardware-configuration.nix
  ];
}
