# Entry point for physical machines.
# Hostname is set in hardware-configuration.nix (not checked in).
# Legacy seam: machines installed by ./install-to-disk use
# nix/nixos/hosts/<name>.nix instead. Once this machine is migrated to
# a committed hosts/<name>.nix, delete this file.
{ ... }: {
  imports = [
    ./common.nix
    ./hardware-configuration.nix
  ];
}
