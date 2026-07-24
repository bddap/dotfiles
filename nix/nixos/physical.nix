# Entry point for physical machines.
# Host-specific config — hostname, generated hardware config, per-machine
# tuning — lives in nix/nixos/local/ (gitignored), conditionally imported
# by common.nix. ./install-to-disk populates local/ on new machines; on an
# existing machine, move your hardware-configuration.nix into local/ and
# add a default.nix that imports it and sets networking.hostName.
{ lib, ... }: {
  imports = [
    ./common.nix
  ];

  # Placeholder so the tree evaluates on a fresh clone (no nix/nixos/local/
  # yet), e.g. for CI. A real machine's local/ hardware config overrides
  # this via normal priority (mkDefault loses to any plain definition).
  fileSystems."/" = lib.mkDefault { device = "none"; fsType = "tmpfs"; };
}
