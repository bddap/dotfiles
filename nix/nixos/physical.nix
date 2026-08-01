# Entry point for physical machines.
# Host-specific config (hostname, generated hardware config, tuning)
# lives in nix/nixos/local/ — see common.nix.
{ lib, ... }: {
  imports = [
    ./common.nix
  ];

  # Placeholders so the tree evaluates on a fresh clone (no
  # nix/nixos/local/ yet), e.g. for CI. A real machine's local/ overrides
  # these via normal priority (mkDefault loses to any plain definition).
  fileSystems."/" = lib.mkDefault { device = "none"; fsType = "tmpfs"; };
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
