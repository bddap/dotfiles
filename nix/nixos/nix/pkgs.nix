let sources = import ./sources.nix;
in import sources."nixos-23.11" {
  overlays = [
    (self: super: { nixpkgs-unstable = import sources.nixpkgs-unstable { }; })
  ];
}
