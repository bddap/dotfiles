let
  sources = import ./sources.nix;
  use_nodejs_22 = (
    final: prev: {
      nodejs = final.nodejs_22;
    }
  );
  overlay = final: prev: {
    bddap = {
      sources = sources;
      nixpkgs-unstable = import sources.nixpkgs-unstable { };
      craneLib = import sources.crane { pkgs = final; };
      refac = import ./refac.nix final;
      uv = import ./uv.nix final.bddap.nixpkgs-unstable.pkgs;
      codex = import ./codex.nix (final.appendOverlays [ use_nodejs_22 ]);
      shitty-nixpath = "nixpkgs=${sources.nixpkgs}:home-manager=${final.home-manager.src}";
      zoom = import ./zoom.nix final;
    };
  };
  pkgs = import sources.nixpkgs { overlays = [ overlay ]; };
in
{ ... }:
pkgs
