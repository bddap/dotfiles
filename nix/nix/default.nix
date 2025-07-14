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
      shitty-nixpath = final.writeTextFile {
        name = "shitty-nixpath";
        text = "nixpkgs=${sources.nixpkgs}:home-manager=${final.home-manager.src}";
      };
      codex = import ./codex.nix final;
    };
  };
  pkgs = import sources.nixpkgs { overlays = [ overlay ]; };
in
{ ... }:
pkgs
