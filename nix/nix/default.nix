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
    };
  };
in
{
  pkgs ? import sources.nixos-stable { },
}:
pkgs.appendOverlays [ overlay ]
