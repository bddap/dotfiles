let
  sources = import ./sources.nix;
  overlay = final: prev: {
    nixpkgs-unstable = import sources.nixpkgs-unstable { };
    craneLib = import sources.crane { pkgs = final; };
    refac = import ./refac.nix {
      pkgs = final;
      craneLib = final.craneLib;
    };
  };
in { pkgs ? import sources.nixos-stable { } }: pkgs.appendOverlays [ overlay ]
