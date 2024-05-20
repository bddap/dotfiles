{ craneLib, pkgs }:
let sources = import ./sources.nix;
in craneLib.buildPackage {
  src = sources.refac;
  buildInputs = [ pkgs.pkg-config pkgs.openssl ];
}
