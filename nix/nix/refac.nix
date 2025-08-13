{ pkg-config, openssl, bddap, ... }:
let
  craneLib = bddap.craneLib;
  sources = import ./sources.nix;
in craneLib.buildPackage {
  src = sources.refac;
  buildInputs = [ pkg-config openssl ];
}
