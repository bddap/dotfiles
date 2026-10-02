# Built from source rather than nixpkgs: its release predates iroh discovery,
# so a ticket held only the relay of the moment and went stale with it.
{ bddap, ... }:
let sources = import ./sources.nix;
in bddap.craneLib.buildPackage {
  src = sources.dumbpipe;
  # Every non-ignored CLI test fails in the network-less build sandbox;
  # nix/nixos/laptop-test.nix runs the binary in a VM instead.
  doCheck = false;
}
