# Zoom 7.2.1.5760 from nixpkgs d5276f27 (NixOS/nixpkgs#568883): the pin's
# derivation with that version, plus the libuuid 7.2 links against.
pkgs:
let
  version = "7.2.1.5760";
  hash = "sha256-Nuh5j0L1bqinPHcqrFlKMPFqGqTRUdYVdrPcgufS5Qo=";
  pinned = "${pkgs.bddap.sources.nixpkgs}/pkgs/by-name/zo/zoom-us/package.nix";
in
pkgs.callPackage
  (builtins.toFile "zoom-us-${version}.nix" (builtins.replaceStrings
    [
      ''versions.x86_64-linux = "7.1.5.4332";''
      ''hash = "sha256-5znZNrySgRrs9I5zhqN5p5dPfXpEHXKf8o2dWeYTPso=";''
    ]
    [
      ''versions.x86_64-linux = "${version}";''
      ''hash = "${hash}";''
    ]
    (builtins.readFile pinned)))
  { targetPkgsFixed = [ pkgs.util-linux.lib ]; }
