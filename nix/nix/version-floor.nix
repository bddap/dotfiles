# Lowest versions the nixpkgs pin may carry for network-facing packages.
let
  pkgs = import ./. { };
  inherit (pkgs) lib;
  floor = {
    openssl = "3.6.5";
    openssh = "10.5p1";
    glibc = "2.42";
    cups = "2.4.19";
    curl = "8.22.0";
    unbound = "1.26.0";
    rsync = "3.5.0";
  };
  below = lib.filterAttrs (name: min: lib.versionOlder pkgs.${name}.version min) floor;
  report = lib.mapAttrsToList (name: min: "${name} ${pkgs.${name}.version} < ${min}") below;
in
lib.assertMsg (below == { }) "nixpkgs pin below version floor: ${lib.concatStringsSep ", " report}"
