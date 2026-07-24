# Build the disko format+mount script for a given disk.
# Usage:
#   nix-build nix/nixos/disko-format-mount.nix \
#     --argstr disk /dev/sdX --argstr passwordFile /path/to/keyfile
{ disk, passwordFile }:
let
  pkgs = import ../nix { };
  sources = import ../nix/sources.nix;
  disko = import sources.disko { };
  diskoConfig = import ./disko.nix { inherit disk passwordFile; };
in disko._cliFormatMount diskoConfig pkgs
