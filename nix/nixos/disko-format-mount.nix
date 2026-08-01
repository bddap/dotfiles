# Build the disko format+mount script for a given disk.
# Usage:
#   nix-build nix/nixos/disko-format-mount.nix \
#     --argstr disk /dev/sdX --argstr passwordFile /path/to/keyfile
{ disk, passwordFile }:
let
  pkgs = import ../nix { };
  sources = import ../nix/sources.nix;
  # Pass lib explicitly: disko's default is `import <nixpkgs/lib>`, which
  # would depend on ambient NIX_PATH (unpinned, or absent on machines
  # installed by this very flow).
  disko = import sources.disko { lib = pkgs.lib; };
  diskoConfig = import ./disko.nix { inherit disk passwordFile; };
# _cliFormatMount is underscore-private disko API — revisit on a disko bump.
in disko._cliFormatMount diskoConfig pkgs
