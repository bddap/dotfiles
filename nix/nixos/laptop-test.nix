let
  pkgs = import ../nix { };
  local = {
    imports = [
      ./laptop-hardware.nix
      ./framework-16-nvidia.nix
      ./steam.nix
    ];
    fileSystems."/" = { device = "/dev/disk/by-label/root"; fsType = "ext4"; };
  };
  laptop = import "${pkgs.path}/nixos" { configuration.imports = [ ./default.nix local ]; };
in {
  system = laptop.config.system.build.toplevel;

  steamEnv = pkgs.runCommand "steam-prime-env" {
    closure = pkgs.closureInfo { rootPaths = [ laptop.config.programs.steam.package ]; };
  } ''
    profile=$(grep -- '-fhsenv-profile$' $closure/store-paths)/etc/profile
    for v in __NV_PRIME_RENDER_OFFLOAD=1 __VK_LAYER_NV_optimus=NVIDIA_only __GLX_VENDOR_LIBRARY_NAME=nvidia; do
      grep -qxF "$v" "$profile" || { echo "missing $v in $profile"; exit 1; }
    done
    touch $out
  '';
}
