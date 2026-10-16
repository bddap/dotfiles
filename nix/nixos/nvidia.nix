# NVIDIA GPU. Import from nix/nixos/local/default.nix on a machine that has one.
{ lib, ... }: {
  hardware.nvidia-container-toolkit.enable = true;
  hardware.nvidia-container-toolkit.mount-nvidia-executables = true;

  # Driver >= 560 requires this set; open modules need Turing+.
  hardware.nvidia.open = lib.mkDefault true;

  services.xserver.videoDrivers = [ "nvidia" ];
}
