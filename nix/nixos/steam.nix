# Steam with Proton and SteamVR (both installed from inside Steam).
# Import from nix/nixos/local/default.nix.
#
# Under PRIME offload the whole client starts on the NVIDIA GPU, so every
# game, Proton prefix and SteamVR process it spawns inherits it.
{ config, lib, pkgs, ... }:
{
  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      extraEnv = lib.optionalAttrs config.hardware.nvidia.prime.offload.enable {
        __NV_PRIME_RENDER_OFFLOAD = "1";
        __VK_LAYER_NV_optimus = "NVIDIA_only";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    };
    # Steam Link and VR streaming reach the client on its Remote Play ports.
    remotePlay.openFirewall = true;
  };
}
