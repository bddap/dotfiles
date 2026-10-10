{ lib, pkgs, ... }:
let
  appPkgs = import pkgs.path {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
  chatgpt = appPkgs.callPackage ../../nix/chatgpt.nix { };
in {
  virtualisation.sharedDirectories.home.target = lib.mkForce "/home/agent/shared";
  powerManagement.enable = false;
  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    displayManager.lightdm.enable = true;
  };
  services.displayManager = {
    autoLogin = { enable = true; user = "agent"; };
    defaultSession = "xfce";
  };
  services.gnome.gnome-keyring.enable = true;
  environment.systemPackages = [ chatgpt pkgs.firefox pkgs.git pkgs.bubblewrap ];
  environment.etc."xdg/autostart/chatgpt.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=ChatGPT
    Exec=${chatgpt}/bin/chatgpt --ozone-platform=x11
    Terminal=false
  '';
}
