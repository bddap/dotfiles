# Entry point for server.
# HOSTNAME env var is read at eval time.
{ ... }:
let
  hostname = builtins.getEnv "HOSTNAME";
in {
  imports = [
    ./common.nix
    ./server-hardware.nix
    ./bot-agent
  ];

  networking.hostName =
    if hostname != "" then hostname
    else throw "HOSTNAME env var must be set";

  security.sudo.wheelNeedsPassword = false;

  networking.firewall.allowedTCPPorts = [ 25565 ];
  networking.firewall.allowedTCPPortRanges = [
    { from = 4096; to = 4200; }
  ];

  users.users.bot = {
    isNormalUser = true;
    description = "bot";
    # wheel => passwordless sudo (security.sudo.wheelNeedsPassword = false above).
    # bot runs the long-running assistant agents and maintains this machine.
    extraGroups = [ "wheel" "networkmanager" "libvirtd" "kvm" ];
  };
}
