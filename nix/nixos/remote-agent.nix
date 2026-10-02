# Remote control over ssh without a reachable address: dumbpipe dials out
# through iroh (direct when NAT allows, otherwise via n0's public relays)
# and forwards each incoming stream to this machine's sshd. Import from
# nix/nixos/local/default.nix.
#
# The ticket only locates the machine; keep it private all the same. The
# tunnel lands on a loopback address where sshd admits only root, only by
# a key in services.remote-agent.authorizedKeys, which nothing else reads.
# The iroh secret in /var/lib/private/remote-agent fixes the ticket across
# restarts and networks; n0's discovery service finds the machine from it.
#
# Setup — in local/default.nix:
#   services.remote-agent.authorizedKeys = [ "ssh-ed25519 AAAA..." ];
# then `./nixos-build switch` and read the ticket:
#   journalctl -u remote-agent -o cat | sed -n 's/^ticket: //p' | tail -1
# The controller connects with dumbpipe 0.39 or newer (older releases cannot
# read the ticket; `nix-build nix/nix -A bddap.dumbpipe` builds this one):
#   ssh -o ProxyCommand='dumbpipe connect <ticket>' -o HostKeyAlias=<name> root@<name>
#
# Revoke a controller: drop its key, switch, and end its live sessions
# (`loginctl terminate-user root`). That trusts it left nothing behind as
# root; if not, rotate the identity below and reinstall.
# Rotate the machine's identity: `systemctl stop remote-agent`, delete
# /var/lib/private/remote-agent/secret, start it; the old ticket is dead.
# Remove: drop the import from local/ and switch.
# Roll back any switch: `sudo nixos-rebuild switch --rollback`, or pick the
# previous generation in the boot menu.
{ config, lib, ... }:
let
  pkgs = import ../nix { };
  dumbpipe = "${pkgs.bddap.dumbpipe}/bin/dumbpipe";
  tunnelAddress = "127.0.0.2";
  sshPort = builtins.head config.services.openssh.ports;
in {
  options.services.remote-agent.authorizedKeys = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    description = "Public keys that may log in as root through the tunnel.";
  };

  config = {
    assertions = [{
      assertion = config.services.openssh.listenAddresses == [ ];
      message = "remote-agent needs services.openssh.listenAddresses = [ ] so sshd also listens on ${tunnelAddress}";
    }];

    # A copy, not a store symlink: sshd refuses keys under the group-writable /nix/store.
    environment.etc."ssh/remote-agent-keys" = {
      mode = "0444";
      text = lib.concatLines config.services.remote-agent.authorizedKeys;
    };

    services.openssh.enable = true;
    # Every tunnelled connection comes from 127.0.0.1; without the exemption,
    # anyone holding the ticket could earn penalties that lock the controller out.
    services.openssh.settings.PerSourcePenaltyExemptList = "127.0.0.0/8";
    services.openssh.extraConfig = lib.mkAfter ''
      Match LocalAddress ${tunnelAddress}
        AllowUsers root
        AuthorizedKeysFile /etc/ssh/remote-agent-keys
        AuthenticationMethods publickey
    '';

    systemd.services.remote-agent = {
      description = "Dial-out ssh tunnel (dumbpipe)";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" "sshd.service" ];
      environment.RUST_LOG = "info";
      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "remote-agent";
        UMask = "0077";
        Restart = "always";
        RestartSec = 5;
      };
      script = ''
        secret=$STATE_DIRECTORY/secret
        if [ ! -s "$secret" ]; then
          od -An -tx1 -N32 /dev/urandom | tr -d ' \n' > "$secret.new"
          mv "$secret.new" "$secret"
        fi
        IROH_SECRET=$(cat "$secret")
        export IROH_SECRET
        ticket=$(${dumbpipe} generate-ticket)
        echo "ticket: $ticket"
        exec ${dumbpipe} listen-tcp --host ${tunnelAddress}:${toString sshPort}
      '';
    };
  };
}
