let
  pkgs = import ../nix { };
  keys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  local = {
    imports = [
      ./laptop-hardware.nix
      ./framework-16-nvidia.nix
      ./steam.nix
      ./remote-agent.nix
    ];
    fileSystems."/" = { device = "/dev/disk/by-label/root"; fsType = "ext4"; };
    services.remote-agent.authorizedKeys = [ keys.snakeOilPublicKey ];
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

  agent = pkgs.testers.runNixOSTest {
    name = "remote-agent";
    nodes.machine = {
      imports = [ ./remote-agent.nix ];
      services.remote-agent.authorizedKeys = [ keys.snakeOilPublicKey ];
      users.users.root.password = "pw";
      users.users.root.hashedPasswordFile = pkgs.lib.mkForce null;
      users.users.a = { isNormalUser = true; password = "pw"; };
      services.openssh.settings.PasswordAuthentication = true;
      services.openssh.settings.PermitRootLogin = "yes";
      environment.systemPackages = [ pkgs.sshpass pkgs.bddap.dumbpipe ];
    };
    testScript = ''
      machine.wait_for_unit("sshd.service")
      machine.wait_for_unit("remote-agent.service")
      machine.succeed("install -m 600 ${keys.snakeOilPrivateKey} /root/key")
      machine.succeed("install -d -o a -m 700 /home/a/.ssh && install -o a -m 600 ${pkgs.writeText "a-key" keys.snakeOilPublicKey} /home/a/.ssh/authorized_keys")
      opts = "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
      key = f"{opts} -o BatchMode=yes -i /root/key"
      secret = "/var/lib/private/remote-agent/secret"

      machine.succeed(f"[ $(tr -d '\\n' < {secret} | wc -c) = 64 ]")
      machine.succeed(f"[ $(stat -c %a {secret}) = 600 ]")

      machine.succeed(f"sshpass -p pw ssh {opts} a@127.0.0.1 true")
      machine.fail(f"sshpass -p pw ssh {opts} a@127.0.0.2 true")
      machine.succeed(f"ssh {key} a@127.0.0.1 true")
      machine.fail(f"ssh {key} a@127.0.0.2 true")
      machine.succeed(f"ssh {key} root@127.0.0.2 true")
      machine.succeed(f"sshpass -p pw ssh {opts} root@127.0.0.1 true")
      machine.fail(f"sshpass -p pw ssh {opts} root@127.0.0.2 true")

      machine.succeed("ssh-keygen -q -t ed25519 -N \"\" -f /root/other")
      machine.succeed("install -d -m 700 /root/.ssh && cp /root/other.pub /root/.ssh/authorized_keys")
      other = f"{opts} -o BatchMode=yes -i /root/other"
      machine.succeed(f"ssh {other} root@127.0.0.1 true")
      machine.fail(f"ssh {other} root@127.0.0.2 true")

      def ticket(prefix="ticket: "):
          return machine.wait_until_succeeds(
              "journalctl -o cat _SYSTEMD_INVOCATION_ID=$(systemctl show -P InvocationID remote-agent)"
              f" | grep -o '^{prefix}endpoint[a-z0-9]*' | tail -1 | sed 's/^{prefix}//' | grep .",
              timeout=60,
          ).strip()

      # The VM has no internet, so discovery cannot resolve the id-only
      # ticket; the listener's own ticket carries its local addresses.
      local = ticket("dumbpipe connect-tcp ")
      out = machine.succeed(
          f"ssh {key} -o ProxyCommand='dumbpipe connect {local}' root@remote echo through-the-pipe"
      )
      assert "through-the-pipe" in out, out
      machine.fail(f"sshpass -p pw ssh {opts} -o ProxyCommand='dumbpipe connect {local}' a@remote true")
      machine.fail(f"ssh {key} -o ProxyCommand='dumbpipe connect {local}' a@remote true")

      first = ticket()
      documented = machine.succeed(
          "journalctl -u remote-agent -o cat | sed -n 's/^ticket: //p' | tail -1"
      ).strip()
      assert documented == first, (documented, first)
      machine.succeed("systemctl restart remote-agent")
      assert ticket() == first

      machine.succeed(f"systemctl stop remote-agent && rm {secret} && systemctl start remote-agent")
      assert ticket() != first
    '';
  };
}
