{ config, options, lib, pkgs, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.virtualisation.sandboxes;
  hostUser = config.users.users.${cfg.hostUser}
    or (throw "virtualisation.sandboxes.hostUser: no such user ${cfg.hostUser}");

  stopTimeout = 120;
  powerdown = pkgs.writeShellScript "sandbox-powerdown" ''
    [ -z "$MAINPID" ] || printf '{"execute":"qmp_capabilities"}{"execute":"system_powerdown"}' \
      | ${pkgs.socat}/bin/socat -t ${toString stopTimeout} - UNIX-CONNECT:"$RUNTIME_DIRECTORY"/qmp,shut-none
  '';

  guest = name: sb: pkgs.nixos [
    ({ config, modulesPath, ... }: {
      imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];
      networking.hostName = name;
      virtualisation = {
        diskImage = sb.disk;
        diskSize = lib.mkDefault 32768;
        useNixStoreImage = true;
        writableStore = true;
        writableStoreUseTmpfs = false;
        graphics = false;
        memorySize = lib.mkDefault 4096;
        cores = lib.mkDefault 2;
        sharedDirectories.home = {
          source = sb.home;
          target = "/home/agent";
          securityModel = "none";
        };
        forwardPorts = [{
          from = "host";
          host.address = "127.0.0.1";
          host.port = sb.sshPort;
          guest.port = 22;
        }];
        qemu.options = [
          "-monitor none"
          "-serial stdio"
          "-chardev socket,id=console,path=\${RUNTIME_DIRECTORY:-$TMPDIR}/console,server=on,wait=off"
          "-serial chardev:console"
          "-qmp unix:\${RUNTIME_DIRECTORY:-$TMPDIR}/qmp,server=on,wait=off"
          "-sandbox on,obsolete=deny,elevateprivileges=deny,spawn=deny,resourcecontrol=deny"
        ];
      };
      boot.postBootCommands = lib.mkAfter "${config.nix.package}/bin/nix-store --verify";
      services.openssh = {
        enable = true;
        settings.PasswordAuthentication = lib.mkDefault false;
        settings.KbdInteractiveAuthentication = lib.mkDefault false;
      };
      services.getty.autologinUser = "agent";
      systemd.targets.getty.wants = [ "serial-getty@ttyS1.service" ];
      systemd.services."serial-getty@ttyS0".enable = false;
      users.users.agent = {
        isNormalUser = true;
        uid = hostUser.uid;
        extraGroups = [ "wheel" ];
      };
      security.sudo.wheelNeedsPassword = false;
      system.stateVersion = lib.mkDefault "25.11";
    })
    sb.module
  ];

  path = types.strMatching "(/[.]*[[:alnum:]_-][[:alnum:]._-]*)+";

  sandbox = { name, config, ... }: {
    options = {
      module = mkOption {
        type = types.deferredModule;
        description = ''
          NixOS module for the guest, qemu-vm options included
          (`virtualisation.memorySize`, `virtualisation.cores`,
          `virtualisation.diskSize`, further
          `virtualisation.sharedDirectories`, `virtualisation.qemu.options`).
          The guest user is `agent`, carrying the host user's uid, with
          passwordless sudo; give it
          `users.users.agent.openssh.authorizedKeys.keys` for ssh.
        '';
      };
      home = mkOption {
        type = path;
        default = "${hostUser.home}/sandboxes/${name}/home";
        defaultText = "<home of hostUser>/sandboxes/<name>/home";
        description = ''
          Host directory mounted as /home/agent inside the sandbox;
          credentials and checkouts go here. An absolute path of letters,
          digits, `.`, `_`, `-` and `/`, normalized, not inside another
          sandbox's home or disk directory. Created by hostUser before each
          start, so its parent must be writable by hostUser; mode 0700 is
          re-applied at every start.
        '';
      };
      disk = mkOption {
        type = path;
        default = "${hostUser.home}/sandboxes/${name}/root.qcow2";
        defaultText = "<home of hostUser>/sandboxes/<name>/root.qcow2";
        description = ''
          The sandbox's root disk, a sparse qcow2 the runner creates on the
          first start, sized by `virtualisation.diskSize` then, and keeps
          across restarts and host reboots; root and the writable store
          overlay live on it. Its directory is created 0700 by hostUser like
          the home and is what the VM's qemu process can see besides the
          home, so it must be the sandbox's own: not hostUser's home itself,
          not shared with or inside another sandbox's home or disk directory,
          not inside this sandbox's home. Stop the sandbox and delete the
          file to start over.
        '';
      };
      sshPort = mkOption {
        type = types.ints.between 1024 65535;
        description = "Host loopback port forwarded to the sandbox's sshd.";
      };
      guest = mkOption {
        type = types.raw;
        readOnly = true;
        internal = true;
        default = guest name config;
      };
    };
  };

  names = lib.attrNames cfg.vms;

  ports = lib.filterAttrs (_: users: lib.length users > 1)
    (lib.groupBy (name: toString cfg.vms.${name}.sshPort) names);

  within = p: q: p == q || lib.hasPrefix "${q}/" p;
  state = name: { home = cfg.vms.${name}.home; "disk directory" = dirOf cfg.vms.${name}.disk; };
  overlaps = lib.concatMap (a: lib.concatMap (b: lib.concatMap (ka: lib.concatMap (kb:
    let p = (state b).${kb}; q = (state a).${ka}; in
    lib.optional ((a != b || (ka == "home" && kb == "disk directory")) && within p q && (p != q || a <= b))
      "virtualisation.sandboxes: ${kb} ${p} of ${b} ${if p == q then "is also" else "is inside"} ${ka} ${q} of ${a}")
    [ "disk directory" "home" ]) [ "disk directory" "home" ]) names) names;
in
{
  options.virtualisation.sandboxes = {
    hostUser = mkOption {
      type = types.str;
      description = "User the sandbox VMs run as; owns their homes and disks, and its uid is the guest user's.";
    };
    vms = mkOption {
      type = types.attrsOf (types.submodule sandbox);
      default = { };
      example = lib.literalExpression ''
        let
          base = { users.users.agent.openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAA..." ]; };
        in {
          codex = {
            sshPort = 2201;
            module = { pkgs, ... }: {
              imports = [ base ];
              virtualisation.memorySize = 8192;
              environment.systemPackages = [ pkgs.bddap.codex ];
            };
          };
          claude = {
            sshPort = 2202;
            module = { pkgs, ... }: {
              imports = [ base ];
              environment.systemPackages = [ pkgs.nodejs ];
            };
          };
        }
      '';
      description = ''
        Headless NixOS VMs, one systemd service each
        (`sandbox-<name>.service`); a rebuild starts the declared ones and
        stops the removed ones. Reach a sandbox with
        `ssh -p <sshPort> agent@localhost` or on its serial console at
        `/run/sandbox/<name>/console`. Stopping one powers the guest off
        through its QMP socket `/run/sandbox/<name>/qmp`; qemu gets SIGTERM
        if the guest is still up ${toString stopTimeout} seconds later.
      '';
    };
  };

  config = {
    assertions =
      lib.mapAttrsToList (port: users: {
        assertion = false;
        message = "virtualisation.sandboxes: sshPort ${port} is used by ${lib.concatStringsSep ", " users}";
      }) ports
      ++ map (message: { assertion = false; inherit message; }) overlaps
      ++ lib.mapAttrsToList (name: sb: {
        assertion = dirOf sb.disk != hostUser.home;
        message = "virtualisation.sandboxes: disk ${sb.disk} of ${name} lies directly in the home of ${cfg.hostUser}; give it its own directory";
      }) cfg.vms
      ++ map (name: {
        assertion = name != "" && options.networking.hostName.type.check name;
        message = "virtualisation.sandboxes.vms: name \"${name}\" is the guest's networking.hostName and part of its unit name; it must be a non-empty DNS label (that option's type)";
      }) names
      ++ [
        {
          assertion = cfg.vms == { } || (hostUser.uid != null && hostUser.uid >= 1000);
          message = "virtualisation.sandboxes: users.users.${cfg.hostUser}.uid must be set and 1000 or above; the agent user inside each sandbox is a normal user with that uid (nixos/modules/config/users-groups.nix), so the shared home is owned consistently on both sides";
        }
      ];

    systemd.services = lib.concatMapAttrs (name: sb: {
      "sandbox-home@${name}" = {
        description = "home and disk directory of sandbox VM ${name}";
        unitConfig = {
          RequiresMountsFor = [ sb.home (dirOf sb.disk) ];
          StartLimitIntervalSec = 0;
        };
        serviceConfig = {
          Type = "oneshot";
          User = cfg.hostUser;
          ExecStart = "${pkgs.coreutils}/bin/install -d -m 0700 ${sb.home} ${dirOf sb.disk}";
        };
      };
      "sandbox-${name}" = {
        description = "sandbox VM ${name}";
        wantedBy = [ "multi-user.target" ];
        requires = [ "sandbox-home@${name}.service" ];
        after = [ "sandbox-home@${name}.service" ];
        unitConfig.RequiresMountsFor = [ sb.home (dirOf sb.disk) ];
        serviceConfig = {
          ExecStart = lib.getExe sb.guest.vm;
          ExecStop = "${powerdown}";
          TimeoutStopSec = stopTimeout;
          User = cfg.hostUser;
          SupplementaryGroups = [ "kvm" ];
          RuntimeDirectory = "sandbox/${name}";
          RuntimeDirectoryMode = "0700";
          PrivateTmp = true;
          ProtectHome = "tmpfs";
          BindPaths = [ sb.home (dirOf sb.disk) ];
          ProtectSystem = "strict";
          NoNewPrivileges = true;
          CapabilityBoundingSet = "";
        };
      };
    }) cfg.vms;
  };
}
