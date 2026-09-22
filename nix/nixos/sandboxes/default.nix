{ config, lib, pkgs, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.virtualisation.sandboxes;
  hostUser = config.users.users.${cfg.hostUser}
    or (throw "virtualisation.sandboxes.hostUser: no such user ${cfg.hostUser}");

  guest = name: sb: pkgs.nixos [
    ({ modulesPath, ... }: {
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
          "-sandbox on,obsolete=deny,elevateprivileges=deny,spawn=deny,resourcecontrol=deny"
        ];
      };
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
      system.stateVersion = lib.trivial.release;
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
          credentials and checkouts go here. A normalized absolute path,
          not inside another sandbox's home. Created by hostUser before
          each start, so its parent must be writable by hostUser; mode 0700
          is re-applied at every start.
        '';
      };
      disk = mkOption {
        type = path;
        default = "${hostUser.home}/sandboxes/${name}/root.qcow2";
        defaultText = "<home of hostUser>/sandboxes/<name>/root.qcow2";
        description = ''
          The sandbox's root disk, a sparse qcow2 of
          `virtualisation.diskSize` MiB the runner creates on first start
          and keeps across restarts and host reboots; root and the writable
          store overlay live on it. Its directory is created 0700 by
          hostUser like the home and is what the VM's qemu process can see
          besides the home, so give the disk its own directory. Stop the
          sandbox and delete the file to start over.
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

  collisions = field:
    lib.filterAttrs (_: names: lib.length names > 1)
      (lib.groupBy (name: toString cfg.vms.${name}.${field}) (lib.attrNames cfg.vms));

  nested = lib.concatMap (outer:
    map (inner: { inherit outer inner; })
      (lib.filter (inner: lib.hasPrefix "${cfg.vms.${outer}.home}/" cfg.vms.${inner}.home) (lib.attrNames cfg.vms)))
    (lib.attrNames cfg.vms);
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
        `/run/sandbox/<name>/console`.
      '';
    };
  };

  config = {
    assertions =
      lib.concatMap (field:
        lib.mapAttrsToList (value: names: {
          assertion = false;
          message = "virtualisation.sandboxes: ${field} ${value} is used by ${lib.concatStringsSep ", " names}";
        }) (collisions field)) [ "sshPort" "home" "disk" ]
      ++ map ({ outer, inner }: {
        assertion = false;
        message = "virtualisation.sandboxes: home ${cfg.vms.${inner}.home} of ${inner} is inside home ${cfg.vms.${outer}.home} of ${outer}";
      }) nested
      ++ [
        {
          assertion = lib.all (name: builtins.match "[[:alnum:]-]+" name != null) (lib.attrNames cfg.vms);
          message = "virtualisation.sandboxes.vms: a name is the guest's host name and part of its unit name; use letters, digits and dashes";
        }
        {
          assertion = cfg.vms == { } || hostUser.uid != null;
          message = "virtualisation.sandboxes: users.users.${cfg.hostUser}.uid must be set; the agent user inside each sandbox gets that uid so the shared home is owned consistently on both sides";
        }
      ];

    systemd.services = lib.concatMapAttrs (name: sb: {
      "sandbox-home@${name}" = {
        description = "home and disk directory of sandbox VM ${name}";
        unitConfig.RequiresMountsFor = [ sb.home (dirOf sb.disk) ];
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
