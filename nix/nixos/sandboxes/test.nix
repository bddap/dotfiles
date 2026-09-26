let
  pkgs = import ../../nix { };
  inherit (pkgs) lib;
  keys = import "${pkgs.path}/nixos/tests/ssh-keys.nix" pkgs;
  base = { users.users.agent.openssh.authorizedKeys.keys = [ keys.snakeOilPublicKey ]; };
  vms = {
    alpha = {
      sshPort = 2201;
      module = {
        imports = [ base ];
        environment.etc.recipe.text = "alpha";
      };
    };
    beta = {
      sshPort = 2202;
      home = "/var/lib/tester/elsewhere/beta";
      module = { pkgs, ... }: {
        imports = [ base ];
        virtualisation.memorySize = 1024;
        virtualisation.cores = 1;
        virtualisation.diskSize = 8192;
        environment.etc.recipe.text = "beta";
        environment.systemPackages = [ pkgs.cowsay ];
      };
    };
  };
  smallAlpha = recipe: vms.alpha // {
    module = { imports = [ base ]; virtualisation.memorySize = 1024; virtualisation.cores = 1; environment.etc.recipe.text = recipe; };
  };

  host = extra: pkgs.nixos [
    ./.
    {
      fileSystems."/" = { device = "none"; fsType = "tmpfs"; };
      boot.loader.grub.enable = false;
      system.stateVersion = lib.trivial.release;
      users.users.tester = { isNormalUser = true; uid = 1000; home = "/var/lib/tester"; group = "tester"; };
      users.groups.tester = { };
    }
    extra
  ];
  declared = (host { virtualisation.sandboxes = { hostUser = "tester"; inherit vms; }; }).config;
  alpha = declared.virtualisation.sandboxes.vms.alpha;
  beta = declared.virtualisation.sandboxes.vms.beta;
  sandboxUnits = c: lib.filter (lib.hasPrefix "sandbox-") (lib.attrNames c.systemd.services);
  failing = c: map (a: a.message) (lib.filter (a: !a.assertion) c.assertions);
  rejected = extra: failing (host { virtualisation.sandboxes = { hostUser = "tester"; } // extra; }).config;

  failures = lib.runTests {
    testNothingDeclaredNoUnitsNoAssertions = {
      expr = let c = (host { }).config; in { units = sandboxUnits c; failing = failing c; };
      expected = { units = [ ]; failing = [ ]; };
    };
    testTwoUnitsPerSandbox = { expr = sandboxUnits declared; expected = [ "sandbox-alpha" "sandbox-beta" "sandbox-home@alpha" "sandbox-home@beta" ]; };
    testNoFailingAssertionsWhenDeclared = { expr = failing declared; expected = [ ]; };
    testUnitRunsTheGuestRunnerAsTheUser = {
      expr = with declared.systemd.services.sandbox-alpha; {
        inherit (serviceConfig) User SupplementaryGroups PrivateTmp RuntimeDirectory RuntimeDirectoryMode;
        runner = serviceConfig.ExecStart == lib.getExe alpha.guest.vm;
        inherit wantedBy;
      };
      expected = {
        User = "tester";
        SupplementaryGroups = [ "kvm" ];
        PrivateTmp = true;
        RuntimeDirectory = "sandbox/alpha";
        RuntimeDirectoryMode = "0700";
        runner = true;
        wantedBy = [ "multi-user.target" ];
      };
    };
    testUnitSeesOnlyTheHomeAndTheDiskDirectory = {
      expr = map (name: with declared.systemd.services."sandbox-${name}"; {
        inherit (serviceConfig) ProtectHome BindPaths ProtectSystem NoNewPrivileges CapabilityBoundingSet;
        mounts = unitConfig.RequiresMountsFor;
      }) [ "alpha" "beta" ];
      expected = map (paths: {
        mounts = paths;
        ProtectHome = "tmpfs";
        BindPaths = paths;
        ProtectSystem = "strict";
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
      }) [
        [ "/var/lib/tester/sandboxes/alpha/home" "/var/lib/tester/sandboxes/alpha" ]
        [ "/var/lib/tester/elsewhere/beta" "/var/lib/tester/sandboxes/beta" ]
      ];
    };
    testHomeAndDiskDefaultUnderTheUsersHome = {
      expr = { inherit (alpha) home disk; };
      expected = { home = "/var/lib/tester/sandboxes/alpha/home"; disk = "/var/lib/tester/sandboxes/alpha/root.qcow2"; };
    };
    testHomesAndDiskDirectoriesAreCreatedPrivateAsTheUserAfterTheirMount = {
      expr = map (name: with declared.systemd.services."sandbox-home@${name}"; {
        service = serviceConfig;
        inherit (unitConfig) RequiresMountsFor StartLimitIntervalSec;
        vm = { inherit (declared.systemd.services."sandbox-${name}") requires after; };
        activation = declared.system.activationScripts ? sandboxes;
      }) [ "alpha" "beta" ];
      expected = map (name: let sb = declared.virtualisation.sandboxes.vms.${name}; in {
        service = {
          Type = "oneshot";
          User = "tester";
          ExecStart = "${pkgs.coreutils}/bin/install -d -m 0700 ${sb.home} ${dirOf sb.disk}";
        };
        RequiresMountsFor = [ sb.home (dirOf sb.disk) ];
        StartLimitIntervalSec = 0;
        vm = { requires = [ "sandbox-home@${name}.service" ]; after = [ "sandbox-home@${name}.service" ]; };
        activation = false;
      }) [ "alpha" "beta" ];
    };
    testHomeIsTheOnlyHostShare = {
      expr = lib.mapAttrs (_: s: { inherit (s) source target securityModel; })
        (lib.filterAttrs (n: _: !(lib.elem n [ "xchg" "shared" ])) beta.guest.config.virtualisation.sharedDirectories);
      expected.home = { source = "/var/lib/tester/elsewhere/beta"; target = "/home/agent"; securityModel = "none"; };
    };
    testAgentIsTheHostUserWithSudo = {
      expr = with alpha.guest.config; {
        inherit (users.users.agent) uid;
        wheel = lib.elem "wheel" users.users.agent.extraGroups;
        nopasswd = !security.sudo.wheelNeedsPassword;
      };
      expected = { uid = 1000; wheel = true; nopasswd = true; };
    };
    testRootAndStoreOverlayOnTheDiskOwnStoreHeadless = {
      expr = with alpha.guest.config; {
        inherit (virtualisation) diskImage useNixStoreImage mountHostNixStore writableStore writableStoreUseTmpfs graphics;
        inherit (system) stateVersion;
        verify = lib.hasInfix "/bin/nix-store --verify" boot.postBootCommands;
      };
      expected = {
        diskImage = "/var/lib/tester/sandboxes/alpha/root.qcow2";
        useNixStoreImage = true;
        mountHostNixStore = false;
        writableStore = true;
        writableStoreUseTmpfs = false;
        graphics = false;
        stateVersion = "25.11";
        verify = true;
      };
    };
    testMemoryCoresDiskDefaultAndOverride = {
      expr = map (sb: { inherit (sb.guest.config.virtualisation) memorySize cores diskSize; }) [ alpha beta ];
      expected = [ { memorySize = 4096; cores = 2; diskSize = 32768; } { memorySize = 1024; cores = 1; diskSize = 8192; } ];
    };
    testSshKeysOnlyOnLoopback = {
      expr = with alpha.guest.config; {
        ports = map (p: { inherit (p.host) address port; guest = p.guest.port; }) virtualisation.forwardPorts;
        inherit (services.openssh) enable;
        inherit (services.openssh.settings) PasswordAuthentication KbdInteractiveAuthentication;
      };
      expected = {
        ports = [ { address = "127.0.0.1"; port = 2201; guest = 22; } ];
        enable = true;
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
    testConsoleOnJournalAndSocket = {
      expr = with alpha.guest.config; {
        serial = lib.filter (o: lib.any (p: lib.hasPrefix p o) [ "-monitor" "-serial" "-chardev" "-sandbox" ]) virtualisation.qemu.options;
        getty = lib.elem "serial-getty@ttyS1.service" systemd.targets.getty.wants;
        journalGetty = systemd.services."serial-getty@ttyS0".enable;
        autologin = services.getty.autologinUser;
      };
      expected = {
        serial = [
          "-monitor none"
          "-serial stdio"
          "-chardev socket,id=console,path=\${RUNTIME_DIRECTORY:-$TMPDIR}/console,server=on,wait=off"
          "-serial chardev:console"
          "-sandbox on,obsolete=deny,elevateprivileges=deny,spawn=deny,resourcecontrol=deny"
        ];
        getty = true;
        journalGetty = false;
        autologin = "agent";
      };
    };
    testStopPowersTheGuestOffOverQmpBeforeSigterm = {
      expr = {
        qmp = lib.filter (lib.hasPrefix "-qmp") alpha.guest.config.virtualisation.qemu.options;
        inherit (declared.systemd.services.sandbox-alpha.serviceConfig) ExecStop TimeoutStopSec;
      };
      expected = {
        qmp = [ "-qmp unix:\${RUNTIME_DIRECTORY:-$TMPDIR}/qmp,server=on,wait=off" ];
        ExecStop = "${pkgs.writeShellScript "sandbox-powerdown" ''
          qmp=$RUNTIME_DIRECTORY/qmp
          [ ! -S "$qmp" ] || printf '{"execute":"qmp_capabilities"}{"execute":"system_powerdown"}' \
            | ${pkgs.socat}/bin/socat -,ignoreeof UNIX-CONNECT:"$qmp"
        ''}";
        TimeoutStopSec = 120;
      };
    };
    testModuleShapesTheGuest = {
      expr = map (sb: { inherit (sb.guest.config.networking) hostName; recipe = sb.guest.config.environment.etc.recipe.text; }) [ alpha beta ];
      expected = [ { hostName = "alpha"; recipe = "alpha"; } { hostName = "beta"; recipe = "beta"; } ];
    };
    testCollidingSshPortRejected = {
      expr = rejected {
        vms = {
          a = { sshPort = 2201; module = { }; };
          b = { sshPort = 2201; module = { }; };
          c = { sshPort = 2203; module = { }; };
        };
      };
      expected = [ "virtualisation.sandboxes: sshPort 2201 is used by a, b" ];
    };
    testNestedHomesRejected = {
      expr = rejected {
        vms = {
          a = { sshPort = 2201; home = "/var/lib/tester/x"; module = { }; };
          b = { sshPort = 2202; home = "/var/lib/tester/x/b"; module = { }; };
          c = { sshPort = 2203; home = "/var/lib/tester/xy"; module = { }; };
        };
      };
      expected = [ "virtualisation.sandboxes: home /var/lib/tester/x/b of b is inside home /var/lib/tester/x of a" ];
    };
    testOverlappingHomesAndDiskDirectoriesRejected = {
      expr = rejected {
        vms = {
          a = { sshPort = 2201; home = "/var/lib/tester/s/a/home"; disk = "/var/lib/tester/s/a/root.qcow2"; module = { }; };
          b = { sshPort = 2202; home = "/var/lib/tester/s/b/home"; disk = "/var/lib/tester/s/a/b.qcow2"; module = { }; };
          c = { sshPort = 2203; home = "/var/lib/tester/s/c"; disk = "/var/lib/tester/s/c/root.qcow2"; module = { }; };
          d = { sshPort = 2204; home = "/var/lib/tester/s/d/home"; disk = "/var/lib/tester/s/a/home/d.qcow2"; module = { }; };
          e = { sshPort = 2205; home = "/var/lib/tester/s/e/home"; disk = "/var/lib/tester/s/e/root.qcow2"; module = { }; };
        };
      };
      expected = [
        "virtualisation.sandboxes: disk directory /var/lib/tester/s/a of b is also disk directory /var/lib/tester/s/a of a"
        "virtualisation.sandboxes: disk directory /var/lib/tester/s/a/home of d is inside disk directory /var/lib/tester/s/a of a"
        "virtualisation.sandboxes: disk directory /var/lib/tester/s/a/home of d is also home /var/lib/tester/s/a/home of a"
        "virtualisation.sandboxes: home /var/lib/tester/s/a/home of a is inside disk directory /var/lib/tester/s/a of b"
        "virtualisation.sandboxes: disk directory /var/lib/tester/s/a/home of d is inside disk directory /var/lib/tester/s/a of b"
        "virtualisation.sandboxes: disk directory /var/lib/tester/s/c of c is also home /var/lib/tester/s/c of c"
      ];
    };
    testDiskDirectlyInTheUsersHomeRejected = {
      expr = rejected { vms.a = { sshPort = 2201; disk = "/var/lib/tester/a.qcow2"; module = { }; }; };
      expected = [ "virtualisation.sandboxes: disk /var/lib/tester/a.qcow2 of a lies directly in the home of tester; give it its own directory" ];
    };
    testNamesMustBeHostNames = {
      expr = map (name: rejected { vms.${name} = { sshPort = 2201; home = "/var/lib/tester/x/home"; disk = "/var/lib/tester/x/root.qcow2"; module = { }; }; })
        [ "-alpha" "alpha-" "a.b" "" (lib.strings.replicate 64 "a") (lib.strings.replicate 63 "a") "alpha-1" ];
      expected = map (name: [ "virtualisation.sandboxes.vms: name \"${name}\" is the guest's networking.hostName and part of its unit name; it must be a non-empty DNS label (that option's type)" ])
        [ "-alpha" "alpha-" "a.b" "" (lib.strings.replicate 64 "a") ] ++ [ [ ] [ ] ];
    };
    testHostUserUidUnsetOrBelow1000Rejected = {
      expr = map (hostUser: failing (host {
        users.users.nouid.isNormalUser = true;
        virtualisation.sandboxes = { inherit hostUser; vms.a = { sshPort = 2201; module = { }; }; };
      }).config) [ "nouid" "root" ];
      expected = map (u: [ "virtualisation.sandboxes: users.users.${u}.uid must be set and 1000 or above; the agent user inside each sandbox is a normal user with that uid (nixos/modules/config/users-groups.nix), so the shared home is owned consistently on both sides" ]) [ "nouid" "root" ];
    };
    testUnknownHostUserRejected = {
      expr = (builtins.tryEval (host { virtualisation.sandboxes = { hostUser = "ghost"; vms.a = { sshPort = 2201; module = { }; }; }; }).config.virtualisation.sandboxes.vms.a.home).success;
      expected = false;
    };
    testHomeDiskAndPortAreTyped = {
      expr = map (v:
        let a = (host { virtualisation.sandboxes = { hostUser = "tester"; vms.a = { module = { }; } // v; }; }).config.virtualisation.sandboxes.vms.a;
        in (builtins.tryEval (builtins.deepSeq { inherit (a) home disk sshPort; } true)).success) [
        { sshPort = 2201; home = "relative/path"; }
        { sshPort = 2201; home = "/with,comma"; }
        { sshPort = 2201; home = "/trailing/"; }
        { sshPort = 2201; home = "/double//slash"; }
        { sshPort = 2201; home = "/dot/./here"; }
        { sshPort = 2201; home = "/dot/../up"; }
        { sshPort = 2201; home = "/"; }
        { sshPort = 2201; disk = "relative.qcow2"; }
        { sshPort = 22; }
        { sshPort = 2201; home = "/var/lib/tester/.hidden/a-b_c.d"; }
      ];
      expected = [ false false false false false false false false false true ];
    };
  };
in
{
  eval =
    if failures == [ ] then pkgs.writeText "sandboxes-eval-tests" "ok\n"
    else throw (builtins.toJSON failures);

  vm = pkgs.testers.runNixOSTest {
    name = "sandboxes";
    nodes.host = { pkgs, lib, ... }: {
      imports = [ ./. ];
      virtualisation = { memorySize = 4096; cores = 4; diskSize = 16384; };
      boot.kernelParams = [ "no-kvmapf" ];
      virtualisation.fileSystems."/home" = { device = "none"; fsType = "tmpfs"; options = [ "mode=0755" "uid=1000" "gid=100" ]; };
      users.users.tester = { isNormalUser = true; uid = 1000; };
      environment.systemPackages = [ pkgs.socat ];
      environment.etc.sandbox-test-key = { source = keys.snakeOilPrivateKey; mode = "0600"; };
      virtualisation.sandboxes = {
        hostUser = "tester";
        vms = {
          alpha = smallAlpha "alpha";
          beta = vms.beta // { home = "/home/tester/elsewhere/beta"; };
        };
      };
      specialisation.only-alpha.configuration.virtualisation.sandboxes.vms = lib.mkForce { alpha = smallAlpha "alpha"; };
      specialisation.new-alpha.configuration.virtualisation.sandboxes.vms = lib.mkForce { alpha = smallAlpha "alpha-2"; };
    };
    testScript = ''
      import shlex

      ssh = "ssh -q -i /etc/sandbox-test-key -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=5"
      alpha_home = "/home/tester/sandboxes/alpha/home"
      alpha_disk = "/home/tester/sandboxes/alpha/root.qcow2"

      def guest(port, cmd):
          return host.succeed(f"{ssh} -p {port} agent@localhost {shlex.quote(cmd)}")

      def wait_ssh(port):
          host.wait_until_succeeds(f"{ssh} -p {port} agent@localhost true", timeout=900)

      def pid(unit):
          return host.succeed(f"systemctl show -p MainPID --value {unit}").strip()

      def disks_created(unit):
          return host.succeed(f"journalctl -u {unit} | grep -c 'creating the virtualisation disk image' || true").strip()

      def no_journal_replay(port):
          replay = [l for l in guest(port, "sudo journalctl -b -o cat").splitlines() if "recover" in l.lower()]
          assert replay == [], replay

      host.wait_for_unit("multi-user.target")

      with subtest("declared sandboxes come up, each with its own recipe; homes and disk directories private, on the mounted /home"):
          host.wait_for_unit("sandbox-alpha.service")
          host.wait_for_unit("sandbox-beta.service")
          modes = host.succeed(f"stat -c '%U %a %n' {alpha_home} /home/tester/sandboxes/alpha /home/tester/sandboxes/beta /home/tester/elsewhere/beta /home/tester/sandboxes")
          print(modes)
          assert modes.split("\n")[:5] == [
              f"tester 700 {alpha_home}",
              "tester 700 /home/tester/sandboxes/alpha",
              "tester 700 /home/tester/sandboxes/beta",
              "tester 700 /home/tester/elsewhere/beta",
              "tester 755 /home/tester/sandboxes",
          ]
          assert host.succeed(f"findmnt -n -o TARGET -T {alpha_home}").strip() == "/home"
          wait_ssh(2201)
          wait_ssh(2202)
          print(host.succeed(f"ls -l {alpha_disk} /home/tester/sandboxes/beta/root.qcow2; du -sh {alpha_disk}"))
          assert disks_created("sandbox-alpha.service") == "1"
          assert guest(2201, "cat /etc/recipe").strip() == "alpha"
          assert guest(2202, "cat /etc/recipe").strip() == "beta"
          guest(2202, "command -v cowsay")
          guest(2201, "! command -v cowsay")

      with subtest("<nixpkgs> in a guest is the host's pinned nixpkgs"):
          assert guest(2202, "nix-instantiate --eval '<nixpkgs>' -A path").strip() == "${toString pkgs.path}"

      with subtest("serial console answers on the runtime socket"):
          host.wait_until_succeeds("(sleep 2; echo 'echo console-$(hostname)'; sleep 2) | timeout 10 socat - UNIX-CONNECT:/run/sandbox/alpha/console | grep console-alpha >/dev/null")

      with subtest("home, root and store survive a restart right after a write, without a journal replay; the home's mode is back to 0700"):
          guest(2201, "echo persisted > ~/marker")
          blob = guest(2201, "dd if=/dev/zero of=/tmp/blob bs=1M count=64 status=none && nix-store --add /tmp/blob && rm /tmp/blob").strip()
          print(guest(2201, "for p in / /nix/.rw-store /nix/store; do findmnt -n -o TARGET,SOURCE,FSTYPE -T $p; done; df -h /nix/.rw-store; free -m"))
          assert guest(2201, "findmnt -n -o FSTYPE -T /nix/.rw-store").strip() == "ext4"
          boot1 = guest(2201, "cat /proc/sys/kernel/random/boot_id")
          assert host.succeed(f"stat -c '%u' {alpha_home}/marker").strip() == "1000"
          host.succeed(f"chmod 755 {alpha_home}")
          guest(2201, "echo persisted | sudo tee /root-marker")
          host.succeed("systemctl restart sandbox-alpha.service")
          assert host.succeed(f"stat -c '%a' {alpha_home}").strip() == "700"
          wait_ssh(2201)
          assert guest(2201, "cat /proc/sys/kernel/random/boot_id") != boot1
          no_journal_replay(2201)
          assert guest(2201, "cat ~/marker /root-marker").split() == ["persisted", "persisted"]
          guest(2201, f"test -e {blob}")
          assert disks_created("sandbox-alpha.service") == "1"
          print(host.succeed(f"du -sh {alpha_disk}"))

      with subtest("deleting the disk gives a fresh root and store; the home stays"):
          host.succeed("systemctl stop sandbox-alpha.service")
          host.succeed(f"rm {alpha_disk}")
          host.succeed("systemctl start sandbox-alpha.service")
          wait_ssh(2201)
          assert disks_created("sandbox-alpha.service") == "2"
          guest(2201, f"test ! -e /root-marker && test ! -e {blob}")
          assert guest(2201, "cat ~/marker").strip() == "persisted"
          host.succeed(f"test -s {alpha_disk}")

      with subtest("only the shared home is visible from the guest"):
          host.succeed("echo unshared > /home/tester/unshared")
          host.succeed(f"ln -s /etc/hostname {alpha_home}/link")
          guest(2201, "test ! -e /home/tester && test ! -e /home/agent/../tester/unshared && test ! -e /home/agent/../root.qcow2")
          assert guest(2201, "ls /home").strip() == "agent"
          assert guest(2201, "cat ~/link").strip() == "alpha"
          guest(2201, "ln -s marker ~/guest-link")
          host.succeed(f"test -L {alpha_home}/guest-link")
          host.succeed("ls -d /nix/store/*-cowsay-*")
          guest(2201, "! ls -d /nix/store/*-cowsay-*")
          tags = guest(2201, "cat /sys/bus/virtio/drivers/9pnet_virtio/virtio*/mount_tag | tr '\\0' ' '").split()
          assert sorted(tags) == ["home", "shared", "xchg"], tags

      with subtest("qemu itself sees only its own sandbox directory of the host"):
          alpha_pid = pid("sandbox-alpha.service")
          host.succeed(f"nsenter -t {alpha_pid} -m true")
          assert host.succeed(f"nsenter -t {alpha_pid} -m ls /home/tester").split() == ["sandboxes"]
          assert host.succeed(f"nsenter -t {alpha_pid} -m ls /home/tester/sandboxes").split() == ["alpha"]
          assert sorted(host.succeed(f"nsenter -t {alpha_pid} -m ls /home/tester/sandboxes/alpha").split()) == ["home", "root.qcow2"]
          host.fail(f"nsenter -t {alpha_pid} -m test -e /home/tester/unshared")
          host.fail(f"nsenter -t {alpha_pid} -m test -e /home/tester/elsewhere")
          host.fail(f"nsenter -t {alpha_pid} -m touch /etc/x")

      with subtest("a rebuild without beta stops beta and leaves alpha running"):
          out = host.succeed("/run/current-system/specialisation/only-alpha/bin/switch-to-configuration test 2>&1")
          print(out)
          host.fail("systemctl is-active sandbox-beta.service")
          host.fail("systemctl cat sandbox-beta.service")
          host.fail("systemctl cat sandbox-home@beta.service")
          host.succeed("systemctl is-active sandbox-alpha.service")
          assert pid("sandbox-alpha.service") == alpha_pid
          host.fail(f"{ssh} -p 2202 agent@localhost true")
          guest(2201, "true")

      with subtest("a rebuild that changes alpha's recipe right after a write stops it without a journal replay and boots the new closure over the existing root, home and store"):
          blob = guest(2201, "dd if=/dev/urandom of=/tmp/blob bs=1M count=64 status=none && nix-store --add /tmp/blob && rm /tmp/blob").strip()
          guest(2201, "echo rebuilt | sudo tee /root-marker")
          out = host.succeed("/run/booted-system/specialisation/new-alpha/bin/switch-to-configuration test 2>&1")
          print(out)
          assert pid("sandbox-alpha.service") != alpha_pid
          wait_ssh(2201)
          no_journal_replay(2201)
          assert guest(2201, "cat /etc/recipe").strip() == "alpha-2"
          assert guest(2201, "cat ~/marker /root-marker").split() == ["persisted", "rebuilt"]
          guest(2201, f"test -e {blob}")
          assert disks_created("sandbox-alpha.service") == "2"

      with subtest("with no qemu to power off, after a poweroff from inside or before qemu starts, a stop leaves the unit inactive, not failed"):
          state = "systemctl show -p ActiveState,Result sandbox-alpha.service"
          host.execute(f"timeout 60 {ssh} -p 2201 agent@localhost sudo systemctl poweroff")
          host.wait_until_succeeds(f"{state} | grep -Ex 'ActiveState=(inactive|failed)'")
          assert host.succeed(state).split() == ["ActiveState=inactive", "Result=success"]
          host.succeed("systemctl start sandbox-alpha.service && systemctl stop sandbox-alpha.service")
          assert host.succeed(state).split() == ["ActiveState=inactive", "Result=success"]
    '';
  };
}
