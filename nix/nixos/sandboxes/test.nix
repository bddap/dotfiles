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
        environment.etc.recipe.text = "beta";
        environment.systemPackages = [ pkgs.cowsay ];
      };
    };
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
    testOneUnitPerSandbox = { expr = sandboxUnits declared; expected = [ "sandbox-alpha" "sandbox-beta" ]; };
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
    testUnitSeesOnlyTheHome = {
      expr = with declared.systemd.services.sandbox-beta.serviceConfig; {
        inherit ProtectHome BindPaths ProtectSystem NoNewPrivileges CapabilityBoundingSet;
        mounts = declared.systemd.services.sandbox-beta.unitConfig.RequiresMountsFor;
      };
      expected = {
        mounts = "/var/lib/tester/elsewhere/beta";
        ProtectHome = "tmpfs";
        BindPaths = [ "/var/lib/tester/elsewhere/beta" ];
        ProtectSystem = "strict";
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
      };
    };
    testHomeDefaultsUnderTheUsersHome = { expr = alpha.home; expected = "/var/lib/tester/sandboxes/alpha"; };
    testHomesAreCreatedPrivateAsTheUser = {
      expr = map (l: lib.hasSuffix "/setpriv --reuid=tester --regid=tester --init-groups \\" l || lib.hasSuffix "/mkdir -p -m 0700 ${alpha.home}" l || lib.hasSuffix "/mkdir -p -m 0700 ${beta.home}" l)
        (lib.filter (l: l != "") (lib.splitString "\n" declared.system.activationScripts.sandboxes.text));
      expected = [ true true true true ];
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
    testEphemeralRootOwnStoreHeadless = {
      expr = { inherit (alpha.guest.config.virtualisation) diskImage useNixStoreImage mountHostNixStore writableStore graphics; };
      expected = { diskImage = null; useNixStoreImage = true; mountHostNixStore = false; writableStore = true; graphics = false; };
    };
    testMemoryAndCoresDefaultAndOverride = {
      expr = map (sb: { inherit (sb.guest.config.virtualisation) memorySize cores; }) [ alpha beta ];
      expected = [ { memorySize = 4096; cores = 2; } { memorySize = 1024; cores = 1; } ];
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
    testModuleShapesTheGuest = {
      expr = map (sb: { inherit (sb.guest.config.networking) hostName; recipe = sb.guest.config.environment.etc.recipe.text; }) [ alpha beta ];
      expected = [ { hostName = "alpha"; recipe = "alpha"; } { hostName = "beta"; recipe = "beta"; } ];
    };
    testCollidingSshPortOrHomeRejected = {
      expr = rejected {
        vms = {
          a = { sshPort = 2201; home = "/var/lib/tester/x"; module = { }; };
          b = { sshPort = 2201; home = "/var/lib/tester/x"; module = { }; };
          c = { sshPort = 2203; module = { }; };
        };
      };
      expected = [
        "virtualisation.sandboxes: sshPort 2201 is used by a, b"
        "virtualisation.sandboxes: home /var/lib/tester/x is used by a, b"
      ];
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
    testBadNameRejected = {
      expr = rejected { vms.my_agent = { sshPort = 2201; module = { }; }; };
      expected = [ "virtualisation.sandboxes.vms: a name is the guest's host name and part of its unit name; use letters, digits and dashes" ];
    };
    testHostUserWithoutUidRejected = {
      expr = failing (host {
        users.users.nouid.isNormalUser = true;
        virtualisation.sandboxes = { hostUser = "nouid"; vms.a = { sshPort = 2201; module = { }; }; };
      }).config;
      expected = [ "virtualisation.sandboxes: users.users.nouid.uid must be set; the agent user inside each sandbox gets that uid so the shared home is owned consistently on both sides" ];
    };
    testUnknownHostUserRejected = {
      expr = (builtins.tryEval (host { virtualisation.sandboxes = { hostUser = "ghost"; vms.a = { sshPort = 2201; module = { }; }; }; }).config.virtualisation.sandboxes.vms.a.home).success;
      expected = false;
    };
    testHomeAndPortAreTyped = {
      expr = map (v:
        let a = (host { virtualisation.sandboxes = { hostUser = "tester"; vms.a = { module = { }; } // v; }; }).config.virtualisation.sandboxes.vms.a;
        in (builtins.tryEval (builtins.deepSeq { inherit (a) home sshPort; } true)).success) [
        { sshPort = 2201; home = "relative/path"; }
        { sshPort = 2201; home = "/with,comma"; }
        { sshPort = 2201; home = "/trailing/"; }
        { sshPort = 2201; home = "/double//slash"; }
        { sshPort = 2201; home = "/dot/./here"; }
        { sshPort = 2201; home = "/dot/../up"; }
        { sshPort = 2201; home = "/"; }
        { sshPort = 22; }
        { sshPort = 2201; home = "/var/lib/tester/.hidden/a-b_c.d"; }
      ];
      expected = [ false false false false false false false false true ];
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
      virtualisation = { memorySize = 4096; cores = 4; diskSize = 8192; };
      users.users.tester = { isNormalUser = true; uid = 1000; };
      environment.systemPackages = [ pkgs.socat ];
      environment.etc.sandbox-test-key = { source = keys.snakeOilPrivateKey; mode = "0600"; };
      virtualisation.sandboxes = {
        hostUser = "tester";
        vms = {
          alpha = vms.alpha // { module = { imports = [ vms.alpha.module ]; virtualisation.memorySize = 1024; virtualisation.cores = 1; }; };
          beta = vms.beta // { home = "/home/tester/elsewhere/beta"; };
        };
      };
      specialisation.only-alpha.configuration.virtualisation.sandboxes.vms =
        lib.mkForce { alpha = vms.alpha // { module = { imports = [ vms.alpha.module ]; virtualisation.memorySize = 1024; virtualisation.cores = 1; }; }; };
    };
    testScript = ''
      import shlex

      ssh = "ssh -q -i /etc/sandbox-test-key -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=5"

      def guest(port, cmd):
          return host.succeed(f"{ssh} -p {port} agent@localhost {shlex.quote(cmd)}")

      def wait_ssh(port):
          host.wait_until_succeeds(f"{ssh} -p {port} agent@localhost true", timeout=900)

      def pid(unit):
          return host.succeed(f"systemctl show -p MainPID --value {unit}").strip()

      host.wait_for_unit("multi-user.target")

      with subtest("declared sandboxes come up, each with its own recipe, homes created private"):
          host.wait_for_unit("sandbox-alpha.service")
          host.wait_for_unit("sandbox-beta.service")
          assert host.succeed("stat -c '%U %a' /home/tester/sandboxes/alpha /home/tester/sandboxes /home/tester/elsewhere/beta").split("\n")[:3] == ["tester 700", "tester 755", "tester 700"]
          wait_ssh(2201)
          wait_ssh(2202)
          assert guest(2201, "cat /etc/recipe").strip() == "alpha"
          assert guest(2202, "cat /etc/recipe").strip() == "beta"
          guest(2202, "command -v cowsay")
          guest(2201, "! command -v cowsay")

      with subtest("serial console answers on the runtime socket"):
          host.wait_until_succeeds("(sleep 2; echo 'echo console-$(hostname)'; sleep 2) | timeout 10 socat - UNIX-CONNECT:/run/sandbox/alpha/console | grep console-alpha >/dev/null")

      with subtest("home persists across a restart, the rest does not"):
          guest(2201, "echo persisted > ~/marker && sudo touch /ephemeral")
          assert host.succeed("stat -c '%u' /home/tester/sandboxes/alpha/marker").strip() == "1000"
          host.succeed("systemctl restart sandbox-alpha.service")
          wait_ssh(2201)
          assert guest(2201, "cat ~/marker").strip() == "persisted"
          guest(2201, "test ! -e /ephemeral")

      with subtest("only the shared home is visible from the guest"):
          host.succeed("echo unshared > /home/tester/unshared")
          host.succeed("ln -s /etc/hostname /home/tester/sandboxes/alpha/link")
          guest(2201, "test ! -e /home/tester && test ! -e /home/agent/../tester/unshared")
          assert guest(2201, "ls /home").strip() == "agent"
          assert guest(2201, "cat ~/link").strip() == "alpha"
          guest(2201, "ln -s marker ~/guest-link")
          host.succeed("test -L /home/tester/sandboxes/alpha/guest-link")
          host.succeed("ls -d /nix/store/*-cowsay-*")
          guest(2201, "! ls -d /nix/store/*-cowsay-*")
          tags = guest(2201, "cat /sys/bus/virtio/drivers/9pnet_virtio/virtio*/mount_tag | tr '\\0' ' '").split()
          assert sorted(tags) == ["home", "shared", "xchg"], tags

      with subtest("qemu itself sees only the shared home of the host"):
          alpha_pid = pid("sandbox-alpha.service")
          host.succeed(f"nsenter -t {alpha_pid} -m true")
          assert host.succeed(f"nsenter -t {alpha_pid} -m ls /home/tester").split() == ["sandboxes"]
          host.fail(f"nsenter -t {alpha_pid} -m test -e /home/tester/unshared")
          host.fail(f"nsenter -t {alpha_pid} -m touch /etc/x")

      with subtest("a rebuild without beta stops beta and leaves alpha running"):
          out = host.succeed("/run/current-system/specialisation/only-alpha/bin/switch-to-configuration test 2>&1")
          print(out)
          host.fail("systemctl is-active sandbox-beta.service")
          host.fail("systemctl cat sandbox-beta.service")
          host.succeed("systemctl is-active sandbox-alpha.service")
          assert pid("sandbox-alpha.service") == alpha_pid
          host.fail(f"{ssh} -p 2202 agent@localhost true")
          guest(2201, "true")
    '';
  };
}
