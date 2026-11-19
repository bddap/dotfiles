let
  pkgs = import ../nix { };
  sample = "${pkgs.bddap.dictate.model}/test_wavs/0.wav";
  asUser = "systemd-run --user -M a@ --collect --quiet";
  windows = {
    wayland = "${pkgs.gnome-console}/bin/kgx -e";
    x11 = "${pkgs.xterm}/bin/xterm -e";
  };
in (import "${pkgs.path}/nixos/lib" { }).runTest {
  name = "dictate";
  hostPkgs = pkgs;

  nodes.machine = {
    imports = [ ./default.nix "${pkgs.home-manager.src}/nixos" ];
    virtualisation.memorySize = 6144;
    virtualisation.cores = 8;
    home-manager.users.a = {
      imports = [ ../home-manager/dictate.nix ];
      home.stateVersion = "23.11";
    };
  };

  testScript = ''
    import re

    machine.wait_for_unit("home-manager-a.service")
    machine.wait_for_file("/run/user/1000/wayland-0")
    # The daemon reads key events from its stdin once the model is loaded.
    machine.wait_until_succeeds("grep -q '^0 0x0 ' /proc/$(pgrep -u a -f bin/.dictate-wrapped)/syscall", timeout=300)
    machine.succeed("${asUser} ${pkgs.pipewire}/bin/pw-loopback --capture-props='media.class=Audio/Sink node.name=mouth' --playback-props='media.class=Audio/Source node.name=mic'")
    machine.sleep(10)
    machine.send_key("esc")
    machine.sleep(1)
    machine.send_key("esc")

    for name, terminal in ${builtins.toJSON windows}.items():
        with subtest(f"speech typed into a {name} window"):
            machine.succeed(f"${asUser} --unit=window-{name} {terminal} sh -c 'stty -icanon; cat > /tmp/{name}'")
            machine.sleep(10)
            machine.send_monitor_command("sendkey alt_r 12000")
            machine.sleep(1)
            machine.succeed("${asUser} --wait ${pkgs.pipewire}/bin/pw-play --target mouth ${sample}")
            machine.screenshot(f"{name}-listening")
            machine.wait_until_succeeds(f"grep -q 'It is certainly very like the old portrait. $' /tmp/{name}", timeout=60)
            machine.screenshot(f"{name}-typed")
            typed = machine.succeed(f"cat /tmp/{name}")
            # Through the virtual microphone the name comes out as Phoebe, Phebe or Pebe from one run to the next.
            assert re.fullmatch(r"Well, I don't wish to see it any more, observed \S+, turning away her eyes\. It is certainly very like the old portrait\. ", typed), typed
            machine.copy_from_machine(f"/tmp/{name}")

        with subtest(f"a tap types nothing into a {name} window"):
            machine.send_monitor_command("sendkey alt_r 100")
            machine.sleep(10)
            assert machine.succeed(f"cat /tmp/{name}") == typed

        with subtest(f"no session-bus caller can make it type into a {name} window"):
            machine.fail("${asUser} --wait --pipe ${pkgs.glib.bin}/bin/gdbus call --session --dest org.gnome.Shell --object-path /org/gnome/Shell/Extensions/Dictate --method org.gnome.Shell.Extensions.Dictate.Type injected")
            machine.sleep(5)
            assert machine.succeed(f"cat /tmp/{name}") == typed

        machine.succeed(f"systemctl --user -M a@ stop window-{name}")
  '';
}
