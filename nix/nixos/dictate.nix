{ ... }:
let
  pkgs = import ../nix { };
  key = "rightalt";
  inherit (pkgs.bddap) dictate;
in {
  services.keyd.keyboards.default.settings = {
    main.${key} = "layer(dictate)";
    dictate = { };
  };

  # keyd's socket lets its holder remap keys to commands run as root, so the user may only read this key's events.
  systemd.tmpfiles.rules = [
    "p ${dictate.events} 0400 root root -"
    "a+ ${dictate.events} - - - - u:a:r"
  ];
  systemd.services.dictate-key = {
    description = "Report the dictate key";
    wantedBy = [ "multi-user.target" ];
    after = [ "keyd.service" ];
    unitConfig.StartLimitIntervalSec = 0;
    script = "${pkgs.keyd}/bin/keyd listen | ${pkgs.gnugrep}/bin/grep --line-buffered -x '[+-]dictate' > ${dictate.events}";
    serviceConfig = {
      Restart = "always";
      RestartSec = 1;
    };
  };

  services.desktopManager.gnome.sessionPath = [ dictate ];

  systemd.user.services.dictate = {
    description = "Enable the dictation extension";
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.gnome-shell}/bin/gnome-extensions enable ${dictate.extension}";
    };
  };
}
