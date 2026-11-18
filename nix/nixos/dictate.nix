{ ... }:
let
  pkgs = import ../nix { };
  key = "rightalt";
  events = "/run/dictate-key";
in {
  services.keyd.keyboards.default.settings = {
    main.${key} = "layer(dictate)";
    dictate = { };
  };

  # keyd's socket lets its holder remap keys to commands run as root, so the user gets only this key's events.
  systemd.tmpfiles.rules = [ "p ${events} 0400 a - -" ];
  systemd.services.dictate-key = {
    description = "Report the dictate key";
    wantedBy = [ "multi-user.target" ];
    after = [ "keyd.service" ];
    unitConfig.StartLimitIntervalSec = 0;
    script = "${pkgs.keyd}/bin/keyd listen | ${pkgs.gnugrep}/bin/grep --line-buffered -x '[+-]dictate' > ${events}";
    serviceConfig = {
      Restart = "always";
      RestartSec = 1;
    };
  };

  services.desktopManager.gnome.sessionPath = [ pkgs.bddap.dictate ];

  systemd.user.services.dictate = {
    description = "Type what is said while the dictate key is held";
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStartPre = "${pkgs.gnome-shell}/bin/gnome-extensions enable ${pkgs.bddap.dictate.extension}";
      ExecStart = "${pkgs.bddap.dictate}/bin/dictate ${events}";
      Restart = "always";
    };
  };
}
