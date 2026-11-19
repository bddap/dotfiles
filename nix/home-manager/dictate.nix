let
  pkgs = import ../nix { };
  inherit (pkgs.bddap) dictate;
in { ... }: {
  xdg.dataFile."gnome-shell/extensions/${dictate.extension}".source =
    "${dictate}/share/gnome-shell/extensions/${dictate.extension}";

  systemd.user.services.dictate = {
    Unit = {
      Description = "Enable the dictation extension";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.gnome-shell}/bin/gnome-extensions enable ${dictate.extension}";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
