let
  pkgs = import ../nix { };
  nixpkgs-unstable = pkgs.bddap.nixpkgs-unstable;
  zoomVersion = "7.1.5.4332";
  zoomSrc = pkgs.fetchurl {
    url = "https://zoom.us/client/${zoomVersion}/zoom_x86_64.pkg.tar.xz";
    hash = "sha256-5znZNrySgRrs9I5zhqN5p5dPfXpEHXKf8o2dWeYTPso=";
  };
  zoomPatched = pkgs.callPackage
    (builtins.toFile "zoom-us-${zoomVersion}.nix" (builtins.replaceStrings
      [
        ''versions.x86_64-linux = "6.6.10.5815";''
        ''hash = "sha256-SvPAhv6Ja37aviG4Gh65FvDc9U4fUDKRJvvu8/tbxls=";''
      ]
      [
        ''versions.x86_64-linux = "${zoomVersion}";''
        ''hash = "${zoomSrc.outputHash}";''
      ]
      (builtins.readFile "${pkgs.bddap.sources.nixpkgs}/pkgs/by-name/zo/zoom-us/package.nix")))
    { targetPkgsFixed = [ pkgs.zstd ]; };
  zoom = pkgs.zoom-us.overrideAttrs (_: (builtins.removeAttrs zoomPatched.drvAttrs
    [ "NIX_MAIN_PROGRAM" ]) // {
    passthru = zoomPatched.passthru;
    meta = zoomPatched.meta;
  });
in { ... }: {
  # Machine-local home-manager config lives in nix/home-manager/local/
  # (gitignored) — same mechanism as nix/nixos/local/ in
  # nix/nixos/default.nix: imported iff present, so a machine without one
  # builds unchanged.
  imports = [ ./headless.nix ]
    ++ pkgs.lib.optional (builtins.pathExists ./local/default.nix)
    ./local/default.nix;

  home.username = "a";
  home.homeDirectory = "/home/a";

  # Pins compatibility defaults from the first install — don't bump.
  home.stateVersion = "23.11";

  home.packages = with pkgs; [
    alacritty
    authenticator
    nixpkgs-unstable.bambu-studio
    openscad
    discord
    firefox
    gnome-tweaks
    google-chrome
    gg-jj
    nixpkgs-unstable.deja-dup
    nvtopPackages.full
    slack
    spotify
    telegram-desktop
    bddap.tts-read
    vlc
    xclip
    zoom
    ollama-cuda
    open-webui
    code-cursor
    zed-editor
    kdePackages.kolourpaint
    lazydocker
    wl-clipboard
    fswebcam
  ];

  systemd.user.services.tts-read = {
    Unit = {
      Description = "Read selection aloud";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.bddap.tts-read}/bin/tts-read --gapplication-service";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  # dconf replaces this list wholesale: shortcuts added in GNOME Settings are
  # dropped on switch, so every custom keybinding has to be declared here.
  # Keep paths named customN: Zoom 6.x stoi-parses the suffix while enumerating
  # these bindings and aborts screen sharing on non-numeric names. See
  # https://github.com/bddap/dotfiles/issues/38.
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings =
      [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" ];
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      name = "Read selection aloud";
      command = "${pkgs.bddap.tts-read}/bin/tts-read";
      binding = "<Super>r";
    };
  };

  programs.home-manager.enable = true;
}
