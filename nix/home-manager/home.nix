let
  pkgs = import ../nix { };
  nixpkgs-unstable = pkgs.bddap.nixpkgs-unstable;
in { ... }: {
  # Machine-local home-manager config lives in nix/home-manager/local/
  # (gitignored) — same mechanism as nix/nixos/local/ in
  # nix/nixos/default.nix: imported iff present, so a machine without one
  # builds unchanged.
  imports =
    if builtins.pathExists ./local/default.nix
    then [ ./local/default.nix ]
    else [ ];

  home.username = "a";
  home.homeDirectory = "/home/a";

  # Pins compatibility defaults from the first install — don't bump.
  home.stateVersion = "23.11";

  home.packages = with pkgs; [
    (pkgs.stdenv.mkDerivation {
      name = "bddap-raw-root";
      src = ./root;
      buildInputs = [ pkgs.python3 ];
      dontUnpack = true;
      dontBuild = true;
      installPhase = ''
        mkdir -p "$out"
        cp -r "$src"/. "$out"/
      '';
    })

    alacritty
    authenticator
    nixpkgs-unstable.bambu-studio
    openscad
    bat
    beautysh
    cached-nix-shell
    colorized-logs
    curl
    dockerfile-language-server
    discord
    (emacs-nox.pkgs.withPackages
      (epkgs: [ epkgs.treesit-grammars.with-all-grammars ]))
    entr
    firefox
    fish
    fx # interactive json/yaml document explorer
    fzf
    git
    gh
    git-lfs
    gitui
    gnome-tweaks
    google-chrome
    graphviz
    htop
    imagemagick
    ispell
    jq
    jujutsu
    gg-jj
    just
    nil
    niv
    nixd
    # nixfmt-rfc-style is not zealous enough, neither is alejandra
    nixfmt-classic
    nix-index
    nixpkgs-unstable.deja-dup
    fnm
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nvtopPackages.full
    pv
    bddap.refac
    ripgrep
    ruff
    slack
    sl
    spotify
    stow
    taplo
    telegram-desktop
    tmux
    bddap.tts-read
    bddap.zellij
    tree
    uv
    vlc
    xclip
    yaml-language-server
    yj
    zoom-us
    copilot-language-server
    nodePackages.prettier
    ollama-cuda
    open-webui
    code-cursor
    zed-editor
    jc
    kdePackages.kolourpaint
    dig
    lazydocker
    mkpasswd
    unixtools.xxd
    bddap.codex
    walk # tui file browser
    wget
    wl-clipboard # wayland clipboard gets up wl-copy and wl-paste

    # Additional dependencies for scripts in ./root/bin
    viu # terminal image viewer
    poetry # Python packaging tool
    python3Packages.isort # Python import sorter
    fswebcam # webcam capture tool
  ];

  # zellij config is generated (not stow'd) so the spiral wasm's store path can be
  # interpolated into the plugin's `location` — KDL can't expand env vars, and the
  # path must be the store path, not a home-relative literal. home-manager owns
  # ~/.config/zellij/ (stow owns nothing under it, avoiding a dir-ownership clash).
  # Only config.kdl carries the path (@wasm@); the layout references it by alias.
  xdg.configFile = {
    "zellij/config.kdl".source = pkgs.replaceVars ./zellij/config.kdl {
      wasm = "${pkgs.bddap.zellij-spiral}/zellij-spiral.wasm";
    };
    "zellij/layouts/default.kdl".source = ./zellij/layouts/default.kdl;
  };

  xdg.autostart = {
    enable = true;
    entries = [ "${pkgs.bddap.tts-read}/share/applications/app.tts_read.desktop" ];
  };

  # dconf replaces this list wholesale: shortcuts added in GNOME Settings are
  # dropped on switch, so every custom keybinding has to be declared here.
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings =
      [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/tts-read/" ];
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/tts-read" = {
      name = "Read selection aloud";
      command = "${pkgs.bddap.tts-read}/bin/tts-read";
      binding = "<Super>r";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
