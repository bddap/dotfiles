let
  pkgs = import ../nix { };
  nixpkgs-unstable = pkgs.bddap.nixpkgs-unstable;
in { config, ... }: {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "a";
  home.homeDirectory = "/home/a";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
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
    beautysh
    cached-nix-shell
    colorized-logs
    curl
    dockerfile-language-server-nodejs
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
    # nodejs
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
    tree
    uv
    vlc
    xclip
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
    bddap.codex
    walk # tui file browser
    wget
    wl-clipboard # wayland clipboard gets up wl-copy and wl-paste

    # Additional dependencies for scripts in ./root/bin
    viu # terminal image viewer
    poetry # Python packaging tool
    python3Packages.isort # Python import sorter
    fswebcam # webcam capture tool
    xorg.xkbcomp # keyboard compiler
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/a/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
