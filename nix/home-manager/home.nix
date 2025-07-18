let
  pkgs = import ../nix { };
  nixpkgs-unstable = pkgs.bddap.nixpkgs-unstable;
in
{ config, ... }:
{
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
    alacritty
    authenticator
    bambu-studio
    beautysh
    colorized-logs
    curl
    dockerfile-language-server-nodejs
    (emacs-nox.pkgs.withPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
    ]))
    entr
    firefox
    fish
    fzf
    git
    git-lfs
    gitui
    gnome-tweaks
    google-chrome
    graphviz
    htop
    ispell
    jq
    nixpkgs-unstable.jujutsu
    nixpkgs-unstable.gg-jj
    just
    nil
    niv
    nixd
    nixfmt-rfc-style
    nix-index
    nixpkgs-unstable.deja-dup
    nodejs
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
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
    bddap.uv
    vlc
    xclip
    yj
    pkgs.bddap.zoom
    nixpkgs-unstable.copilot-language-server
    nodePackages.prettier
    ollama-cuda
    open-webui
    nixpkgs-unstable.code-cursor
    nixpkgs-unstable.zed-editor
    jc
    kolourpaint
    dig
    lazydocker
    bddap.codex
    wl-clipboard # wayland clipboard gets up wl-copy and wl-paste

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
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
