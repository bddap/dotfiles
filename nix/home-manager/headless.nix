let pkgs = import ../nix { };
in { ... }: {
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

    bat
    beautysh
    cached-nix-shell
    colorized-logs
    curl
    dockerfile-language-server
    (emacs-nox.pkgs.withPackages
      (epkgs: [ epkgs.treesit-grammars.with-all-grammars ]))
    entr
    fish
    fx
    fzf
    git
    gh
    git-lfs
    gitui
    graphviz
    htop
    imagemagick
    ispell
    jq
    jujutsu
    just
    nil
    niv
    nixd
    # nixfmt-rfc-style is not zealous enough, neither is alejandra
    nixfmt-classic
    nix-index
    fnm
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    pv
    bddap.refac
    ripgrep
    ruff
    sl
    stow
    taplo
    tmux
    bddap.zellij
    tree
    uv
    yaml-language-server
    yj
    copilot-language-server
    nodePackages.prettier
    jc
    dig
    mkpasswd
    unixtools.xxd
    bddap.claude-code
    bddap.codex
    walk
    wget

    # Additional dependencies for scripts in ./root/bin
    viu
    poetry
    python3Packages.isort
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
}
