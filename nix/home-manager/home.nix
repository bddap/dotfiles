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
    wl-clipboard # wayland clipboard gets up wl-copy and wl-paste

    # Additional dependencies for migrated scripts
    # cog # AI model runner - might not be in nixpkgs
    viu # terminal image viewer
    poetry # Python packaging tool
    python3Packages.isort # Python import sorter
    fswebcam # webcam capture tool  
    xorg.xkbcomp # keyboard compiler

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # Migrated scripts from ./home/bin
    (pkgs.writeShellScriptBin "browser" ''
      set -euo pipefail
      ${pkgs.python3}/bin/python3 << 'EOF'
      import sys
      import webbrowser
      import tempfile
      import os

      html = sys.stdin.read()

      fh, path = tempfile.mkstemp(suffix=".html")
      url = "file://" + path

      with open(path, "w") as fp:
          fp.write(html)

      try:
          webbrowser.open(url)
      finally:
          os.remove(path)
      EOF
    '')

    (pkgs.writeShellScriptBin "csv2json" ''
      set -ueo pipefail
      ${pkgs.python3}/bin/python3 -c 'import csv, json, sys; print(json.dumps([dict(r) for r in csv.DictReader(sys.stdin)]))'
    '')

    (pkgs.writeShellScriptBin "json2csv" ''
      set -ueo pipefail
      ${pkgs.python3}/bin/python3 -c '
      import sys
      import json
      import csv

      data = json.load(sys.stdin)

      if len(data) == 0:
          print("json2csv unable to determine header, no data found", file=sys.stderr)
          sys.exit(0)

      writer = csv.DictWriter(sys.stdout, fieldnames=data[0].keys())
      writer.writeheader()
      for row in data:
          writer.writerow(row)
      '
    '')

    (pkgs.writeShellScriptBin "next-rustc-err" ''
      set -euo pipefail

      function get_target_span {
          all_messages=$(
              ${pkgs.cargo}/bin/cargo clippy --all-targets --examples --tests --message-format json 2>/dev/null \
                  | ${pkgs.jq}/bin/jq 'select(.reason == "compiler-message") | .message | select(.spans | length > 0)'
          )

          all_errs=$(
              echo "$all_messages" \
                  | ${pkgs.jq}/bin/jq 'select(.level == "error")'
          )

          all_warns=$(
              echo "$all_messages" \
                  | ${pkgs.jq}/bin/jq 'select(.level == "warning")'
          )

          echo "$all_warns" "$all_errs" \
              | ${pkgs.jq}/bin/jq '.spans[0]' \
              | ${pkgs.jq}/bin/jq -sr '.[-1]'
      }

      target_span=$(get_target_span)
      if [[ $target_span == null ]]; then
          echo No errors or warnings. >&2
          exit 1
      fi

      rootpath=$(${pkgs.cargo}/bin/cargo metadata --format-version 1 | ${pkgs.jq}/bin/jq -r .workspace_root)
      relativepath=$(echo "$target_span" | ${pkgs.jq}/bin/jq -r '.file_name')
      fullpath=$rootpath/$relativepath
      byte_start=$(echo "$target_span" | ${pkgs.jq}/bin/jq .byte_start)

      cat <<EOF
      {
        "file_name": "$fullpath",
        "byte_start": $byte_start
      }
      EOF
    '')

    (pkgs.writeShellScriptBin "bunch" ''
      set -ueo pipefail

      # "tiled" has no limit on expandability
      ${pkgs.tmux}/bin/tmux select-layout tiled

      while IFS= read -r line; do
        # after each command is done it will re-layout
        ${pkgs.tmux}/bin/tmux split-window -d bash -c "$line ; ${pkgs.tmux}/bin/tmux select-layout tiled"
        # refresh layout after launching all commands
        ${pkgs.tmux}/bin/tmux select-layout tiled
      done
    '')

    (pkgs.writeShellScriptBin "onch" ''
      set -euo pipefail
      ${pkgs.ripgrep}/bin/rg -l . | ${pkgs.entr}/bin/entr -rc "$@"
    '')

    (pkgs.writeShellScriptBin "onchs" ''
      set -ueo pipefail

      if [ "$#" -ne 1 ]; then
        echo onchs: wrong number of arguments 1>&2
        exit 1
      fi

      ${pkgs.ripgrep}/bin/rg -l . | ${pkgs.entr}/bin/entr -rcs "$1"
    '')

    (pkgs.writeShellScriptBin "cop" ''
      set -euo pipefail
      ${pkgs.xclip}/bin/xclip -selection clipboard
    '')

    (pkgs.writeShellScriptBin "pas" ''
      set -ueo pipefail
      ${pkgs.xclip}/bin/xclip -o -selection clipboard
    '')

    (pkgs.writeShellScriptBin "fmt-python" ''
      set -euo pipefail

      if command -v ${pkgs.python3Packages.isort}/bin/isort &> /dev/null; then
          isort="${pkgs.python3Packages.isort}/bin/isort --stdout --profile black -"
      else
          isort="cat"
      fi

      ${pkgs.ruff}/bin/ruff format - | $isort
    '')

    (pkgs.writeShellScriptBin "pred" ''
      set -euo pipefail

      args=("$@")
      EDITOR="''${EDITOR:-/bin/vim}"

      ${pkgs.poetry}/bin/poetry run "$EDITOR" "''${args[@]}"
    '')

    (pkgs.writeShellScriptBin "aspng" ''
      set -euo pipefail

      W="''${IMG_W:-128}"

      tmp_raw="$(mktemp)"
      trap 'rm -f "$tmp_raw"' EXIT

      # Capture stdin
      cat > "$tmp_raw"
      L=$(wc -c < "$tmp_raw"); L=''${L//[[:space:]]/}

      # Calculate height based on total pixels needed
      pixels=$(( (L + 2) / 3 ))
      H=$(( (pixels + W - 1) / W ))

      # Pad with zeros if needed
      need=$(( W * H * 3 ))
      pad=$(( need - L ))
      if (( pad > 0 )); then
        dd if=/dev/zero bs=1 count="$pad" status=none >> "$tmp_raw"
      fi

      # Output PNG
      ${pkgs.imagemagick}/bin/magick -size "''${W}x''${H}" -depth 8 rgb:- png:- < "$tmp_raw"
    '')

    (pkgs.writeShellScriptBin "show-vega" ''
      set -euo pipefail

      vega_spec=$(${pkgs.jq}/bin/jq .)

      browser <<EOF
      <!DOCTYPE html>
      <html>
        <head>
          <title>Embedding Vega-Lite</title>
          <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
          <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
          <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
        </head>
        <body>
          <div id="vis"></div>

          <script type="text/javascript">
            var yourVlSpec = $vega_spec;
            vegaEmbed('#vis', yourVlSpec);
          </script>
        </body>
      </html>
      EOF
    '')

    (pkgs.writeShellScriptBin "paintforme" ''
      set -ueo pipefail

      mkdir -p /tmp/aghnuiwenfuiwnuiosiue

      # Note: cog may not be available in nixpkgs, using system cog if available
      if command -v cog &> /dev/null; then
        cog predict r8.im/afiaka87/pyglide@sha256:6ce94eff1c99e4eedd5f1139f1f984631cbcdf7dae5714c3d66cefc108637fe0 \
          -i prompt="$1" \
          -i batch_size=1 \
          -i side_x=64 \
          -i side_y=64 \
          -i guidance_scale=4 \
          -i upsample_temp=0.996 \
          -i timestep_respacing=50 \
          -i seed=0 \
          -o /tmp/aghnuiwenfuiwnuiosiue/out.png

        ${pkgs.viu}/bin/viu /tmp/aghnuiwenfuiwnuiosiue/out.png
      else
        echo "Error: cog command not found. Please install cog separately." >&2
        exit 1
      fi
    '')

    (pkgs.writeShellScriptBin "aci" ''
      set -ueo pipefail
      ${pkgs.xorg.xkbcomp}/bin/xkbcomp -w0 -I$HOME/.xkb $HOME/.xkb/keymap/kbd $DISPLAY
    '')

    (pkgs.writeShellScriptBin "capture-soul" ''
      set -ueo pipefail

      ${pkgs.fswebcam}/bin/fswebcam --no-banner - \
        | ${pkgs.imagemagick}/bin/convert jpg:- -set colorspace Gray -separate -average jpg:- \
        | ${pkgs.viu}/bin/viu -
    '')

    (pkgs.writeShellScriptBin "cdoc" ''
      #! /usr/bin/env nix-shell
      #! nix-shell -i bash -p python3

      set -ueo pipefail

      crate="$1"

      ${pkgs.python3}/bin/python3 -m webbrowser "https://docs.rs/''${crate}"
    '')

    (pkgs.writeShellScriptBin "tmnew" ''
      set -euo pipefail
      ${pkgs.tmux}/bin/tmux new-session -A -s $(${pkgs.git}/bin/git rev-parse --show-toplevel 2> /dev/null || pwd)
    '')
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
