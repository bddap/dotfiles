{ lib, ... }: {
  home.file = lib.genAttrs [
    ".bash_profile"
    ".bashrc"
    ".config/bat/config"
    ".config/fish/conf.d/fnm.fish"
    ".config/fish/config.fish"
    ".config/fish/fish_variables"
    ".config/gitui/theme.ron"
    ".editorconfig"
    ".emacs.d/early-init.el"
    ".emacs.d/init.el"
    ".emacs.d/straight/versions/default.el"
    ".tmux.conf"
  ] (path: { source = ../../home + "/${path}"; });
}
