export EDITOR='emacs'
export HISTSIZE=
export HISTFILESIZE=
export PS1="\W ♨ "

function appendp {
    if [ -d "$1" ]; then
        export PATH="$PATH:$1"
    fi
}

function maybesrc {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

appendp "/usr/local/go/bin"
appendp "$HOME/go/bin"
appendp "$HOME/.yarn/bin"

maybesrc "$HOME/.cargo/env"
maybesrc "$HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br"
maybesrc "$HOME/.nix-profile/etc/profile.d/nix.sh"
maybesrc "$HOME/.fzf.bash"
