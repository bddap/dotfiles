alias ls='ls -G'

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

appendp "$HOME/bin"
appendp "/usr/local/go/bin"
appendp "$HOME/go/bin"
appendp "$HOME/.yarn/bin"

maybesrc "$HOME/.cargo/env"
maybesrc "$HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br"
maybesrc "$HOME/.nix-profile/etc/profile.d/nix.sh"
maybesrc "$HOME/.fzf.bash"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('$HOME/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

complete -C /usr/bin/terraform terraform

if which cortex > /dev/null; then
    source <(cortex completion bash)
fi

source <(cortex completion bash)
