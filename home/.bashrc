alias ls='ls -G'
alias onch='ag -l | entr -rc'

export EDITOR='emacs'
export HISTSIZE=
export HISTFILESIZE=
export PS1="\W ♨ "

export PATH="$HOME/bin:$PATH"

if [ -f $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi

if [ -f "$HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br" ]; then
    source $HOME/Library/Preferences/org.dystroy.broot/launcher/bash/br
fi

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
	source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('$HOME/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "$HOME/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="$HOME/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<


complete -C /usr/bin/terraform terraform

source <(cortex completion bash)

export PATH=$PATH:/usr/local/go/bin
