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

if [ -f "/Users/a/Library/Preferences/org.dystroy.broot/launcher/bash/br" ]; then
    source /Users/a/Library/Preferences/org.dystroy.broot/launcher/bash/br
fi

if [ -f "/home/a/.nix-profile/etc/profile.d/nix.sh" ]; then
	source /home/a/.nix-profile/etc/profile.d/nix.sh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
