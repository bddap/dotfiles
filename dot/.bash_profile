alias ls='ls -G'
alias screen='screen-4.6.2'
alias tmnew='tmux new-session -A -s `pwd`'
alias onch='ag -l | entr -rc'

export EDITOR='emacs'
export HISTSIZE=
export HISTFILESIZE=
export PS1="\W ♨ "

export ANDROID_SDK_ROOT="/usr/local/share/android-sdk"
export ANDROID_NDK_HOME="/usr/local/share/android-ndk"
export PATH="$ANDROID_SDK_ROOT:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" &> /dev/null  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" &> /dev/null  # This loads nvm bash_completion
nvm use default &> /dev/null

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/d/flutter/bin:$PATH"
export PATH="$HOME/bin:$PATH"
