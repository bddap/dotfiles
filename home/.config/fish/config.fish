
function appendp
    set pa $argv[1]
    if test -d $pa
       set -gx PATH $pa $PATH
    end
end

appendp /usr/local/bin
appendp /usr/local/go/bin
appendp ~/.cargo/bin
appendp ~/bin
appendp ~/go/bin
appendp ~/.local/bin

function ef --description "edit your fish config"
    eval $EDITOR ~/d/dotfiles/home/.config/fish/config.fish
end

function ee --description "edit your emacs config"
    eval $EDITOR ~/d/dotfiles/home/.emacs
end

set -gx GOPATH ~/go
set -Ux EDITOR emacs

function bddap_key_bindings -d "key bindings for fish"
    fish_default_key_bindings
    bind --erase \el # disable __fish_list_current_token for alt-l
    bind \es __fish_list_current_token # set it to alt-s instead
    bind \el downcase-word
end
set -g fish_key_bindings bddap_key_bindings

alias ytob='ytop -c default-dark'

function fish_right_prompt
    # intentionally left blank
end

function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -l normal (set_color normal)

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l prefix
    set -l suffix '>'
    if contains -- $USER root toor
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # If we're running via SSH, change the host color.
    set -l color_host $fish_color_host
    if set -q SSH_TTY
        set color_host $fish_color_host_remote
    end

    # Write pipestatus
    set -l prompt_status (__fish_print_pipestatus " [" "]" "|" (set_color $fish_color_status) \
	  (set_color --bold $fish_color_status) $last_pipestatus)

    echo -n -s (set_color $fish_color_user) "$USER" $normal @ (set_color $color_host) (prompt_hostname) $normal ' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal $prompt_status $suffix " "
end

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# eval $HOME/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# # <<< conda initialize <<<

# The next line updates PATH for the Google Cloud SDK.
if [ -f '$HOME/Downloads/google-cloud-sdk/path.fish.inc' ]; . '$HOME/Downloads/google-cloud-sdk/path.fish.inc'; end

