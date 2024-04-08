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
appendp ~/.tfenv/bin

function fish_logo \
    --description="Fish-shell colorful ASCII-art logo" \
    --argument-names outer_color medium_color inner_color mouth eye

    # defaults:
    [ $outer_color  ]; or set outer_color  'red'
    [ $medium_color ]; or set medium_color 'f70'
    [ $inner_color  ]; or set inner_color  'yellow'
    [ $mouth ]; or set mouth '['
    [ $eye   ]; or set eye   'O'

    set usage 'Usage: fish_logo <outer_color> <medium_color> <inner_color> <mouth> <eye>
See set_color --help for more on available colors.'

    if contains -- $outer_color '--help' '-h' '-help'
        echo $usage
        return 0
    end

    # shortcuts:
    set o (set_color $outer_color)
    set m (set_color $medium_color)
    set i (set_color $inner_color)

    if test (count $o) != 1; or test (count $m) != 1; or test (count $i) != 1
        echo 'Invalid color argument'
        echo $usage
        return 1
    end

    echo '                     '$o'___
      ___======____='$m'-'$i'-'$m'-='$o')
    /T            \_'$i'--='$m'=='$o')
    '$mouth' \ '$m'('$i$eye$m')   '$o'\~    \_'$i'-='$m'='$o')
     \      / )J'$m'~~    '$o'\\'$i'-='$o')
      \\\\___/  )JJ'$m'~'$i'~~   '$o'\)
       \_____/JJJ'$m'~~'$i'~~    '$o'\\
       '$m'/ '$o'\  '$i', \\'$o'J'$m'~~~'$i'~~     '$m'\\
      (-'$i'\)'$o'\='$m'|'$i'\\\\\\'$m'~~'$i'~~       '$m'L_'$i'_
      '$m'('$o'\\'$m'\\)  ('$i'\\'$m'\\\)'$o'_           '$i'\=='$m'__
       '$o'\V    '$m'\\\\'$o'\) =='$m'=_____   '$i'\\\\\\\\'$m'\\\\
              '$o'\V)     \_) '$m'\\\\'$i'\\\\JJ\\'$m'J\)
                          '$o'/'$m'J'$i'\\'$m'J'$o'T\\'$m'JJJ'$o'J)
                          (J'$m'JJ'$o'| \UUU)
                           (UU)
'(set_color normal)
end

function fish_greeting
   fish_logo
end

if which yarn &> /dev/null
   appendp (yarn global bin)
end

function ef --description "edit your fish config"
    eval $EDITOR ~/d/dotfiles/home/.config/fish/config.fish
end

function ee --description "edit your emacs config"
    eval $EDITOR ~/d/dotfiles/home/.emacs.d/init.el
end

set -x GOPATH ~/go
set -x EDITOR emacs

function bddap_key_bindings -d "bddap's cusrom key bindings for fish"
    fish_default_key_bindings
	
    # get alt-l back
    bind --erase \el # disable __fish_list_current_token for alt-l
    bind \es __fish_list_current_token # set it to alt-s instead
    bind \el downcase-word

    # get alt-c back
	bind --erase \ec
	bind \ec capitalize-word
end
set -g fish_key_bindings bddap_key_bindings

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

if [ -f "$HOME/miniconda3/bin/conda" ]
    eval "$HOME/miniconda3/bin/conda" "shell.fish" "hook" | source
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '$HOME/Downloads/google-cloud-sdk/path.fish.inc' ]; . '$HOME/Downloads/google-cloud-sdk/path.fish.inc'; end

alias ytob='ytop -c default-dark'
