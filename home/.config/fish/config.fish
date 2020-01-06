
function appendp
    set pa $argv[1]
    if test -d $pa
       set -gx PATH $pa $PATH
    end
end

appendp /usr/local/bin
appendp ~/.cargo/bin
appendp ~/bin
appendp ~/go/bin

set -gx GOPATH ~/go
set -Ux EDITOR emacs

alias onch='ag -l | entr -rc'

function bddap_key_bindings -d "key bindings for fish"
    fish_default_key_bindings
    bind --erase \el # disable __fish_list_current_token for alt-l
    bind \es __fish_list_current_token # set it to alt-s instead
    bind \el downcase-word
end
set -g fish_key_bindings bddap_key_bindings
