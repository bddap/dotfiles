
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
