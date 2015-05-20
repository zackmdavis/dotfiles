
alias sudo="sudo "

# Git

## also `__git_complete g _git` in
## /usr/share/bash-completion/completions/git for tab completion to
## work
alias g="git"
alias gs="git status"
alias gco="git commit"

# Emacs
alias e="emacs"
alias te="emacs -nw"

# grep
alias grh="history | grep"
alias grl="ls -la | grep"

# Python virtualenv
function sba ()
{
    local original_directory=$PWD;
    while [[ ! -f bin/activate ]] && [[ $PWD != / ]]; do
        cd ..
    done
    if [[ -f bin/activate ]]; then
        source bin/activate;
    else
        echo "couldn't find virtualenv"
    fi
    cd $original_directory
}

# Racket with up-arrow history
alias racket_repl="racket -il readline"
