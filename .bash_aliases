# -*- mode: sh; -*-

alias sudo="sudo "
alias r="reset"

# Git

## also `__git_complete g _git` in
## /usr/share/bash-completion/completions/git for tab completion to
## work
alias g="git"
alias gs="git status"
alias gco="git commit"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log"
alias gg="git log --graph --oneline --decorate"

# Emacs
alias e="emacs"
alias te="emacs -nw"

# grep
alias grh="history | grep"
alias grl="ls -la | grep"
alias gre="env | grep"
alias grps="ps aux | grep"

# Vagrant
alias v="vagrant"

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
