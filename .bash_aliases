# -*- mode: sh; -*-

alias sudo="sudo "
alias r="tput reset"

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
alias gw="git diff --word-diff"
alias gwc="git diff --word-diff --cached"

function re_base_on () {
    git checkout "$1" &&
    git pull &&
    git checkout - &&
    git rebase "$1"
}

function rebase_on_master () {
    re_base_on master
}

function repush_off_master () {
    branch=$(git rev-parse --abbrev-ref HEAD)
    rebase_on_master
    if git show-branch remotes/origin/$branch 2> /dev/null; then
        git push origin $branch --force-with-lease
    else
        git push origin $branch
    fi
}

function merge_up () {
    branch=$(git rev-parse --abbrev-ref HEAD)
    git checkout master &&
    git merge $branch &&
    git push origin master &&
    git push origin --delete $branch &&
    git branch -d $branch
}

# Emacs
alias e="emacs"
alias te="emacs -nw"

# grep
alias grh="history | grep"
alias grl="ls -la | grep"
alias gre="env | grep"
alias grps="ps aux | grep"
alias grports="netstat -tulpn | grep"

# cat
alias 🐱="cat"

# Vagrant
alias v="vagrant"

# the only thing I use `sed` for
function replace ()
{
    if [[ -z "$1" ]] || [[ -z "$2" ]]; then
        echo "for non-disastrous results, this function needs two arguments"
        return 2
    fi
    find . -type f -not -path "./.git/*" -print0 | xargs -0 sed -i "s/$1/$2/g"
}

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

alias cdgow="cd ~/Code/go_workspace/src"
