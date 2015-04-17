import os
import sys

from invoke import task, run

# bootstap the ability to run these tasks by getting Python 3 and Invoke

@task
def apt_get_install(package):
    run("sudo apt-get install {}".format(package))

MY_PACKAGES = ("emacs24", "python3", "curl", "gitk", "pandoc",
               "chromium-browser", "sqlite", "redshift", "cowsay")


def git_clone(origin, destination):
    run("git clone {} {}".format(origin, destination))

def github_clone(user, repository, destination):
    # XXX use the SSH clone URL
    git_clone("https://github.com/{user}/{repository}.git".format(**locals()),
              destination)

@task
def my_github_clone(repository):
    github_clone("zackmdavis", repository, "/home/zmd/Code/" + repository)

@task
def make_directory_in_home(subpath):
    os.mkdir("/home/zmd/" + path, 0o775)

@task
def symlink_dotfiles():
    my_dotfiles = ('.emacs', '.gitconfig', '.bash_aliases', '.agignore', '.lein')
    for dotfile in my_dotfiles:
        run("ln -s /home/zmd/Code/dotfiles/{0} /home/zmd/{0}".format(dotfile))

@task
def install_leiningen():
    run("curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /tmp/lein")
    # TODO ...
