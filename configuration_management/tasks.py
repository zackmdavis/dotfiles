import os
import sys

from invoke import task, run

# bootstap the ability to run these tasks by getting Python 3 and Invoke


# apt

@task
def apt_get_update():
    run("sudo apt-get update")

@task
def apt_get_install(package):
    run("sudo apt-get install {}".format(package))

MY_APT_PACKAGES = ("emacs24", "python3", "python3-pip", "curl", "git", "gitk", "pandoc",
                   "default-jre", "chromium-browser", "sqlite", "redshift", "cowsay")

@task
def apt_get_my_packages():
    apt_get_install(" ".join(package for package in MY_APT_PACKAGES))


# pip

MY_PIP_PACKAGES = ("pudb", "invoke")

@task
def pip_install(package, sudo=True, upgrade=False):
    run("sudo pip install")

@task
def pip_install_my_packages():
    pip_install(" ".join(package for package in MY_PIP_PACKAGES))


# git

def git_clone(origin, destination):
    run("git clone {} {}".format(origin, destination))

def github_clone(user, repository, destination):
    git_clone("git@github.com:{user}/{repository}.git".format(**locals()),
              destination)


# files

@task
def make_directory_in_home(subpath):
    os.mkdir("/home/zmd/" + path, 0o775)

@task
def mark_executable(path, sudo=False):
    run("{}chmod +x {}".format("sudo " if sudo else '', path))



# development setup

@task
def my_github_clone(repository):
    github_clone("zackmdavis", repository, "/home/zmd/Code/" + repository)

@task
def symlink_dotfiles():
    # TODO: constantize shared subpaths more
    if not os.path.exists("/home/zmd/Code/dotfiles"):
        my_github_clone(repository)
    else:
        print("dotfiles repository already exists, continuing ...")
    my_dotfiles = ('.emacs', '.gitconfig', '.bash_aliases', '.agignore', '.lein')
    for dotfile in my_dotfiles:
        if not os.path.islink("/home/zmd/" + dotfile):
            run("ln -s /home/zmd/Code/dotfiles/{0} /home/zmd/{0}".format(dotfile))
        else:
            print("{} symlink already exists, continuing ...".format(dotfile))

@task
def install_leiningen(path="/usr/local/bin/lein"):
    run("curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /tmp/lein")
    run("sudo cp /tmp/lein {}".format(path))
    mark_executable(path, sudo=True)
