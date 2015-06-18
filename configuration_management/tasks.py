import os
import sys
import urllib.request

from invoke import task, run

# bootstap the ability to run these tasks by getting Python 3 and Invoke


# general task helpers

def idempotence_guard(condition):
    if not condition:
        ...  # TODO


# apt

@task
def apt_get_update():
    run("sudo apt-get update")

@task
def apt_get_install(packages):
    run("sudo apt-get install {}".format(packages))

MY_APT_PACKAGES = ("emacs24", "python3", "python3-pip", "curl", "git", "gitk", "pandoc",
                   "silversearcher-ag", "default-jre", "chromium-browser", "sqlite",
                   "redshift", "cowsay", "build-essential", "dkms")

@task
def apt_get_my_packages():
    apt_get_install(" ".join(package for package in MY_APT_PACKAGES))


# pip

MY_PIP_PACKAGES = ("pudb", "invoke", "ipython")

@task
def pip_install(packages, sudo=True, upgrade=False):
    run("sudo pip3 install {}".format(packages))

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

DOTFILES_REPO_PATH = "/home/zmd/Code/dotfiles"

## generalized tasks

@task
def my_github_clone(repository):
    github_clone("zackmdavis", repository, "/home/zmd/Code/" + repository)

@task
def install_deb(deb_url):
    local_destination = "/tmp/{}".format(deb_url.rsplit('/', 1)[1])
    if not os.path.exists(local_destination):
        urllib.request.urlretrieve(
            deb_url, local_destination
        )
    run("sudo dpkg -i {}".format(local_destination))


## configuration

@task
def symlink_dotfiles():
    if not os.path.exists(DOTFILES_REPO_PATH):
        my_github_clone(repository)
    else:
        print("dotfiles repository already exists, continuing ...")
    my_dotfiles = ('.emacs', '.bash_aliases', '.agignore', '.lein',
                   '.emacs.d/themes', '.aspell.en.prepl', '.aspell.en.pws')
    for dotfile in my_dotfiles:
        if not os.path.islink("/home/zmd/" + dotfile):
            print("linking {}".format(dotfile))
            run("ln -s /home/zmd/Code/dotfiles/{0} /home/zmd/{0}".format(dotfile))
        else:
            print("{} symlink already exists, continuing ...".format(dotfile))

@task
def install_gitconfig():
    run("cp {}/.gitconfig /home/zmd/.gitconfig".format(DOTFILES_REPO_PATH))


@task
def generate_ssh_keys(email, machine):
    run("ssh-keygen -t rsa -b 4096 -C \"{} ({})\"".format(email, machine))


## particular applications

@task
def install_leiningen(path="/usr/local/bin/lein"):
    run("curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /tmp/lein")
    run("sudo cp /tmp/lein {}".format(path))
    mark_executable(path, sudo=True)


@task
def install_vagrant():
    vagrant_deb_url = "https://dl.bintray.com/mitchellh/vagrant/vagrant_1.7.1_x86_64.deb"
    install_deb(vagrant_deb_url)


@task
def install_virtualbox():
    run("wget -q http://download.virtualbox.org/virtualbox/debian/oracle_vbox.asc -O- | sudo apt-key add -")
    run("sudo sh -c 'echo \"deb http://download.virtualbox.org/virtualbox/debian trusty contrib\" >> /etc/apt/sources.list.d/virtualbox.list'")
    apt_get_update()
    apt_get_install("virtualbox-4.3")