import os
import sys
import urllib.request
from functools import wraps

from invoke import task, run

# bootstap the ability to run these tasks by getting Python 3 and Invoke

# XXX TODO: update to new Invoke (this seems to work with 0.10, possibly a
# little later)

# general task helpers

def not_if_any(predicate_manifest):
    def derived_decorator(func):
        @wraps(func)
        def core(*args, **kwargs):
            blocking = {reason for reason, predicate
                        in predicate_manifest.items() if predicate()}
            if blocking:
                print("skipping {}: {}".format(
                    func.__name__, ', '.join(reason for reason in blocking)))
            else:
                print("running {}".format(func.__name__))
                return func(*args, **kwargs)
        return core
    return derived_decorator

def not_if_content_in_file(content, my_file_path):
    def content_in_file():
        try:
            with open(my_file_path) as my_file:
                file_contents = my_file.read()
            return content in file_contents
        except FileNotFoundError:
            print("{} does not exist".format(my_file_path))
            return False
    return not_if_any(
        {"{!r} already in {}".format(content, my_file_path): content_in_file}
    )

def not_if_any_paths(predicate, *paths):
    return not_if_any(
        {predicate.__name__: lambda: predicate(path) for path in paths}
    )

def not_if_any_is_symlink(*paths):
    return not_if_any_paths(os.path.islink, *paths)


# apt

@task
def apt_get_update():
    run("sudo apt-get update")

@task
def apt_get_install(packages):
    run("sudo apt-get install {}".format(packages))

MY_APT_PACKAGES = ("emacs24", "python3", "python3-dev", "python3-tk", "curl", "git",
                   "gitk", "pandoc", "at",
                   "silversearcher-ag", "default-jre", "chromium-browser", "sqlite",
                   "lm-sensors",
                   "python-dev", "tig", "git-flow",
                   "redshift", "cowsay", "build-essential", "dkms", "libreadline-dev",
                   "nodejs", "npm", "git-review")

@task
def apt_get_my_packages():
    apt_get_install(" ".join(package for package in MY_APT_PACKAGES))


# pip

MY_PIP_PACKAGES = ("pudb", "invoke", "ipython", "hy")

@task
def pip_install(packages, sudo=True, upgrade=False):
    run("sudo pip3 install {}".format(packages))

@task
def pip_install_my_packages():
    pip_install(" ".join(package for package in MY_PIP_PACKAGES))


# git

def git_clone(origin, destination):
    run("git clone {} {}".format(origin, destination))

@task
def github_clone(user, repository, destination=None):
    if not destination:
        destination = "~/Code/{}".format(repository)
    git_clone("git@github.com:{user}/{repository}.git".format(**locals()),
              destination)


# files

@task
def make_dir_in_home(subpath):
    os.mkdir("/home/zmd/" + path, 0o775)

@task
def mark_executable(path, sudo=False):
    run("{}chmod +x {}".format("sudo " if sudo else '', path))



# development setup

HOME = os.path.join(os.sep, 'home', 'zmd')
DOTFILES_REPO_PATH = os.path.join(HOME, 'Code', 'dotfiles')

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
    my_dotfiles = ('.emacs', '.bash_aliases', '.agignore', '.lein',
                   '.emacs.d/themes', '.aspell.en.prepl', '.aspell.en.pws')
    for dotfile in my_dotfiles:
        if not os.path.islink("/home/zmd/" + dotfile):
            print("linking {}".format(dotfile))
            run("ln -s /home/zmd/Code/dotfiles/{0} /home/zmd/{0}".format(dotfile))
        else:
            print("{} symlink already exists, continuing ...".format(dotfile))

@task
def symlink_dayjob_dotfiles():
    ... # TODO

@task
def symlink_scripts():
    for script in os.listdir(os.path.join(DOTFILES_REPO_PATH, 'bin')):
        dotfiles_script_path = os.path.join(DOTFILES_REPO_PATH, 'bin', script)
        bin_script_path = os.path.join(HOME, 'bin', script)
        if not os.path.islink(bin_script_path):
            print("linking {}".format(script))
            run("ln -s {} {}".format(dotfiles_script_path, bin_script_path))
        else:
            print("{} symlink already exists, continuing ...".format(script))


@task
def install_gitconfig():
    run("cp {}/.gitconfig /home/zmd/.gitconfig".format(DOTFILES_REPO_PATH))


@task
def generate_ssh_keys(email, machine):
    run("ssh-keygen -t rsa -b 4096 -C \"{} ({})\"".format(email, machine))

@task
def make_home_bin_dir():
    run("mkdir -p /home/zmd/bin")


@task
def install_go():
    run("cd /tmp")
    run("wget https://storage.googleapis.com/golang/go1.7.1.linux-amd64.tar.gz")
    run("sudo tar -C /usr/local -xzf go1.7.1.linux-amd64.tar.gz")


@task
@not_if_content_in_file("GOPATH", "/home/zmd/.bashrc")
def export_gopath_in_bashrc():
    run("echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc")
    run("echo 'export GOPATH=~/Code/go_workspace' >> ~/.bashrc")
    run("echo 'export PATH=$PATH:$GOPATH/bin' >> ~/.bashrc")


@task
@not_if_content_in_file("cargo/bin", "/home/zmd/.bashrc")
def export_cargo_bins_in_bashrc():
    cargo_bin_dirs = ["~/.multirust/toolchains/{}/cargo/bin".format(toolchain)
                      for toolchain in ('stable', 'nightly')]
    run("echo 'export PATH=$PATH:{}:{}' >> ~/.bashrc".format(*cargo_bin_dirs))


@task
def install_goimports():
    run("go get golang.org/x/tools/cmd/goimports")


@task
@not_if_content_in_file("~/bin", "/home/zmd/.bashrc")
def export_home_bin_in_path():
    run("echo 'PATH=$PATH:~/bin' >> ~/.bashrc")


@task
@not_if_content_in_file("ctrl:nocaps", "/home/zmd/.bashrc")
def no_caps_lock():
    run("echo 'setxkbmap -layout us -option ctrl:nocaps' >> ~/.bashrc")


# TODO: abstract "appending to .bashrc" into own function
# set revert-all-at-newline on

## particular applications

@task
def install_multirust():
    run("curl https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh > /tmp/blastoff.sh")
    mark_executable("/tmp/blastoff.sh")
    run("/tmp/blastoff.sh")


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
    apt_get_install("virtualbox-5.0")
