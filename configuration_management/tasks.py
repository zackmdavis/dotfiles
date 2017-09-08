import os
import sys
import urllib.request
from functools import wraps

from invoke import task, run

# bootstap the ability to run these tasks by getting Python 3 and Invoke

HOME = os.path.join(os.sep, 'home', 'zmd')
DOTFILES_REPO_PATH = os.path.join(HOME, 'Code', 'dotfiles')

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
def apt_get_update(ctx):
    run("sudo apt-get update")

@task
def apt_get_install(ctx, packages):
    ctx.run("sudo apt-get install {}".format(packages))

MY_APT_PACKAGES = [
    "emacs24", "python3", "python3-dev", "python3-tk", "curl", "git",
    "gitk", "pandoc", "at", "silversearcher-ag", "default-jre",
    "chromium-browser", "sqlite", "lm-sensors", "htop", "redshift",
    "build-essential", "dkms", "libreadline-dev", "nodejs", "npm",
]

@task
def apt_get_my_packages(ctx):
    apt_get_install(ctx, " ".join(package for package in MY_APT_PACKAGES))


# pip

MY_PIP_PACKAGES = ("pudb", "invoke", "ipython", "hy")

@task
def pip_install(ctx, packages, sudo=True, upgrade=False):
    ctx.run("sudo pip3 install {}".format(packages))

@task
def pip_install_my_packages(ctx):
    pip_install(ctx, " ".join(package for package in MY_PIP_PACKAGES))


# git

def git_clone(ctx, origin, destination):
    ctx.run("git clone {} {}".format(origin, destination))

@task
def github_clone(ctx, user, repository, destination=None):
    if not destination:
        destination = "~/Code/{}".format(repository)
    git_clone(ctx, "git@github.com:{user}/{repository}.git".format(**locals()),
              destination)


# files

@task
def make_dir_in_home(ctx, subpath):
    os.mkdir("/home/zmd/" + path, 0o775)

@task
def mark_executable(ctx, path, sudo=False):
    ctx.run("{}chmod +x {}".format("sudo " if sudo else '', path))


@task
def append_to_bashrc(ctx, line):
    bashrc_path = os.path.join(HOME, ".bashrc")
    with open(bashrc_path) as bashrc:
        content = bashrc.read()
    if line in content:
        print("line {!r} already present in .bashrc".format(line))
        return
    content += "{}\n".format(line)
    with open(bashrc_path, 'w') as bashrc:
        bashrc.write(content)

        
## generalized tasks

@task
def my_github_clone(ctx, repository):
    github_clone(ctx, "zackmdavis", repository, "/home/zmd/Code/" + repository)

@task
def install_deb(ctx, deb_url):
    local_destination = "/tmp/{}".format(deb_url.rsplit('/', 1)[1])
    if not os.path.exists(local_destination):
        urllib.request.urlretrieve(
            deb_url, local_destination
        )
    ctx.run("sudo dpkg -i {}".format(local_destination))


## configuration

@task
def symlink_dotfiles(ctx):
    my_dotfiles = ('.emacs', '.bash_aliases', '.agignore', '.lein',
                   '.emacs.d/themes', '.aspell.en.prepl', '.aspell.en.pws')
    for dotfile in my_dotfiles:
        if not os.path.islink("/home/zmd/" + dotfile):
            print("linking {}".format(dotfile))
            ctx.run("ln -s /home/zmd/Code/dotfiles/{0} /home/zmd/{0}".format(dotfile))
        else:
            print("{} symlink already exists, continuing ...".format(dotfile))

@task
def symlink_scripts(ctx):
    for script in os.listdir(os.path.join(DOTFILES_REPO_PATH, 'bin')):
        dotfiles_script_path = os.path.join(DOTFILES_REPO_PATH, 'bin', script)
        bin_script_path = os.path.join(HOME, 'bin', script)
        if not os.path.islink(bin_script_path):
            print("linking {}".format(script))
            ctx.run("ln -s {} {}".format(dotfiles_script_path, bin_script_path))
        else:
            print("{} symlink already exists, continuing ...".format(script))


@task
def install_gitconfig(ctx):
    ctx.run("cp {}/.gitconfig /home/zmd/.gitconfig".format(DOTFILES_REPO_PATH))


@task
def generate_ssh_keys(ctx, email, machine):
    ctx.run("ssh-keygen -t rsa -b 4096 -C \"{} ({})\"".format(email, machine))

@task
def make_home_bin_dir(ctx):
    ctx.run("mkdir -p /home/zmd/bin")


@task
def install_go(ctx):
    ctx.run("cd /tmp")
    # TODO: tarball will have changed
    ctx.run("wget https://storage.googleapis.com/golang/go1.7.1.linux-amd64.tar.gz")
    ctx.run("sudo tar -C /usr/local -xzf go1.7.1.linux-amd64.tar.gz")


@task
def export_gopath_in_bashrc(ctx):
    ctx.run("echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc")
    ctx.run("echo 'export GOPATH=~/Code/go_workspace' >> ~/.bashrc")
    ctx.run("echo 'export PATH=$PATH:$GOPATH/bin' >> ~/.bashrc")

@task
def install_goimports(ctx):
    ctx.run("go get golang.org/x/tools/cmd/goimports")


@task
def export_home_bin_in_path(ctx):
    append_to_bashrc(ctx, "PATH=$PATH:~/bin")


@task
def no_caps_lock(ctx):
    append_to_bashrc(ctx, "setxkbmap -layout us -option ctrl:nocaps")

@task
def no_edit_terminal_history(ctx):
    append_to_bashrc(ctx, "bind 'revert-all-at-newline on'")


## particular applications


@task
def install_leiningen(ctx, path="/usr/local/bin/lein"):
    ctx.run("curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /tmp/lein")
    ctx.run("sudo cp /tmp/lein {}".format(path))
    mark_executable(path, sudo=True)


@task
def install_vagrant(ctx):
    # TODO: debball or other installation method will have changed
    vagrant_deb_url = "https://dl.bintray.com/mitchellh/vagrant/vagrant_1.7.1_x86_64.deb"
    install_deb(ctx, vagrant_deb_url)


@task
def add_oracle_repo(ctx):
    # TODO: check if current
    ctx.run("wget -q http://download.virtualbox.org/virtualbox/debian/oracle_vbox.asc -O- | sudo apt-key add -")
    ctx.run("sudo sh -c 'echo \"deb http://download.virtualbox.org/virtualbox/debian trusty contrib\" >> /etc/apt/sources.list.d/virtualbox.list'")

@task
def install_virtualbox(ctx):
    add_oracle_repo(ctx)
    apt_get_update(ctx)
    # TODO: check if current
    apt_get_install(ctx, "virtualbox-5.1")


@task
def simple_move_in(ctx):
    apt_get_my_packages(ctx)
    pip_install_my_packages(ctx)
    symlink_dotfiles(ctx)
    make_home_bin_dir(ctx)
    symlink_scripts(ctx)
    no_caps_lock(ctx)
    no_edit_terminal_history(ctx)
