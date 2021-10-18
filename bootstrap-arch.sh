#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

HOSTNAME=$(cat /etc/hostname)

if [ "$EUID" -eq 0 ]; then
    echo "Do not run this script as root, instead run it as the non-root user you want to set up."
    exit 1
fi

base_stuff() {
    git submodule update --init

    ./aconfmgr apply

    ## Generate locales
    sudo locale-gen

    ## Install homies
    ./install

    ## oh-my-zsh
    if [ ! -d ~/.oh-my-zsh ]; then
        git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    fi
}

laptop_stuff() {
    # Here we're setting up some symlinks into Dropbox, but this doesn't work
    # if our Dropbox has not yet sync-ed. We hack around this by creating the
    # expected folders if they don't yet exist.
    mkdir -p ~/Dropbox/linux-secrets/{kaladin-ssh,gnupg}
    mkdir -p ~/Dropbox/pics/lolcommits
    ln -sf ~/.ssh ~/Dropbox/linux-secrets/kaladin-ssh
    chmod -R u=rwX,og= ~/.ssh/ # Fix ssh key permissions
    ln -sf ~/.gnupg ~/Dropbox/linux-secrets/gnupg
    chmod -R u=rwX,og= ~/.gnupg # Fix gnupg permissions
    ln -sf ~/.lolcommits ~/Dropbox/pics/lolcommits

    # Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from Github.
    # [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
    if ! [ -x "$(command -v volnoti)" ]; then
        (
            THIRD_REPOS_DIR=~/thirdrepos
            cd "$THIRD_REPOS_DIR"
            rm -rf volnoti
            git clone https://github.com/hcchu/volnoti.git
            cd volnoti
            ./prepare.sh
            ./configure --prefix=/usr
            (
                # See https://ubuntuforums.org/showthread.php?t=2215264&p=12978792#post12978792
                cd src;
                rm value-client-stub.h && make value-client-stub.h
                dbus-binding-tool --prefix=volume_object --mode=glib-client specs.xml > value-client-stub.h
                rm value-daemon-stub.h && make value-daemon-stub.h
                dbus-binding-tool --prefix=volume_object --mode=glib-server specs.xml > value-daemon-stub.h
            )
            make
            sudo make install
        )
    fi
}

nuc_nas_stuff() {
    # Enable sshd
    enable_service sshd

    # Install and configure transmission.
    arch_package transmission-cli
    enable_service transmission
    # Annoyingly, if transmission seems to clobber any changes to its config
    # files when it exists. That means we need to stop it (and wait for it to
    # shut down) before we start tweaking its settings.
    sudo systemctl stop transmission && sleep 1
    sudo sed -i 's_\( *"download-dir": \).*_\1"/mnt/media/torrents",_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-host-whitelist": \).*_\1"*",_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-host-whitelist-enabled": \).*_\1false,_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-whitelist": \).*_\1"*",_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-whitelist-enabled": \).*_\1false,_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo systemctl start transmission

    # Configure and start HTPC webui docker container.
    docker compose -f ~/.dotfiles/containers/htpc-ui/docker-compose.yml up --detach

    # Start the home-assistant docker container
    docker compose -f ~/.dotfiles/containers/home-assistant/docker-compose.yml up --detach
}

if [ "$HOSTNAME" = "dalinar" ]; then
    device_specific_command=laptop_stuff
elif [ "$HOSTNAME" = "clark" ]; then
    device_specific_command=nuc_nas_stuff
else
    echo "Unrecognized hostname: '$HOSTNAME'"
    exit 2
fi

base_stuff
$device_specific_command

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
