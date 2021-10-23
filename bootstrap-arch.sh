#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

if [ "$EUID" -eq 0 ]; then
    echo "Do not run this script as root, instead run it as the non-root user you want to set up."
    exit 1
fi

doit() {
    git submodule update --init

    ## Install and configure most things.
    ./aconfmgr apply --yes

    ## Generate locales
    sudo locale-gen

    ## Install homies
    ./install
}

install_volnoti() {
    # TODO: package this up in some useful way or find an alternative?
    # Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from Github.
    # [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
    if ! [ -x "$(command -v volnoti)" ]; then
        (
            mkdir -p ~/thirdrepos
            cd ~/thirdrepos
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

# TODO: port to aconfmgr.
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

doit

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
