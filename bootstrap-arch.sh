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

    ## Install some more things
    nix-env -irf my-nix '.*'

    ## Generate locales
    sudo locale-gen

    ## Install homies
    ./install
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
