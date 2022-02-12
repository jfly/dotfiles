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

doit

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
