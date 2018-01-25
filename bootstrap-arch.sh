#!/usr/bin/env bash

THIRD_REPOS_DIR=~/thirdrepos

set -e
cd "$(dirname "$0")"

arch_package() {
    sudo pacman -S --noconfirm --needed $@
}

aur_package() {
    for aur_package_name in $@; do
        if [ -n "$(pacman -Qs ${aur_package_name}$)" ]; then
            echo "warning: AUR package $aur_package_name is already installed -- skipping"
        else
            (
                wrk_dir=$THIRD_REPOS_DIR/$aur_package_name
                rm -rf $wrk_dir
                mkdir -p $wrk_dir
                cd $wrk_dir
                curl -o PKGBUILD "https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=$aur_package_name"
                makepkg --syncdeps --noconfirm --needed --install PKGBUILD
            )
        fi
    done
}

enable_service() {
    sudo systemctl enable --now "$1"
}

enable_service_not_now() {
    sudo systemctl enable "$1"
}

## Some hacky pre-setup before running dotbot
# Dotbot sets up some symlinks into Dropbox, but this doesn't work if our
# Dropbox has not yet sync-ed. We hack around this by creating the expected
# folders if they don't yet exist.
mkdir -p ~/Dropbox/linux-secrets/{kaladin-ssh,gnupg}
mkdir -p ~/Dropbox/pics/lolcommits
sudo mkdir -p /root/Dropbox/linux-secrets/{kaladin-ssh,gnupg}
sudo mkdir -p /root/Dropbox/pics/lolcommits

## Install dotfiles
arch_package python
sudo ./install
./install

# Install dropbox
aur_package dropbox

## Dependencies to install stuff from the AUR
arch_package wget base-devel

## Python
arch_package python-pip python-pexpect openssh
sudo pip install setproctitle # needed by spawn-and-stuff

## Vim
arch_package gvim editorconfig-core-c
vim +PlugInstall +qall
# Symlink vi to vim if it has not been symlinked already.
if [ `readlink /usr/bin/vi` != "/usr/bin/vim" ]; then
    sudo mv /usr/bin/vi /usr/bin/vi.bak
    sudo ln -s /usr/bin/vim /usr/bin/vi
fi

## Misc
arch_package zsh mosh the_silver_searcher fzf hub efibootmgr
if [ ! -d ~/.oh-my-zsh ]; then
    git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

## Keyboard stuff
# Run xmodmap every time a keyboard appears.
enable_service fixinputs.path

## Setting up X11 and xmonad
# Install the appropriate video card driver: https://wiki.archlinux.org/index.php/xorg#Driver_installation
# driconf is supposed to help with video tearing (see http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)
arch_package xf86-video-intel driconf
# Install everything else needed for the desktop environment.
arch_package xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb
arch_package xmonad xmonad-contrib xmobar
arch_package feh wmname network-manager-applet alsa-utils maim xclip numlockx xsel xdotool slop byzanz dunst
arch_package termite chromium
aur_package trayer-srg-git dmenu2
# Fonts
arch_package ttf-liberation ttf-bitstream-vera noto-fonts noto-fonts-emoji
aur_package ttf-merriweather ttf-merriweather-sans ttf-opensans ttf-oswald ttf-quintessential ttf-signika ttf-google-fonts-git

# Lock screen on suspend.
# https://wiki.archlinux.org/index.php/Slock
arch_package slock
enable_service_not_now slock@jeremy.service

## Setting up wireless with network manager
arch_package networkmanager network-manager-applet networkmanager-vpnc gnome-keyring
enable_service NetworkManager.service

## Printer
arch_package cups
enable_service org.cups.cupsd.service

## Bluetooth
arch_package bluez bluez-utils gnome-bluetooth blueman
enable_service bluetooth.service

## Audio
arch_package pulseaudio pamixer pavucontrol paprefs bc sox
aur_package pasystray-gtk2
# Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from Github.
# [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
if ! [ -x "$(command -v volnoti)" ]; then
    (
        cd $THIRD_REPOS_DIR
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

## Power stuff
# https://wiki.archlinux.org/index.php/Laptop_Mode_Tools
# You can edit `/etc/laptop-mode/laptop-mode.conf` as you see fit (https://push.cx/2015/dual-booting-arch-linux-on-lenovo-x1-carbon-3rd-gen suggests changing LM_BATT_MAX_LOST_WORK_SECONDS)
aur_package laptop-mode-tools
arch_package acpi acpid ethtool wireless_tools
enable_service laptop-mode

## Increase number of inotify watchers
# https://github.com/webpack/docs/wiki/troubleshooting#not-enough-watchers
sudo bash -c "echo fs.inotify.max_user_watches=524288 > /etc/sysctl.d/99-sysctl.conf"
sudo sysctl --system

## GPG stuff
arch_package gnupg

## Some FUSE stuff
arch_package sshfs
# MTP stuff (see https://wiki.archlinux.org/index.php/MTP#simple-mtpfs)
aur_package simple-mtpfs

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
