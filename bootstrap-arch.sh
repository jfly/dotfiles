#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

HOSTNAME=$(cat /etc/hostname)

if [ "$EUID" -eq 0 ]; then
    echo "Do not run this script as root, instead run it as the non-root user you want to set up."
    exit 1
fi

arch_package() {
    to_install=""
    for arch_package_name in "$@"; do
        if [ -n "$(pacman -Qs "^${arch_package_name}$")" ]; then
            echo "warning: Arch package $arch_package_name is already installed -- skipping"
        else
            to_install="$to_install $arch_package_name"
        fi
    done

    if [ -n "$to_install" ]; then
        sudo pacman -S --noconfirm --needed $to_install
    fi
}

THIRD_REPOS_DIR=~/thirdrepos
aur_package() {
    for aur_package_name in "$@"; do
        if [ -n "$(pacman -Qs "^${aur_package_name}$")" ]; then
            echo "warning: AUR package $aur_package_name is already installed -- skipping"
        else
            (
                cd "$THIRD_REPOS_DIR"
                tar_name="${aur_package_name}.tar.gz"
                curl -o "$tar_name" "https://aur.archlinux.org/cgit/aur.git/snapshot/$tar_name"
                tar xvf "$tar_name"
                cd "${aur_package_name}"

                makepkg --syncdeps --noconfirm --needed --install PKGBUILD
            )
        fi
    done
}

enable_service() {
    sudo systemctl enable --now "$@"
}

enable_service_not_now() {
    sudo systemctl enable "$@"
}

base_stuff() {
    ## Set timezone
    # https://wiki.archlinux.org/index.php/installation_guide#Time_zone
    sudo ln -sf /usr/share/zoneinfo/America/Los_Angeles /etc/localtime
    sudo hwclock --systohc || true # RPI doesn't even have a RTC module!

    ## Set locale
    sudo sed -i 's/^#\(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen
    sudo locale-gen
    sudo bash -c "echo LANG=en_US.UTF-8 > /etc/locale.conf"

    ## Some hacky pre-setup before running dotbot
    # Dotbot sets up some symlinks into Dropbox, but this doesn't work if our
    # Dropbox has not yet sync-ed. We hack around this by creating the expected
    # folders if they don't yet exist.
    mkdir -p ~/Dropbox/linux-secrets/{kaladin-ssh,gnupg}
    mkdir -p ~/Dropbox/pics/lolcommits
    sudo mkdir -p /root/Dropbox/linux-secrets/{kaladin-ssh,gnupg}
    sudo mkdir -p /root/Dropbox/pics/lolcommits

    ## Install dotfiles
    arch_package which python
    git submodule update --init
    sudo ./install
    ./install

    # TODO - for some reason, symlinking this file gives a
    # "Parsing /etc/bluetooth/main.conf failed: Permission denied" error when bluetoothd
    # starts up.
    sudo mkdir -p /etc/bluetooth/ && sudo cp etc_bluetooth_main.conf /etc/bluetooth/main.conf

    # This file must be copied, not symlinked. See https://github.com/systemd/systemd/issues/12410 for more information.
    sudo mkdir -p /etc/systemd/system/systemd-logind.service.d/
    sudo cp etc/systemd/system/systemd-logind.service.d/override.conf /etc/systemd/system/systemd-logind.service.d/override.conf

    ## Dependencies to install stuff from the AUR
    arch_package wget base-devel gcc make fakeroot patch autoconf automake

    ## Python
    arch_package python-pip python-pexpect openssh
    if [ "$HOSTNAME" != "jpi" ]; then # Unfortunately, direnv is not available for the 'armv6h' architecture.
        aur_package direnv
    fi

    ## Misc
    arch_package zsh mosh the_silver_searcher fzf hub efibootmgr dnsutils screen rsync oath-toolkit inetutils
    if [ ! -d ~/.oh-my-zsh ]; then
        git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    fi
    if sudo [ ! -d /root/.oh-my-zsh ]; then
        sudo git clone git://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh
    fi

    ## Enable NTP
    sudo timedatectl set-ntp true

    ## Prevent issue where suspending with bluetooth devices connected
    ## would cause us to immediately wake from sleep.
    enable_service disable-bt-wakeup.service

    ## Keyboard stuff
    # Run xmodmap every time a keyboard appears.
    enable_service "fixinputs@$(whoami).path"

    ## Printer
    arch_package cups ghostscript
    enable_service cups.service

    ## Bluetooth
    arch_package bluez bluez-utils
    enable_service bluetooth.service

    ## GPG stuff
    arch_package gnupg

    ## Some FUSE stuff
    arch_package sshfs

    ## MDNS stuff
    arch_package avahi nss-mdns
    enable_service avahi-daemon

    ## Keep mirrorlist up to date
    arch_package reflector

    ## Install delta (used by git diff)
    cd ~/thirdrepos
    if [ -n "$(pacman -Qs "^git-delta$")" ]; then
        echo "warning: Arch package git-delta is already installed -- skipping"
    else
        rm -rf git-delta
        git clone https://aur.archlinux.org/git-delta.git
        cd git-delta
        makepkg -csri
    fi
}

install_vim() {
    ## Vim
    arch_package "$1" editorconfig-core-c ctags
    vim +PlugInstall +qall
}

install_docker() {
    ## Docker stuff
    arch_package docker docker-compose gnome-keyring
    aur_package docker-credential-secretservice
}

laptop_stuff() {
    ln -sf ~/.ssh ~/Dropbox/linux-secrets/kaladin-ssh
    ln -sf ~/.gnupg ~/Dropbox/linux-secrets/gnupg
    ln -sf ~/.lolcommits ~/Dropbox/pics/lolcommits

    install_vim gvim

    ## MTP stuff (see https://wiki.archlinux.org/index.php/MTP#simple-mtpfs)
    aur_package simple-mtpfs

    ## Install dropbox
    aur_package dropbox

    ## Bluetooth stuff
    arch_package blueman

    ## Setting up X11 and xmonad
    # Install the appropriate video card driver: https://wiki.archlinux.org/index.php/xorg#Driver_installation
    # driconf is supposed to help with video tearing (see http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)
    arch_package xf86-video-intel driconf
    # Needed for hardware acceleration on Parsec and Chrome. See
    # https://github.com/jfly/dotfiles/commit/eef1e079114aaee1fe0740151a5340e1574b4659
    # for details.
    arch_package libva-intel-driver intel-gpu-tools
    # Install everything else needed for the desktop environment.
    arch_package xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb xorg-xinput xorg-xprop
    arch_package xmonad xmonad-contrib xmobar
    arch_package feh network-manager-applet alsa-utils maim xclip numlockx xsel xdotool slop byzanz dunst
    arch_package chromium
    arch_package termite
    # 2020-07-12: i'm experimenting with alacritty
    arch_package alacritty
    aur_package xcwd-git
    aur_package trayer-srg-git dmenu2 xsettingsd
    # Fonts
    arch_package ttf-liberation ttf-bitstream-vera noto-fonts noto-fonts-emoji
    aur_package ttf-merriweather ttf-merriweather-sans ttf-opensans ttf-oswald ttf-quintessential ttf-signika ttf-google-fonts-git nerd-fonts-ubuntu-mono

    # Lock screen on suspend.
    # https://wiki.archlinux.org/index.php/Slock
    arch_package slock
    enable_service_not_now "slock@$(whoami).service"

    ## Setting up wireless with network manager
    arch_package networkmanager network-manager-applet networkmanager-vpnc gnome-keyring
    enable_service NetworkManager.service

    ## Audio
    arch_package pipewire pipewire-{alsa,jack,media-session,pulse}
    arch_package pamixer pavucontrol pasystray paprefs bc sox
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
    enable_service laptop-mode acpid

    ## Increase number of inotify watchers
    # https://github.com/webpack/docs/wiki/troubleshooting#not-enough-watchers
    sudo bash -c "echo fs.inotify.max_user_watches=524288 > /etc/sysctl.d/99-sysctl.conf"
    sudo sysctl --system

    ## Git stuff
    # https://github.com/so-fancy/diff-so-fancy
    arch_package diff-so-fancy

    ## Fix rich console output in Gradle.
    # See https://github.com/adammurdoch/native-platform/issues/24
    # and https://discuss.gradle.org/t/how-do-i-always-force-console-auto-in-a-gradle-properties-or-environment-variable/12039/8
    aur_package ncurses5-compat-libs

    ## Interception tools + space2meta
    aur_package interception-tools interception-space2meta
    #<<<enable_service udevmon # seems to be slowing down boot by a while?

    install_docker

    music_stuff
}

music_stuff() {
    arch_package mpd
    systemctl enable --now --user mpd

    # This was a pain to get installed.
    # I had to run
    #   gpg --keyserver 'hkps://keyserver.ubuntu.com' --recv-keys 0392335A78083894A4301C43236E8A58C6DB4512
    # (see https://aur.archlinux.org/packages/mpdscribble/)
    aur_package mpdscribble
    systemctl enable --now --user mpdscribble

    # I couldn't install this without manually removing the "gmock>=1.10" from
    # the PKGBUILD file.
    #  https://aur.archlinux.org/packages/ashuffle/#comment-815297
    aur_package ashuffle
}

pi_htpc_stuff() {
    ## Install kodi
    # https://wiki.archlinux.org/index.php/Kodi#Raspberry_Pi_.28all_generations.29
    # and http://blog.monkey.codes/how-to-setup-kodi-on-a-raspberry-pi/
    arch_package kodi-rbp libcec-rpi
    sudo sed -i 's/gpu_mem=64/gpu_mem=320/' /boot/config.txt

    enable_service_not_now kodi

    kodi_stuff
    nas_stuff
}

nuc_nas_stuff() {
    install_docker
    enable_service docker

    kodi_stuff
    nas_stuff
}

kodi_stuff() {
    # Configure kodi
    sudo cp kodi/userdata/guisettings.xml /var/lib/kodi/.kodi/userdata/guisettings.xml
    sudo cp kodi/userdata/sources.xml /var/lib/kodi/.kodi/userdata/sources.xml
    sudo cp -r kodi/userdata/keymaps /var/lib/kodi/.kodi/userdata/keymaps
    sudo chown -R kodi:kodi /var/lib/kodi/.kodi/userdata/
}

nas_stuff() {
    install_vim vim

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
elif [ "$HOSTNAME" = "kent" ]; then
    device_specific_command=pi_htpc_stuff
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
