#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

if [ "$EUID" -eq 0 ]; then
    echo "Do not rush this script as root, instead run it as the non-root user you want to set up."
    exit 1
fi

arch_package() {
    sudo pacman -S --noconfirm --needed $@
}

THIRD_REPOS_DIR=~/thirdrepos
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
    sudo systemctl enable --now $@
}

enable_service_not_now() {
    sudo systemctl enable $@
}

base_stuff() {
    ## Set timezone
    # https://wiki.archlinux.org/index.php/installation_guide#Time_zone
    sudo ln -sf /usr/share/zoneinfo/America/Los_Angeles /etc/localtime
    sudo hwclock --systohc || true # RPI doesn't even have a RTC module!

    ## Set locale
    sudo sed -i 's/^#\(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen
    sudo locale-gen

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

    ## Dependencies to install stuff from the AUR
    arch_package wget base-devel

    ## Python
    arch_package python-pip python-pexpect openssh
    sudo pip install setproctitle # needed by spawn-and-stuff

    ## Misc
    arch_package zsh mosh the_silver_searcher fzf hub efibootmgr dnsutils screen
    if [ ! -d ~/.oh-my-zsh ]; then
        git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    fi

    ## Enable NTP
    sudo timedatectl set-ntp true

    ## Prevent issue where suspending with bluetooth devices connected
    ## would cause us to immediately wake from sleep.
    enable_service disable-bt-wakeup.service

    ## Keyboard stuff
    # Run xmodmap every time a keyboard appears.
    enable_service fixinputs@$(whoami).path

    ## Printer
    arch_package cups ghostscript
    enable_service org.cups.cupsd.service

    ## Bluetooth
    arch_package bluez bluez-utils gnome-bluetooth blueman
    enable_service bluetooth.service

    ## GPG stuff
    arch_package gnupg

    ## Some FUSE stuff
    arch_package sshfs
}

install_vim() {
    ## Vim
    arch_package $1 editorconfig-core-c
    vim +PlugInstall +qall
    # Symlink vi to vim if it has not been symlinked already.
    if [ `readlink /usr/bin/vi` != "/usr/bin/vim" ]; then
        sudo mv /usr/bin/vi /usr/bin/vi.bak
        sudo ln -s /usr/bin/vim /usr/bin/vi
    fi
}

laptop_stuff() {
    install_vim gvim

    ## MTP stuff (see https://wiki.archlinux.org/index.php/MTP#simple-mtpfs)
    aur_package simple-mtpfs

    ## Install dropbox
    aur_package dropbox

    ## Setting up X11 and xmonad
    # Install the appropriate video card driver: https://wiki.archlinux.org/index.php/xorg#Driver_installation
    # driconf is supposed to help with video tearing (see http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)
    arch_package xf86-video-intel driconf
    # Install everything else needed for the desktop environment.
    arch_package xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb xorg-xinput
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
    enable_service_not_now slock@$(whoami).service

    ## Setting up wireless with network manager
    arch_package networkmanager network-manager-applet networkmanager-vpnc gnome-keyring
    enable_service NetworkManager.service

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
    # Automatically switch to Bluetooth or USB headset when connected
    # See https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting#Automatically_switch_to_Bluetooth_or_USB_headset
    # and https://wiki.archlinux.org/index.php/PulseAudio#Switch_on_connect.
    if ! grep 'load-module module-switch-on-connect' /etc/pulse/default.pa &> /dev/null; then
        sudo bash -c "echo 'load-module module-switch-on-connect' >> /etc/pulse/default.pa"
        pulseaudio -k
        pulseaudio --start
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
}

htpc_stuff() {
    install_vim vim

    ## Install kodi
    # https://wiki.archlinux.org/index.php/Kodi#Raspberry_Pi_.28all_generations.29
    # and http://blog.monkey.codes/how-to-setup-kodi-on-a-raspberry-pi/
    arch_package kodi-rbp libcec-rpi
    sudo sed -i 's/gpu_mem=64/gpu_mem=256/' /boot/config.txt

    enable_service_not_now kodi

    if [ ! -d ~/gitting/jpi.jflei.com ]; then
        mkdir -p ~/gitting/
        git clone https://github.com/jfly/jpi.jflei.com.git ~/gitting/jpi.jflei.com
    fi

    arch_package nginx nodejs npm

    # Install and configure transmission.
    arch_package transmission-cli
    enable_service transmission
    sudo systemctl stop transmission
    sudo sed -i 's_\( *"download-dir: \).*_\1"/home/media/torrents",_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-host-whitelist": \).*_\1"*",_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo sed -i 's_\( *"rpc-host-whitelist-enabled": \).*_\1false,_' /var/lib/transmission/.config/transmission-daemon/settings.json
    sudo systemctl start transmission

    mkdir -p ~/.config/systemd/user/
    cat > ~/.config/systemd/user/gatekeeper.service <<EOL
[Unit]
Description=gatekeeper and nginx

[Service]
ExecStart=$HOME/gitting/jpi.jflei.com/runall.sh
Type=oneshot
RemainAfterExit=yes

[Install]
WantedBy=default.target
EOL
    systemctl enable --now --user gatekeeper.service

    if [ ! -f ~/gitting/jpi.jflei.com/htpasswd ]; then
        echo "Enter the password you want to use for HTTP basic access from the outside word."
        echo -n "> "
        read -s password
        echo
        echo "kent:{PLAIN}$password" > ~/gitting/jpi.jflei.com/htpasswd
    fi
}

if [ "$(hostname)" = "breq" ] || [ "$(hostname)" = "dalinar" ]; then
    device_specific_command=laptop_stuff
elif [ "$(hostname)" = "kent" ]; then
    device_specific_command=htpc_stuff
else
    echo "Unrecognized hostname '$(hostname)'"
    exit 2
fi

base_stuff
$device_specific_command

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
