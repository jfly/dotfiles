#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

HOSTNAME=$(cat /etc/hostname)

if [ "$EUID" -eq 0 ]; then
    echo "Do not rush this script as root, instead run it as the non-root user you want to set up."
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
    arch_package wget base-devel gcc make fakeroot

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
    enable_service org.cups.cupsd.service

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
}

install_vim() {
    ## Vim
    arch_package "$1" editorconfig-core-c ctags
    vim +PlugInstall +qall
    # Symlink vi to vim if it has not been symlinked already.
    if [ "$(readlink /usr/bin/vi)" != "/usr/bin/vim" ]; then
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

    ## Bluetooth stuff
    arch_package gnome-bluetooth blueman

    ## Setting up X11 and xmonad
    # Install the appropriate video card driver: https://wiki.archlinux.org/index.php/xorg#Driver_installation
    # driconf is supposed to help with video tearing (see http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)
    arch_package xf86-video-intel driconf
    # Install everything else needed for the desktop environment.
    arch_package xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb xorg-xinput xorg-xprop
    arch_package xmonad xmonad-contrib xmobar
    arch_package feh network-manager-applet alsa-utils maim xclip numlockx xsel xdotool slop byzanz dunst
    arch_package termite chromium
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
    arch_package pulseaudio pamixer pavucontrol pasystray paprefs bc sox
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

    ## Docker stuff
    arch_package docker
    aur_package docker-credential-secretservice
}

pi_htpc_stuff() {
    ## Install kodi
    # https://wiki.archlinux.org/index.php/Kodi#Raspberry_Pi_.28all_generations.29
    # and http://blog.monkey.codes/how-to-setup-kodi-on-a-raspberry-pi/
    arch_package kodi-rbp libcec-rpi
    sudo sed -i 's/gpu_mem=64/gpu_mem=320/' /boot/config.txt

    enable_service_not_now kodi

    htpc_stuff
}

nuc_htpc_stuff() {
    ## Install kodi
    # https://wiki.archlinux.org/index.php/Kodi#kodi-standalone_service
    arch_package kodi
    aur_package kodi-standalone-service
    enable_service kodi

    # TODO: install + configure resilio sync
    # TODO: investigate seafile as a truely free alternative
    #       https://www.reddit.com/r/torrents/comments/5xpfy9/is_there_any_free_alternative_to_resilio_sync/demq52s/

    htpc_stuff
}

htpc_stuff() {
    install_vim vim

    # Configure kodi
    sudo cp kodi/userdata/guisettings.xml /var/lib/kodi/.kodi/userdata/guisettings.xml
    sudo cp kodi/userdata/sources.xml /var/lib/kodi/.kodi/userdata/sources.xml
    sudo cp -r kodi/userdata/keymaps /var/lib/kodi/.kodi/userdata/keymaps
    sudo chown -R kodi:kodi /var/lib/kodi/.kodi/userdata/

    if [ ! -d ~/gitting/jpi.jflei.com ]; then
        mkdir -p ~/gitting/
        git clone https://github.com/jfly/jpi.jflei.com.git ~/gitting/jpi.jflei.com
    fi

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
    sudo systemctl start transmission

    ## Docker stuff
    arch_package docker docker-compose
    enable_service docker

    # Configure and start HTPC webui docker container.
    (
        set -e
        cd ~/.dotfiles/containers/htpc-ui/
        if [ ! -f etc/nginx/basic_auth_file ]; then
            echo "Enter the password you want to use for HTTP basic access from the outside word."
            echo -n "> "
            read -r -s password
            echo
            echo "$HOSTNAME:{PLAIN}$password" > etc/nginx/basic_auth_file
        fi
        docker-compose up --detach
    )

    # Start the roomba docker container
    (
        cd ~/.dotfiles/containers/roomba
        docker-compose up --detach
    )

    # Install and configure ddclient for dynamic dns
    arch_package ddclient
    if [ ! -f /etc/ddclient/ddclient.conf.bak ]; then
        sudo cp /etc/ddclient/ddclient.conf /etc/ddclient/ddclient.conf.before

        echo "Enter the domain name you want to update via Google Domains"
        echo -n "> "
        read -r domain_name
        echo

        # shellcheck disable=SC2001
        main_domain=$(echo "$domain_name" | sed 's/.*\.\(.*\..*\)/\1/')
        dns_url="https://domains.google.com/m/registrar/${main_domain}/dns"
        echo "Enter the generated username for $domain_name (on $dns_url)"
        echo -n "> "
        read -r google_domains_username
        echo

        echo "Enter the generated password for $domain_name (on $dns_url)"
        echo -n "> "
        read -r google_domains_password
        echo

        sudo bash -c "cat > /etc/ddclient/ddclient.conf" <<EOL
daemon=600                # check every 600 seconds
syslog=yes                # log update msgs to syslog
mail=root                 # mail all msgs to root
mail-failure=root         # mail failed update msgs to root
pid=/var/run/ddclient.pid # record PID in file.
ssl=yes                   # use ssl-support.  Works with
                          # ssl-library

## To obtain an IP address from Web status page (using the proxy if defined)
## by default, checkip.dyndns.org is used if you use the dyndns protocol.
## Using use=web is enough to get it working.
## WARNING: set deamon at least to 600 seconds if you use checkip or you could
## get banned from their service.
use=web, web=checkip.dyndns.org/, web-skip='IP Address' # found after IP Address

###
### Google Domains
###
ssl=yes
protocol=googledomains
login=$google_domains_username
password=$google_domains_password
$domain_name
EOL
        sudo mv /etc/ddclient/ddclient.conf.before /etc/ddclient/ddclient.conf.bak
    fi
    enable_service ddclient.service
}

if [ "$HOSTNAME" = "dalinar" ]; then
    device_specific_command=laptop_stuff
elif [ "$HOSTNAME" = "kent" ]; then
    device_specific_command=pi_htpc_stuff
elif [ "$HOSTNAME" = "clark" ]; then
    device_specific_command=nuc_htpc_stuff
else
    echo "Unrecognized hostname: '$HOSTNAME'"
    exit 2
fi

base_stuff
$device_specific_command

echo ""
echo "Successfully bootstrapped your new Arch system. Happy Linuxing!"
