jfly/dotfiles
=============

I bought a new laptop, so it felt like it's finally time to do this.

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot) to
manage everything. Just git clone, and run the `./install` script!

# Directions for fresh Arch install

- `echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf` - disable system beep
- `pacman -S vim && mv /usr/bin/vi /usr/bin/vi.bak && ln -s /usr/bin/vim /usr/bin/vi` - install and set up vim as default

- `pacman -S sudo && visudo` - install and configure sudo
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy` - create user and set their password

- `pacman -S git python` - install dependencies to install jfly/dotfiles
- `git clone https://github.com/jfly/dotfiles.git && cd dotfiles && ./install` - you probably want to `rm ~/.bashrc` first, as dotbot will not clobber it for you.


## Setting up x11 and xmonad
- `pacman -S xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb xmonad xmonad-contrib xmobar feh roxterm dmenu2 wmname trayer network-manager-applet openssh alsa-utils scrot`
- Install the appropriate [video card driver](https://wiki.archlinux.org/index.php/xorg#Driver_installation)

## AUR utils
- `pacman -S wget base-devel`

## Install volnoti
- Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from github. [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
- Install [volnoti-brightness](https://aur.archlinux.org/packages/volnoti-brightness-git/) from the AUR
    - This has a missing dependency on [bs](https://www.archlinux.org/packages/extra/i686/bc/)


## Fonts
- `pacman -S ttf-liberation ttf-bitstream-vera`
- Install [ttf-google-fonts-git](https://aur.archlinux.org/packages/ttf-google-fonts-git/) from the AUR

## Setting up wireless with network manager
- `pacman -S networkmanager network-manager-applet gnome-keyring`
- `systemctl enable NetworkManager.service && systemctl start NetworkManager.service`

## Setup GNOME keyring and ssh keys
- `pacman -S gnome-keyring seahorse`
- [Generate ssh keys](https://help.github.com/articles/generating-ssh-keys/)
- Setup for Git
    - Add ssh key to github: https://github.com/settings/ssh
    - Configure Git to use GNOME-keyring: https://wiki.archlinux.org/index.php/GNOME_Keyring#GNOME_Keyring_and_Git

## wrk
- `pacman -S mosh networkmanager-vpnc`

## Misc
- `timedatectl set-ntp true` - enable time sync
- Install [google-chrome](https://aur.archlinux.org/packages/go/google-chrome/google-chrome.tar.gz) from the AUR.
    - For yoga's hidpi display, build [chromium-dev](https://aur.archlinux.org/packages/chromium-dev/) with `-Denable_hidpi=1` as per https://wiki.archlinux.org/index.php/HiDPI#Chromium_.2F_Google_Chrome.
        - If you do this, don't forget to install [chromium-pepper-flash](https://aur.archlinux.org/packages/chromium-pepper-flash/) from the AUR

## Lenovo specific
- Fix trackpoint middle button scroll by creating a `/etc/X11/xorg.conf.d/20-trackpoint.conf` as per https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_T410.
- Enable [synaptics touchpad](https://wiki.archlinux.org/index.php/Touchpad_Synaptics)
    - `pacman -S xf86-input-synaptics`
    - Create /etc/X11/xorg.conf.d/50-synaptics.conf file as described on the wiki
- Install [xf86-input-mtrack](https://aur.archlinux.org/packages/xf/xf86-input-mtrack/xf86-input-mtrack.tar.gz) from the AUR.
