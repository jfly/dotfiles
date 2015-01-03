jfly/dotfiles
=============

I bought a new laptop, so it felt like it's finally time to do this.

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot) to
manage everything. Just git checkout, and run the `./install` script!

# Directions for fresh Arch install

- `echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf` - disable system beep
- `pacman -S vim && mv /usr/bin/vi /usr/bin/vi.bak && ln -s /usr/bin/vim /usr/bin/vi` - install and set up vim as default

- `pacman -S sudo && visudo` - install and configure sudo
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy` - create user and set their password

- `pacman -S git python` - install dependencies to install jfly/dotfiles
- `git checkout https://github.com/jfly/dotfiles.git && cd dotfiles && ./install` - you probably want to `rm ~/.bashrc` first, as dotbot will not clobber it for you.


## Setting up x11 and xmonad
- `pacman -S xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xmonad xmonad-contrib feh roxterm dmenu wmname trayer network-manager-applet openssh alsa-utils scrot`

## AUR utils
- `pacman -S wget base-devel`

## Install volnoti
- Install [volnoti](https://aur.archlinux.org/packages/volnoti) from the AUR
    - Has a missing dependency on librsvg.

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

## Misc
- `timedatectl set-ntp true` - enable time sync
- Install [google-chrome](https://aur.archlinux.org/packages/go/google-chrome/google-chrome.tar.gz) from the AUR.

## Lenovo specific
- Fix trackpoint middle button scroll by creating a `/etc/X11/xorg.conf.d/20-trackpoint.conf` as per https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_T410.
- Enable [synaptics touchpad](https://wiki.archlinux.org/index.php/Touchpad_Synaptics)
    - `pacman -S xf86-input-synaptics`
    - Create /etc/X11/xorg.conf.d/50-synaptics.conf file as described on the wiki
