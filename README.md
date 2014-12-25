jfly/dotfiles
=============

I bought a new laptop, so it felt like it's finally time to do this.

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot) to
manage everything. Just git checkout, and run the `./install` script!

## Directions for fresh arch install

- `pacman -S sudo && visudo` - install and configure sudo
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy` - create user and set their password

- `pacman -S git python` - install dependencies to install jfly/dotfiles
- `git checkout https://github.com/jfly/dotfiles.git && cd dotfiles && ./install` - you probably want to `rm ~/.bashrc` first, as dotbot will not clobber it for you.

- `pacman -S vim && mv /usr/bin/vi /usr/bin/vi.bak && && ln -s /usr/bin/vim /usr/bin/vi` - install and set up vim as default


# Setting up x11 and xmonad
- `pacman -S xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xmonad xmonad-contrib feh roxterm dmenu wmname trayer network-manager-applet openssh`

# TODO - <<<volnoti & ubuntu font >>>

# Setting up wireless with network manager
- `pacman -S networkmanager network-manager-applet gnome-keyring`
- `systemctl enable NetworkManager.service && systemctl start NetworkManager.service`
