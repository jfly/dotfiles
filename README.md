# jfly/dotfiles

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot).

See instructions on [this Google Doc](https://docs.google.com/document/d/1Ji1dfnQxlb9KJGmVin4W6oAqN4-SWokSlXGYumss74M/edit#heading=h.1gvhtuttse8f).

## Pre-install (if dual booting with Windows)
  - Windows
    - Disable hibernation in Windows (`powercfg /hibernate on`)
    - Resize Windows partition with Disk Management. (Sometimes this gets tricky with immovable files over in Windows. Forcing a defrag can help, but there are some system files that defrag won't move, and you'll need to move them yourself).
    - Set Windows to use UTC hardware clock time ([instructions from here](https://wiki.archlinux.org/index.php/time#UTC_in_Windows)): `reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_QWORD /f`
  - UEFI/BIOS
    - Disable Secure Boot and "os optimized" in restart tab: https://blog.fpmurphy.com/2012/09/lenovo-t430-t530-now-support-uefi-secure-boot.html
    - Enable virtualization in BIOS (otherwise you will see a message "kvm:disabled by bios")

## Directions for fresh Arch install

Follow the wiki! https://wiki.archlinux.org/index.php/installation_guide

I chose to use the [rEFInd bootloader](https://wiki.archlinux.org/index.php/REFInd). You might want to restyle rEFInd by adding a theme such [rEFInd-minimal-black-flat](https://github.com/dnaf/rEFInd-minimal-black-flat).

- `pacman -S sudo && visudo` - Install and configure sudo.
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy && su jeremy` - Create user and set their password.
- `pacman -S git && mkdir ~/gitting && cd ~/gitting && git clone https://github.com/jfly/dotfiles.git && cd dotfiles` - Checkout and cd into this repo!

- `pacman -S git python python-pip python-pexpect openssh` - install dependencies to install jfly/dotfiles
    - `pip install setproctitle`
- `pacman -S wget base-devel` - needed to install stuff from the AUR
- `git clone https://github.com/jfly/dotfiles.git && cd dotfiles && ./install`
- `pacman -S gvim editorconfig-core-c && mv /usr/bin/vi /usr/bin/vi.bak && ln -s /usr/bin/vim /usr/bin/vi && vim +PlugInstall +qall` - install and set up vim as default
- `pacman -S mosh the_silver_searcher fzf` - misc stuff
- `systemctl enable fixinputs.path` - run xmodmap everytime a keyboard appears

## Setting up x11 and xmonad
- Install the appropriate [video card driver](https://wiki.archlinux.org/index.php/xorg#Driver_installation)
  - `pacman -S driconf` - Fix for video tearing (see [here](http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)).
- `pacman -S xorg-server xorg-xinit xorg-xsetroot xorg-xmodmap xorg-xmessage xorg-xrandr xorg-xrdb xmonad xmonad-contrib xmobar feh wmname network-manager-applet openssh alsa-utils maim xclip numlockx xvkbd xsel xdotool slop byzanz`
- [trayer-srg](https://aur.archlinux.org/packages/trayer-srg-git/)
- [dmenu2](https://aur.archlinux.org/packages/dmenu2/)
- `pacman -S roxterm termite chromium`
- Fonts
  - `pacman -S ttf-liberation ttf-bitstream-vera noto-fonts noto-fonts-emoji`
  - Install [ttf-google-fonts-git](https://aur.archlinux.org/packages/ttf-google-fonts-git/) from the AUR

## Setting up wireless with network manager
- `pacman -S networkmanager network-manager-applet networkmanager-vpnc gnome-keyring`
- `systemctl enable --now NetworkManager.service`

## Printer
- `sudo pacman -S cups`
- `sudo systemctl enable --now org.cups.cupsd.service`
- When adding printer, use ppd file from <http://www.openprinting.org/printer/Brother/Brother-HL-2240>.

## Bluetooth
See <https://wiki.archlinux.org/index.php/Bluetooth_keyboard>.

- `pacman -S bluez bluez-utils gnome-bluetooth blueman`
- `systemctl enable --now bluetooth.service`
- Note: Dualbooting with bluetooth is a *pain*. See: https://unix.stackexchange.com/a/255510 for more details and an awesome workaround.

## Audio
- `pacman -S pulseaudio pamixer pavucontrol paprefs bc sox`
- Install [pasystray](https://aur.archlinux.org/cgit/aur.git/snapshot/pasystray.tar.gz)
- Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from github. [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
- NOTE: Confusingly enough, I had to change the Profile setting under "Configuration" to get HDMI output working.

## Power stuff
- Install from AUR: `https://aur.archlinux.org/packages/laptop-mode-tools/`
- `pacman -S acpi acpid ethtool wireless_tools`
- `systemctl enable --now laptop-mode`
- Edit `/etc/laptop-mode/laptop-mode.conf` accordingly (https://push.cx/2015/dual-booting-arch-linux-on-lenovo-x1-carbon-3rd-gen suggests changing LM_BATT_MAX_LOST_WORK_SECONDS)

## Dropbox
- https://aur.archlinux.org/packages/dropbox/
- `ln -s Dropbox/pics/lolcommits .lolcommits` - set up lolcommits
- ssh keys (or if you want to, you can [generate new ssh keys](https://help.github.com/articles/generating-ssh-keys/))
  - `rm -r .ssh && ln -s Dropbox/kaladin-ssh/ .ssh`
  - `chmod 600 ~/.ssh/id_rsa ~/.ssh/*.pem`

## TODO
- Add `"detachKeys": "ctrl-^,q"` to `~/.docker/config.json`
- Prevent autosuspend of usb mouse: https://fitzcarraldoblog.wordpress.com/2013/02/26/how-to-prevent-a-usb-mouse-auto-suspending-in-linux-when-a-laptops-power-supply-is-disconnected/
- Headphone noise is due to power_save mode - https://bbs.archlinux.org/viewtopic.php?pid=1554497#p1554497
