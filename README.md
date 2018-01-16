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
- `sudo pacman -S git && mkdir ~/gitting && cd ~/gitting && git clone https://github.com/jfly/dotfiles.git && cd dotfiles` - Checkout and cd into this repo!
- `./bootstrap-arch.sh` - Bootstrap Arch Linux installation, installing all dependencies. Make sure this command succeeds! This will also symlink dotfiles by running `./install`.

## Printer
- When actually adding printer, use ppd file from <http://www.openprinting.org/printer/Brother/Brother-HL-2240>.

## Bluetooth
See <https://wiki.archlinux.org/index.php/Bluetooth_keyboard>.
Note: Dualbooting with bluetooth is a *pain*. See: https://unix.stackexchange.com/a/255510 for more details and crazy workaround.

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
