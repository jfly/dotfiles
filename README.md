# jfly/dotfiles

I bought a new laptop, so it felt like it's finally time to do this.

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot) to
manage everything. Just git clone, and run the `./install` script!

## Directions for fresh Arch install
- Pre-install
  - Disable hibernation in Windows, resize Windows partition
  - Set Windows to use UTC hardware clock time ([instructions from here](https://wiki.archlinux.org/index.php/time#UTC_in_Windows)): `reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_QWORD /f`
  - Disable Secure Boot in BIOS
  - Enable virtualization in BIOS (otherwise you will see a message "kvm:disabled by bios")
- I chose to use the [rEFInd bootloader](https://wiki.archlinux.org/index.php/REFInd). You might want to restyle rEFInd by adding a theme such as [rEFInd-minimal](https://github.com/EvanPurkhiser/rEFInd-minimal).

- `pacman -S vim && mv /usr/bin/vi /usr/bin/vi.bak && ln -s /usr/bin/vim /usr/bin/vi` - install and set up vim as default
- `pacman -S sudo && visudo` - install and configure sudo
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy` - create user and set their password
- `pacman -S git python openssh` - install dependencies to install jfly/dotfiles
- `pacman -S wget base-devel` - needed to install stuff from the AUR
- `git clone https://github.com/jfly/dotfiles.git && cd dotfiles && ./install`

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
  - TODO: Look into [networkmanager-dmenu](https://github.com/firecat53/networkmanager-dmenu)?
- `systemctl enable NetworkManager.service && systemctl start NetworkManager.service`

## Bluetooth
See <https://wiki.archlinux.org/index.php/Bluetooth_keyboard>.

- `pacman -S bluez bluez-utils gnome-bluetooth blueman`
- `systemctl enable bluetooth.service && systemctl start bluetooth.service`

## Audio
- `pacman -S pulseaudio pamixer pavucontrol paprefs bc sox`
- Install [pasystray](https://aur.archlinux.org/cgit/aur.git/snapshot/pasystray.tar.gz)
- Install [hcchu/volnoti](https://github.com/hcchu/volnoti#new-options-in-this-fork) from github. [volnoti](https://aur.archlinux.org/packages/volnoti) doesn't have the features needed for volnoti-brightness.
- NOTE: Confusingly enough, I had to change the Profile setting under "Configuration" to get HDMI output working.

## Power stuff
- Install from AUR: `https://aur.archlinux.org/packages/laptop-mode-tools/`
- `pacman -S acpi acpid ethtool wireless_tools`
- `systemctl enable laptop-mode`
- Edit `/etc/laptop-mode/laptop-mode.conf` accordingly (https://push.cx/2015/dual-booting-arch-linux-on-lenovo-x1-carbon-3rd-gen suggests changing LM_BATT_MAX_LOST_WORK_SECONDS)

## Misc
- `pacman -S mosh`
- `pacman -S ctags && sudo npm install -g git://github.com/ramitos/jsctags.git` - for vim tagbar plugin
- `pacman -S the_silver_searcher` - for faster ctrl+p in vim
- [byzanz](https://aur.archlinux.org/packages/byzanz/)
  - [xrectsel](https://aur.archlinux.org/packages/xrectsel/)

## Dropbox
- https://aur.archlinux.org/packages/dropbox/
- `ln -s Dropbox/pics/lolcommits .lolcommits` - set up lolcommits
- ssh keys (or if you want to, you can [generate new ssh keys](https://help.github.com/articles/generating-ssh-keys/))
  - `rm -r .ssh && ln -s Dropbox/kaladin-ssh/ .ssh`
  - `chmod 600 ~/.ssh/id_rsa ~/.ssh/*.pem`

## TODO
- xmobar is periodically crashing. See one potential workaround here: https://github.com/jaor/xmobar/issues/310#issuecomment-335212681.
- When switching monitors, change DPI and update running applications
  - http://unix.stackexchange.com/questions/12613/is-there-a-way-to-find-all-x-resources-an-application-uses
  - backup plan: chrome://settings/search#zoom

## breq specific

- I ran into an issue where the trackpoint was very jumpy (unusably so). Rebooting without my (third party) charging cable seems to make the problem go away. See https://forums.lenovo.com/t5/ThinkPad-X-Series-Laptops/X220-Touchpad-jumpy-response-when-used-with-90w-slim-AC-adapter/ta-p/766543 for something possibly related.
  - Update: after more reboots, the problem seems to appear sometimes even without a charging cabled plugged in =(

## Skipped on breq
- Add `"detachKeys": "ctrl-^,q"` to `~/.docker/config.json`
- Prevent autosuspend of usb mouse: https://fitzcarraldoblog.wordpress.com/2013/02/26/how-to-prevent-a-usb-mouse-auto-suspending-in-linux-when-a-laptops-power-supply-is-disconnected/
- Headphone noise is due to power_save mode - https://bbs.archlinux.org/viewtopic.php?pid=1554497#p1554497

### kaladin specific

- `echo "options psmouse proto=imps" > /etc/modprobe.d/psmouse.conf` - http://natalian.org/archives/2015/02/18/Archlinux_on_a_Lenovo_X1C3/
  - Note: not needed with newer kernels.
- https://bbs.archlinux.org/viewtopic.php?pid=1492564#p1492564
~/thirdrepos/downgrader @kaladin> cat /proc/sys/net/core/wmem_max
212992
~/thirdrepos/downgrader @kaladin> sudo bash -c "echo 83886080 > /proc/sys/net/core/wmem_max"
~/thirdrepos/downgrader @kaladin> # http://www.linuxquestions.org/questions/linux-networking-3/sendmsg-no-buffer-space-available-334631/
~/thirdrepos/downgrader @kaladin> cat /proc/sys/net/core/wmem_max83886080
~/thirdrepos/downgrader @kaladin>
