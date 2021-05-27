# jfly/dotfiles

I'm using the excellent [dotbot](https://github.com/anishathalye/dotbot).

See instructions on [this Google Doc](https://docs.google.com/document/d/1Ji1dfnQxlb9KJGmVin4W6oAqN4-SWokSlXGYumss74M/edit#heading=h.1gvhtuttse8f).

## Pre-install (if dual booting with Windows)
  - Windows
    - Disable hibernation in Windows (`powercfg /hibernate on`)
    - Resize Windows partition with Disk Management. (Sometimes this gets tricky with immovable files over in Windows. Forcing a defrag can help, but there are some system files that defrag won't move, and you'll need to move them yourself).
    - Set Windows to use UTC hardware clock time ([instructions from here](https://wiki.archlinux.org/index.php/time#UTC_in_Windows)): `reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_QWORD /f`
  - UEFI/Secure Boot
    - In order to boot from the Arch USB, you will need to disable Secure Boot in the BIOS security tab.
    - Then, you can install a bootloader and set up Secue Boot. I have had good luck with rEFInd + shim.
        - [rEFInd bootloader](https://wiki.archlinux.org/index.php/REFInd). You might want to restyle rEFInd by adding a theme such [rEFInd-minimal-black-flat](https://github.com/dnaf/rEFInd-minimal-black-flat). I also changed `timeout` in `/boot/EFI/refind/refind.conf` to 5 seconds, and added `default_selection Linux`.
        - Install [shim-signed](https://aur.archlinux.org/packages/shim-signed/) and `sbsigntools`
        - `refind-install --shim /usr/share/shim-signed/shimx64.efi --localkeys`
        - `# sbsign --key /etc/refind.d/keys/refind_local.key --cert /etc/refind.d/keys/refind_local.crt --output /boot/vmlinuz-linux /boot/vmlinuz-linux`
            warning: file-aligned section .text extends beyond end of file
            warning: checksum areas are greater than image size. Invalid section table?
            Signing Unsigned original image
        - If you want to add a boot entry for MokManager: `efibootmgr --disk /dev/nvme0n1 --part 1 --create --label "MokManager" --loader /EFI/refind/mmx64.efi`. Then re-run `refind-install` as above.
        - Once in MokManager add refind_local.cer to MoKList. refind_local.cer can be found inside a directory called keys in the rEFInd's installation directory, e.g. esp/EFI/refind/keys/refind_local.cer.

    - Enable virtualization in BIOS (otherwise you will see a message "kvm:disabled by bios")

## Directions for fresh Arch install

Follow the wiki! https://wiki.archlinux.org/index.php/installation_guide

- Set hostname: https://wiki.archlinux.org/index.php/installation_guide#Network_configuration
- `passwd` - Set the root password
- `pacman -S sudo && visudo` - Install and configure sudo.
- `useradd -m -G wheel -s /bin/bash jeremy && passwd jeremy && su jeremy` - Create user and set their password.
- `sudo pacman -S git && mkdir ~/gitting && cd ~/gitting && git clone https://github.com/jfly/dotfiles.git && cd dotfiles` - Checkout and cd into this repo!
- `./bootstrap-arch.sh` - Bootstrap Arch Linux installation, installing all dependencies. Make sure this command succeeds!
- If you want fancy fonts, you might want to look into https://aur.archlinux.org/packages/ttf-ms-win10/, which is a bit of an adventure to install.

## Swap
See https://wiki.archlinux.org/index.php/Swap#Swap_file_creation.

## Printer
When actually adding printer, use ppd file from <http://www.openprinting.org/printer/Brother/Brother-HL-2240>.

## Bluetooth
See <https://wiki.archlinux.org/index.php/Bluetooth_keyboard>.
Note: Dualbooting with bluetooth is a *pain*. See: https://unix.stackexchange.com/a/255510 for more details and a crazy workaround. `inspect-windows-registry.sh` is a useful script to help with this.

Note: Dualbooting with BLE is even more of a pain. See http://console.systems/2014/09/how-to-pair-low-energy-le-bluetooth.html and https://cat.devbra.in/pair-a-bluetooth-le-device-with-a-dualboot-of-linux-windows-10-2/ for some information. I haven't actually managed to get this working yet. I see the following from `journalctl -f`:

    Apr 05 09:33:59 dalinar bluetoothd[25157]: No cache for EF:F6:DE:0B:14:00
    Apr 05 09:33:59 dalinar bluetoothd[25157]: BATT attribute not found
    Apr 05 09:33:59 dalinar bluetoothd[25157]: batt-profile profile accept failed for EF:F6:DE:0B:14:00
    Apr 05 09:33:59 dalinar bluetoothd[25157]: GAP attribute not found
    Apr 05 09:33:59 dalinar bluetoothd[25157]: gap-profile profile accept failed for EF:F6:DE:0B:14:00
    Apr 05 09:33:59 dalinar bluetoothd[25157]: input-hog profile accept failed for EF:F6:DE:0B:14:00

    cd /var/lib/bluetooth/F8\:94\:C2\:2F\:A9\:7B/
    vi 28\:11\:A5\:36\:83\:33/info
    # Replace [LinkKey] > Key with Windows value (`sudo inspect-windows-registry.sh`, `cd ControlSet001\Service\BTHPORT\Parameters\Keys`, `cd f894c22fa97b`, `hex 2811a5368333`)
    sudo systemctl restart bluetooth

## Dropbox
- `dropbox` - Need to manually start dropbox and log in.

## TODO
- Look into https://gitlab.com/tozd/docker/nginx-proxy
- Have to install `coc-python`, `coc-json`, `coc-tsserver`: https://github.com/neoclide/coc-python#install
- For HTPC, make sure DHCPCD keeps retrying, rather than giving up after 30 seconds: https://wiki.archlinux.org/index.php/dhcpcd#Timeout_delay.
- Add `"detachKeys": "ctrl-^,q"` to `~/.docker/config.json`
- Prevent autosuspend of usb mouse: https://fitzcarraldoblog.wordpress.com/2013/02/26/how-to-prevent-a-usb-mouse-auto-suspending-in-linux-when-a-laptops-power-supply-is-disconnected/
- Headphone noise is due to power_save mode - https://bbs.archlinux.org/viewtopic.php?pid=1554497#p1554497
- Bluetooth headset works great as speakers (audio profile A2DP sink), but don't work as a microphone (HSP/HFP profile)?
    - https://bbs.archlinux.org/viewtopic.php?id=209979
    - https://bugs.launchpad.net/ubuntu/+source/pulseaudio/+bug/508522
    - https://bugs.launchpad.net/ubuntu/+source/pulseaudio/+bug/1711087
