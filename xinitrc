autoperipherals

# https://wiki.archlinux.org/index.php/HiDPI
xrdb -merge ~/.Xresources

# volnoti is a volume notification daemon
volnoti -t 1

# Enable numlock on boot (thanks James!)
numlockx on

# Gross hack to disable audio powersave, which causes awful noise
# (https://bbs.archlinux.org/viewtopic.php?pid=1554497#p1554497).
sudo rmmod -f snd_hda_intel && sudo modprobe snd_hda_intel power_save=0

# Start dropbox if it's installed (we do it here rather than via systemd
# because the icon won't show up otherwise).
if [ -x /usr/bin/dropboxd ]; then
    /usr/bin/dropboxd &
fi

if [ -x /usr/bin/pasystray ]; then
    pasystray &
fi

if [ -x /usr/bin/nm-applet ]; then
    nm-applet &
fi

if [ -x /usr/bin/blueman-applet ]; then
    blueman-applet &
fi

if [ -x /usr/bin/dunst ]; then
    dunst &
fi

if [ -x /usr/bin/xsettingsd ]; then
    touch ~/.xsettingsd
    /usr/bin/xsettingsd &
fi

if [ -x /usr/local/bin/flameshot ]; then
    # See https://wiki.archlinux.org/index.php/Flameshot#Option_2:_Run_as_background_process
    /usr/local/bin/flameshot &
fi

# From https://wiki.archlinux.org/index.php/GNOME/Keyring#xinitrc_method.
if [ -x /usr/bin/gnome-keyring-daemon ]; then
    dbus-update-activation-environment --systemd DISPLAY
    eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
    export SSH_AUTH_SOCK
fi

# xmonad doesn't set a cursor.
xsetroot -cursor_name left_ptr
exec xmonad
