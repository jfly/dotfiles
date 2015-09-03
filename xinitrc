# Start gnome-keyring-daemon
#  https://wiki.archlinux.org/index.php/GNOME_Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Hack to fix Java swing applications from http://tools.suckless.org/wmname
#  https://wiki.archlinux.org/index.php/xmonad#Problems_with_Java_applications
wmname LG3D

feh --randomize --bg-fill $HOME/Dropbox/Camera\ Uploads/wallpaper/*

# Set up an icon tray
if [ "`hostname`" == "kaladin" ]; then
    trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 27 --transparent true --alpha 0 --tint 0 &
else
    trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 18 --transparent true --alpha 0 --tint 0 &
fi

# Disable tapping and scrolling when typing
syndaemon -t -k -i 0.2 &

# volnoti is a volume notification daemon
volnoti -t 1

# Enable numlock on boot (thanks james!)
numlockx on

# start dropbox if it's installed (we do it here rather than via systemd
# because the icon won't show up otherwise)
if [ -x /usr/bin/dropboxd ]; then
    /usr/bin/dropboxd &
fi

if [ -x /usr/bin/nm-applet ] ; then
    nm-applet &
fi

# https://wiki.archlinux.org/index.php/HiDPI
xrdb -merge ~/.Xresources

# https://wiki.archlinux.org/index.php/Mouse_acceleration
xset m 2.5 0

xmodmap $HOME/.Xmodmap

# https://wiki.archlinux.org/index.php/TrackPoint#Middle_button_scroll
#<<<xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation" 1
#<<<xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation Button" 2
#<<<xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation Timeout" 200
#<<<xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation Axes" 6 7 4 5
#<<<xinput set-prop "PS/2 Synaptics TouchPad" "Device Accel Constant Deceleration" 0.75

# xmonad doesn't set a cursor
xsetroot -cursor_name left_ptr
exec xmonad
