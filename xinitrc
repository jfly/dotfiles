fixkeys
fixmouse
fixmonitors

# https://wiki.archlinux.org/index.php/HiDPI
xrdb -merge ~/.Xresources

# Hack to fix Java swing applications from http://tools.suckless.org/wmname
#  https://wiki.archlinux.org/index.php/xmonad#Problems_with_Java_applications
wmname LG3D

# Disable tapping and scrolling when typing
syndaemon -t -k -i 0.2 &

# Disable touchpad
xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0

# volnoti is a volume notification daemon
volnoti -t 1

# Enable numlock on boot (thanks James!)
numlockx on

# Gross hack to disable audio powersave, which causes awful noise
# (https://bbs.archlinux.org/viewtopic.php?pid=1554497#p1554497)
sudo rmmod -f snd_hda_intel && sudo modprobe snd_hda_intel power_save=0

# start dropbox if it's installed (we do it here rather than via systemd
# because the icon won't show up otherwise)
if [ -x /usr/bin/dropboxd ]; then
    /usr/bin/dropboxd &
fi

if [ -x /usr/bin/pasystray ]; then
    pasystray &
fi

if [ -x /usr/bin/nm-applet ]; then
    nm-applet &
fi

dpi=`xrdb -query Xft.dpi | grep "Xft.dpi" | grep -oE "[0-9]+$"`

if [ "$dpi" == "144" ]; then
    TRAYER_HEIGHT=25
else
    TRAYER_HEIGHT=18
fi
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height $TRAYER_HEIGHT --transparent true --alpha 0 --tint 0 &

sudo systemctl start fixkeys.path
sudo systemctl start fixmouse.path

# xmonad doesn't set a cursor
xsetroot -cursor_name left_ptr
exec xmonad
