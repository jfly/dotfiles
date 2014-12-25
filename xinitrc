# Hack to fix Java swing applications from http://tools.suckless.org/wmname
# https://wiki.archlinux.org/index.php/xmonad#Problems_with_Java_applications
wmname LG3D

feh --randomize --bg-fill $HOME/Screenshots/wallpaper/*
xmodmap $HOME/.Xmodmap

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 18 --transparent true --alpha 0 --tint 0 &

# volnoti is a volume notification daemon
volnoti -t 1

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet &
fi

# xmonad doesn't set a cursor
xsetroot -cursor_name left_ptr
exec xmonad
