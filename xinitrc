# Hack to fix Java swing applications from http://tools.suckless.org/wmname
wmname LG3D

feh --randomize --bg-fill $HOME/Screenshots/wallpaper/*
conky | while read -r; do xsetroot -name "$REPLY"; done &
xmodmap $HOME/.Xmodmap
exec dwm
