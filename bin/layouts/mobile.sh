#!/bin/sh
xrandr --output HDMI1 --off --output VIRTUAL1 --off --output DP1 --off --output eDP1 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output DP2 --off
fixtray

# Switch to laptop speakers.
pactl set-card-profile 1 output:analog-stereo+input:analog-stereo

# Disable bluetooth
rfkill block bluetooth
