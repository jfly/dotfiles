#!/bin/sh
xrandr --output HDMI1 --off --output VIRTUAL1 --off --output DP1 --off --output eDP1 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output DP2 --off
fixtray

# Switch to laptop speakers.
# TODO - this sometimes fails with "No such entity"
# Look into why these indices are changing, or use `pactl list cards` to find the card?
pactl set-card-profile 1 output:analog-stereo+input:analog-stereo

# Disable bluetooth
rfkill unblock bluetooth
