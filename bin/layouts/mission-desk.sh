#!/bin/sh
xrandr --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off --output DP1 --off --output eDP1 --off --output DP2 --off

# Switch to HDMI audio.
pactl set-card-profile 0 output:hdmi-stereo+input:analog-stereo

fixtray
