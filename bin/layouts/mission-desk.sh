#!/bin/sh

# NOTE: This is basically a copy of honor-desk

xrandr --output HDMI1 --auto --pos 0x0 --rotate normal --output VIRTUAL1 --off --output DP1 --off --output eDP1 --off --output DP2 --off
fixtray

# Switch to HDMI audio.
pactl set-card-profile alsa_card.pci-0000_00_1f.3 output:hdmi-stereo+input:analog-stereo

# Enable bluetooth
rfkill unblock bluetooth
