#!/bin/bash

# Copied from https://aur.archlinux.org/packages/volnoti-brightness-git/
BRIGHTNESS=$(</sys/class/backlight/intel_backlight/brightness)
MAX_BRIGHTNESS=$(</sys/class/backlight/intel_backlight/max_brightness)
PERCENT=$(echo "$BRIGHTNESS*100/$MAX_BRIGHTNESS" | bc)
volnoti-show -s /usr/share/pixmaps/volnoti-brightness/display-brightness-symbolic.svg $PERCENT
