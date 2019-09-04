#!/usr/bin/env bash

echo "WIP WIP WIP this does not work yet"
echo "This may be helpful: https://blog.thewalr.us/2017/09/26/raspberry-pi-zero-w-simultaneous-ap-and-managed-mode-wifi/"
exit 1

iw dev wlp4s0 interface add wlan0_sta type managed addr d6:56:a2:97:30:68
iw dev wlp4s0 interface add wlan0_ap type managed addr d6:56:a2:97:30:69
