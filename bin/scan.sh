#!/usr/bin/env bash

# Built from https://wiki.archlinux.org/index.php/SANE#Verification

# Got this device name from `scanimage -L`
DEVICE=pixma:MG5400_192.168.1.7
DEVICE=pixma:04A91764_00BA32

SCANS_DIR=$HOME/scans
mkdir -p "$SCANS_DIR"
FILENAME="$SCANS_DIR/$(date +"%Y-%m-%d_%H-%M-%S")_$HOSTNAME.png"

scanimage --device "$DEVICE" --format=png --output-file "$FILENAME" --resolution 300 --progress
echo "Saved scan to $FILENAME"
