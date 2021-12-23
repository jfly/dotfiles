#!/usr/bin/env bash

set -e

# Built from https://wiki.archlinux.org/index.php/SANE#Verification

# Got this device name from `scanimage -L`
DEVICE="airscan:e0:Canon LiDE 300 (USB)"

SCANS_DIR=$HOME/scans
mkdir -p "$SCANS_DIR"
FILENAME="$SCANS_DIR/$(date +"%Y-%m-%d_%H-%M-%S")_$HOSTNAME.png"

scanimage --device "$DEVICE" --format=png --output-file "$FILENAME" --resolution 300 --progress
echo "Saved scan to $FILENAME"
