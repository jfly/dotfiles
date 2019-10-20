#!/usr/bin/env bash

bluetoothctl connect 28:11:A5:36:83:33
pacmd set-card-profile bluez_card.28_11_A5_36_83_33 headset_head_unit
