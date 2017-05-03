#!/usr/bin/sh

echo -n 200 > /sys/devices/platform/i8042/serio1/serio2/sensitivity
echo -n 250 > /sys/devices/platform/i8042/serio1/serio2/speed
