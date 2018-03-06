#!/usr/bin/env bash

OCCUPIED=$(curl -s http://g2g.honordev.com/api | jq ".occupied")

if [ "$OCCUPIED" = "true" ]; then
    echo "<fc=red>Not good to go</fc>"
else
    echo "<fc=#006000>Good to go</fc>"
fi
