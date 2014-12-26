#!/usr/bin/env bash

if [ "`hostname`" == "slaptop" ]; then
    # For some reason, the t410 (slaptop) is muting all by itself?
    sleep 0.1;
else
    amixer set Master toggle;
fi

