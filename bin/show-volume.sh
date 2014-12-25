#!/usr/bin/env bash


if amixer get Master | grep '\[on'; then
    volnoti-show `amixer get Master | grep '[0-9]*%' -o | head -n 1`
else
    volnoti-show -m
fi
