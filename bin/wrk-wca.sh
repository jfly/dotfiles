#!/usr/bin/env bash

function terminalOn() {
    CMD=$1
    WORKSPACE=$2

    termite -r "send to $WORKSPACE" -e "shell-and-stuff \"$CMD\"" &
    sleep 0.1 # slow down spawning termites so things don't behave intermittently
}

sudo systemctl start mysqld

cd ~/gitting/worldcubeassociation.org/WcaOnRails/
terminalOn "bin/rails s" "wrk"
terminalOn "fat-runner become" "wrk"
terminalOn "vim" "wrk"
