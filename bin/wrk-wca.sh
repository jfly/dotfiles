#!/usr/bin/env bash

function terminalOn() {
    CMD=$1
    WORKSPACE=$2

    termite -r "send to $WORKSPACE" -e "shtuff new \"$CMD\"" &
    sleep 0.1 # slow down spawning termites so things don't behave intermittently
}

sudo systemctl start mysqld

cd ~/wca/worldcubeassociation.org/WcaOnRails/
terminalOn "bin/rails s" "wrk"
terminalOn 'shtuff as $(pwd)' "wrk"
terminalOn "vim" "wrk"
