#!/usr/bin/env bash

function startOn() {
    CMD=$1
    WORKSPACE=$2

    termite -d $DIR -r "send to $WORKSPACE" -e "bash -ic '$CMD; exec bash'" &
    sleep 0.1 # slow down spawning termites so things don't behave intermittently
}

DIR=~/gitting/worldcubeassociation.org/WcaOnRails/
startOn "cd WcaOnRails; bin/rails s" "wrk"

DIR=~/gitting/worldcubeassociation.org/
startOn "sudo systemctl start mysqld" "wrk"
startOn "vim" "wrk"
