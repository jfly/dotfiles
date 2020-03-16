#!/usr/bin/env bash

set -e

function terminalOn() {
    CMD=$1
    WORKSPACE=$2

    termite -r "send to $WORKSPACE" -e "shtuff new \"$CMD\"" &
    sleep 0.1 # slow down spawning termites so things don't behave intermittently
}

sudo systemctl start docker
CUSTOM_MYSQL_DIR=/tmp/mysql-5_7-conf
mkdir -p "$CUSTOM_MYSQL_DIR"
echo "[mysqld]
sql_mode=TRADITIONAL" > $CUSTOM_MYSQL_DIR/sql_mode.cnf
docker start mysql-5_7 || docker run --name mysql-5_7 -v $CUSTOM_MYSQL_DIR:/etc/mysql/conf.d -e MYSQL_ALLOW_EMPTY_PASSWORD=yes -d --publish=3357:3306 mysql:5.7

cd ~/honor/external-api
terminalOn "make run" "play"
terminalOn "shtuff-as-pwd" "play"
terminalOn "./pythonenv vim" "play"

cd ~/honor/external-web
terminalOn "make run" "wrk"
terminalOn "shtuff-as-pwd" "wrk"
terminalOn "./pythonenv vim" "wrk"
