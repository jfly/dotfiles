#!/usr/bin/env bash

set -e

function terminalOn() {
    CMD=$1
    WORKSPACE=$2

    alacritty --class "send to $WORKSPACE" -e "shtuff" new "$CMD" &
    sleep 0.1 # slow down spawning termites so things don't behave intermittently
}

sudo systemctl start docker
CUSTOM_MYSQL_DIR=/tmp/mysql-5_7-conf
mkdir -p "$CUSTOM_MYSQL_DIR"
echo "[mysqld]
sql_mode=TRADITIONAL" > $CUSTOM_MYSQL_DIR/sql_mode.cnf
docker start mysql-5_7 || docker run --name mysql-5_7 -v $CUSTOM_MYSQL_DIR:/etc/mysql/conf.d -e MYSQL_ALLOW_EMPTY_PASSWORD=yes -d --publish=3357:3306 mysql:5.7

cd ~/honor/external-api
terminalOn "make run" "be"
terminalOn "shtuff-as-pwd" "be"
terminalOn "./pythonenv vim" "be"

cd ~/honor/external-web
# terminalOn "make run" "fe"
terminalOn "shtuff-as-pwd" "fe"
terminalOn "./pythonenv vim" "fe"
