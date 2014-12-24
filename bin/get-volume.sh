#!/usr/bin/env bash
amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "<fc=#ff0000>MM</fc>" } else { print $2 "%" }}' | head -n 1
