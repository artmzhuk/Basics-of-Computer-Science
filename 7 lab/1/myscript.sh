#!/bin/bash

tfile="$(mktemp ./outputXXXXX.txt)" || exit 1
tfile2="$(mktemp ./errorXXXXX.txt)" || exit 1


program="$1"
interval="$2"

echo script started

sudo chmod +x ${program}

while true; do
    ./$program >> "$tfile" 2>> "$tfile2" &
    echo prog must be started
    sleep $(( "$interval" * 1 ))
    while pidof -x "$program" > /dev/null; do
     sleep $(( "$interval" * 1 ))
    done
done
