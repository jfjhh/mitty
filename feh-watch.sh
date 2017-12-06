#!/bin/bash

set -e

IMG="$1"
[ -f $IMG ] || exit
feh -A ';echo %V' -g 512x512 -B black -Z $IMG &> /dev/null &
while :; do
	inotifywait -e modify $IMG
	xdotool search --name feh key Return
done

