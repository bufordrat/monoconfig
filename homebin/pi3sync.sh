#!/bin/sh
ping -c 10 pitype.local && rsync -aizvP --delete /home/teichman/Stuff/Music/cdCollection/ pi@pitype.local:/mnt/pi/music
