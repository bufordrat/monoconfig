#!/bin/sh
ping -c 10 pitype.local && rsync -aizvP --delete /home/teichman/Stuff/Music/cdCollection/ pi@pitype.local:/media/pi/sda1-ata-WDC_WD10SDRW-11A
