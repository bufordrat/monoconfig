#!/bin/sh
ping -c 10 mzero.local && rsync -aizvP --delete /home/teichman/Stuff/Music/cdCollection/ pi@mzero.local:/media/pi/sda1-ata-WDC_WD20SDZW-11Z/Music
