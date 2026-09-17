#!/bin/sh
ANDROID_PATH=/sdcard/DCIM/Camera
ADB_DUMP_PATH=~/adb-dump
RSYNC_SOURCE=~/adb-dump/Camera/
RSYNC_TARGET=~/Stuff/Photos/pixel10/current
adb pull -a $ANDROID_PATH $ADB_DUMP_PATH 2> /dev/null && rsync -aizvP --out-format='photos update: %i %n' --delete $RSYNC_SOURCE $RSYNC_TARGET | grep -oE '^photos update:.*' || true
