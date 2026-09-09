#!/bin/sh
ANDROID_PATH=/sdcard/DCIM/Camera
ADB_DUMP_PATH=~/adb-dump
RSYNC_TARGET=~/Stuff/Photos/pixel10/current
RSYNC_SOURCE=~/adb-dump/Camera/
adb pull -a $ANDROID_PATH $ADB_DUMP_PATH && rsync -aizvP --out-format='photos update: %i %n' --delete $RSYNC_SOURCE $RSYNC_TARGET | grep -oE '^photos update:.*' || true
