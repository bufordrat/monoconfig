#!/bin/sh
ANDROID_PATH=/sdcard/Phone
ADB_DUMP_PATH=~/adb-dump
RSYNC_SOURCE=~/adb-dump/Phone/
RSYNC_TARGET=~/Stuff/bkup/phone/current
adb pull -a $ANDROID_PATH $ADB_DUMP_PATH 2> /dev/null && rsync -aizvP $RSYNC_SOURCE $RSYNC_TARGET

