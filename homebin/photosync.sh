#!/bin/sh
LOCAL_PATH=~/Stuff/Photos/pixel10/current
REMOTE_PATH='pixel10:~/storage/shared/DCIM/Camera/'
rsync -aizvP --out-format='photos update: %i %n' --delete $REMOTE_PATH $LOCAL_PATH | grep -oE '^photos update:.*' || true
