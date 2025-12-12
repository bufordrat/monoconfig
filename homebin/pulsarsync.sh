#!/bin/sh
LOCAL_PATH=~/.android-music/
REMOTE_PATH='pixel10:~/storage/shared/Music/android-music'
rsync -aizvP --out-format='android music update: %i %n' --delete --no-perms $LOCAL_PATH $REMOTE_PATH | grep -oE '^android music update:.*' || true
