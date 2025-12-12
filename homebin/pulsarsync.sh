#!/bin/sh
LOCAL_PATH=~/.android-music/
REMOTE_PATH='pixel10:~/storage/shared/Music/android-music'
rsync -aizvP --delete --no-perms $LOCAL_PATH $REMOTE_PATH
