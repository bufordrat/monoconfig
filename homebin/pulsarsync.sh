#!/bin/sh
LOCAL_PATH=~/.android-music/
REMOTE_PATH='pixel10:~/storage/shared/Music/android-music'
rsync -aizvP --delete $LOCAL_PATH $REMOTE_PATH


