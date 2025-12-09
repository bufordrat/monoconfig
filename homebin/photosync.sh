#!/bin/sh
LOCAL_PATH=~/Stuff/Photos/pixel10/current
REMOTE_PATH='pixel10:~/storage/shared/DCIM/Camera/'
rsync -aizvP --delete -e 'ssh -p 8022' $REMOTE_PATH $LOCAL_PATH
