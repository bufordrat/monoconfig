#!/bin/sh
LOCAL_PATH=~/Stuff/termux_test/photo_bkup_test
REMOTE_PATH='pixel8:~/storage/shared/DCIM/Camera/'
rsync -aizvP --delete -e 'ssh -p 8022' $REMOTE_PATH $LOCAL_PATH
