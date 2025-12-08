#!/bin/sh
LOCAL_PATH=~/Stuff/bkup/phone/current
REMOTE_PATH='pixel10:~/storage/shared/Phone/'
rsync -aizvP -e 'ssh -p 8022' $REMOTE_PATH $LOCAL_PATH
