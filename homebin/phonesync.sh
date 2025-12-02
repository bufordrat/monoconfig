#!/bin/sh
LOCAL_PATH=~/Stuff/termux_test/phone_bkup_test
REMOTE_PATH='pixel8:~/storage/shared/Phone/'
rsync -aizvP -e 'ssh -p 8022' $REMOTE_PATH $LOCAL_PATH
