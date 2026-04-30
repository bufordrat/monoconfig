#!/bin/sh

waypipe ssh $1 env $2=true emacsclient -cn &

