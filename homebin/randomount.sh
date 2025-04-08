#!/bin/sh
sudo mount -o uid=$(id -u),gid=$(id -g),fmask=113,dmask=002 "${1?}" "${2?}"
