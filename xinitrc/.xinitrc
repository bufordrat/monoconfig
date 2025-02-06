#!/bin/sh

# dot general and host-specific xinitrc files
. ~/xinitrc/general_xinitrc
. ~/xinitrc/$(uname -n | cut -d. -f1)_xinitrc

# fire herbst up
exec herbstluftwm -l
