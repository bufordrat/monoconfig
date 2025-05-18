#!/bin/sh
ping -c 10 semigroup.local && rsync -aizvP --delete /home/teichman/Stuff/Music/cdCollection/ teichman@semigroup.local:/home/teichman/Music/cdCollection

