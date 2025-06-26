#!/bin/sh

find $1 -maxdepth 1 -mindepth 1 -type d ! -name .\* |
while read dir
do
    (
	echo git -C $dir fetch --all
	git -C $dir fetch --all
    )
done
