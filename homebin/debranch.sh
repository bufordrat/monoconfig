#!/bin/bash

ORIGINAL_DIR=$(pwd)
NEWNAME=$(echo $1 | sed  "s/-.*\$//")
OLDNAME=$(basename -s .zip $1)
TMPDIR=$(mktemp -d)
mv $1 $TMPDIR
cd $TMPDIR
unzip $1
mv $OLDNAME $NEWNAME
zip -r $NEWNAME.zip $NEWNAME
mv $NEWNAME.zip $ORIGINAL_DIR
cd $ORIGINAL_DIR
