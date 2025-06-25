#!/bin/sh

NOT=
find $1 -maxdepth 1 -type d ! -name .\* |
while read dir
do
  (
    cd "$dir"
    $NOT git fetch --all
    for branch in $(git branch -a --format "%(refname:short)" | sed '/^origin\//!d;s;^origin/;;');
    do $NOT git checkout    "$branch";
       $NOT git pull origin "$branch" ;
    done
  )
done
