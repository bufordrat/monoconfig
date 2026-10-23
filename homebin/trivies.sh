#!/bin/sh
trivy clean --all 2> /dev/null
for project in $@
do
    trivy fs --quiet --scanners vuln --format json ~/stuff/github/$project | cvebump
done 2> /dev/null
