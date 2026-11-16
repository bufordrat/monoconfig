#!/bin/sh
trivy clean --all 2> /dev/null
for project in $@
do
    # putting the old one back temporarily while I fix cvebump
    trivy fs --quiet --scanners vuln --format json $project | jq -c
done 2> /dev/null
