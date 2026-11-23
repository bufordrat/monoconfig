#!/bin/sh
trivy clean --all 2> /dev/null
for project in $@
do
    trivy fs --quiet --scanners vuln --format json $project
done 2> /dev/null
