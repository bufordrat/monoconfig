#!/bin/sh
for project in $@
do
    trivy fs --quiet --scanners vuln,secret --format json ~/stuff/github/$project | jq '.Results | select (. != null)'
done
