#!/bin/sh
for project in $@
do
    trivy fs --scanners vuln,secret --format json ~/stuff/github/$project | jq '.Results | select (. != null)'
done
