#!/bin/sh
for project in $@
do
    trivy fs --quiet --scanners vuln --format json ~/stuff/github/$project | jq '.Results[]?.Vulnerabilities'
done
