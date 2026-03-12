#!/bin/sh
trivy clean --all 2> /dev/null
for project in $@
do
    trivy fs --quiet --scanners vuln --format json ~/stuff/github/$project | jq '.Results[]?.Vulnerabilities | select (. != null)[] | {"PkgName" : .PkgName, "InstalledVersion" : .InstalledVersion, "FixedVersion": .FixedVersion, "Severity" : .Severity, "VulnerabilityID" : .VulnerabilityID, "References": .References }'
done 2> /dev/null
