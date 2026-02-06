#!/bin/sh
for project in $@
do
    trivy fs --quiet --scanners vuln --format json ~/stuff/github/$project | jq '.Results[]?.Vulnerabilities | select (. != null)[] | {"PkgName" : .PkgName, "InstalledVersion" : .InstalledVersion, "FixedVersion": .FixedVersion, "Severity" : .Severity, "VulnerabilityID" : .VulnerabilityId, "References": .References }'
    # trivy fs --quiet --scanners vuln --format json ~/stuff/github/$project | jq '.Results[]?.Vulnerabilities | select (. != null)' 
done


