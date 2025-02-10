#!/bin/sh
# borgtastic
# borgtastic.sh
#
# Keith Waclena <https://www.lib.uchicago.edu/keith/>
#
# Copyright 2021 Keith Waclena. All rights reserved.
# Distributed under the GPL2 license, see terms at the end of the file.
#

REPONAME=$(uname -n)
CONFIG=~/.config/borg-config
PATTERNS=~/.config/borg-patterns

BORG="borg"

ME=`basename $0 .sh`
AUTHOR="Keith Waclena"
WWW="https://www.lib.uchicago.edu/keith/software/borgtastic/"

USAGE="Usage: $ME [-c CONFIGFILE][-n][-p PATTERNSFILE][-H|--help]"
HELP="$USAGE

  -c | --config FILE    use FILE for configuration (default: $CONFIG)
  -n | --not-really     do a dry-run, don't really backup
  -p | --patterns FILE  use FILE for patterns (default: $PATTERNS)
  -H | --help           print this help message

Backup \$ROOT to \$REPO using \$BORG, passphrase via \$BORG_PASSCOMMAND.
Configuration comes from \$CONFIG.
Include / exclude patterns come from \$PATTERNS.

CONFIG=$CONFIG
PATTERNS=$PATTERNS
"

DRYRUN=

progress () {
    if [ -z "$($BORG list --short "$REPO")" ]
    then echo --progress        # first time
    else echo --list            # subsequent times
    fi
}

# main

# getopt
while true
do
    case "$1" in
    -H|--help)       test -n "$HELP" && printf "$HELP"\\n; exit 0 ;;
    -c|--config)     shift; CONFIG="$1"; shift ;;
    -n|--not-really) shift; DRYRUN="--dry-run" ;;
    -p|--patterns)   shift; PATTERNS="$1"; shift ;;
    --)              shift; break ;;
    -)               break ;;
    -*)              printf "$USAGE" >&2; exit 124 ;;
    *)               break ;;
    esac
done

if [ -f "$CONFIG" ]
then . "$CONFIG"
else echo "$ME: no config file $CONFIG" >&2; exit 1
fi

: ${BACKUPSDIR?} ${REPONAME?} ${DRYRUN?}

REPO=${BACKUPSDIR}/$REPONAME

if ! [ -d "$REPO" ]
then if ! mkdir -p "$REPO"
     then echo "can't find REPO=$REPO!"; exit 1
     fi
fi

$BORG create $DRYRUN                    \
        --verbose                       \
        $(progress)                     \
        --filter AME                    \
        --stats                         \
        --show-rc                       \
        --compression lz4               \
        --exclude-caches                \
        --patterns-from $PATTERNS       \
        "$REPO"::'{hostname}-{now}'     \
        "$ROOT"

echo "Pruning $REPO..." >&2

# Use the `prune` subcommand to maintain 7 daily, 4 weekly and 6 monthly
# archives of THIS machine. The '{hostname}-' prefix is very important to
# limit prune's operation to this machine's archives and not apply to
# other machines' archives also:

$BORG prune $DRYRUN                   \
      --verbose                       \
      --list                          \
      --prefix '{hostname}-'          \
      --show-rc                       \
      --keep-daily    7               \
      --keep-weekly   4               \
      --keep-monthly  6               \
      "$REPO"
