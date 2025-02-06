#!/bin/sh

if echo "$INSIDE_EMACS" | grep -q tramp
then # we are in an Emacs shell launched from a tramped location
    echo "no \$EDITOR over a tramp connection" ; exit 1
elif [ -z "$INSIDE_EMACS" -a "$TERM" = ansi ]
then # we are in an Emacs shell that ssh-ed into a server from sequent
    echo "no \$EDITOR in an ssh-ed Emacs shell" ; exit 1
elif [ -n "$INSIDE_EMACS" ]
then # we're in a local Emacs shell i.e. not sshed/tramped in
    emacsclient -a emacs "$@"
else # we are in a straight terminal and possibly ssh-ed in somewhere
    if emacs --version > /dev/null
    then emacsclient -nw -a 'emacs -nw' "$@"
    else vim "$@" || vi "$@" || echo 'no available $EDITOR in this terminal' 1>&2
    fi
fi
