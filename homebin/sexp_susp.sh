#!/bin/bash

(ssh sequent "emacsclient -eval '(zap-waypiped-client-frames)'"; systemctl suspend; sleep 0.1) && waylock
