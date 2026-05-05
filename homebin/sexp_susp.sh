#!/bin/bash

((nmcli -t -f name,active connection show | grep "UChicagoVPN:yes" || nmcli -t -f name,active connection show | grep "eduroam:yes" || nmcli -t -f name,active connection show | grep "CVPN:yes") && ssh sequent "emacsclient -eval '(zap-waypiped-client-frames)'"; systemctl suspend; sleep 0.1) && waylock
