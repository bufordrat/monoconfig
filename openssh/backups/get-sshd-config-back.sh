#!/bin/bash

tar xOaf $(ls /var/cache/pacman/pkg/openssh* | grep -Fv .sig | sort -Vr | head -1) usr/share/factory/etc/ssh/sshd_config
