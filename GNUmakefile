# -*- makefile-gmake -*-

CONFIG_PATH = ~/.config
HOST = $(shell uname -n | cut -d. -f1)
HOMEBIN_DIR = ~/bin

ARCH_PACKAGES = herbstluftwm fish openssh gnupg zsh
PI_PACKAGES = fish openssh gnupg zsh 
MACOS_PACKAGES = fish iterm pinentry-mac

ARCH_RULES = emacs herbstluftwm bash fish zsh openssh gnupg x11 homebin
PI_RULES = emacs bash fish zsh openssh gnupg homebin mpd raspi
MACOS_RULES = homebin emacs bash fish zsh iterm openssh gnupg


# mother of all rules
all: $(HOST)


# host rules
sequent: arch dunst firehol fstab borg

kleisli: arch mpd samba intel

substructural: macos 

subtype: arch netctl

semigroup: 

pitype:

fomega: pi 

mzero: 

profunctor: macos


# os rules
arch: $(ARCH_RULES)

macos: $(MACOS_RULES)

pi: $(PI_RULES)


# app/config rules
herbstluftwm::
	mkdir -p $(CONFIG_PATH)/$@
	install -m 555 $@/autostart $(CONFIG_PATH)/$@/autostart
	install -m 555 $@/general_as $(CONFIG_PATH)/$@/general_as
	install -m 555 $@/$(HOST)_as $(CONFIG_PATH)/$@/$(HOST)_as
	install -m 444 $@/bg.png $(CONFIG_PATH)/bg.png
.PHONY: herbstluftwm

fish::
	mkdir -p $(CONFIG_PATH)/$@
	install -m 444 $@/config.fish $(CONFIG_PATH)/$@/config.fish
	install -m 444 $@/general.fish $(CONFIG_PATH)/$@/general.fish
	install -m 444 $@/ssh_gpg.fish $(CONFIG_PATH)/$@/ssh_gpg.fish
	install -m 444 $@/$(HOST).fish $(CONFIG_PATH)/$@/$(HOST).fish
.PHONY: fish

dunst::
	mkdir -p $(CONFIG_PATH)/$@
	install -m 444 $@/dunstrc $(CONFIG_PATH)/$@/dunstrc
.PHONY: dunst

iterm::
	install -m 644 $@/hushlogin ~/.hushlogin
.PHONY: iterm

xdefaults::
	install -m 444 $@/$(HOST)_xdefaults ~/.Xdefaults
.PHONY: xdefaults

xinitrc::
	mkdir -p ~/$@
	install -m 555 $@/.xinitrc ~/.xinitrc
	install -m 555 $@/general_xinitrc ~/$@/general_xinitrc
	install -m 555 $@/$(HOST)_xinitrc ~/$@/$(HOST)_xinitrc
.PHONY: xinitrc

x11: xinitrc xdefaults

openssh::
	install -m 444 $@/$(HOST)_ssh_config ~/.ssh/config
.PHONY: openssh

gnupg::
	mkdir -m 700 -p ~/.$@
	install -m 444 $@/dummy.gpg ~
	install -m 444 $@/$(HOST)_gpg_agent_conf ~/.$@/gpg-agent.conf
.PHONY: gnupg

firehol::
	pgrep gpg-agent
	gpg -d --pinentry=loopback $@/$@_conf.gpg | sudo install -m 444 /dev/stdin /etc/$@/$@.conf
.PHONY: firehol

fstab::
	sudo install -m 644 $@/$(HOST)_fstab /etc/fstab
.PHONY: fstab

homebin::
	mkdir -p $(HOMEBIN_DIR)
	install -m 555 $@/dmenu_run_history.sh $(HOMEBIN_DIR)/dmenu_run_history
	install -m 555 $@/pi0sync.sh $(HOMEBIN_DIR)/pi0sync
	install -m 555 $@/pi3sync.sh $(HOMEBIN_DIR)/pi3sync
	install -m 555 $@/figure_out_editor_variable.sh $(HOMEBIN_DIR)/figure_out_editor_variable
	# will need to move this out into a separate rule, since Macs don't need a susp script
	# install -m 555 $@/$(HOST)_susp.sh $(HOMEBIN_DIR)/susp
.PHONY: homebin

# note: I have not yet set this repo up on semigroup, pitype, or
# mzero, so these mpd config files are currently only here for backup;
# the only one that's being used is kleisli
mpd::
	mkdir -p $(CONFIG_PATH)/mpd
	install -m 444 $@/$(HOST)_mpd_conf $(CONFIG_PATH)/$@/mpd.conf
.PHONY: mpd

samba::
	sudo install -m 444 $@/$(HOST)_smb_conf /etc/samba/smb.conf
.PHONY: samba

zsh::
	mkdir -p ~/zshrc
	install -m 444 $@/.zshrc ~/.zshrc
	install -m 444 $@/general_zshrc ~/zshrc/general_zshrc
	install -m 444 $@/$(HOST)_zshrc ~/zshrc/$(HOST)_zshrc
	install -m 444 $@/$(HOST)_zshenv ~/.zshenv
.PHONY: zsh

intel::
	sudo install -m 444 $@/$(HOST)_20_intel_conf /etc/X11/xorg.conf.d/20-intel.conf
.PHONY: intel

raspi::
	sudo install -m 755 $@/$(HOST)_boot_config_txt /boot/firmware/config.txt
	sudo install -m 644 $@/$(HOST)_console_setup /etc/default/console-setup
.PHONY: raspi

netctl::
	pgrep gpg-agent
	gpg -d --pinentry=loopback ~/.secrets/cnetid.gpg 2> /dev/null | tr -d '\012' | m4 -D LAMBDATASTIC='include(/dev/stdin)' $@/eduroam | sudo install -m 644 /dev/stdin /etc/$@/eduroam
.PHONY: netctl

bash::
	install -m 444 $@/$(HOST)_bashrc ~/.bashrc
	install -m 444 $@/$(HOST)_bash_profile ~/.bash_profile
.PHONY: bash

borg::
	install -m 555 $@/borgtastic.sh $(HOMEBIN_DIR)/borgtastic
	install -m 444 $@/$(HOST)_borg_config $(CONFIG_PATH)/borg-config
.PHONY: borg

emacs::
	mkdir -p ~/.emacs.d/lisp
	install -m 444 $@/entry-point.el ~/.emacs.d/lisp/entry-point.el
	install -m 444 $@/fonts.el ~/.emacs.d/lisp/fonts.el
	install -m 444 $@/fishy-prompt.el ~/.emacs.d/lisp/fishy-prompt.el
	install -m 444 $@/general-init.el ~/.emacs.d/lisp/general-init.el
	install -m 444 $@/general-init-nw.el ~/.emacs.d/lisp/general-init-nw.el
	install -m 444 $@/$(HOST)-init.el ~/.emacs.d/lisp/$(HOST)-init.el
.PHONY: emacs


# package manager rules
pacman::
	sudo pacman -S $(ARCH_PACKAGES)
.PHONY: pacman

brew::
	brew install $(MACOS_PACKAGES)
.PHONY: brew
