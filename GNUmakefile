# -*- makefile-gmake -*-

CONFIG_PATH = ~/.config
HOST = $(shell uname -n | cut -d. -f1)
HOMEBIN_DIR = ~/bin

ARCH_PACKAGES = herbstluftwm fish openssh gnupg
PI_PACKAGES = herbstluftwm
MACOS_PACKAGES = fish iterm pinentry-mac

ARCH_RULES = herbstluftwm fish openssh gnupg x11 homebin
PI_RULES = herbstluftwm x11
MACOS_RULES = fish iterm openssh gnupg


# mother of all rules
all: $(HOST)


# host rules
sequent: arch dunst firehol fstab 

kleisli: arch mpd

substructural: macos

subtype: arch


# os rules
arch: $(ARCH_RULES)

macos: $(MACOS_RULES)


# app/config rules
herbstluftwm::
	install -m 555 $@/autostart $(CONFIG_PATH)/$@/autostart
	install -m 555 $@/general_as $(CONFIG_PATH)/$@/general_as
	install -m 555 $@/$(HOST)_as $(CONFIG_PATH)/$@/$(HOST)_as
	install -m 444 $@/bg.png $(CONFIG_PATH)/bg.png
.PHONY: herbstluftwm

fish::
	install -m 444 $@/config.fish $(CONFIG_PATH)/$@/config.fish
	install -m 444 $@/general.fish $(CONFIG_PATH)/$@/general.fish
	install -m 444 $@/ssh_gpg.fish $(CONFIG_PATH)/$@/ssh_gpg.fish
	install -m 444 $@/$(HOST).fish $(CONFIG_PATH)/$@/$(HOST).fish
.PHONY: fish

dunst::
	install -m 444 $@/dunstrc $(CONFIG_PATH)/$@/dunstrc
.PHONY: dunst

iterm::
	install -m 644 $@/hushlogin ~/.hushlogin
.PHONY: iterm

xdefaults::
	install -m 444 $@/$(HOST)_xdefaults ~/.Xdefaults
.PHONY: xdefaults

xinitrc::
	install -m 555 $@/.xinitrc ~/.xinitrc
	install -m 555 $@/general_xinitrc ~/xinitrc/general_xinitrc
	install -m 555 $@/$(HOST)_xinitrc ~/xinitrc/$(HOST)_xinitrc
.PHONY: xinitrc

x11: xinitrc xdefaults

openssh::
	install -m 444 $@/$(HOST)_ssh_config ~/.ssh/config
.PHONY: openssh

gnupg::
	install -m 444 $@/dummy.gpg ~
	install -m 444 $@/$(HOST)_gpg_agent_conf ~/.$@/gpg-agent.conf
.PHONY: gnupg

firehol::
	gpg -d $@/$@_conf.gpg | sudo install -m 444 /dev/stdin /etc/$@/$@.conf
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
.PHONY: homebin

mpd::
	mkdir -p $(CONFIG_PATH)/mpd
	install -m 444 $@/$(HOST)_mpd_conf $(CONFIG_PATH)/$@/mpd.conf
.PHONY: mpd

# package manager rules
pacman::
	sudo pacman -S $(ARCH_PACKAGES)
.PHONY: pacman

brew::
	brew install $(MACOS_PACKAGES)
.PHONY: brew
