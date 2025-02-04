# -*- makefile-gmake -*-

CONFIG_PATH = ~/.config
HOST = $(shell uname -n | cut -d. -f1)

LINUX_PACKAGES = herbstluftwm fish
MACOS_PACKAGES = fish iterm

LINUX_RULES = herbstluftwm fish x11
MACOS_RULES = fish iterm


# mother of all rules
all: $(HOST)


# host rules
sequent: linux dunst

kleisli: linux

substructural: macos


# os rules
linux: $(LINUX_RULES)

macos: $(MACOS_RULES)


# app rules
herbstluftwm::
	install -m 555 $@/autostart $(CONFIG_PATH)/$@/autostart
	install -m 555 $@/general_as $(CONFIG_PATH)/$@/general_as
	install -m 555 $@/$(HOST)_as $(CONFIG_PATH)/$(HOST/)$(HOST)_as
.PHONY: herbstluftwm

fish::
	install -m 444 $@/config.fish $(CONFIG_PATH)/$@/config.fish
	install -m 444 $@/general.fish $(CONFIG_PATH)/$@/general.fish
	install -m 444 $@/$(HOST).fish $(CONFIG_PATH)/$@/$(HOST).fish
.PHONY: fish

dunst::
	install -m 444 $@/dunstrc $(CONFIG_PATH)/$@/dunstrc
.PHONY: dunst

iterm::
	install -m 644 $@/hushlogin ~/.hushlogin
.PHONY: iterm

x11::
	install -m 555 $@/$(HOST)_xinitrc ~/.xinitrc
.PHONY: x11


# package manager rules
pacman::
	sudo pacman -S $(LINUX_PACKAGES)
.PHONY: pacman

brew::
	brew install $(MACOS_PACKAGES)
.PHONY: brew
