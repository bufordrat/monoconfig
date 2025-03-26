# -*- makefile-gmake -*-

# run it in bash
SHELL = bash

# hostname
HOST = $(shell uname -n | cut -d. -f1)

# paths
CONFIG_DIR = ~/.config
HOMEBIN_DIR = ~/bin
ENVS_DIR = $(HOME)/envs
ENVS_CONFIG_DIR = $(ENVS_DIR)/config
VENV_NAME = py-basics
VENV_DIR = $(ENVS_DIR)/virtualenvs/py-default
VENV_REQUIREMENTS_DIR = $(ENVS_CONFIG_DIR)/$(VENV_NAME)
SWITCH_NAME = ocaml-basics
SWITCH_VERSION = 4.14.1
OCAML_BASICS = dune utop prelude etude spinup mrmime ocamlnet cmdliner ocamlformat ocp-index alcotest
CABAL_VERSION = 3.14.1.1
GHC_VERSION = 9.8.4
AGDA_STDLIB_VERSION = 2.2

# make rulesets
BASIC_RULES = homebin emacs bash fish zsh openssh gnupg
ARCH_RULES = $(BASIC_RULES) herbstluftwm x11 python etc_pacman_conf boot_loader

PI_RULES = $(BASIC_RULES) mpd raspi
MACOS_RULES = $(BASIC_RULES) iterm python
INTERNET_RULES = install-python install-ocaml install-agda

# mother of all rules
all: $(HOST)

# use the internet
internet: all $(INTERNET_RULES)

# host rules
sequent: arch dunst firehol borg
# etc_hosts
# fstab

kleisli: arch mpd samba intel

substructural: macos 

subtype: arch netctl

semigroup: 

pitype:

fomega: pi 

mzero:

profunctor: macos

hedwig: homebin zsh emacs

# os rules
arch: $(ARCH_RULES)

macos: $(MACOS_RULES)

pi: $(PI_RULES)

# app/config rules
herbstluftwm::
	mkdir -p $(CONFIG_DIR)/$@
	install -m 555 $@/autostart $(CONFIG_DIR)/$@/autostart
	install -m 555 $@/general_as $(CONFIG_DIR)/$@/general_as
	install -m 555 $@/$(HOST)_as $(CONFIG_DIR)/$@/$(HOST)_as
	install -m 444 $@/bg.png $(CONFIG_DIR)/bg.png
.PHONY: herbstluftwm

fish::
	mkdir -p $(CONFIG_DIR)/$@
	install -m 444 $@/config.fish $(CONFIG_DIR)/$@/config.fish
	install -m 444 $@/general.fish $(CONFIG_DIR)/$@/general.fish
	install -m 444 $@/ssh_gpg.fish $(CONFIG_DIR)/$@/ssh_gpg.fish
	install -m 444 $@/$(HOST).fish $(CONFIG_DIR)/$@/$(HOST).fish
.PHONY: fish

dunst::
	mkdir -p $(CONFIG_DIR)/$@
	install -m 444 $@/dunstrc $(CONFIG_DIR)/$@/dunstrc
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

firehol: homebin
	pgrep gpg-agent
	gpg -d --pinentry-mode loopback ~/dummy.gpg
	gpg -d --pinentry-mode loopback $@/$@_conf.gpg | sudo install -m 444 /dev/stdin /etc/$@/$@.conf
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
	install -m 555 $@/sudo-lockout.sh $(HOMEBIN_DIR)/sudo-lockout
.PHONY: homebin

# note: I have not yet set this repo up on semigroup, pitype, or
# mzero, so these mpd config files are currently only here for backup;
# the only one that's being used is kleisli
mpd::
	mkdir -p $(CONFIG_DIR)/mpd
	install -m 444 $@/$(HOST)_mpd_conf $(CONFIG_DIR)/$@/mpd.conf
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

netctl: homebin
	pgrep gpg-agent
	gpg -d --pinentry-mode loopback ~/dummy.gpg
	gpg -d --pinentry-mode loopback ~/.secrets/cnetid.gpg 2> /dev/null | tr -d '\012' | m4 -D LAMBDATASTIC='include(/dev/stdin)' $@/eduroam | sudo install -m 644 /dev/stdin /etc/$@/eduroam
.PHONY: netctl

bash::
	install -m 444 $@/$(HOST)_bashrc ~/.bashrc
	install -m 444 $@/$(HOST)_bash_profile ~/.bash_profile
.PHONY: bash

borg::
	install -m 555 $@/borgtastic.sh $(HOMEBIN_DIR)/borgtastic
	install -m 444 $@/$(HOST)_borg_config $(CONFIG_DIR)/borg-config
.PHONY: borg

emacs::
	mkdir -p ~/.emacs.d/lisp
	test -f ~/.emacs.d/customizes.el || touch ~/.emacs.d/customizes.el
	install -m 444 $@/init.el ~/.emacs.d
	install -m 444 $@/fonts.el ~/.emacs.d/lisp
	install -m 444 $@/fishy-prompt.el ~/.emacs.d/lisp
	install -m 444 $@/toggle-gui.el ~/.emacs.d/lisp
	install -m 444 $@/shells.el ~/.emacs.d/lisp
	install -m 444 $@/general-init.el ~/.emacs.d/lisp
	install -m 444 $@/$(HOST)-init.el ~/.emacs.d/lisp/$(HOST)-init.el
	cp ~/.emacs.d/customizes.el $@/customizes/$(HOST)_customizes
	cp ~/.emacs.d/bookmarks $@/bookmarks/$(HOST)_bookmarks
.PHONY: emacs

etc_hosts::
	sudo install -m 444 $@/$(HOST)_etc_hosts /etc/hosts
.PHONY: etc_hosts

etc_pacman_conf::
	sudo install -m 444 $@/$(HOST)_pacman_conf /etc/pacman.conf
.PHONY: pacman_conf

remove-virtualenv::
	rm -rf $(VENV_DIR)
.PHONY: remove-virtualenv

python::
	mkdir -p $(VENV_REQUIREMENTS_DIR)
	install -m 444 $@/config_lsp_requirements $(VENV_REQUIREMENTS_DIR)/requirements.txt
.PHONY: python

install-python:: remove-virtualenv python
	mkdir -p $(VENV_DIR)
	python3 -m venv $(VENV_DIR)
	source $(VENV_DIR)/bin/activate && python3 -m ensurepip && pip install --upgrade pip && pip install -r $(VENV_REQUIREMENTS_DIR)/requirements.txt && deactivate
.PHONY: install-python

install-opam::
	if [ -d ~/.opam ]; then echo 'opam already initialized'; else cd ~ && opam init -y && cd -; fi
.PHONY: install-opam

remove-switch::
	opam switch remove -y $(SWITCH_NAME) || true
.PHONY: remove-switch

install-ocaml:: install-opam remove-switch
	opam switch create -y $(SWITCH_NAME) $(SWITCH_VERSION) && opam switch set $(SWITCH_NAME) && eval $$(opam env) && opam repository add dldc 'https://dldc.lib.uchicago.edu/opam' && opam update -y && opam upgrade -y && opam install -y $(OCAML_BASICS) && opam switch set ocaml-basics && eval $$(opam env)
.PHONY: install-ocaml

install-haskell::
	ghcup nuke
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | env BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
	ghcup install stack 3.3.1
	ghcup set stack 3.3.1
	ghcup install hls 2.9.0.1
	ghcup set hls 2.9.0.1
	ghcup install ghc $(GHC_VERSION)
	ghcup set ghc $(GHC_VERSION)
	mkdir -p ~/.stack
	install -m 444 $@/config_yaml ~/.stack/config.yaml
.PHONY: install-haskell

install-agda:: install-haskell
	ghcup install cabal $(CABAL_VERSION)
	ghcup set cabal $(CABAL_VERSION)
	cabal update
	cabal install --overwrite-policy=always --install-method=copy Agda
	cd $(shell dirname $(shell agda-mode locate)) && emacs --batch --eval '(push "." load-path)' -f batch-byte-compile eri.el *.el
	ghcup rm cabal $(CABAL_VERSION)
	rm -rf $$(agda --print-agda-app-dir)
	mkdir -p $$(agda --print-agda-app-dir)
	cd $$(agda --print-agda-app-dir) && wget -O stdlib.tar.gz 'https://github.com/agda/agda-stdlib/archive/v$(AGDA_STDLIB_VERSION).tar.gz' && tar xzvf stdlib.tar.gz
	echo $$(agda --print-agda-app-dir)/agda-stdlib-$(AGDA_STDLIB_VERSION)/standard-library.agda-lib > $$(agda --print-agda-app-dir)/libraries
	echo standard-library > $$(agda --print-agda-app-dir)/defaults
.PHONY: install-agda

boot_loader::
	mkdir -p /boot/loader/entries
	sudo install -m 555 $@/loader_conf /boot/loader/loader.conf
	lsblk -P -o fstype,uuid | grep crypto_LUKS | awk -F= '{print $$3}' | tr -d '"\012' | m4 -D SIGMALICIOUS='include(/dev/stdin)' $@/$(HOST)_arch_conf | sudo install -m 555 /dev/stdin /boot/loader/entries/arch.conf
	lsblk -P -o fstype,uuid | grep crypto_LUKS | awk -F= '{print $$3}' | tr -d '"\012' | m4 -D SIGMALICIOUS='include(/dev/stdin)' $@/$(HOST)_arch_lts_conf | sudo install -m 555 /dev/stdin /boot/loader/entries/arch-lts.conf
.PHONY: boot_loader

mkinitcpio_conf:
	install -m 555 $@/$(HOST)_mkinitcpio_conf /etc/mkinitcpio.conf
.PHONY: mkinitcpio_conf

# packages to install
ARCH_PACKAGES = herbstluftwm fish openssh gnupg zsh dunst emacs opam rxvt-unicode xorg-server xorg-xinit xorg-twm xorg-xclock xterm udisks2 udiskie m4 ascii xclip picom dhcpcd
AUR_PACKAGES = yay profont-otb ttf-mplus montecarlo-font
PI_PACKAGES = fish openssh gnupg zsh mpd ascii xclip
MACOS_PACKAGES = fish iterm pinentry-mac opam ascii xclip make wget

# package manager rules
pacman::
	sudo pacman -S $(ARCH_PACKAGES)
.PHONY: pacman

brew::
	brew install $(MACOS_PACKAGES)
.PHONY: brew
