# -*- makefile-gmake -*-

HERBST_INSTALL_PATH = ~/.config/herbstluftwm
HOST = $(shell uname -n | cut -d. -f1)

all: linux

sequent: linux

kleisli: linux

linux: herbstluftwm

herbstluftwm::
	install -m 555 $@/autostart $(HERBST_INSTALL_PATH)/autostart
	install -m 555 $@/general_as $(HERBST_INSTALL_PATH)/general_as
	install -m 555 $@/$(HOST)_as $(HERBST_INSTALL_PATH)/$(HOST)_as
.PHONY: herbstluftwm


