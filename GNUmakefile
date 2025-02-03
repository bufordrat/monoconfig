# -*- makefile-gmake -*-

HERBST_INSTALL_PATH = ~/.config/herbstluftwm

all: herbstluftwm

herbstluftwm::
	install -m 555 $@/autostart $(HERBST_INSTALL_PATH)/autostart
	install -m 555 $@/general_as $(HERBST_INSTALL_PATH)/general_as
	install -m 555 $@/sequent_as $(HERBST_INSTALL_PATH)/sequent_as
.PHONY: herbstluftwm


