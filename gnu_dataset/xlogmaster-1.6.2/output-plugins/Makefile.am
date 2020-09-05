## Process this file with automake to produce Makefile.in

CLEANFILES=*~ .deps/*

PLUGINS		= dipshit

install-exec-hook:
if INSTALLSOUND
	cp -f $(PLUGINS) $(XLM_LIB)/output-plugins/
endif

