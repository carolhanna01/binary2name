# V/Makefile.  Generated from Makefile.in by configure.

SHELL = /bin/sh
SUBDIRS = lib

# If you wish to debug or test Vlib, uncomment this definition ...
#SUBDIRS = lib test

defaulttarget: all

all install uninstall clean mostlyclean realclean TAGS tags info dvi dist test check depend:
	@for i in $(SUBDIRS) ; do \
		echo Making $@ in ./$${i} ... ; \
		( cd $$i ; $(MAKE) $(MFLAGS) $@ ) ; \
	done

distclean:
	@for i in $(SUBDIRS) ; do \
		echo Making $@ in ./$${i} ... ; \
		( cd $$i ; $(MAKE) $(MFLAGS) $@ ) ; \
	done
	rm -f Makefile test/Makefile
