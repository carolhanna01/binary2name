## Process this file with automake to produce Makefile.in

CLEANFILES=*~ .deps/*

SCRIPTFILES	=	\
	README.usage	\
	xlm-mail	\
	xlm-halt	\
	xlm-write	\
	xlm-mgetty-login-mail


install-exec-hook:
if INSTALLSCRIPTS
	cp -f $(SCRIPTFILES) $(XLM_LIB)/scripts/
endif
