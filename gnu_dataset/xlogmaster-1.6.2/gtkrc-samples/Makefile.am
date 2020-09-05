## Process this file with automake to produce Makefile.in

CLEANFILES=*~ .deps/*

GTKRCFILES	=	\
	green.gtkrc	\
	hell.gtkrc	\
	simpsons.gtkrc	\
	steelblue.gtkrc


install-exec-hook:
if INSTALLGTKRC
	cp -f $(GTKRCFILES) $(XLM_HOME)/
	$(SHELL) -c 'test -f "$(XLM_HOME)/$(GTKRCNAME)" || ln -s $(DEFGTKRC) $(XLM_HOME)/$(GTKRCNAME)'
endif
