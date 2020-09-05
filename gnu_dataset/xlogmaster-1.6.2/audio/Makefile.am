## Process this file with automake to produce Makefile.in

CLEANFILES=*~ .deps/*

SOUNDFILES	=	\
	carramba.wav	\
	chackabeep.wav	\
	glass.wav

install-exec-hook:
if INSTALLSOUND
	cp -f $(SOUNDFILES) $(XLM_LIB)/sound/
	$(SHELL) -c 'test -f "$(XLM_LIB)/sound/notice" || ( cd  $(XLM_LIB)/sound/ && $(LN_S) glass.wav notice )'
	$(SHELL) -c 'test -f "$(XLM_LIB)/sound/alert" || ( cd  $(XLM_LIB)/sound/ && $(LN_S) carramba.wav alert )'
	$(SHELL) -c 'test -f "$(XLM_LIB)/sound/uniconify" || ( cd  $(XLM_LIB)/sound/ && $(LN_S) chackabeep.wav uniconify )'
endif

