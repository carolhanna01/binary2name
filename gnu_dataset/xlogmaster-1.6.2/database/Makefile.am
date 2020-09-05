## Process this file with automake to produce Makefile.in

CLEANFILES=*~ .deps/*

XLMDBFILES	=	\
	mem.xlmrc	\
	rtc.xlmrc	\
	cat.xlmgrp	\
	prg.xlmgrp	\
	syslog.xlmrc	\
	warn.xlmrc	\
	df.xlmrc	\
	pstree.xlmrc	\
	tail.xlmgrp	\
	w.xlmrc		\
	mailq.xlmrc





install-exec-hook:
	cp -f $(XLMDBFILES) $(XLM_DB)/

