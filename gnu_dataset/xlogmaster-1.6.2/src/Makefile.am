## Process this file with automake to produce Makefile.in

if DEVELMODE
DEVELFLAGS = -g -Wall
endif

CLEANFILES=*~ .deps/*
AM_CXXFLAGS = $(GTK_CFLAGS)
AM_CXXFLAGS += -DXLM_HOME=\"$(XLM_HOME)\" -DXLM_DB=\"$(XLM_DB)\" -DXLM_USER_DB=\"$(XLM_USER_DB)\" -DXLM_LIB=\"$(XLM_LIB)\" 
##AM_CXXFLAGS += -I../import $(DEVELFLAGS)
AM_LDFLAGS = $(GTK_LIBS)


bin_PROGRAMS = xlogmaster
xlogmaster_SOURCES = xlogmaster.cc \
	alert.cc \
	audio.cc \
	database.cc \
	dialog.cc \
	execute.cc \
	file.cc \
	logclass.cc \
	menu.cc \
	customize.cc \
	watchdog.cc \
	output.cc 



##if NEED_LIBIMPORT
##xlogmaster_LDADD = ../import/libimport.a
##endif


install-exec-hook:
	$(mkinstalldirs) $(XLM_HOME)
	$(mkinstalldirs) $(XLM_LIB)/sound
	$(mkinstalldirs) $(XLM_LIB)/output-plugins
	$(mkinstalldirs) $(XLM_LIB)/scripts
	$(mkinstalldirs) $(XLM_DB)

if DEVELMODE
run:
	./xlogmaster -gtkrc ../gtkrc-samples/steelblue.gtkrc -terse
endif
