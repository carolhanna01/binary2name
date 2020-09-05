## Process this file with automake to produce Makefile.in

bin_PROGRAMS = ggradebook

ggradebook_SOURCES = \
	class.c \
	class.h \
	feedback.c \
	feedback.h \
	gg.c \
	gg.h \
	global.c \
	global.h \
	grades.c \
	grades.h \
	helpabout.c \
	helpabout.h \
	message.c \
	message.h \
	period.c \
	period.h \
	preferences.c \
	preferences.h \
	print.c \
	print.h \
	student.c \
	student.h \
	to.c \
	to.h

INCLUDES = -I. $(GTK_CFLAGS) -Wall -DPKGDATADIR=\"${pkgdatadir}/\"
ggradebook_LDADD = $(GTK_LIBS)
