# Copyright (C) 1999, 2000, 2001  Free Software Foundation, Inc.
#  
# This file is part of GNU gengetopt 
#
# GNU gengetopt is free software; you can redistribute it and/or modify 
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option) 
# any later version. 
#
# GNU gengetopt is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details. 
#
# You should have received a copy of the GNU General Public License along 
# with gengetopt; see the file COPYING. If not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 


# .ggonu is used in this test suite for command line parsers that have
# to be created without --unamed-opt

SUFFIXES = .ggo .ggonu .ggoconf

PROGNAME = $(top_builddir)/src/gengetopt$(EXEEXT)

GGO_COMPILE = $(PROGNAME) --input $< --unamed-opt --func-name $*_parser --file-name $*

.ggo.c:
	$(GGO_COMPILE)

.ggonu.c:
	$(PROGNAME) --input $< --func-name $*_parser --file-name $*

.ggoconf.c:
	$(GGO_COMPILE) --conf-parser

# we don't have to link to anything
LIBS =

# check if we need to link with getopt.o and strdup.o
if NO_GETOPTLONG
GETOPTLONG_O = $(top_builddir)/src/getopt.o $(top_builddir)/src/getopt1.o
else
GETOPTLONG_O =
endif

if NO_STRDUP
STRDUP_O = $(top_builddir)/src/strdup.o
else
STRDUP_O =
endif

INCLUDES = -I@top_srcdir@/src/

# INCLUDES is added to LDFLAGS because .ggo file are sources, and thus
# the .c files are passed to the linker, which also compiles, thus
# the include options should be passed to it as well
AM_LDFLAGS = $(DEFS) $(INCLUDES)

LDADD = $(GETOPTLONG_O) $(STRDUP_O)

EXPECT = 1

TESTS = no_strings no_unamed no_optgiven.sh more_than_once.sh canonize-names \
default_values default_cmdline test_all_opts.sh test_groups.sh \
test_conf_parser.sh test_conf_parser_err.sh test_multiple.sh

check_PROGRAMS = no_strings no_unamed no_optgiven more_than_once \
canonize-names default_values default_cmdline test_all_opts \
test_groups test_conf_parser test_multiple

no_strings_SOURCES = no_strings_cmd.ggo no_strings.c
no_unamed_SOURCES = no_unamed_cmd.ggonu no_unamed.c
no_optgiven_SOURCES = no_optgiven_cmd.ggo no_optgiven.c
more_than_once_SOURCES = more_than_once_cmd.ggo more_than_once.c
canonize_names_SOURCES = canonize-names-cmd.ggo canonize-names.c
default_values_SOURCES = default_values_cmd.ggo default_values.c
default_cmdline_SOURCES = cmdline.ggo default_cmdline.c
test_all_opts_SOURCES = test_all_opts_cmd.ggo test_all_opts.c
test_groups_SOURCES = test_groups_cmd.ggo test_groups.c
test_conf_parser_SOURCES = test_conf_parser_cmd.ggoconf test_conf_parser.c
test_multiple_SOURCES = test_multiple_cmd.ggo test_multiple.c

BUILT_SOURCES = no_strings_cmd.c no_strings_cmd.h \
no_unamed_cmd.c no_unamed_cmd.h \
no_optgiven_cmd.c no_optgiven_cmd.h \
more_than_once_cmd.c more_than_once_cmd.h \
canonize-names-cmd.c canonize-names-cmd.h \
default_values_cmd.c default_values_cmd.h \
cmdline.c cmdline.h \
test_all_opts_cmd.c test_all_opts_cmd.h \
test_groups_cmd.c test_groups_cmd.h \
test_conf_parser_cmd.c test_conf_parser_cmd.h \
test_multiple_cmd.c test_multiple_cmd.h

GENERATED_TESTS	= no_strings_cmd.c.test no_strings_cmd.h.test \
no_unamed_cmd.c.test no_unamed_cmd.h.test \
no_optgiven_cmd.c.test no_optgiven_cmd.h.test \
more_than_once_cmd.c.test more_than_once_cmd.h.test \
canonize-names-cmd.c.test canonize-names-cmd.h.test \
default_values_cmd.c.test default_values_cmd.h.test \
cmdline.c.test cmdline.h.test \
test_all_opts_cmd.c.test test_all_opts_cmd.h.test \
test_groups_cmd.c.test test_groups_cmd.h.test \
test_conf_parser_cmd.c.test test_conf_parser_cmd.h.test \
test_multiple_cmd.c.test test_multiple_cmd.h.test

no_optgiven_cmd.c: no_optgiven_cmd.ggo
	$(GGO_COMPILE) --no-handle-error

# using $(top_builddir) should test also the removal of leading path
# for the generation of the #ifndef in the generated header file
canonize-names-cmd.c: canonize-names-cmd.ggo
	$(PROGNAME) --input $(srcdir)/canonize-names-cmd.ggo --func-name $*-parser --file-name $(top_builddir)/tests/$*

cmdline.c: cmdline.ggo
	$(PROGNAME) --input $(srcdir)/cmdline.ggo

TESTSOUT = test.out
TESTSOUTPUT = test.output

EXTRA_DIST = no_optgiven.sh.in more_than_once.sh.in test_all_opts.sh.in \
test_groups.sh.in test_conf_parser.sh.in test_conf_parser_err.sh.in \
test_multiple.sh.in \
$(GENERATED_TESTS) \
$(TESTSOUTPUT) \
test_conf.conf test_conf_err.conf

$(TESTSOUTPUT):
	rm -f $(TESTSOUTPUT); \
        for source in $(TESTS); do \
	  $(top_builddir)/tests/$$source >> $@ 2>&1; \
	done

store-backup: $(BUILT_SOURCES) $(TESTSOUTPUT)
	rm -f *.test; \
	for source in $(BUILT_SOURCES); do \
	  back_output="`echo $$source.test`"; \
	  echo "saving $$back_output"; \
	  cp $$source $(srcdir)/$$back_output; \
	done 

check-diff:
	for source in $(BUILT_SOURCES); do \
	  back_output="`echo $$source.test`"; \
	  echo "diff $$source $$back_output"; \
	  if ! diff $$source $(srcdir)/$$back_output; \
	  then errors=1; fi; \
	done && \
	echo "creating output of tests..."; \
	rm -f $(TESTSOUT); \
        for tests in $(TESTS); do \
	  $(top_builddir)/tests/$$tests >> $(srcdir)/$(TESTSOUT) 2>&1 ; \
	done && \
        echo "diff $(TESTSOUTPUT) $(TESTSOUT)"; \
        if ! diff $(TESTSOUTPUT) $(srcdir)/$(TESTSOUT); \
        then errors=1; fi; \
        test -z $$errors

CLEANFILES = *_cmd.c *_cmd.h *-cmd.h *-cmd.c cmdline.c cmdline.h
