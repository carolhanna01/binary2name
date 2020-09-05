# Copyright (C) 2002 Lorenzo Bettini <bettini@gnu.org>
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# if gengen is not installed we simply ignore the changes
# FIXME: maybe we have to notify the developer about it

if NO_GENGEN
%.h: %.h_skel
	touch $@
else
GENERATE = $(GENGEN)

%.h: %.h_skel
	$(GENERATE) -i $< --file-name $@ --gen-name $*  --separate-files
endif

INCLUDES = -I@top_srcdir@/src

noinst_LIBRARIES = libgen.a

libgen_a_SOURCES = header.cc c_source.cc handle_help.cc handle_version.cc \
generic_option.cc no_short_option.cc required_option.cc \
string_opt_arg.cc int_opt_arg.cc long_opt_arg.cc float_opt_arg.cc \
flag_opt_arg.cc handle_unamed.cc \
generic_option_group.cc group_counter.cc group_option.cc \
print_group.cc \
config_parser_header.cc config_parser_handle_option.cc \
config_parser_source.cc config_parser_update_option.cc \
multiple_opt_list.cc multiple_opt_list.h multiple_option.cc multiple_option.h \
multiple_fill_array.cc multiple_fill_array.h 

BUILT_SOURCES = header.h c_source.h header.cc c_source.cc \
handle_help.h handle_version.h handle_help.cc handle_version.cc \
generic_option.h no_short_option.h required_option.h \
generic_option.cc no_short_option.cc required_option.cc \
string_opt_arg.h int_opt_arg.h long_opt_arg.h float_opt_arg.h \
string_opt_arg.cc int_opt_arg.cc long_opt_arg.cc float_opt_arg.cc \
flag_opt_arg.h handle_unamed.h \
flag_opt_arg.cc handle_unamed.cc \
generic_option_group.h group_counter.h group_option.h \
generic_option_group.cc group_counter.cc group_option.cc \
print_group.h print_group.cc \
config_parser_header.h config_parser_header.cc \
config_parser_source.h config_parser_source.cc \
config_parser_handle_option.h config_parser_handle_option.cc \
config_parser_update_option.h config_parser_update_option.cc \
multiple_opt_list.cc multiple_opt_list.h multiple_option.cc multiple_option.h \
multiple_fill_array.cc multiple_fill_array.h 

header.cc: header.h
c_source.cc: c_source.h
handle_help.cc: handle_help.h
handle_version.cc: handle_version.h
generic_option.cc: generic_option.h
no_short_option.cc: no_short_option.h
required_option.cc: required_option.h
string_opt_arg.cc: string_opt_arg.h
int_opt_arg.cc: int_opt_arg.h
long_opt_arg.cc: long_opt_arg.h
float_opt_arg.cc: float_opt_arg.h
flag_opt_arg.cc: flag_opt_arg.h
handle_unamed.cc: handle_unamed.h
multiple_opt_list.cc: multiple_opt_list.h
multiple_option.cc: multiple_option.h
multiple_fill_array.cc: multiple_fill_array.h 
#config_parser_header.cc: config_parser_header.h
#config_parser_source.cc: config_parser_source.h
#config_parser_handle_option.cc: config_parser_handle_option.h

EXTRA_DIST = header.h_skel c_source.h_skel handle_help.h_skel \
handle_version.h_skel generic_option.h_skel no_short_option.h_skel \
required_option.h_skel string_opt_arg.h_skel int_opt_arg.h_skel \
long_opt_arg.h_skel float_opt_arg.h_skel flag_opt_arg.h_skel \
handle_unamed.h_skel \
generic_option_group.h_skel group_counter.h_skel group_option.h_skel \
print_group.h_skel \
config_parser_header.h_skel config_parser_handle_option.h_skel \
config_parser_source.h_skel config_parser_update_option.h_skel \
multiple_opt_list.h_skel multiple_option.h_skel \
multiple_fill_array.h_skel \
$(BUILT_SOURCES)

built-clean:
	rm -f $(BUILT_SOURCES)