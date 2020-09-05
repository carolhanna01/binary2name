/* options.h - struct options.
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#ifndef OPTIONS_H
#define OPTIONS_H

#define DEFAULT_CONTEXT    60
#define DEFAULT_DISTANCE   10
#define DEFAULT_OCCURRENCES  1

struct options {
  char *filename;
  unsigned int context;
  unsigned int distance;
  unsigned int occurrences;
  unsigned int show_help;
  unsigned int out_byte:1;
  unsigned int out_line:1;
  unsigned int out_quiet:1;
  unsigned int count_matches:1;
  unsigned int fixed_string:1;
  unsigned int ignore_case:1;
  unsigned int no_pattern:1;
  unsigned int with_pattern:1;
  unsigned int no_filename:1;
  unsigned int with_filename:1;
  unsigned int suppress_errors:1;
  unsigned int list_files:2;
};

extern struct options opt;

#endif
