/* Copyright (C) 1993, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

/*$RCSfile: gsfname.h,v $ $Revision: 1.2.2.1 $ */

#ifndef gsfname_INCLUDED
#  define gsfname_INCLUDED

/*
 * Structure and procedures for parsing file names.
 *
 * Define a structure for representing a parsed file name, consisting of
 * an IODevice name in %'s, a file name, or both.  Note that the file name
 * may be either a gs_string (no terminator) or a C string (null terminator).
 *
 * NOTE: You must use parse_[real_]file_name to construct parsed_file_names.
 * Do not simply allocate the structure and fill it in.
 */
#ifndef gx_io_device_DEFINED
#  define gx_io_device_DEFINED
typedef struct gx_io_device_s gx_io_device;
#endif

typedef struct gs_parsed_file_name_s {
    gs_memory_t *memory;	/* allocator for terminated name string */
    gx_io_device *iodev;
    const char *fname;
    uint len;
} gs_parsed_file_name_t;

/* Parse a file name into device and individual name. */
int gs_parse_file_name(P3(gs_parsed_file_name_t *, const char *, uint));

/* Parse a real (non-device) file name and convert to a C string. */
int gs_parse_real_file_name(P5(gs_parsed_file_name_t *, const char *, uint,
			       gs_memory_t *, client_name_t));

/* Convert a file name to a C string by adding a null terminator. */
int gs_terminate_file_name(P3(gs_parsed_file_name_t *, gs_memory_t *,
			      client_name_t));

/* Free a file name that was copied to a C string. */
void gs_free_file_name(P2(gs_parsed_file_name_t *, client_name_t));

#endif /* gsfname_INCLUDED */
