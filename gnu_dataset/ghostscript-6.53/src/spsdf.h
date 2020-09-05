/* Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: spsdf.h,v $ $Revision: 1.2.2.1 $ */
/* Common output syntax and parameters for PostScript and PDF writers */

#ifndef spsdf_INCLUDED
#  define spsdf_INCLUDED

#include "gsparam.h"

/* Define an opaque type for streams. */
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif

/* ---------------- Symbolic data printing ---------------- */

/* Print a PostScript string in the most efficient form. */
#define PRINT_BINARY_OK 1
#define PRINT_ASCII85_OK 2
#define PRINT_HEX_NOT_OK 4
void s_write_ps_string(P4(stream * s, const byte * str, uint size,
			  int print_ok));

/*
 * Create a stream that just keeps track of how much has been written
 * to it.  We use this for measuring data that will be stored rather
 * than written to an actual stream.
 */
int s_alloc_position_stream(P2(stream ** ps, gs_memory_t * mem));

/*
 * Create/release a parameter list for printing (non-default) filter
 * parameters.  This should probably migrate to a lower level....
 */
typedef struct param_printer_params_s {
    const char *prefix;		/* before entire object, if any params */
    const char *suffix;		/* after entire object, if any params */
    const char *item_prefix;	/* before each param */
    const char *item_suffix;	/* after each param */
    int print_ok;
} param_printer_params_t;
/*
 * The implementation structure should be opaque, but there are a few
 * clients that need to be able to stack-allocate it.
 */
typedef struct printer_param_list_s {
    gs_param_list_common;
    stream *strm;
    param_printer_params_t params;
    bool any;
} printer_param_list_t;
#define private_st_printer_param_list()	/* in spsdf.c */\
  gs_private_st_ptrs1(st_printer_param_list, printer_param_list_t,\
    "printer_param_list_t", printer_plist_enum_ptrs, printer_plist_reloc_ptrs,\
    strm)

#define param_printer_params_default_values 0, 0, 0, "\n", 0
extern const param_printer_params_t param_printer_params_default;
int s_alloc_param_printer(P4(gs_param_list ** pplist,
			     const param_printer_params_t * ppp, stream * s,
			     gs_memory_t * mem));
void s_free_param_printer(P1(gs_param_list * plist));
/* Initialize or release a list without allocating or freeing it. */
int s_init_param_printer(P3(printer_param_list_t *prlist,
			    const param_printer_params_t * ppp, stream * s));
void s_release_param_printer(P1(printer_param_list_t *prlist));


#endif /* spsdf_INCLUDED */
