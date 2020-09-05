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

/*$RCSfile: gsccolor.h,v $ $Revision: 1.2.2.1 $ */
/* Client color structure definition */

#ifndef gsccolor_INCLUDED
#  define gsccolor_INCLUDED

#include "gsstype.h"		/* for extern_st */

/* Pattern instance, usable in color. */
#ifndef gs_pattern_instance_DEFINED
#  define gs_pattern_instance_DEFINED
typedef struct gs_pattern_instance_s gs_pattern_instance_t;
#endif

/*
 * Define the maximum number of components in a client color.
 * This must be at least 4, and should be at least 6 to accommodate
 * hexachrome DeviceN color spaces.
 */
#define GS_CLIENT_COLOR_MAX_COMPONENTS 6

/* Paint (non-Pattern) colors */
typedef struct gs_paint_color_s {
    float values[GS_CLIENT_COLOR_MAX_COMPONENTS];
} gs_paint_color;

/* General colors */
#ifndef gs_client_color_DEFINED
#  define gs_client_color_DEFINED
typedef struct gs_client_color_s gs_client_color;

#endif
struct gs_client_color_s {
    gs_paint_color paint;	/* also color for uncolored pattern */
    gs_pattern_instance_t *pattern;
};

extern_st(st_client_color);
#define public_st_client_color() /* in gscolor.c */\
  gs_public_st_ptrs1(st_client_color, gs_client_color, "gs_client_color",\
    client_color_enum_ptrs, client_color_reloc_ptrs, pattern)
#define st_client_color_max_ptrs 1

#endif /* gsccolor_INCLUDED */
