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

/*$RCSfile: icremap.h,v $ $Revision: 1.2.2.1 $ */
/* Interpreter color remapping structure */

#ifndef icremap_INCLUDED
#  define icremap_INCLUDED

#include "gsccolor.h"

/*
 * Define the structure used to communicate information back to the
 * interpreter for color remapping.  Pattern remapping doesn't use the
 * tint values, DeviceN remapping does.
 */
#ifndef int_remap_color_info_DEFINED
#  define int_remap_color_info_DEFINED
typedef struct int_remap_color_info_s int_remap_color_info_t;
#endif
struct int_remap_color_info_s {
    op_proc_t proc;		/* remapping procedure */
    float tint[GS_CLIENT_COLOR_MAX_COMPONENTS];
};

#define private_st_int_remap_color_info() /* in zgstate.c */\
  gs_private_st_simple(st_int_remap_color_info, int_remap_color_info_t,\
    "int_remap_color_info_t")

#endif /* icremap_INCLUDED */
