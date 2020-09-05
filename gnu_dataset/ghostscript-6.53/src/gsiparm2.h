/* Copyright (C) 1997, 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gsiparm2.h,v $ $Revision: 1.2.2.1 $ */
/* ImageType 2 image parameter definition */

#ifndef gsiparm2_INCLUDED
#  define gsiparm2_INCLUDED

#include "gsiparam.h"

/* Opaque type for a path */
#ifndef gx_path_DEFINED
#  define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif

/*
 * See Section 7.1 of the Adobe PostScript Version 3010 Supplement
 * for a definition of ImageType 2 images.
 */

typedef struct gs_image2_s {
    gs_image_common;
    gs_state *DataSource;
    float XOrigin, YOrigin;
    float Width, Height;
    /*
     * If UnpaintedPath is not 0, any unpainted path will be appended to it.
     */
    gx_path *UnpaintedPath;
    bool PixelCopy;
} gs_image2_t;
#define private_st_gs_image2()	/* in gximage2.c */\
  extern_st(st_gs_image_common);\
  gs_private_st_suffix_add2(st_gs_image2, gs_image2_t, "gs_image2_t",\
    image2_enum_ptrs, image2_reloc_ptrs, st_gs_image_common,\
    DataSource, UnpaintedPath)

/*
 * Initialize an ImageType 2 image.  Defaults:
 *      UnpaintedPath = 0
 *      PixelCopy = false
 */
void gs_image2_t_init(P1(gs_image2_t * pim));

#endif /* gsiparm2_INCLUDED */
