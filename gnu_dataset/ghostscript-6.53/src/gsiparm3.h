/* Copyright (C) 1997, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gsiparm3.h,v $ $Revision: 1.4.2.1 $ */
/* ImageType 3 image parameter definition */

#ifndef gsiparm3_INCLUDED
#  define gsiparm3_INCLUDED

#include "gsiparam.h"

/*
 * See Section 4.3 of the Adobe PostScript Version 3010 Supplement
 * for a definition of ImageType 3 and 4 images.
 */

/*
 * If InterleaveType is 3, the data source for the mask is provided as an
 * additional data source *before* the data sources for the pixel data.  For
 * both InterleaveType 2 and 3, the client is responsible for always
 * providing mask data before the pixel data that it masks.  (The
 * implementation does not currently check this, but it should.)
 */
typedef enum {
    interleave_chunky = 1,
    interleave_scan_lines = 2,
    interleave_separate_source = 3
} gs_image3_interleave_type_t;
typedef struct gs_image3_s {
    gs_pixel_image_common;	/* DataDict */
    int InterleaveType;
    gs_data_image_t MaskDict;
} gs_image3_t;

#define private_st_gs_image3()	/* in gximage3.c */\
  gs_private_st_suffix_add0(st_gs_image3, gs_image3_t, "gs_image3_t",\
    image3_enum_ptrs, image3_reloc_ptrs, st_gs_pixel_image)

/*
 * Initialize an ImageType 3 image.
 */
void gs_image3_t_init(P3(gs_image3_t * pim, const gs_color_space * color_space,
			 gs_image3_interleave_type_t interleave_type));

#endif /* gsiparm3_INCLUDED */
