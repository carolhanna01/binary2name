/* Copyright (C) 1997, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: iimage2.h,v $ $Revision: 1.2.2.1 $ */
/* Level 2 image operator support */
/* Requires gsiparam.h */

#ifndef iimage2_INCLUDED
#  define iimage2_INCLUDED

/* These procedures are exported by zimage2.c for other modules. */

/*
 * Define a structure for image parameters other than those defined
 * in the gs_*image*_t structure.
 */
typedef struct image_params_s {
    bool MultipleDataSources;
    ref DataSource[gs_image_max_components];
    const float *pDecode;
} image_params;

/* Extract and check parameters for an image. */
int data_image_params(P6(const ref *op, gs_data_image_t *pim,
			 image_params *pip, bool require_DataSource,
			 int num_components, int max_bits_per_component));
int pixel_image_params(P5(i_ctx_t *i_ctx_p, const ref *op,
			  gs_pixel_image_t *pim, image_params * pip,
			  int max_bits_per_component));

/*
 * Process an image that has no explicit source data.  This isn't used by
 * standard Level 2, but it's a very small procedure and is needed by
 * both zdps.c and zdpnext.c.
 */
int process_non_source_image(P3(i_ctx_t *i_ctx_p,
				const gs_image_common_t * pim,
				client_name_t cname));

#endif /* iimage2_INCLUDED */
