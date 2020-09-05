/* Copyright (C) 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gximage3.h,v $ $Revision: 1.4.2.1 $ */
/* ImageType 3 internal API */

#ifndef gximage3_INCLUDED
#  define gximage3_INCLUDED

#include "gsiparm3.h"
#include "gxiparam.h"

/*
 * The machinery for splitting an ImageType3 image into pixel and mask data
 * is used both for imaging per se and for writing high-level output.
 * We implement this by making the procedures for setting up the mask image
 * and clipping devices virtual.
 */

/*
 * Make the mask image device -- the device that processes mask bits.
 * The device is closed and freed at the end of processing the image.
 */
#define IMAGE3_MAKE_MID_PROC(proc)\
  int proc(P5(gx_device **pmidev, gx_device *dev, int width, int height,\
	      gs_memory_t *mem))
typedef IMAGE3_MAKE_MID_PROC((*image3_make_mid_proc_t));

/*
 * Make the mask clip device -- the device that uses the mask image to
 * clip the (opaque) image data -- and its enumerator.
 * The device is closed and freed at the end of processing the image.
 */
#define IMAGE3_MAKE_MCDE_PROC(proc)\
  int proc(P13(/* The initial arguments are those of begin_typed_image. */\
	       gx_device *dev,\
	       const gs_imager_state *pis,\
	       const gs_matrix *pmat,\
	       const gs_image_common_t *pic,\
	       const gs_int_rect *prect,\
	       const gx_drawing_color *pdcolor,\
	       const gx_clip_path *pcpath, gs_memory_t *mem,\
	       gx_image_enum_common_t **pinfo,\
	       /* The following arguments are added. */\
	       gx_device **pmcdev, gx_device *midev,\
	       gx_image_enum_common_t *pminfo,\
	       const gs_int_point *origin))
typedef IMAGE3_MAKE_MCDE_PROC((*image3_make_mcde_proc_t));

/*
 * Begin processing an ImageType 3 image, with the mask device creation
 * procedures as additional parameters.
 */
int gx_begin_image3_generic(P11(gx_device * dev,
				const gs_imager_state *pis,
				const gs_matrix *pmat,
				const gs_image_common_t *pic,
				const gs_int_rect *prect,
				const gx_drawing_color *pdcolor,
				const gx_clip_path *pcpath, gs_memory_t *mem,
				IMAGE3_MAKE_MID_PROC((*make_mid)),
				IMAGE3_MAKE_MCDE_PROC((*make_mcde)),
				gx_image_enum_common_t **pinfo));

#endif /* gximage3_INCLUDED */
