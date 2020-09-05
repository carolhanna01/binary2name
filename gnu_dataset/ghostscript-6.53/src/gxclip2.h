/* Copyright (C) 1993, 1996, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gxclip2.h,v $ $Revision: 1.2.2.1 $ */
/* Tiled mask clipping device and interface */

#ifndef gxclip2_INCLUDED
#  define gxclip2_INCLUDED

#include "gxmclip.h"

/* The structure for tile clipping is the same as for simple mask clipping. */
typedef gx_device_mask_clip gx_device_tile_clip;
#define st_device_tile_clip st_device_mask_clip
/*
 * We can't just make this macro empty, since it is processed as a top-level
 * declaration and would lead to an extraneous semicolon.  The least damage
 * we can do is make it declare a constant (and not static, since static
 * would lead to a compiler warning about an unreferenced variable).
 */
#define private_st_device_tile_clip() /* in gxclip2.c */\
  const byte gxclip2_dummy = 0

/*
 * Initialize a tile clipping device from a mask.
 * We supply an explicit phase.
 */
int tile_clip_initialize(P6(gx_device_tile_clip * cdev,
			    const gx_strip_bitmap * tiles,
			    gx_device * tdev, int px, int py,
			    gs_memory_t *mem));

/*
 * Set the phase of the tile -- used in the tiling loop when
 * the tile doesn't simply fill the plane.
 */
void tile_clip_set_phase(P3(gx_device_tile_clip * cdev, int px, int py));

#endif /* gxclip2_INCLUDED */
