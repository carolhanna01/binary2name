/* Copyright (C) 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gxp1impl.h,v $ $Revision: 1.2.2.1 $ */
/* PatternType 1 implementation interface */
/* Requires gxpcolor.h */

#ifndef gxp1impl_INCLUDED
#  define gxp1impl_INCLUDED

/*
 * Declare the filling algorithms implemented in gxp1fill.c.
 * We use 'masked_fill_rect' instead of 'masked_fill_rectangle'
 * in order to limit identifier lengths to 32 characters.
 */
dev_color_proc_fill_rectangle(gx_dc_pattern_fill_rectangle);
dev_color_proc_fill_rectangle(gx_dc_pure_masked_fill_rect);
dev_color_proc_fill_rectangle(gx_dc_binary_masked_fill_rect);
dev_color_proc_fill_rectangle(gx_dc_colored_masked_fill_rect);

/*
 * Declare the Pattern color mapping procedures exported by gxpcmap.c.
 */
int gx_pattern_load(P4(gx_device_color *, const gs_imager_state *,
		       gx_device *, gs_color_select_t));
pattern_proc_remap_color(gs_pattern1_remap_color);

#endif /* gxp1impl_INCLUDED */
