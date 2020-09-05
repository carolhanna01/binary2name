/* Copyright (C) 1992, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gdevpccm.h,v $ $Revision: 1.3.2.1 $ */
/* PC color mapping support */
/* Requires gxdevice.h */

#ifndef gdevpccm_INCLUDED
#  define gdevpccm_INCLUDED

/* Color mapping routines for EGA/VGA-style color. */
dev_proc_map_rgb_color(pc_4bit_map_rgb_color);
dev_proc_map_color_rgb(pc_4bit_map_color_rgb);
#define dci_pc_4bit dci_values(3, 4, 3, 2, 4, 3)

/* Color mapping routines for 8-bit color (with a fixed palette). */
dev_proc_map_rgb_color(pc_8bit_map_rgb_color);
dev_proc_map_color_rgb(pc_8bit_map_color_rgb);
#define dci_pc_8bit dci_values(3, 8, 6, 6, 7, 7)

/* Write the palette on a file. */
int pc_write_palette(P3(gx_device *, uint, FILE *));

#endif /* gdevpccm_INCLUDED */
