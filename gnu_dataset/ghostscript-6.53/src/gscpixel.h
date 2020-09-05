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

/*$RCSfile: gscpixel.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to DevicePixel color space */
/* Requires gscspace.h */

#ifndef gscpixel_INCLUDED
#  define gscpixel_INCLUDED

/* Initialize a DevicePixel color space. */
int gs_cspace_init_DevicePixel(P2(gs_color_space * pcs, int depth));

#endif /* gscpixel_INCLUDED */
