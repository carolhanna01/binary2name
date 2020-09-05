/* Copyright (C) 1996, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gsflip.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to routines for "flipping" image data */

#ifndef gsflip_INCLUDED
#  define gsflip_INCLUDED

/*
 * Convert planar (MultipleDataSource) input to chunky format.  The input
 * data starts at planes[0] + offset ... planes[num_planes-1] + offset; the
 * output is stored at buffer.  This procedure assumes that the input
 * consists of an integral number of pixels; in particular, for 12-bit
 * input, nbytes is rounded up to a multiple of 3.  num_planes must be >=0;
 * bits_per_sample must be 1, 2, 4, 8, or 12.  Returns -1 if num_planes or
 * bits_per_sample is invalid, otherwise 0.
 */
extern int image_flip_planes(P6(byte * buffer, const byte ** planes,
				int offset, int nbytes,
				int num_planes, int bits_per_sample));

#endif /* gsflip_INCLUDED */
