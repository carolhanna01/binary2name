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

/*$RCSfile: gxchrout.h,v $ $Revision: 1.2.2.1 $ */
/* Shared procedures for outline character rendering */

#ifndef gxchrout_INCLUDED
#  define gxchrout_INCLUDED

#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif

/*
 * Determine the flatness for rendering a character in an outline font.
 * This may be less than the flatness in the imager state.
 * The second argument is the default scaling for the font: 0.001 for
 * Type 1 fonts, 1.0 for TrueType fonts.
 */
double gs_char_flatness(P2(const gs_imager_state *pis, floatp default_scale));

#endif /* gxchrout_INCLUDED */
