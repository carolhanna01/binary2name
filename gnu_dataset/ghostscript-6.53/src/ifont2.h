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

/*$RCSfile: ifont2.h,v $ $Revision: 1.2.2.1 $ */
/* Type 2 font utilities 2 */

#ifndef ifont2_INCLUDED
#  define ifont2_INCLUDED

/* Declare the Type 2 interpreter. */
extern charstring_interpret_proc(gs_type2_interpret);

/* Default value of lenIV */
#define DEFAULT_LENIV_2 (-1)

/*
 * Get the additional parameters for a Type 2 font (or FontType 2 FDArray
 * entry in a CIDFontType 0 font), beyond those common to Type 1 and Type 2
 * fonts.
 */
int type2_font_params(P3(const_os_ptr op, charstring_font_refs_t *pfr,
			 gs_type1_data *pdata1));

#endif /* ifont2_INCLUDED */
