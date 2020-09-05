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

/*$RCSfile: ifont1.h,v $ $Revision: 1.3.2.1 $ */
/* Type 1 font utilities shared with Type 2 */

#ifndef ifont1_INCLUDED
#  define ifont1_INCLUDED

/*
 * Define the temporary structure for holding pointers to substructures of a
 * CharString-based font.  This is used for parsing Type 1, 2, and 4 fonts.
 */
typedef struct charstring_font_refs_s {
    ref *Private;
    ref no_subrs;
    ref *OtherSubrs;
    ref *Subrs;
    ref *GlobalSubrs;
} charstring_font_refs_t;

/* Define the default lenIV value for a Type 1 font. */
#define DEFAULT_LENIV_1 4

/*
 * Parse the substructures of a CharString-based font.
 */
int charstring_font_get_refs(P2(const_os_ptr op, charstring_font_refs_t *pfr));

/*
 * Get the parameters of a CharString-based font or a FDArray entry for a
 * CIDFontType 0 font.  The client has filled in pdata1->interpret,
 * subroutineNumberBias, lenIV, and (if applicable) the Type 2 elements.
 */
int charstring_font_params(P3(const_os_ptr op, charstring_font_refs_t *pfr,
			      gs_type1_data *pdata1));

/*
 * Fill in a newly built CharString-based font or FDArray entry.
 */
int charstring_font_init(P3(gs_font_type1 *pfont,
			    const charstring_font_refs_t *pfr,
			    const gs_type1_data *pdata1));

/*
 * Finish building a CharString-based font.  The client has filled in the
 * same elements as for charstring_font_params.
 */
int build_charstring_font(P7(i_ctx_t *i_ctx_p, os_ptr op,
			     build_proc_refs * pbuild, font_type ftype,
			     charstring_font_refs_t *pfr,
			     gs_type1_data *pdata1,
			     build_font_options_t options));

#endif /* ifont1_INCLUDED */
