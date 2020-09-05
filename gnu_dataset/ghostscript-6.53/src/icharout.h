/* Copyright (C) 1996, 1997, 1998, 1999, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: icharout.h,v $ $Revision: 1.2.2.2 $ */
/* Interface to zcharout.c */

#ifndef icharout_INCLUDED
#  define icharout_INCLUDED

/* Execute an outline defined by a PostScript procedure. */
int zchar_exec_char_proc(P1(i_ctx_t *));

/*
 * Get the metrics for a character from the Metrics dictionary of a base
 * font.  If present, store the l.s.b. in psbw[0,1] and the width in
 * psbw[2,3].
 */
typedef enum {
    metricsNone = 0,
    metricsWidthOnly = 1,
    metricsSideBearingAndWidth = 2
} metrics_present;
int /*metrics_present*/
  zchar_get_metrics(P3(const gs_font_base * pbfont, const ref * pcnref,
		       double psbw[4]));

/* Get the vertical metrics for a character from Metrics2, if present. */
int /*metrics_present*/
  zchar_get_metrics2(P3(const gs_font_base * pbfont, const ref * pcnref,
			double pwv[4]));

/*
 * Consult Metrics2 and CDevProc, and call setcachedevice[2].  Return
 * o_push_estack if we had to call a CDevProc, or if we are skipping the
 * rendering process (only getting the metrics).
 */
int zchar_set_cache(P9(i_ctx_t *i_ctx_p, const gs_font_base * pbfont,
		       const ref * pcnref, const double psb[2],
		       const double pwidth[2], const gs_rect * pbbox,
		       int (*cont_fill) (P1(i_ctx_t *)),
		       int (*cont_stroke) (P1(i_ctx_t *)),
		       const double Metrics2_sbw_default[4]));

/*
 * Get the CharString data corresponding to a glyph.  Return typecheck
 * if it isn't a string.
 */
int zchar_charstring_data(P3(gs_font *font, const ref *pgref,
			     gs_const_string *pstr));

/*
 * Enumerate the next glyph from a directory.  This is essentially a
 * wrapper around dict_first/dict_next to implement the enumerate_glyph
 * font procedure.
 */
int zchar_enumerate_glyph(P3(const ref *prdict, int *pindex, gs_glyph *pglyph));

#endif /* icharout_INCLUDED */
