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

/*$RCSfile: gxchrout.c,v $ $Revision: 1.2.2.1 $ */
#include "math_.h"
#include "gx.h"
#include "gxchrout.h"
#include "gxfarith.h"
#include "gxistate.h"

/*
 * Determine the flatness for rendering a character in an outline font.
 * This may be less than the flatness in the imager state.
 * The second argument is the default scaling for the font: 0.001 for
 * Type 1 fonts, 1.0 for TrueType fonts.
 */
double
gs_char_flatness(const gs_imager_state *pis, floatp default_scale)
{
    /*
     * Set the flatness to a value that is likely to produce reasonably
     * good-looking curves, regardless of its current value in the
     * graphics state.  If the character is very small, set the flatness
     * to zero, which will produce very accurate curves.
     */
    double cxx = fabs(pis->ctm.xx), cyy = fabs(pis->ctm.yy);

    if (is_fzero(cxx) || (cyy < cxx && !is_fzero(cyy)))
	cxx = cyy;
    if (!is_xxyy(&pis->ctm)) {
	double cxy = fabs(pis->ctm.xy), cyx = fabs(pis->ctm.yx);

	if (is_fzero(cxx) || (cxy < cxx && !is_fzero(cxy)))
	    cxx = cxy;
	if (is_fzero(cxx) || (cyx < cxx && !is_fzero(cyx)))
	    cxx = cyx;
    }
    /* Correct for the default scaling. */
    cxx *= 0.001 / default_scale;
    /* Don't let the flatness be worse than the default. */
    if (cxx > pis->flatness)
	cxx = pis->flatness;
    /* If the character is tiny, force accurate curves. */
    if (cxx < 0.2)
	cxx = 0;
    return cxx;
}
