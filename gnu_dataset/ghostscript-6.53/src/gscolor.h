/* Copyright (C) 1991, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gscolor.h,v $ $Revision: 1.3.2.1 $ */
/* Client interface to color routines */

#ifndef gscolor_INCLUDED
#  define gscolor_INCLUDED

#include "gxtmap.h"

/* Color and gray interface */
int gs_setgray(P2(gs_state *, floatp));
int gs_currentgray(P2(const gs_state *, float *));
int gs_setrgbcolor(P4(gs_state *, floatp, floatp, floatp));
int gs_currentrgbcolor(P2(const gs_state *, float[3]));
int gs_setnullcolor(P1(gs_state *));

/* Transfer function */
int gs_settransfer(P2(gs_state *, gs_mapping_proc));
int gs_settransfer_remap(P3(gs_state *, gs_mapping_proc, bool));
gs_mapping_proc gs_currenttransfer(P1(const gs_state *));

#endif /* gscolor_INCLUDED */
