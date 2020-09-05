/* Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: ifilter2.h,v $ $Revision: 1.2.2.1 $ */
/* Utilities for Level 2 filters */

#ifndef ifilter2_INCLUDED
#  define ifilter2_INCLUDED

/* Import setup code from zfdecode.c */
int zcf_setup(P3(os_ptr op, stream_CF_state * pcfs, gs_ref_memory_t *imem));
int zlz_setup(P2(os_ptr op, stream_LZW_state * plzs));
int zpd_setup(P2(os_ptr op, stream_PDiff_state * ppds));
int zpp_setup(P2(os_ptr op, stream_PNGP_state * ppps));

#endif /* ifilter2_INCLUDED */
