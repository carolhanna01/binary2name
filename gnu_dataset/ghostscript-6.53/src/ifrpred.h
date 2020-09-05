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

/*$RCSfile: ifrpred.h,v $ $Revision: 1.2.2.1 $ */
/* filter_read_predictor prototype */

#ifndef ifrpred_INCLUDED
#  define ifrpred_INCLUDED

/* Exported by zfdecode.c for zfzlib.c */
int filter_read_predictor(P4(i_ctx_t *i_ctx_p, int npop,
			     const stream_template * template,
			     stream_state * st));

#endif /* ifrpred_INCLUDED */
