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

/*$RCSfile: itoken.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to exported procedures in ztoken.c */

#ifndef itoken_INCLUDED
#  define itoken_INCLUDED

/*
 * Continue after handling a procedure stream refill or other callout
 * while reading tokens in the interpreter.
 */
int ztokenexec_continue(P1(i_ctx_t *i_ctx_p));

/*
 * Handle a scan_Comment or scan_DSC_Comment return from scan_token.
 */
#ifndef scanner_state_DEFINED
#  define scanner_state_DEFINED
typedef struct scanner_state_s scanner_state;
#endif
int ztoken_handle_comment(P8(i_ctx_t *i_ctx_p, const ref *fop,
			     scanner_state *sstate, const ref *ptoken,
			     int scan_code, bool save, bool push_file,
			     op_proc_t cont));

/*
 * Update the cached scanner_options in the context state after doing a
 * setuserparams.  (We might move this procedure somewhere else eventually.)
 */
int ztoken_scanner_options(P2(const ref *upref, int old_options));

#endif /* itoken_INCLUDED */
