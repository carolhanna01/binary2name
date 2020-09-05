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

/*$RCSfile: iscanbin.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to binary token scanner */

#ifndef iscanbin_INCLUDED
#  define iscanbin_INCLUDED

/*
 * Scan a binary token.  The main scanner calls this iff recognize_btokens()
 * is true.  Return e_unregistered if Level 2 features are not included.
 * Return 0 or scan_BOS on success, <0 on failure.
 *
 * This header file exists only because there are two implementations of
 * this procedure: a dummy one for Level 1 systems, and the real one.
 * The interface is entirely internal to the scanner.
 */
int scan_binary_token(P4(i_ctx_t *i_ctx_p, stream *s, ref *pref,
			 scanner_state *pstate));

#endif /* iscanbin_INCLUDED */
