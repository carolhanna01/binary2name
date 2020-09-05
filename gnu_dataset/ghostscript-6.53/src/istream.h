/* Copyright (C) 1994, 1995, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: istream.h,v $ $Revision: 1.2.2.1 $ */
/* Interpreter support procedures for streams */
/* Requires scommon.h */

#ifndef istream_INCLUDED
#  define istream_INCLUDED

/* Procedures exported by zfproc.c */

	/* for zfilter.c - procedure stream initialization */
int sread_proc(P3(ref *, stream **, gs_ref_memory_t *));
int swrite_proc(P3(ref *, stream **, gs_ref_memory_t *));

	/* for interp.c, zfileio.c, zpaint.c - handle a procedure */
	/* callback or an interrupt */
int s_handle_read_exception(P6(i_ctx_t *, int, const ref *, const ref *,
			       int, op_proc_t));
int s_handle_write_exception(P6(i_ctx_t *, int, const ref *, const ref *,
				int, op_proc_t));

#endif /* istream_INCLUDED */
