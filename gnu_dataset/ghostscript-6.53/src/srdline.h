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

/*$RCSfile: srdline.h,v $ $Revision: 1.2.2.1 $ */
/* Interface for readline */
/* Requires gsmemory.h, gstypes.h */

#ifndef srdline_INCLUDED
#  define srdline_INCLUDED

/*
 * Read a line from s_in, starting at index *pcount in buf.  Start by
 * printing prompt on s_out.  If the string is longer than size - 1 (we need
 * 1 extra byte at the end for a null or an EOL), use bufmem to reallocate
 * buf; if bufmem is NULL, just return 1.  In any case, store in *pcount the
 * first unused index in buf.  *pin_eol is normally false; it should be set
 * to true (and true should be recognized) to indicate that the last
 * character read was a ^M, which should cause a following ^J to be
 * discarded.  is_stdin(s) returns true iff s is stdin: this is needed for
 * an obscure condition in the default implementation.
 */
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif
#define sreadline_proc(proc)\
  int proc(P9(stream *s_in, stream *s_out, void *readline_data,\
	      gs_const_string *prompt, gs_string *buf,\
	      gs_memory_t *bufmem, uint *pcount, bool *pin_eol,\
	      bool (*is_stdin)(P1(const stream *))))

/* Declare the default implementation. */
extern sreadline_proc(sreadline);

#endif /* srdline_INCLUDED */
