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

/*$RCSfile: gp_strdl.c,v $ $Revision: 1.2.2.1 $ */
/* Default, stream-based readline implementation */
#include "std.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gp.h"

int
gp_readline_init(void **preadline_data, gs_memory_t * mem)
{
    return 0;
}

int
gp_readline(stream *s_in, stream *s_out, void *readline_data,
	    gs_const_string *prompt, gs_string * buf,
	    gs_memory_t * bufmem, uint * pcount, bool *pin_eol,
	    bool (*is_stdin)(P1(const stream *)))
{
    return sreadline(s_in, s_out, readline_data, prompt, buf, bufmem, pcount,
		     pin_eol, is_stdin);
}

void
gp_readline_finit(void *readline_data)
{
}
