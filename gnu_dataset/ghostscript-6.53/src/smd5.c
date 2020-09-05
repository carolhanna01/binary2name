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

/*$RCSfile: smd5.c,v $ $Revision: 1.3.2.1 $ */
/* MD5Encode filter */
#include "memory_.h"
#include "strimpl.h"
#include "smd5.h"

/* ------ MD5Encode ------ */

private_st_MD5E_state();

/* Initialize the state. */
private int
s_MD5E_init(stream_state * st)
{
    stream_MD5E_state *const ss = (stream_MD5E_state *) st;

    md5_init(&ss->md5);
    return 0;
}

/* Process a buffer. */
private int
s_MD5E_process(stream_state * st, stream_cursor_read * pr,
	       stream_cursor_write * pw, bool last)
{
    stream_MD5E_state *const ss = (stream_MD5E_state *) st;
    int status = 0;

    if (pr->ptr < pr->limit) {
	md5_append(&ss->md5, pr->ptr + 1, pr->limit - pr->ptr);
	pr->ptr = pr->limit;
    }
    if (last) {
	if (pw->limit - pw->ptr >= 16) {
	    md5_finish(&ss->md5, pw->ptr + 1);
	    pw->ptr += 16;
	    status = EOFC;
	} else
	    status = 1;
    }
    return status;
}

/* Stream template */
const stream_template s_MD5E_template = {
    &st_MD5E_state, s_MD5E_init, s_MD5E_process, 1, 16
};
