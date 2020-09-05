/* Copyright (C) 1997, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zpaint.c,v $ $Revision: 1.2.2.1 $ */
/* Painting operators */
#include "ghost.h"
#include "oper.h"
#include "gspaint.h"
#include "igstate.h"

/* - fill - */
private int
zfill(i_ctx_t *i_ctx_p)
{
    return gs_fill(igs);
}

/* - eofill - */
private int
zeofill(i_ctx_t *i_ctx_p)
{
    return gs_eofill(igs);
}

/* - stroke - */
private int
zstroke(i_ctx_t *i_ctx_p)
{
    return gs_stroke(igs);
}

/* ------ Non-standard operators ------ */

/* - .fillpage - */
private int
zfillpage(i_ctx_t *i_ctx_p)
{
    return gs_fillpage(igs);
}

/* <width> <height> <data> .imagepath - */
private int
zimagepath(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;
    int code;

    check_type(op[-2], t_integer);
    check_type(op[-1], t_integer);
    check_read_type(*op, t_string);
    if (r_size(op) < ((op[-2].value.intval + 7) >> 3) * op[-1].value.intval)
	return_error(e_rangecheck);
    code = gs_imagepath(igs,
			(int)op[-2].value.intval, (int)op[-1].value.intval,
			op->value.const_bytes);
    if (code >= 0)
	pop(3);
    return code;
}

/* ------ Initialization procedure ------ */

const op_def zpaint_op_defs[] =
{
    {"0eofill", zeofill},
    {"0fill", zfill},
    {"0stroke", zstroke},
		/* Non-standard operators */
    {"0.fillpage", zfillpage},
    {"3.imagepath", zimagepath},
    op_def_end(0)
};
