/* Copyright (C) 1995, 1996, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zrop.c,v $ $Revision: 1.2.2.1 $ */
/* RasterOp control operators */
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsrop.h"
#include "gsutil.h"
#include "gxdevice.h"
#include "idict.h"
#include "idparam.h"
#include "igstate.h"
#include "store.h"

/* <int8> .setrasterop - */
private int
zsetrasterop(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;
    int param;
    int code = int_param(op, 0xff, &param);

    if (code < 0)
	return code;
    gs_setrasterop(igs, (gs_rop3_t)param);
    pop(1);
    return 0;
}

/* - .currentrasterop <int8> */
private int
zcurrentrasterop(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;

    push(1);
    make_int(op, (int)gs_currentrasterop(igs));
    return 0;
}

/* <bool> .setsourcetransparent - */
private int
zsetsourcetransparent(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;

    check_type(*op, t_boolean);
    gs_setsourcetransparent(igs, op->value.boolval);
    pop(1);
    return 0;
}

/* - .currentsourcetransparent <bool> */
private int
zcurrentsourcetransparent(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;

    push(1);
    make_bool(op, gs_currentsourcetransparent(igs));
    return 0;
}

/* <bool> .settexturetransparent - */
private int
zsettexturetransparent(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;

    check_type(*op, t_boolean);
    gs_settexturetransparent(igs, op->value.boolval);
    pop(1);
    return 0;
}

/* - .currenttexturetransparent <bool> */
private int
zcurrenttexturetransparent(i_ctx_t *i_ctx_p)
{
    os_ptr op = osp;

    push(1);
    make_bool(op, gs_currenttexturetransparent(igs));
    return 0;
}

/* ------ Initialization procedure ------ */

const op_def zrop_op_defs[] =
{
    {"0.currentrasterop", zcurrentrasterop},
    {"0.currentsourcetransparent", zcurrentsourcetransparent},
    {"0.currenttexturetransparent", zcurrenttexturetransparent},
    {"1.setrasterop", zsetrasterop},
    {"1.setsourcetransparent", zsetsourcetransparent},
    {"1.settexturetransparent", zsettexturetransparent},
    op_def_end(0)
};
