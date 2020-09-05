/* Copyright (C) 1995, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zfzlib.c,v $ $Revision: 1.3.2.1 $ */
/* zlib and Flate filter creation */
#include "ghost.h"
#include "oper.h"
#include "idict.h"
#include "strimpl.h"
#include "spdiffx.h"
#include "spngpx.h"
#include "szlibx.h"
#include "idparam.h"
#include "ifilter.h"
#include "ifrpred.h"
#include "ifwpred.h"

/* Common setup for zlib (Flate) filter */
private int
filter_zlib(i_ctx_t *i_ctx_p, stream_zlib_state *pzls)
{
    os_ptr op = osp;
    int code = 0;

    (*s_zlibE_template.set_defaults)((stream_state *)pzls);
    if (r_has_type(op, t_dictionary))
	code = dict_int_param(op, "Effort", -1, 9, -1, &pzls->level);
    return code;
}

/* <source> zlibEncode/filter <file> */
/* <source> <dict> zlibEncode/filter <file> */
private int
zzlibE(i_ctx_t *i_ctx_p)
{
    stream_zlib_state zls;
    int code = filter_zlib(i_ctx_p, &zls);

    if (code < 0)
	return code;
    return filter_write(i_ctx_p, 0, &s_zlibE_template, (stream_state *)&zls, 0);
}

/* <target> zlibDecode/filter <file> */
/* <target> <dict> zlibDecode/filter <file> */
private int
zzlibD(i_ctx_t *i_ctx_p)
{
    stream_zlib_state zls;

    (*s_zlibD_template.set_defaults)((stream_state *)&zls);
    return filter_read(i_ctx_p, 0, &s_zlibD_template, (stream_state *)&zls, 0);
}

/* <source> FlateEncode/filter <file> */
/* <source> <dict> FlateEncode/filter <file> */
private int
zFlateE(i_ctx_t *i_ctx_p)
{
    stream_zlib_state zls;
    int code = filter_zlib(i_ctx_p, &zls);

    if (code < 0)
	return code;
    return filter_write_predictor(i_ctx_p, 0, &s_zlibE_template,
				  (stream_state *)&zls);
}

/* <target> FlateDecode/filter <file> */
/* <target> <dict> FlateDecode/filter <file> */
private int
zFlateD(i_ctx_t *i_ctx_p)
{
    stream_zlib_state zls;

    (*s_zlibD_template.set_defaults)((stream_state *)&zls);
    return filter_read_predictor(i_ctx_p, 0, &s_zlibD_template,
				 (stream_state *)&zls);
}

/* ------ Initialization procedure ------ */

const op_def zfzlib_op_defs[] =
{
    op_def_begin_filter(),
    {"1zlibEncode", zzlibE},
    {"1zlibDecode", zzlibD},
    {"1FlateEncode", zFlateE},
    {"1FlateDecode", zFlateD},
    op_def_end(0)
};
