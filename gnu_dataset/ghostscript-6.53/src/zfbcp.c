/* Copyright (C) 1994, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zfbcp.c,v $ $Revision: 1.3.2.1 $ */
/* (T)BCP filter creation */
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "stream.h"
#include "strimpl.h"
#include "sbcp.h"
#include "ifilter.h"

/* Define null handlers for the BCP out-of-band signals. */
private int
no_bcp_signal_interrupt(stream_state * st)
{
    return 0;
}
private int
no_bcp_request_status(stream_state * st)
{
    return 0;
}

/* <source> BCPEncode/filter <file> */
/* <source> <dict> BCPEncode/filter <file> */
private int
zBCPE(i_ctx_t *i_ctx_p)
{
    return filter_write_simple(i_ctx_p, &s_BCPE_template);
}

/* <target> BCPDecode/filter <file> */
/* <target> <dict> BCPDecode/filter <file> */
private int
zBCPD(i_ctx_t *i_ctx_p)
{
    stream_BCPD_state state;

    state.signal_interrupt = no_bcp_signal_interrupt;
    state.request_status = no_bcp_request_status;
    return filter_read(i_ctx_p, 0, &s_BCPD_template, (stream_state *)&state, 0);
}

/* <source> TBCPEncode/filter <file> */
/* <source> <dict> TBCPEncode/filter <file> */
private int
zTBCPE(i_ctx_t *i_ctx_p)
{
    return filter_write_simple(i_ctx_p, &s_TBCPE_template);
}

/* <target> TBCPDecode/filter <file> */
/* <target> <dict> TBCPDecode/filter <file> */
private int
zTBCPD(i_ctx_t *i_ctx_p)
{
    stream_BCPD_state state;

    state.signal_interrupt = no_bcp_signal_interrupt;
    state.request_status = no_bcp_request_status;
    return filter_read(i_ctx_p, 0, &s_TBCPD_template, (stream_state *)&state, 0);
}

/* ------ Initialization procedure ------ */

const op_def zfbcp_op_defs[] =
{
    op_def_begin_filter(),
    {"1BCPEncode", zBCPE},
    {"1BCPDecode", zBCPD},
    {"1TBCPEncode", zTBCPE},
    {"1TBCPDecode", zTBCPD},
    op_def_end(0)
};
