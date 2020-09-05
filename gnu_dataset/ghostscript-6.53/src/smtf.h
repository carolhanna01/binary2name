/* Copyright (C) 1995 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: smtf.h,v $ $Revision: 1.2.2.1 $ */
/* Definitions for MoveToFront filters */
/* Requires scommon.h; strimpl.h if any templates are referenced */

#ifndef smtf_INCLUDED
#  define smtf_INCLUDED

/* MoveToFrontEncode/Decode */
typedef struct stream_MTF_state_s {
    stream_state_common;
    /* The following change dynamically. */
    union _p {
	ulong l[256 / sizeof(long)];
	byte b[256];
    } prev;
} stream_MTF_state;
typedef stream_MTF_state stream_MTFE_state;
typedef stream_MTF_state stream_MTFD_state;

#define private_st_MTF_state()	/* in sbwbs.c */\
  gs_private_st_simple(st_MTF_state, stream_MTF_state,\
    "MoveToFrontEncode/Decode state")
extern const stream_template s_MTFE_template;
extern const stream_template s_MTFD_template;

#endif /* smtf_INCLUDED */
