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

/*$RCSfile: sa85d.h,v $ $Revision: 1.2.2.2 $ */
/* ASCII85Decode filter interface */
/* Requires scommon.h; strimpl.h if any templates are referenced */

#ifndef sa85d_INCLUDED
#  define sa85d_INCLUDED

/* ASCII85Decode */
typedef struct stream_A85D_state_s {
    stream_state_common;
    int odd;			/* # of odd digits */
    ulong word;			/* word being accumulated */
} stream_A85D_state;

#define private_st_A85D_state()	/* in sfilter2.c */\
  gs_private_st_simple(st_A85D_state, stream_A85D_state,\
    "ASCII85Decode state")
/* We define the initialization procedure here, so that the scanner */
/* can avoid a procedure call. */
#define s_A85D_init_inline(ss)\
  ((ss)->min_left = 1, (ss)->word = 0, (ss)->odd = 0)
extern const stream_template s_A85D_template;

#endif /* sa85d_INCLUDED */
