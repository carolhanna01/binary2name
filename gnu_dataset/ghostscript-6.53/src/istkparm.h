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

/*$RCSfile: istkparm.h,v $ $Revision: 1.2.2.1 $ */
/* Parameter structure for expandable stacks of refs */

#ifndef istkparm_INCLUDED
#  define istkparm_INCLUDED

/*
 * Define the structure for stack parameters set at initialization.
 */
/*typedef struct ref_stack_params_s ref_stack_params_t;*/ /* in istack.h */
struct ref_stack_params_s {
    uint bot_guard;		/* # of guard elements below bot */
    uint top_guard;		/* # of guard elements above top */
    uint block_size;		/* size of each block */
    uint data_size;		/* # of data slots in each block */
    ref guard_value;		/* t__invalid or t_operator, */
				/* bottom guard value */
    int underflow_error;	/* error code for underflow */
    int overflow_error;		/* error code for overflow */
    bool allow_expansion;	/* if false, don't expand */
};
#define private_st_ref_stack_params() /* in istack.c */\
  gs_private_st_simple(st_ref_stack_params, ref_stack_params_t,\
    "ref_stack_params_t")

#endif /* istkparm_INCLUDED */
