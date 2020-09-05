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

/*$RCSfile: iosdata.h,v $ $Revision: 1.2.2.1 $ */
/* Generic operand stack API */

#ifndef iosdata_INCLUDED
#  define iosdata_INCLUDED

#include "isdata.h"

/* Define the operand stack structure. */
/* Currently this is just a generic ref stack. */
typedef struct op_stack_s {

    ref_stack_t stack;		/* the actual operand stack */

} op_stack_t;

#define public_st_op_stack()	/* in interp.c */\
  gs_public_st_suffix_add0(st_op_stack, op_stack_t, "op_stack_t",\
    op_stack_enum_ptrs, op_stack_reloc_ptrs, st_ref_stack)
#define st_op_stack_num_ptrs st_ref_stack_num_ptrs

#endif /* iosdata_INCLUDED */
