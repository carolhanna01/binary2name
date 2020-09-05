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

/*$RCSfile: iesdata.h,v $ $Revision: 1.2.2.1 $ */
/* Generic execution stack structure definition */

#ifndef iesdata_INCLUDED
#  define iesdata_INCLUDED

#include "isdata.h"

/* Define the execution stack structure. */
typedef struct exec_stack_s {

    ref_stack_t stack;		/* the actual execution stack */

/*
 * To improve performance, we cache the currentfile pointer
 * (i.e., `shallow-bind' it in Lisp terminology).  The invariant is as
 * follows: either esfile points to the currentfile slot on the estack
 * (i.e., the topmost slot with an executable file), or it is 0.
 * To maintain the invariant, it is sufficient that whenever a routine
 * pushes or pops anything on the estack, if the object *might* be
 * an executable file, invoke esfile_clear_cache(); alternatively,
 * immediately after pushing an object, invoke esfile_check_cache().
 */
    ref *current_file;

} exec_stack_t;

/*
 * current_file is cleared by garbage collection, so we don't declare it
 * as a pointer.
 */
#define public_st_exec_stack()	/* in interp.c */\
  gs_public_st_suffix_add0(st_exec_stack, exec_stack_t, "exec_stack_t",\
    exec_stack_enum_ptrs, exec_stack_reloc_ptrs, st_ref_stack)
#define st_exec_stack_num_ptrs st_ref_stack_num_ptrs

#endif /* iesdata_INCLUDED */
