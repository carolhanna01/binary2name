/* Copyright (C) 1994, 1995, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: idebug.h,v $ $Revision: 1.2.2.1 $ */
/* Prototypes for debugging procedures in idebug.c */

#ifndef idebug_INCLUDED
#  define idebug_INCLUDED

/* Print individual values. */
void debug_print_name(P1(const ref *));
void debug_print_name_index(P1(uint /*name_index_t*/));
void debug_print_ref(P1(const ref *));
void debug_print_ref_packed(P1(const ref_packed *));

/* Dump regions of memory. */
void debug_dump_one_ref(P1(const ref *));
void debug_dump_refs(P3(const ref * from, uint size, const char *msg));
void debug_dump_array(P1(const ref * array));

/* Dump a stack.  Using this requires istack.h. */
#ifndef ref_stack_DEFINED
typedef struct ref_stack_s ref_stack_t;	/* also defined in isdata.h */
#  define ref_stack_DEFINED
#endif
void debug_dump_stack(P2(const ref_stack_t * pstack, const char *msg));

#endif /* idebug_INCLUDED */
