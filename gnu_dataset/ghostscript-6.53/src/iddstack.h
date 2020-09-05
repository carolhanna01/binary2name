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

/*$RCSfile: iddstack.h,v $ $Revision: 1.2.2.1 $ */
/* Dictionary stack API subset needed by idict.h */

#ifndef iddstack_INCLUDED
#  define iddstack_INCLUDED

#ifndef dict_stack_DEFINED
#  define dict_stack_DEFINED
typedef struct dict_stack_s dict_stack_t;
#endif

/*
 * Reset the cached top values.  Every routine that alters the
 * dictionary stack (including changing the protection or size of the
 * top dictionary on the stack) must call this.
 */
void dstack_set_top(P1(dict_stack_t *));

/* Check whether a dictionary is one of the permanent ones on the d-stack. */
bool dstack_dict_is_permanent(P2(const dict_stack_t *, const ref *));

#endif /* iddstack_INCLUDED */
