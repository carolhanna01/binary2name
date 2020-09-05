/* Copyright (C) 1994, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: iparray.h,v $ $Revision: 1.2.2.1 $ */
/* Packed array constructor for Ghostscript */
/* Requires ipacked.h, istack.h */

#ifndef iparray_INCLUDED
#  define iparray_INCLUDED

/*
 * The only reason to put this in a separate header is that it requires
 * both ipacked.h and istack.h; putting it in either one would make it
 * depend on the other one.  There must be a better way....
 */

/* Procedures implemented in zpacked.c */

/* Make a packed array from the top N elements of a stack. */
int make_packed_array(P5(ref *, ref_stack_t *, uint, gs_dual_memory_t *,
			 client_name_t));

#endif /* iparray_INCLUDED */
