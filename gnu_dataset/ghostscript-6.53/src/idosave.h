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

/*$RCSfile: idosave.h,v $ $Revision: 1.2.2.1 $ */
/* Supporting procedures for 'save' recording. */

#ifndef idosave_INCLUDED
#  define idosave_INCLUDED

/*
 * Save a change that must be undone by restore.  We have to pass the
 * pointer to the containing object to alloc_save_change for two reasons:
 *
 *      - We need to know which VM the containing object is in, so we can
 * know on which chain of saved changes to put the new change.
 *
 *      - We need to know whether the object is an array of refs (which
 * includes dictionaries) or a struct, so we can properly trace and
 * relocate the pointer to it from the change record during garbage
 * collection.
 */
int alloc_save_change(P4(gs_dual_memory_t *dmem, const ref *pcont,
			 ref_packed *ptr, client_name_t cname));
int alloc_save_change_in(P4(gs_ref_memory_t *mem, const ref *pcont,
			    ref_packed *ptr, client_name_t cname));

#endif /* idosave_INCLUDED */
