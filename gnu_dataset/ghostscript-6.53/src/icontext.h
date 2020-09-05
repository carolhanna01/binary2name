/* Copyright (C) 1997, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: icontext.h,v $ $Revision: 1.2.2.1 $ */
/* Externally visible context state */
/* Requires iref.h, stdio_.h */

#ifndef icontext_INCLUDED
#  define icontext_INCLUDED

#include "gsstype.h"		/* for extern_st */
#include "icstate.h"

/* Declare the GC descriptor for context states. */
extern_st(st_context_state);

/*
 * Define the procedure for resetting user parameters when switching
 * contexts. This is defined in either zusparam.c or inouparm.c.
 */
extern int set_user_params(P2(i_ctx_t *i_ctx_p, const ref * paramdict));

/* Allocate the state of a context, always in local VM. */
/* If *ppcst == 0, allocate the state object as well. */
int context_state_alloc(P3(gs_context_state_t ** ppcst,
			   const ref *psystem_dict,
			   const gs_dual_memory_t * dmem));

/* Load the state of the interpreter from a context. */
/* The argument is not const because caches may be updated. */
int context_state_load(P1(gs_context_state_t *));

/* Store the state of the interpreter into a context. */
int context_state_store(P1(gs_context_state_t *));

/* Free the contents of the state of a context, always to its local VM. */
/* Return a mask of which of its VMs, if any, we freed. */
int context_state_free(P1(gs_context_state_t *));

#endif /* icontext_INCLUDED */
