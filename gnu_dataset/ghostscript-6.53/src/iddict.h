/* Copyright (C) 1992, 1996, 1997, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: iddict.h,v $ $Revision: 1.2.2.1 $ */
/* Dictionary API with implicit dict_stack argument */

#ifndef iddict_INCLUDED
#  define iddict_INCLUDED

#include "idict.h"
#include "icstate.h"		/* for access to dict_stack */

/* Define the dictionary stack instance for operators. */
#define idict_stack (i_ctx_p->dict_stack)

#define idict_put(pdref, key, pvalue)\
  dict_put(pdref, key, pvalue, &idict_stack)
#define idict_put_string(pdref, kstr, pvalue)\
  dict_put_string(pdref, kstr, pvalue, &idict_stack)
#define idict_undef(pdref, key)\
  dict_undef(pdref, key, &idict_stack)
#define idict_copy(dfrom, dto)\
  dict_copy(dfrom, dto, &idict_stack)
#define idict_copy_new(dfrom, dto)\
  dict_copy_new(dfrom, dto, &idict_stack)
#define idict_resize(pdref, newmax)\
  dict_resize(pdref, newmax, &idict_stack)
#define idict_grow(pdref)\
  dict_grow(pdref, &idict_stack)
#define idict_unpack(pdref)\
  dict_unpack(pdref, &idict_stack)

#endif /* iddict_INCLUDED */
