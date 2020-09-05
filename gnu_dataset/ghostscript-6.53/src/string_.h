/* Copyright (C) 1989, 1992, 1993, 1994, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: string_.h,v $ $Revision: 1.2.2.1 $ */
/* Generic substitute for Unix string.h */

#ifndef string__INCLUDED
#  define string__INCLUDED

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

#ifdef BSD4_2
#  include <strings.h>
#  define strchr index
#else
#  ifdef MEMORY__NEED_MEMMOVE
#    undef memmove		/* This is disgusting, but so is GCC */
#  endif
#  include <string.h>
#  if defined(THINK_C)
	/* Patch strlen to return a uint rather than a size_t. */
#    define strlen (uint)strlen
#  endif
#  ifdef MEMORY__NEED_MEMMOVE
#    define memmove(dest,src,len) gs_memmove(dest,src,len)
#  endif
#endif

#endif /* string__INCLUDED */
