/* Copyright (C) 2000 Artifex Software, Inc. All rights reserved.
  
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

/*$RCSfile: unistd_.h,v $ $Revision: 1.1.2.3 $ */
/* Generic substitute for Unix unistd.h */

#ifndef unistd__INCLUDED
#  define unistd__INCLUDED

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

/*
 * It's likely that you will have to edit the next lines on some Unix
 * and most non-Unix platforms, since there is no standard (ANSI or
 * otherwise) for where to find these definitions.
 */

#if defined(_MSC_VER) && defined(__WIN32__)
#  include <io.h>
#  define fsync(handle) _commit(handle)
#else
#  include <unistd.h>
#endif

#endif   /* unistd__INCLUDED */
