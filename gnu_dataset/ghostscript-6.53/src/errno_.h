/* Copyright (C) 1993, 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: errno_.h,v $ $Revision: 1.2.2.1 $ */
/* Generic substitute for Unix errno.h */

#ifndef errno__INCLUDED
#  define errno__INCLUDED

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

/* All environments provide errno.h, but in some of them, errno.h */
/* only defines the error numbers, and doesn't declare errno. */
#include <errno.h>
#ifndef errno			/* in case it was #defined (very implausible!) */
extern int errno;

#endif

#endif /* errno__INCLUDED */
