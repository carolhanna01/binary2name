/* Copyright (C) 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: fcntl_.h,v $ $Revision: 1.3.2.1 $ */
/* Declaration of the O_* flags for open */

#ifndef fcntl__INCLUDED
#  define fcntl__INCLUDED

/*
 * This absurd little file is needed because Microsoft C, in defiance
 * of multiple standards, does not define the O_ modes for 'open'.
 */

/*
 * We must include std.h before any file that includes (or might include)
 * sys/types.h.
 */
#include "std.h"
#include <fcntl.h>

#if !defined(O_APPEND) && defined(_O_APPEND)
#  define O_APPEND _O_APPEND
#endif
#if !defined(O_BINARY) && defined(_O_BINARY)
#  define O_BINARY _O_BINARY
#endif
#if !defined(O_CREAT) && defined(_O_CREAT)
#  define O_CREAT _O_CREAT
#endif
#if !defined(O_EXCL) && defined(_O_EXCL)
#  define O_EXCL _O_EXCL
#endif
#if !defined(O_RDONLY) && defined(_O_RDONLY)
#  define O_RDONLY _O_RDONLY
#endif
#if !defined(O_RDWR) && defined(_O_RDWR)
#  define O_RDWR _O_RDWR
#endif
#if !defined(O_TRUNC) && defined(_O_TRUNC)
#  define O_TRUNC _O_TRUNC
#endif
#if !defined(O_WRONLY) && defined(_O_WRONLY)
#  define O_WRONLY _O_WRONLY
#endif

#endif /* fcntl__INCLUDED */
