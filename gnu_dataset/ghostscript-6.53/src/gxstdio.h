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

/*$RCSfile: gxstdio.h,v $ $Revision: 1.2.2.1 $ */
/* stdio back door */

#ifndef gxstdio_INCLUDED
#  define gxstdio_INCLUDED

/*
 * The library and interpreter never use stdin/out/err directly, but
 * for the moment we have to keep them around for contributed drivers,
 * some of which write to stdout or stderr.
 */
#include "gsio.h"

#undef stdin
#define stdin gs_stdin
#undef stdout
#define stdout gs_stdout
#undef stderr
#define stderr gs_stderr
#undef fgetchar

#endif /* gxstdio_INCLUDED */

