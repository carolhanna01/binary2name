/* Copyright (C) 1994, 1995, 1997 artofcode LLC.  All rights reserved.
  
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

#ifndef __sys_types_h__
#define __sys_types_h__

#include <MacTypes.h>
#include <unix.h>
#define CHECK_INTERRUPTS
#define SYSTIME_H
#define HAVE_SYS_TIME_H

#define main gs_main

#if (0)
#define fprintf myfprintf
#define fputs myfputs
#define getenv mygetenv
int myfprintf(FILE *file, const char *fmt, ...);
int myfputs(const char *string, FILE *file);
#endif

#ifndef __MACINTOSH__
#define __MACINTOSH__
#endif

#endif /* __sys_types_h__ */
