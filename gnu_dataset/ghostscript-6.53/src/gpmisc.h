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

/*$RCSfile: gpmisc.h,v $ $Revision: 1.2.2.1 $ */
/* Miscellaneous support for platform facilities */

#ifndef gpmisc_INCLUDED
#  define gpmisc_INCLUDED

/*
 * The facilities defined in this file and implemented in gpmisc.c are
 * generic utilities shared among multiple gp_ platform files.
 */

/*
 * Get the name of the directory for temporary files, if any.  Currently
 * this checks the TMPDIR and TEMP environment variables, in that order.
 * The return value and the setting of *ptr and *plen are as for gp_getenv.
 */
int gp_gettmpdir(P2(char *ptr, int *plen));

/*
 * Open a temporary file, using O_EXCL and S_IRWXU to prevent race
 * conditions and symlink attacks.
 */
FILE *gp_fopentemp(P2(const char *fname, const char *mode));

#endif /* gpmisc_INCLUDED */
