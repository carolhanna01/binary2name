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

/*$RCSfile: gp_mktmp.c,v $ $Revision: 1.2.2.1 $ */
/* Replacement for missing mktemp */
#include "stat_.h"
#include "string_.h"

/* This procedure simulates mktemp on platforms that don't provide it. */
char *
mktemp(char *fname)
{
    struct stat fst;
    int len = strlen(fname);
    char *end = fname + len - 6;
    
    if (len < 6 || strcmp(end, "XXXXXX"))
	return (char *)0;	/* invalid  */
    strcpy(end, "AA.AAA");

    while (stat(fname, &fst) == 0) {
	char *inc = fname + len - 1;

	while (*inc == 'Z' || *inc == '.') {
	    if (inc == end)
		return (char *)0;	/* failure */
	    if (*inc == 'Z')
		*inc = 'A';
	    --inc;
	}
	++*inc;
    }
    return fname;
}
