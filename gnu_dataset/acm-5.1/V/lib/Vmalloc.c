/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1998  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */


#include "Vlib.h"

#ifdef WIN32
#undef Vmalloc
#endif

char     *
Vmalloc(int size)
{

	char     *p;

	if ((p = malloc(size)) == (char *) NULL) {
		fprintf(stderr, "V package memory allocation error.\n");
		fprintf(stderr, "An error was encountered allocating\
 %d bytes.\n", size);
		exit(1);
	}
	return p;
}

#ifdef WIN32

#include <crtdbg.h>

char     *
Vmalloc_dbg(int size, const char *file, const int line)
{

	char     *p;

	if ((p = _malloc_dbg(size, _NORMAL_BLOCK, file, line)) == (char *) NULL) {
		fprintf(stderr, "V package memory allocation error.\n");
		fprintf(stderr, "An error was encountered allocating\
 %d bytes.\n", size);
		exit(1);
	}
	return p;
}

#endif
