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
#include <string.h>

VColor   *
VAllocColor(char *name)
{
	return VAllocDepthCueuedColor(name, 0);
}

VColor   *
VAllocDepthCueuedColor(char *name, int flag)
{

	VColor   *p = _VDefaultWorkContext->VColorList, *prev = 0, **q;

/*
 *  Search for this color among those already allocated.
 */

	while (p != (VColor *) 0) {
		if (strcmp(p->color_name, name) == 0) {
			if ((flag && (p->flags & ColorEnableDepthCueing)) ||
				(flag == 0 && p->flags == 0)) {
				return p;
			}
		}
		prev = p;
		p = p->next;
	}

/*
 *  The color was not in the list; allocate a new list element.
 */

	if (prev == (VColor *) 0)
		q = &_VDefaultWorkContext->VColorList;
	else
		q = &(prev->next);

	*q = (VColor *) Vmalloc(sizeof(VColor));
	(*q)->color_name = strdup(name);
	(*q)->cIndex = 0;
	(*q)->flags = flag ? ColorEnableDepthCueing : 0;
	(*q)->next = 0;

	return *q;
}
