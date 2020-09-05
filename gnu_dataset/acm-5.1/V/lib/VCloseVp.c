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

void
VCloseViewport(Viewport * v)
{

	v->Close(v);

	VDestroyPolygon(v->clipPoly);
	free(v->aPixel);
	free((char *) v->zpool);
	free((char *) v);
}

void
releaseVResources (void)
{
	VColor   *p = _VDefaultWorkContext->VColorList, *next;
	while (p) {
		free (p->color_name);
		next = p->next;
		free (p);
		p = next;
	}
	_VDefaultWorkContext->VColorList = NULL;
	if (_VDefaultWorkContext->visTable) {
		free(_VDefaultWorkContext->visTable);
		_VDefaultWorkContext->visTable = NULL;
	}
}
