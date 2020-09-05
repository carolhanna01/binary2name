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

VPolygon *
VGetPlanes(VPolygon * poly)
{

	VPoint    tmp[64], *p;
	int       i, lasti;

	lasti = poly->numVtces - 1;
	p = poly->vertex;

	for (i = 0; i < poly->numVtces; ++i) {
		tmp[i].x = p->y * poly->vertex[lasti].z - p->z *
			poly->vertex[lasti].y;
		tmp[i].y = p->z * poly->vertex[lasti].x - p->x *
			poly->vertex[lasti].z;
		tmp[i].z = p->x * poly->vertex[lasti].y - p->y *
			poly->vertex[lasti].x;
		lasti = i;
		p++;
	}

	for (i = 0; i < poly->numVtces; ++i)
		poly->vertex[i] = tmp[i];

	return poly;
}
