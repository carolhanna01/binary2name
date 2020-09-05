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

int
VWriteObject(FILE * f, VObject * obj)
{

	int       i, j, k, points;
	VPolygon **q;
	VPoint   *p;

/*
 *  Total the number of vertices in all of the object's polygons
 */

	points = 0;
	q = obj->polygon;
	for (i = 0; i < obj->numPolys; ++i) {
		points += q[i]->numVtces;
	}

/*
 *  Print the header
 */

	fprintf(f, "%s\n%d %d\n", obj->name, points, obj->numPolys);

/*
 *  Print the point list
 */

	k = 1;
	q = obj->polygon;
	for (i = 0; i < obj->numPolys; ++i) {
		for ((j = 0, p = q[i]->vertex); j < q[i]->numVtces; (++p, ++j)) {
			fprintf(f, "%d %g %g %g\n", k, p->x, p->y, p->z);
			++k;
		}
	}

/*
 *  Print the polygon list
 */

	k = 1;
	q = obj->polygon;
	for (i = 0; i < obj->numPolys; ++i) {
		if (q[i]->backColor) {
			fprintf(f, "(%s %s) %d",
					q[i]->color->color_name,
					q[i]->backColor->color_name,
					q[i]->numVtces);;
		}
		else if (q[i]->flags & PolyClipBackface) {
			fprintf(f, "(%s clip) %d",
					q[i]->color->color_name,
					q[i]->numVtces);
		}
		else {
			fprintf(f, "%s %d",
					q[i]->color->color_name,
					q[i]->numVtces);
		}
		for (j = 0; j < q[i]->numVtces; ++j)
			fprintf(f, " %d", k++);
		fprintf(f, "\n");
	}

	return ferror(f) ? -1 : 0;
}
