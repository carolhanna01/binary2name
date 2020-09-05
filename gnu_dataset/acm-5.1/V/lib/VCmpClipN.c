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
#include <math.h>

void
VComputeClipNormals(Viewport * v)
{

	VPoint   *p, *q;
	int       i, max;
	double    mag;

	p = v->clipPoly->vertex;
	q = v->clipNormals;

/*
 *  We only reserved space for four clipping plane normals in clipNormals
 */

	max = (v->clipPoly->numVtces > 4) ? 4 : v->clipPoly->numVtces;

/*
 *  Compute the unit-normal vectors corresponding to each clipping plane
 */

	for (i = 0; i < max; ++i) {
		mag = sqrt(p->x * p->x + p->y * p->y + p->z * p->z);
		q->x = p->x / mag;
		q->y = p->y / mag;
		q->z = p->z / mag;
		q++;
		p++;
	}

}
