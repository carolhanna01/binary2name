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
VCreatePolygon(int numVtces, VPoint * vert, VColor * color)
{
	VPolygon  template;

	template.color = color;
	template.backColor = (VColor *) NULL;
	template.flags = 0;
	template.assignedDepth = -1;
	template.cullDistance = 0.0;

	return VCreatePolygonFromTemplate(numVtces, vert, &template);
}

VPolygon *
VCreatePolygonFromTemplate(int numVtces, VPoint * vert, VPolygon * template)
{
	VPolygon *p;
	VPoint    a, b;

	p = (VPolygon *) Vmalloc(sizeof(VPolygon));

	*p = *template;
	p->numVtces = numVtces;
	p->vertex = (VPoint *) Vmalloc(sizeof(VPoint) * numVtces);
	memcpy((char *) p->vertex,
		   (char *) vert, sizeof(VPoint) * p->numVtces);

	if ((p->flags & PolyNormalValid) == 0) {
		if ((p->flags & PolyClipBackface) != 0 ||
			p->backColor != (VColor *) NULL) {
			a.x = vert[0].x - vert[1].x;
			a.y = vert[0].y - vert[1].y;
			a.z = vert[0].z - vert[1].z;
			b.x = vert[2].x - vert[1].x;
			b.y = vert[2].y - vert[1].y;
			b.z = vert[2].z - vert[1].z;
			VCrossProd(&a, &b, &p->normal);
			p->flags |= PolyNormalValid;
		}
	}

	return p;
}
