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
_VClipPolygon(VPolygon * poly, VPoint * clipPlane)
{

	register int j, lastj, numPts = 0, clipped = 0;
	double    d1, d2, a;
	VPoint    tmpPoint[VmaxVP];
	VPolygon *p;

	if (poly->numVtces > 0) {

		lastj = poly->numVtces - 1;
		d1 = VDotProd(&(poly->vertex[poly->numVtces - 1]), clipPlane);
		numPts = 0;

/*
 *  Examine each vertex and determine if it is inside or outside of the
 *  specified clipping plane.
 */

		for (j = 0; j < poly->numVtces; ++j) {

/* Leading vertex inside? */

			if (d1 > 0.0)
				tmpPoint[numPts++] = poly->vertex[lastj];

			d2 = VDotProd(&(poly->vertex[j]), clipPlane);

/* Does the edge straddle the window? If so, add a vertex on the window */

			if (d1 * d2 < 0.0) {
				clipped = 1;
				a = d1 / (d1 - d2);
				tmpPoint[numPts].x = a * poly->vertex[j].x +
					(1.0 - a) * poly->vertex[lastj].x;
				tmpPoint[numPts].y = a * poly->vertex[j].y +
					(1.0 - a) * poly->vertex[lastj].y;
				tmpPoint[numPts++].z = a * poly->vertex[j].z +
					(1.0 - a) * poly->vertex[lastj].z;
			}

			lastj = j;
			d1 = d2;
		}
	}

/*
 *  If the polygon was completely out of bounds, delete this polygon.
 */

	if (numPts == 0) {
		p = (VPolygon *) NULL;
		VDestroyPolygon(poly);
#ifdef DEBUG
		fprintf(stderr, "VClipPolygon: polygon outside area of interest\n");
#endif
	}

/*
 *  If we did any clipping, return the clipped polygon.
 */

	else if (clipped) {
		p = VCreatePolygonFromTemplate(numPts, tmpPoint, poly);
#ifdef DEBUG
		fprintf(stderr, "VClipPolygon: Polygon has been clipped:\n");
		fprintf(stderr, "Before Clipping:\n");
		VPrintPolygon(stderr, poly);
		fprintf(stderr, "\nAfter Clipping:\n\n");
		VPrintPolygon(stderr, p);
#endif
		VDestroyPolygon(poly);
	}
	else
		p = poly;

	return p;
}

VPolygon *
VClipPolygon(VPolygon * poly, VPolygon * clipPoly)
{

	int       i;
	VPolygon *p = poly;

/*
 *  Clip against each clipping plane supplied, one at a time.
 */

	for (i = 0; i < clipPoly->numVtces; ++i) {

		if (p == (VPolygon *) NULL)
			break;

		p = _VClipPolygon(p, &(clipPoly->vertex[i]));

	}

	return p;
}

VPolygon *						/*ARGSUSED */
VClipSidedPolygon(Viewport * v, VPolygon * poly, VPolygon * clipPoly)
{

	int       i;
	VPolygon *p = poly;

	if (p->flags & PolyNormalValid) {
		if (VDotProd(&p->vertex[0], &p->normal) >= 0.0) {
			if (p->backColor) {
				p->flags |= PolyUseBackColor;
			}
			else if (p->flags & PolyClipBackface) {
				VDestroyPolygon(p);
				return (VPolygon *) NULL;
			}
		}
		else {
			p->flags &= ~(PolyUseBackColor);
		}
	}

/*
 *  Clip against each clipping plane supplied, one at a time.
 */

	for (i = 0; i < clipPoly->numVtces; ++i) {

		if (p == (VPolygon *) NULL)
			break;

		p = _VClipPolygon(p, &(clipPoly->vertex[i]));

	}

	return p;
}
