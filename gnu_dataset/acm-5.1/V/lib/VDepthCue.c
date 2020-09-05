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
VSetVisibility(double d)
{
	int       i;
	double    k;

	_VDefaultWorkContext->visibility = d;
	if (_VDefaultWorkContext->visTable) {
		free((char *) _VDefaultWorkContext->visTable);
	}

	_VDefaultWorkContext->visTable = (double *) Vmalloc(sizeof(double) *
									_VDefaultWorkContext->depthCueSteps);

	k = log(1.0 / (double) _VDefaultWorkContext->depthCueSteps) /
		_VDefaultWorkContext->visibility;

#ifdef DEBUG
	fprintf(stderr, "k = %lg\n", k);
#endif

	for (i = 0; i < _VDefaultWorkContext->depthCueSteps - 1; ++i) {
		_VDefaultWorkContext->visTable[i] =
			log(1.0 - (double) (i + 1) /
				(double) _VDefaultWorkContext->depthCueSteps) / k;
#ifdef DEBUG
		fprintf(stderr, "next threshold is %lg units\n",
				_VDefaultWorkContext->visTable[i]);
#endif
	}
}

int
VComputePolygonColor(Viewport * v, VPolygon * poly)
{
	VColor   *c;
	VPoint   *p;
	int       i;
	double    d;

/*
 *  First, are we seeing the front or the back of this polygon?
 */

	if (poly->flags & PolyUseBackColor) {
		c = poly->backColor;
	}
	else {
		c = poly->color;
	}

/*
 *  If depth queueing isn't turned on, or this color is not a depth-cueued
 *  color, then simply return the color index.
 */

	if ((v->flags & VPDepthCueing) == 0 ||
		(c->flags & ColorEnableDepthCueing) == 0) {
		return v->pixel[c->cIndex];
	}

/*
 *  Okay, it is a depth cueued color.  Check this polygon's distance against
 *  the different cutoff points for the current world visibility value.  If the
 *  distance is greater than any of the cutoff values, then render the object
 *  as a pure "haze" color.
 */

	else {
		if ((i = poly->assignedDepth) > -1) {
			return v->pixel[(i < _VDefaultWorkContext->depthCueSteps - 1) ?
							c->cIndex + i :
							_VDefaultWorkContext->depthCueColor->cIndex];
		}
		p = &poly->vertex[0];
		d = sqrt(p->x * p->x + p->y * p->y + p->z * p->z);
		for (i = 0; i < _VDefaultWorkContext->depthCueSteps - 1; ++i) {
			if (d < _VDefaultWorkContext->visTable[i]) {
				return v->pixel[c->cIndex + i];
			}
		}
		return v->pixel[_VDefaultWorkContext->depthCueColor->cIndex];
	}
}
