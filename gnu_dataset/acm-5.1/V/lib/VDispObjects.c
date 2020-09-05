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
VDisplayObjects()
{

	Point3    TmpPt;
	int       I, J;
	XPoint    xpt[MaxPts];

	for (I = 0; I < NumPolys; ++I) {

		for (J = 0; J < Polygons[I].PolyVtces; ++J) {
			Transform(&Points[Vertices[Polygons[I].Start + J]], &EyeSpace, &TmpPt);
			MakeDisplayable(&TmpPt);
			xpt[J].x = (int) TmpPt.X;
			xpt[J].y = (int) TmpPt.Y;
		}
/*              xpt[J] = xpt[0];
   XDrawLines (dpy, win, gc, &xpt, J+1, CoordModeOrigin); */
		XFillPolygon(dpy, win, curGC, &xpt, J, Nonconvex, CoordModeOrigin);
	}

}
