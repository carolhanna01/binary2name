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


#include <Alib.h>

#define IsInside(w, pt)	pt.x < w->clip.x1 || pt.x >= w->clip.x2 || \
			pt.y < w->clip.y1 || pt.y >= w->clip.y2

/*
 *  A quick and dirty 2D polygon clipping algorithm.
 */

void
Clip2DPolygon(AWindow * w, Point * pts, int npts, Point * clippedPts, int *nClippedPts)
{

	register int numPts, j, lastj, inside, lastInside;
	int       x1, y1, x2, y2;

	lastj = npts - 1;
	numPts = 0;

	lastInside = IsInside(w, pts[lastj]) ? 1 : 0;

	for (j = 0; j < npts; ++j) {

		if (lastInside == 1) {
			clippedPts[numPts++] = pts[lastj];
		}

		inside = (IsInside(w, pts[j])) ? 1 : 0;

/*
 *  Fast, but not accurate.  This method does not handle polygons in corners
 *  well.  A more accurate method would be to clip by each edge independently.
 */

		if ((inside ^ lastInside) == 1) {
			x1 = pts[lastj].x;
			y1 = pts[lastj].y;
			x2 = pts[j].x;
			y2 = pts[j].y;
			ClipLine(w, &x1, &y1, &x2, &y2);
			if (x2 == pts[lastj].x && y2 == pts[lastj].y) {
				clippedPts[numPts].x = x1;
				clippedPts[numPts++].y = y1;
			}
			else {
				clippedPts[numPts].x = x2;
				clippedPts[numPts++].y = y2;
			}
		}

		lastInside = inside;
		lastj = j;
	}

	*nClippedPts = numPts;
}
