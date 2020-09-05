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

extern int MakeET(AWindow * w, Point * pts, int npts, ZInfo * zinfo);

void
FillRectangle(AWindow * w, int x, int y, int width, int height, ZInfo * zinfo)
{

	Point     pts[4];
	register int yextent, xextent;

/*
 *  (Minimally) clip the box.
 */

	xextent = x + width - 1;
	yextent = y + height - 1;

	if (x > w->clip.x2 || y > w->clip.y2 ||
		xextent < w->clip.x1 || yextent < w->clip.y1) {
		return;
	}
	if (x < w->clip.x1) {
		x = w->clip.x1;
	}
	if (y < w->clip.y1) {
		y = w->clip.y1;
	}
	if (xextent > w->clip.x2) {
		xextent = w->clip.x2;
	}
	if (yextent > w->clip.y2) {
		yextent = w->clip.y2;
	}

	pts[0].x = x;
	pts[0].y = y;
	pts[1].x = xextent;
	pts[1].y = y;
	pts[2].x = xextent;
	pts[2].y = yextent;
	pts[3].x = x;
	pts[3].y = yextent;

	MakeET(w, pts, 4, zinfo);
}
