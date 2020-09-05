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

typedef int outcode_t;

#define NONE	0
#define Top    1
#define Bottom 2
#define Left   8
#define Right  4

static    outcode_t
CompOutCode(AWindow * w, int x, int y)
{
	outcode_t code = NONE;

	if (y > w->clip.y2) {
		code |= Top;
	}
	else if (y < w->clip.y1) {
		code |= Bottom;
	}

	if (x > w->clip.x2) {
		code |= Right;
	}
	else if (x < w->clip.x1) {
		code |= Left;
	}
	return code;
}

/*
 *  Cohen/Sutherland 2D clipping alorithm as described by Foley, van Dam,
 *  Feiner and Hughes
 */

int
ClipLine(AWindow * w, int *x0, int *y0, int *x1, int *y1)
{

	outcode_t outcode0, outcode1, outcodeOut;
	int       accept = 1, done = 0;

	outcode0 = CompOutCode(w, *x0, *y0);
	outcode1 = CompOutCode(w, *x1, *y1);

	do {
		if (outcode0 == 0 && outcode1 == 0) {
			accept = 0;
			done = 1;
		}
		else if (outcode0 & outcode1) {
			done = 1;
		}
		else {
			int       x, y;

			outcodeOut = outcode0 ? outcode0 : outcode1;

			if (outcodeOut & Top) {
				x = *x0 + (*x1 - *x0) * (w->clip.y2 - *y0) / (*y1 - *y0);
				y = w->clip.y2;
			}
			else if (outcodeOut & Bottom) {
				x = *x0 + (*x1 - *x0) * (w->clip.y1 - *y0) / (*y1 - *y0);
				y = w->clip.y1;
			}
			else if (outcodeOut & Right) {
				y = *y0 + (*y1 - *y0) * (w->clip.x2 - *x0) / (*x1 - *x0);
				x = w->clip.x2;
			}
			else {				/* Left */
				y = *y0 + (*y1 - *y0) * (w->clip.x1 - *x0) / (*x1 - *x0);
				x = w->clip.x1;
			}

			if (outcodeOut == outcode0) {
				*x0 = x;
				*y0 = y;
				outcode0 = CompOutCode(w, *x0, *y0);
			}
			else {
				*x1 = x;
				*y1 = y;
				outcode1 = CompOutCode(w, *x1, *y1);
			}
		}

	} while (done == 0);

	return accept;
}
