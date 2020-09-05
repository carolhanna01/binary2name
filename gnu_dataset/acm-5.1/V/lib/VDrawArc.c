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

#define TICKtoRAD(a)	(a * M_PI / (180.0 * 64.0))
#define INCR		(30 * 64)

void
VDrawArc(Viewport * v, int x, int y, int width, int height, int angle1, int angle2, Color color)
{

	register ZInfo *z;
	register double w, h, xc, yc;
	register int incr, x1, x2, y1, y2;

	if (v->ztop == v->zsize) {
		fprintf(stderr, "Z-information pool overflow\n");
		return;
	}

	z = &(v->zpool[(v->ztop)++]);
	z->depth = --v->depth;
	z->color = color;

	w = width / 2.0;
	h = height / 2.0;
	xc = x + width / 2.0;
	yc = y + height / 2.0;

	if (angle2 < 0) {
		incr = -INCR;
		angle2 = -angle2;
	}
	else
		incr = INCR;

	if (angle2 > 360 * 64)
		angle2 = 360 * 64;

	x1 = (int) (xc + (w * cos(TICKtoRAD(angle1))));
	y1 = (int) (yc - (h * sin(TICKtoRAD(angle1))));

	while (angle2 != 0) {
		angle1 += incr;
		angle2 -= INCR;
		if (angle2 < 0) {
			angle1 -= angle2;
			angle2 = 0;
		}
		x2 = (int) (xc + (w * cos(TICKtoRAD(angle1))));
		y2 = (int) (yc - (h * sin(TICKtoRAD(angle1))));
		v->DrawLine(v, x1, y1, x2, y2, z->color);
		x1 = x2;
		y1 = y2;
	}
}
