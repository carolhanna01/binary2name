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
#include <stdio.h>

void
DrawPoint(AWindow * w, int x, int y, ZInfo * zinfo)
{

	register Edge *e, *e1;

	if (x < w->clip.x1 || x > w->clip.x2 || y < w->clip.y1 || y > w->clip.y2)
		return;

	zinfo->next = NotAnElement;

	if (w->EPTop >= w->EPSize - 1) {
		fprintf(stderr, "Edge Pool Overflow\n");
		return;
	}

	e = &(w->edgePool[(w->EPTop)++]);
	e1 = &(w->edgePool[(w->EPTop)++]);

	e->y2 = e1->y2 = y + 1;
#ifdef FLOAT_SLOPE
	e->x1 = e1->x1 = x;
	e->Dx = 0.0;
#else
	e->x1 = e1->x1 = x << 16;
	e->Dx = 0;
#endif
	e->p = e1->p = zinfo;

	e1->nexte = w->edges[y].head;
	e->nexte = e1;
	w->edges[y].head = e;

	if (y < w->ymin)
		w->ymin = y;

	if (y > w->ymax)
		w->ymax = y;
}
