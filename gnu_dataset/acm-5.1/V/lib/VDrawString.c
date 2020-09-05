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
VDrawString(Viewport * v, Window win, GC gc, VPoint * p, char *str, int len)
{

	register int x, y;

	if (v->flags & VPPerspective) {
		x = (v->Middl.x + (int) (v->Scale.x * p->x / p->z)) >> 2;
		y = (v->Middl.y - (int) (v->Scale.y * p->y / p->z)) >> 2;
	}
	else {
		x = (v->Middl.x + (int) (v->Scale.x * p->x)) >> 2;
		y = (v->Middl.y - (int) (v->Scale.y * p->y)) >> 2;
	}

	XDrawString(v->dpy, win, gc, x, y, str, len);

}
