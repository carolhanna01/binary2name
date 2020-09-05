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

extern void VComputeClipNormals(Viewport * v);
extern void ResizeAWindow(AWindow * w, int height, int width);

void
VResizeViewport(Viewport * v, double unit, double dist, double scale, int width, int height)
{

	VPoint    clip[4];

	v->width = width;
	v->height = height;

/*
 *  Allocate a new monochrome bitmap to buffer the displayed image.
 */

	if (v->flags & VPPixmap) {
		XFreePixmap(v->dpy, v->monoPixmap);
		v->monoPixmap = XCreatePixmap(v->dpy,
					  RootWindow(v->dpy, v->screen), v->width, v->height,
									  DisplayPlanes(v->dpy, v->screen));
	}

/*
 * Calculate screen resolution in pixels per unit.
 */

	v->dist = dist;
	v->units = unit;

/*
 *  Use that info to set scaling factors.
 */

	v->Scale.x = v->xres * dist * scale * 4;
	v->Scale.y = v->yres * dist * scale * 4;
	v->Scale.z = 1.0;

/*
 *  Middl should have been a "double" for accuracy, but was made an "int" for
 *  speed.  Sooo, to eliminate some stick rounding problems, we'll consider it
 *  a fixed point number with the right two bits after the decimal point.
 */

	v->Middl.x = width * 2;
	v->Middl.y = height * 2;

/*
 *  Build the clipping planes for our view into the eye space.
 */

	clip[0].x = -width / v->xres / 2.0 / scale;
	clip[0].y = -height / v->yres / 2.0 / scale;
	clip[0].z = dist;
	clip[1].x = -width / v->xres / 2.0 / scale;
	clip[1].y = height / v->yres / 2.0 / scale;
	clip[1].z = dist;
	clip[2].x = width / v->xres / 2.0 / scale;
	clip[2].y = height / v->yres / 2.0 / scale;
	clip[2].z = dist;
	clip[3].x = width / v->xres / 2.0 / scale;
	clip[3].y = -height / v->yres / 2.0 / scale;
	clip[3].z = dist;

	VDestroyPolygon(v->clipPoly);
	v->clipPoly = VCreatePolygon(4, clip, (VColor *) 0);
	VGetPlanes(v->clipPoly);
	VComputeClipNormals(v);

}
