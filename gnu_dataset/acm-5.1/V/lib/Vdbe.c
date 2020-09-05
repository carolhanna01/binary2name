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

#include <Vlib.h>
#include <X11/extensions/Xdbe.h>

void
VdbeExposeBuffer(Viewport * v, GC gc)
{
  XdbeSwapInfo si;
  si.swap_window = v->win;
  si.swap_action = XdbeUntouched;
  XdbeSwapBuffers ( v->dpy, &si, 1 );
  v->lastPixel = 0xffffffff;
}

void
VdbeDrawLine(Viewport *v, int x1, int y1, int x2, int y2,
	     Color color)
{
  Drawable d;
  d = (Drawable) v->dbeDrawable;
  if (color != v->lastPixel) {
    XSetForeground (v->dpy, v->gc, color);
    v->lastPixel = color;
  }
  XDrawLine (v->dpy, d, v->gc, x1, y1, x2, y2);
}

void
VdbeFillRectangle(Viewport *v,
		  int x, int y, int w, int h, Color color)
{
  Drawable d;
  if (color != UnchangedColor) {
    d = (Drawable) v->dbeDrawable;
    if (color != v->lastPixel) {
      XSetForeground (v->dpy, v->gc, color);
      v->lastPixel = color;
    }
    XFillRectangle(v->dpy, d, v->gc, x, y, w, h);
  }
}

void
VdbeDrawSegments(Viewport * v, Segment * seg, int nseg, Color color)
{
  int i;
  Drawable  d;
  Display *dpy = v->dpy;
  GC gc = v->gc;

  if ( nseg > 1024 ) {
    printf("nseg > 1024; only the first 1024 will be used.\n");
    nseg = 1024;
  }

  d = (Drawable) v->dbeDrawable;

  if (color != v->lastPixel) {
    XSetForeground (dpy, gc, color);
    v->lastPixel = color;
  }

  XDrawSegments (dpy, d, gc, seg, nseg);
}

void
VdbeFillPolygon(Viewport * v, Window win, GC gc, VPolygon * poly)
{
  VPoint   *p;
  XPoint    xpt[VmaxVP], *lastpt;
  register int i, k;
  Drawable  d;
  Color c;
  Display *dpy = v->dpy;
  GC xgc = v->gc;

  d = (Drawable) v->dbeDrawable;

  if (poly == (VPolygon *) NULL)
    return;

  k = 0;
  lastpt = &xpt[0];

  c = VComputePolygonColor(v, poly);

  if (c != v->lastPixel) {
    XSetForeground (dpy, xgc, c);
    v->lastPixel = c;
  }

  for ((i = 0, p = poly->vertex); i < poly->numVtces; (++i, ++p)) {

    if (v->flags & VPPerspective && p->z != 0.0) {
      xpt[k].x = (v->Middl.x + (int) (v->Scale.x * p->x / p->z)) >> 2;
      xpt[k].y = (v->Middl.y - (int) (v->Scale.y * p->y / p->z)) >> 2;
    }
    else {
      xpt[k].x = (v->Middl.x + (int) (v->Scale.x * p->x)) >> 2;
      xpt[k].y = (v->Middl.y - (int) (v->Scale.y * p->y)) >> 2;
    }

#ifdef COLLAPSEUNUSEDPOINTS
    if (k == 0 || !(xpt[k].x == lastpt->x && xpt[k].y == lastpt->y))
      lastpt = &xpt[k++];
#else
    ++k;
#endif

  }

  if (k > 0) {
#ifdef COLLAPSEUNUSEDPOINTS
    if (k == 1) {

      XDrawPoint(dpy, d, xgc, xpt[0].x, xpt[0].y);
    }
    else if (k == 2) {
      XDrawLine(dpy, d, xgc, xpt[0].x, xpt[0].y,
		xpt[1].x, xpt[1].y);
    }
    else
#endif
      XFillPolygon(dpy, d, xgc, xpt, k, Convex, CoordModeOrigin);
  }
}

void
VdbeSetClipRect(Viewport *v, int x1, int y1, 
			  int x2, int y2)
{
  XRectangle rect[1];

  if (x2 == -1 && y2 == -1) {       
    XSetClipMask(v->dpy, v->gc, None);
  }
  else {
    rect[0].x = x1;
    rect[0].y = y1;
    rect[0].width = x2 - x1 + 1;
    rect[0].height = y2 - y1 + 1;
    XSetClipRectangles(v->dpy, v->gc, 0,
       0, rect, 1, YXBanded);
  }

}

void
VdbeClose(Viewport *v)
{
}

Status
VdbeAllocColor(Viewport *v, Colormap colormap, XColor *c)
{
  Status result;
  result = XAllocColor (v->dpy, colormap, c);
  return result;
}
