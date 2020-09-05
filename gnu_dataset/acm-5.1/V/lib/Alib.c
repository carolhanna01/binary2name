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

void
AlibDrawSegments(Viewport * v, Segment * seg, int nseg, Color color)
{
  ZInfo *z;

  if (v->ztop == v->zsize) {
    printf("Z-information pool overflow\n");
    return;
  }

  z = &(v->zpool[(v->ztop)++]);
  z->depth = --v->depth;
  z->color = color;

  for (; nseg > 0; --nseg) {
    DrawLine(v->w, seg->x1, seg->y1, seg->x2, seg->y2, z);
    ++seg;
  }
}

void
AlibDrawLine(Viewport *v, int x1, int y1, int x2, int y2,
	     Color color)
{
  ZInfo *z;

  if (v->ztop == v->zsize) {
    printf("Z-information pool overflow\n");
    return;
  }

  z = &(v->zpool[(v->ztop)++]);
  z->depth = --v->depth;
  z->color = color;

  DrawLine(v->w, x1, y1, x2, y2, z);
}

void
AlibFillRectangle(Viewport *v,
		  int x, int y, int w, int h, Color color)
{
  ZInfo *z;

  if (v->ztop == v->zsize) {
    printf("Z-information pool overflow\n");
    return;
  }

  z = &(v->zpool[(v->ztop)++]);
  z->depth = --v->depth;
  z->color = color;

  FillRectangle(v->w, x, y, w, h, z);
}

void
AlibSetClipRect(Viewport *v, int x1, int y1, 
		int x2, int y2)
{
  if (x2 == -1 && y2 == -1) {
    v->w->clip.x1 = 0;
    v->w->clip.y1 = 0;
    v->w->clip.x2 = v->w->width - 1;
    v->w->clip.y2 = v->w->height - 1;
  }
  else {
    v->w->clip.x1 = x1;
    v->w->clip.y1 = y1;
    v->w->clip.x2 = x2;
    v->w->clip.y2 = y2;
  }
}

void
AlibFillPolygon(Viewport * v, Window win, GC gc, VPolygon * poly)
{

  VPoint   *p;
  XPoint    xpt[VmaxVP], *lastpt;
  register int i, k;
  Drawable  d;
  ZInfo    *z;

  if (v->ztop == v->zsize) {
    fprintf(stderr, "Z-information pool overflow\n");
    return;
  }

  z = &(v->zpool[(v->ztop)++]);

  d = (v->flags & VPPixmap) ? (Drawable) v->monoPixmap : (Drawable) win;

  if (poly == (VPolygon *) NULL)
    return;

  k = 0;
  lastpt = &xpt[0];

  /*
   */

  z->depth = --v->depth;
  z->color = VComputePolygonColor(v, poly);

  for ((i = 0, p = poly->vertex); i < poly->numVtces; (++i, ++p)) {

    if (v->flags & VPPerspective && p->z != 0.0) {
      xpt[k].x = (v->Middl.x + (int) (v->Scale.x * p->x / p->z)) >> 2;
      xpt[k].y = (v->Middl.y - (int) (v->Scale.y * p->y / p->z)) >> 2;
    }
    else {
      xpt[k].x = (v->Middl.x + (int) (v->Scale.x * p->x)) >> 2;
      xpt[k].y = (v->Middl.y - (int) (v->Scale.y * p->y)) >> 2;
    }

    /*
     *  This hack insures that improperly 3-D clipped polygons will not
     *  induce a core dump.  This problem appears when the height is an even value.
     */

    if (xpt[k].x >= v->width)
      xpt[k].x = v->width - 1;
    if (xpt[k].y >= v->height)
      xpt[k].y = v->height - 1;

#ifdef COLLAPSEUNUSEDPOINTS
    if (k == 0 || !(xpt[k].x == lastpt->x && xpt[k].y == lastpt->y))
      lastpt = &xpt[k++];
#else
    ++k;
#endif

  }

  if (k > 0) {
#ifdef COLLAPSEUNUSEDPOINTS
    if (k == 1)
      DrawPoint(v->w, xpt[0].x, xpt[0].y, z);
    else if (k == 2)
      DrawLine(v->w, xpt[0].x, xpt[0].y,
	       xpt[1].x, xpt[1].y, z);
    else
#endif
      FillPolygon(v->w, xpt, k, z);
  }

}

void
AlibExposeBuffer(Viewport * v, GC gc)
{

  if (v->flags & VPFastAnimation) {
    v->ztop = 0;
    v->depth = MaxDepth;
    FrameComplete(v->w);
    XFlush(v->dpy);
    return;
  }

  /*
   * this is one area where more-than-one window per display gets hairy
   * -- this call really exposes the drawings buffered in all windows
   * associated with a given display.  the calling sequence needs to be
   * changed to reflect this.
   */

  if (v->flags & VPMono) {
    XCopyArea(v->dpy, v->monoPixmap, v->win, gc,
	      0, 0, v->width, v->height, 0, 0);
    XSetForeground(v->dpy, gc, WhitePixel(v->dpy, v->screen));
    XFillRectangle(v->dpy, v->monoPixmap, gc, 0, 0, v->width, v->height);
    XSetForeground(v->dpy, gc, BlackPixel(v->dpy, v->screen));
  }
  else if (v->flags & VPPixmap) {
    XCopyArea(v->dpy, v->monoPixmap, v->win, gc,
	      0, 0, v->width, v->height, 0, 0);
  }
  else if (v->set == 0) {
    v->set = 1;
    XStoreColors(v->dpy, v->cmap, v->aColor, v->colors * v->colors);
    v->pixel = &(v->bPixel[0]);
    XSetPlaneMask(v->dpy, gc, v->bMask);
    v->mask = v->bMask;
    /*
      for ((p=VColorList, q = &(v->bPixel[1])); p != (VColor *) 0; ++q) {
      p->xcolor.pixel = *q;
      p = p->next;
      }
    */
  }
  else {
    v->set = 0;
    XStoreColors(v->dpy, v->cmap, v->bColor, v->colors * v->colors);
    v->pixel = &(v->aPixel[0]);
    XSetPlaneMask(v->dpy, gc, v->aMask);
    v->mask = v->aMask;
    /*
      for ((p=VColorList, q = &(v->aPixel[1])); p != (VColor *) 0; ++q) {
      p->xcolor.pixel = *q;
      p = p->next;
      }
    */
  }

  XFlush(v->dpy);
}

void
AlibClose(Viewport *v)
{
  CloseAWindow( v->w );
}

Status
AlibAllocColor(Viewport *v, Colormap colormap, XColor *c)
{
  return AAllocColor (v->w, v->dpy, colormap, c);
}

