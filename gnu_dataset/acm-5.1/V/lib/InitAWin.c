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
#include <memory.h>

#ifdef X11

#include <X11/Xlib.h>

/*extern char *malloc(size_t);*/

#ifdef HAS_FRAME_BUFFER
void
LocateWindowOrigin(display, w, xo, yo)
Display  *display;
Window    w;
int      *xo, *yo;
{
	XWindowAttributes win;
	Window    child;
	int       wx, wy;

	XGetWindowAttributes(display, w, &win);

	XTranslateCoordinates(display, w, win.root,
				 -win.border_width, -win.border_width, &wx, &wy, &child);

	*xo = wx;
	*yo = wy;

}

#endif

AWindow  *
InitializeX11AWindow(Display * dpy, int screen, Drawable d, int flags)
{

	register AWindow *w;
	register int i;
	int       x, y;
	Window    root;
	unsigned int width, height, bwidth, depth;
	XGCValues gcv;

	XGetGeometry(dpy, d, &root, &x, &y, &width, &height, &bwidth, &depth);

	if ((w = (AWindow *) malloc(sizeof(AWindow))) == (AWindow *) NULL)
		return w;

	w->width = width;
	w->height = height;

	w->clip.x1 = 0;
	w->clip.y1 = 0;
	w->clip.x2 = width - 1;
	w->clip.y2 = height - 1;

	w->edges = (EdgeList *) malloc((w->height + 1) * sizeof(EdgeList));
	w->lines = (EdgeList *) malloc((w->height + 1) * sizeof(EdgeList));
	w->EPSize = 64 * 1024;
	w->edgePool = (Edge *) malloc(w->EPSize * sizeof(Edge));
	memset(w->edgePool, 0, w->EPSize * sizeof(Edge));

	w->CSSize0 = w->height * 32;
	w->csPool0 = (ColorSegment *) malloc(w->CSSize0 * sizeof(ColorSegment));
	memset(w->csPool0, 0, w->CSSize0 * sizeof(ColorSegment));

	w->CSSize1 = w->height * 32;
	w->csPool1 = (ColorSegment *) malloc(w->CSSize1 * sizeof(ColorSegment));
	memset(w->csPool1, 0, w->CSSize1 * sizeof(ColorSegment));

	w->CSSize2 = 0;
	w->csPool2 = (ColorSegment *) NULL;

	w->scanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	w->lastScanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	w->otherLastScanLine = NULL;
	w->doubleBuffered = 0;

	w->EPTop = w->CSTop0 = w->CSTop1 = w->curPool = 0;

	w->ymin = 0x7FFF;
	w->ymax = -1;

	for (i = 0; i < w->height + 1; ++i) {

		w->scanLine[i].count = 0;
		w->scanLine[i].head = 0;
		w->scanLine[i].tail = 0;
		w->lastScanLine[i].count = 0;
		w->lastScanLine[i].head = 0;
		w->lastScanLine[i].tail = 0;
		w->edges[i].head = (Edge *) NULL;
		w->lines[i].head = (Edge *) NULL;

	}

	w->color_to_pixel_map_top = 0;

	w->bsegSize = 256;

	for (i = 0; i < (sizeof(w->bseg) / sizeof(w->bseg[0])); ++i)
		w->bseg[i].count = -1;

	w->display = dpy;
	w->d = d;
	gcv.cap_style = CapButt;
	w->gc = XCreateGC(dpy, RootWindow(dpy, screen),
					  GCCapStyle, &gcv);
	w->flags = flags;

	if (flags & AMonoMode)
		XSetFillStyle(dpy, w->gc, FillOpaqueStippled);

#ifdef HAS_FRAME_BUFFER
	FrameBufferIOInit();
	LocateWindowOrigin(dpy, d, &x, &y);
	FrameBufferSetOrigin(x, y);
#endif

	return w;
}

#endif

int       _AlibDoubleBuffered = 0;

AWindow  *
InitializeAWindow(unsigned int width, unsigned int height)
{

	register AWindow *w;
	register int i;

	if ((w = (AWindow *) malloc(sizeof(AWindow))) == (AWindow *) NULL)
		return w;

	w->doubleBuffered = _AlibDoubleBuffered;

	w->width = width;
	w->height = height;

	w->edges = (EdgeList *) malloc(w->height * sizeof(EdgeList));
	w->lines = (EdgeList *) malloc(w->height * sizeof(EdgeList));
	w->EPSize = 64 * 1024;
	w->edgePool = (Edge *) malloc(w->EPSize * sizeof(Edge));
	memset(w->edgePool, 0, w->EPSize * sizeof(Edge));

	w->CSSize0 = w->height * 32;
	w->csPool0 = (ColorSegment *) malloc(w->CSSize0 * sizeof(ColorSegment));
	memset(w->csPool0, 0, w->CSSize0 * sizeof(ColorSegment));

	w->CSSize1 = w->height * 32;
	w->csPool1 = (ColorSegment *) malloc(w->CSSize1 * sizeof(ColorSegment));
	memset(w->csPool1, 0, w->CSSize1 * sizeof(ColorSegment));

	if (w->doubleBuffered) {
		w->CSSize2 = w->height * 32;
		w->csPool2 = (ColorSegment *) malloc(w->CSSize2 * sizeof(ColorSegment));
		memset(w->csPool2, 0, w->CSSize2 * sizeof(ColorSegment));
	}
	else {
		w->CSSize2 = 0;
		w->csPool2 = (ColorSegment *) NULL;
	}

	w->scanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	w->lastScanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));

	if (w->doubleBuffered) {
		w->otherLastScanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	}
	else {
		w->otherLastScanLine = NULL;
	}

	w->EPTop = w->CSTop0 = w->CSTop1 = w->CSTop2 = w->curPool = 0;

	w->ymin = 0x7FFF;
	w->ymax = -1;

	for (i = 0; i < w->height; ++i) {

		w->scanLine[i].count = 0;
		w->scanLine[i].head = 0;
		w->scanLine[i].tail = 0;
		w->lastScanLine[i].count = 0;
		w->lastScanLine[i].head = 0;
		w->lastScanLine[i].tail = 0;
		if (w->doubleBuffered) {
			w->otherLastScanLine[i].count = 0;
			w->otherLastScanLine[i].head = 0;
			w->otherLastScanLine[i].tail = 0;
		}
		w->edges[i].head = (Edge *) NULL;
		w->lines[i].head = (Edge *) NULL;

	}

	return w;
}

void							/*ARGSUSED */
ResizeAWindow(AWindow * w, int height, int width)
{
	register int i;

        if (!w) {
	  return;
	}

	if (w->edges) {
		free((char *) w->edges);
	}
	if (w->lines) {
		free((char *) w->lines);
	}
	if (w->edgePool) {
		free((char *) w->edgePool);
	}
	if (w->scanLine) {
		free((char *) w->scanLine);
	}
	if (w->lastScanLine) {
		free((char *) w->lastScanLine);
	}
	if (w->otherLastScanLine) {
		free((char *) w->otherLastScanLine);
	}
	if(w->csPool0) {
		free(w->csPool0);
	}
	if (w->csPool1) {
		free(w->csPool1);
	}
	if (w->csPool2) {
		free(w->csPool2);
	}

	w->width = width;
	w->height = height;
	w->clip.x2 = width - 1;
	w->clip.y2 = height - 1;

	w->edges = (EdgeList *) malloc((w->height + 1) * sizeof(EdgeList));
	w->lines = (EdgeList *) malloc((w->height + 1) * sizeof(EdgeList));
	w->EPSize = 64 * 1024;
	w->edgePool = (Edge *) malloc(w->EPSize * sizeof(Edge));
	memset(w->edgePool, 0, w->EPSize * sizeof(Edge));

	w->CSSize0 = w->height * 32;
	w->csPool0 = (ColorSegment *) malloc(w->CSSize0 * sizeof(ColorSegment));
	memset(w->csPool0, 0, w->CSSize0 * sizeof(ColorSegment));

	w->CSSize1 = w->height * 32;
	w->csPool1 = (ColorSegment *) malloc(w->CSSize1 * sizeof(ColorSegment));
	memset(w->csPool1, 0, w->CSSize1 * sizeof(ColorSegment));

	if (w->doubleBuffered) {
		w->CSSize2 = w->height * 32;
		w->csPool2 = (ColorSegment *) malloc(w->CSSize2 * sizeof(ColorSegment));
		memset(w->csPool2, 0, w->CSSize2 * sizeof(ColorSegment));
	}
	else {
		w->CSSize2 = 0;
		w->csPool2 = (ColorSegment *) NULL;
	}

	w->scanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	w->lastScanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));

	if (w->doubleBuffered) {
		w->otherLastScanLine = (ScanLine *) malloc((w->height + 1) * sizeof(ScanLine));
	}
	else {
		w->otherLastScanLine = NULL;
	}

	w->EPTop = w->CSTop0 = w->CSTop1 = w->CSTop2 = w->curPool = 0;

	w->ymin = 0x7FFF;
	w->ymax = -1;

	for (i = 0; i < w->height + 1; ++i) {

		w->scanLine[i].count = 0;
		w->scanLine[i].head = 0;
		w->scanLine[i].tail = 0;
		w->lastScanLine[i].count = 0;
		w->lastScanLine[i].head = 0;
		w->lastScanLine[i].tail = 0;
		if (w->doubleBuffered) {
			w->otherLastScanLine[i].count = 0;
			w->otherLastScanLine[i].head = 0;
			w->otherLastScanLine[i].tail = 0;
		}
		w->edges[i].head = (Edge *) NULL;
		w->lines[i].head = (Edge *) NULL;

	}
}
