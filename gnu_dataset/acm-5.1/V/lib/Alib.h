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

#ifdef WIN32
#define HAS_FRAME_BUFFER
#include <stdlib.h>
/*#include <crtdbg.h>*/

#define printf  acm_printf
#define fprintf acm_fprintf

#include <stdio.h>

#if defined(__cplusplus)
extern    "C" {
#endif

extern int _cdecl acm_printf( const char *format, ... );
extern int _cdecl acm_fprintf( FILE *, const char *format, ... );

#if defined(__cplusplus)
};
#endif

#else
#define X11
#endif

#include <stdio.h>

#ifdef X11
#undef HAS_FRAME_BUFFER			/* use direct frame buffer output */
#define X11_DRAW_SEGMENTS		/* draw segments instead of rectangles */
#include <X11/Xlib.h>
#endif

#ifdef HAS_FRAME_BUFFER
extern void FrameBufferIOInit(), FrameBufferRun();

#endif

typedef unsigned short Color;

#define UnchangedColor	0xffff	/* special Color value */

#ifdef X11

typedef XPoint Point;
typedef XSegment Segment;
typedef XRectangle Rectangle;

#else							/* X11 */

typedef struct {
	short     x, y;
} Point;

typedef struct {
	short     x1, y1, x2, y2;
} Segment;

typedef struct {
	short     x, y;
	unsigned short width, height;
} xRectangle;

typedef Point XPoint;
typedef Segment XSegment;
typedef xRectangle XRectangle;
typedef int Bool;

#define DisplayHeightMM(x, s)  (168)
#define DisplayWidthMM(x, s)   (224)
#define RootWindow(x, s)	0
#define BlackPixel(x, s) 0
#define WhitePixel(x, s) 1

#define XParseColor(d, c, name, color)	PCParseColor(name, color)

#endif							/* X11 */

typedef struct _ZInfo {
	Color     color;			/* color of this polygon */
	unsigned long depth;		/* depth of this polygon */
	struct _ZInfo *next;		/* next polygon in plane sweep set */
	struct _ZInfo *prev;		/* previous polygon in plane sweep set */
} ZInfo;

#define NotAnElement		((ZInfo *) -1)

#define MaxDepth		0xFFFFFFFF
#define MinDepth		0

/*
 *  Polygons are built from edges
 */

typedef struct _edge {
	short     y2;				/* Ending y location */
#ifdef FLOAT_SLOPE
	float     x1;				/* Starting x location */
	float     Dx;				/* Inverse slope of edge line */
#else
	long      x1;				/* Starting x location */
	long      Dx;				/* Inverse slope of edge line */
#endif
	ZInfo    *p;				/* depth and color information */
	struct _edge *nexte;		/* next edge on this edge list */
	struct _edge *next;			/* next edge on active edge list */
} Edge;

/*
 *  An edge list contains a pointer to a list of Edges.
 */

typedef struct {
	Edge     *head;				/* pointer to first edge in list */
} EdgeList;

/*
 *  A ColorSegment is a row of adjacent pixels all of the same color.
 */

typedef struct {
	short     x;				/* starting x location */
	unsigned short length;		/* number of pixels of this color */
	Color     color;			/* color of this segment */
} ColorSegment;

/*
 *  A ScanLine is composed of a vector of ColorSegment's.
 */

typedef struct {
	ColorSegment *head;			/* first element in the list */
	ColorSegment *tail;			/* last element in the list */
	int       count;
} ScanLine;

#ifdef X11

/*
 *  Randomly changing drawing colors in X would require that we add an XChangeGC
 *  request before virtually every line segment draw.
 *  Once the scan line segment output has been optimized by ScanLineDifference,
 *  we can add a further optimization to reduce the number of XChangeGC
 *  requests that are required.  We'll add a structure to group drawing
 *  requests by color.  When some watermark is past, an XChangeCG request may
 *  be issued, followed by all requests for that color.  This request buffer
 *  will be flushed with a call to FrameComplete().
 */

typedef struct {
#ifdef X11_DRAW_SEGMENTS
	XSegment *head;				/* pointer to a vector of BufferedSegment's */
#else
	XRectangle *head;			/* pointer to a vector of BufferedSegment's */
#endif
	int       count;			/* number of seg's currently in use */
} SegmentGroup;

#define AMaxPixelValue	255		/* we'll only handle up-to 8-bit Visuals */

#endif

/*
 *  To perform animation in a window, we associate an AWindow structure
 *  with it.
 */

typedef struct {
#ifdef X11
	Display  *display;			/* X display for this window */
	Drawable  d;				/* dest drawable for this window */
	GC        gc;
	int       bsegSize;			/* Count of elements in each bseg vec */
	SegmentGroup bseg[AMaxPixelValue + 1];
	unsigned long color_to_pixel_map[AMaxPixelValue+1];
	long color_to_pixel_map_top;

	Pixmap   *stipple;			/* pointer to a list of pixmaps */
	int       flags;
#endif
	short     width, height;	/* dimensions of this window */
	short     ymin, ymax;		/* range of EdgeList's with polygons */
	int       doubleBuffered;	/* 0=single video buffer; 1=double buffer */
	Segment   clip;				/* clipping bounds */
	ScanLine *scanLine;			/* a vector with height elements */
	ScanLine *lastScanLine;		/* a vector with height elements */
	ScanLine *otherLastScanLine;	/* a vector with height elements */
	EdgeList *edges;			/* a vector with height elements */
	EdgeList *lines;			/* a vector with height elements */
	Edge     *edgePool;			/* a pool of polygon edges */
	unsigned int EPSize;		/* number of entries in edgePool */
	unsigned int EPTop;			/* index of first free edge */
	unsigned int curPool;		/* selects csPool (0 or 1 (or 2 when double buffering)) */
	ColorSegment *csPool0;
	unsigned int CSSize0;		/* number of entries in csPool0 */
	unsigned int CSTop0;		/* index of first free color seg */
	ColorSegment *csPool1;
	unsigned int CSSize1;		/* number of entries in csPool1 */
	unsigned int CSTop1;		/* index of first free color seg */
	ColorSegment *csPool2;
	unsigned int CSSize2;		/* number of entries in csPool2 */
	unsigned int CSTop2;		/* index of first free color seg */
} AWindow;

#if defined(__cplusplus)
extern    "C" {
#endif

#ifdef X11
	extern AWindow *InitializeX11AWindow(Display * dpy, int screen, Drawable d, int flags);
	extern void AX11FlushBufferedSegments(AWindow * w);
#endif

#define AMonoMode	1			/* monochrome flag */

	extern AWindow *InitializeAWindow(unsigned int, unsigned int);
	extern void CloseAWindow(AWindow *);
	extern void DrawPolygon();
	extern void FillPolygon(AWindow * w, Point * pts, int npts, ZInfo * zinfo);
	extern void FillRectangle(AWindow * w, int x, int y, int width, int height, ZInfo * zinfo);
	extern void DrawLine(AWindow * w, int x1, int y1, int x2, int y2, ZInfo * zinfo);
	extern void DrawPoint(AWindow * w, int x, int y, ZInfo * zinfo);
	extern void DrawLines();
	extern void DrawArc();
	extern void FrameComplete(AWindow *);
	extern void ForceWindowRedraw(AWindow *);
	extern int ClipLine(AWindow * w, int *x0, int *y0, int *x1, int *y1);
	extern void ResizeAWindow(AWindow *, int, int);

	extern int _AlibDoubleBuffered;		/* defined in InitAWin.c */

#ifndef X11
typedef int Status;
typedef struct {
	unsigned short red, green, blue;
	unsigned long rgb;
	unsigned short pixel;
	unsigned short flags;
} XColor;

typedef int Drawable;
typedef int Display;
typedef int Window;
typedef int Pixmap;
typedef int Colormap;
typedef struct _visual {
	int       map_entries;
} Visual;

#define GC	long
#define AllPlanes (-1)
#define True	(1)
#define False	(0)
#endif

	extern Status AAllocColor(AWindow *a, Display *display, Colormap colormap, XColor *c);

#if defined(__cplusplus)
};

#endif
