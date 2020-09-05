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

#ifndef __Vlib
#define __Vlib

#include <Vlibmath.h>

#define HAS_FRAME_BUFFER

#define VmaxVP	2048			/* max # of vertices in a polygon */

#define MAXCOLORS 16	   		/* max number of colors available */
				/* when double buffering */

#if (MAXCOLORS==4)
#define PLANECOUNT 2
#endif
#if (MAXCOLORS==8)
#define PLANECOUNT 3
#endif
#if (MAXCOLORS==16)
#define PLANECOUNT 4
#endif

#define NUM_ASPECTS 10

#ifdef WIN32

#include <stdlib.h>
#define SYSV

#else /* ! WIN32 */

#include <X11/Xlib.h>
#include <X11/extensions/Xdbe.h>
#endif

#include <Alib.h>
#include <stdio.h>
#include <stdlib.h>
#if !defined(__GNUC__)
#include <memory.h>
#endif

#ifdef WIN32
extern int DisplayWidth();
extern int DisplayHeight();
extern int DisplayPlanes();
extern Pixmap XCreatePixmap(
    Display*            /* display */,
    Drawable            /* d */,
    unsigned int        /* width */,
    unsigned int        /* height */,
    unsigned int        /* depth */
);
extern XFreePixmap(
    Display*            /* display */,
    Pixmap              /* pixmap */
);
extern XFlush(
    Display*            /* display */
);
extern XCopyArea(
    Display*            /* display */,
    Drawable            /* src */,
    Drawable            /* dest */,
    GC                  /* gc */,
    int                 /* src_x */,
    int                 /* src_y */,
    unsigned int        /* width */,
    unsigned int        /* height */,
    int                 /* dest_x */,
    int                 /* dest_y */
);
extern XSetForeground(
    Display*            /* display */,
    GC                  /* gc */,
    unsigned long       /* foreground */
);
extern XFillRectangle(
    Display*            /* display */,
    Drawable            /* d */,
    GC                  /* gc */,
    int                 /* x */,
    int                 /* y */,
    unsigned int        /* width */,
    unsigned int        /* height */
);
extern Status XAllocColorCells(
    Display*            /* display */,
    Colormap            /* colormap */,
    Bool                /* contig */,
    unsigned long*      /* plane_masks_return */,
    unsigned int        /* nplanes */,
    unsigned long*      /* pixels_return */,
    unsigned int        /* npixels */
); 
extern XStoreColors(
    Display*            /* display */,
    Colormap            /* colormap */,
    XColor*             /* color */,
    int                 /* ncolors */
);

extern XSetPlaneMask(
    Display*            /* display */,
    GC                  /* gc */,
    unsigned long       /* plane_mask */
); 
#endif

typedef struct _Viewport {
	AWindow  *w;
	unsigned long depth;		/* polygon depth while plotting */
	unsigned long flags;		/* viewport flags */
	VMatrix   eyeSpace;			/* transforms from world to eyeSpace system */
	VPolygon *clipPoly;			/* planes to clip viewed polygons */
	VPoint    clipNormals[4];	/* normal vectors corresponding to clipPoly */
	double    units;			/* world units expressed in meters */
	double    dist;				/* distance in units from eye to screen */
	double    xres;				/* x screen resolution in dots per unit */
	double    yres;				/* y screen resolution in dots per unit */
	int       width;			/* width of window in dots */
	int       height;			/* height of window in dots */
	Point     Middl;			/* center of the window */
	VPoint    Scale;			/* scaling factor */
	Display  *dpy;				/* Display associated with this viewport */
	int       screen;			/* X screen number */
	Window    win;
	Pixmap    monoPixmap;		/* Pixmap used to buffer drawing */
	XdbeBackBuffer dbeDrawable;
	GC        gc;
	Pixmap    halftone[8];		/* Pixmaps used for dithering images */

	int       colors;			/* color count */
	int       set;				/* id of buffer currently in use */
	unsigned long mask;			/* current plane mask */
	unsigned long aMask, bMask;
	Color *pixel;		/* current pixel drawing values */
	Color *aPixel;
	Color bPixel[MAXCOLORS];
	XColor    xdepthCueColor;
	XColor    aColor[MAXCOLORS * MAXCOLORS];
	XColor    bColor[MAXCOLORS * MAXCOLORS];
	Colormap  cmap;
	Visual   *visual;
	ZInfo    *zpool;
	long      zsize;
	long      ztop;
	void      (*colorTweakProc) ();
	Status    (*AllocColor)(struct _Viewport *v, 
							Colormap colormap, 
							XColor *c);
	void      (*DrawLine)(struct _Viewport *v, 
						  int x1, 
						  int y1, 
						  int x2, 
						  int y2,
						  Color color);
	void      (*DrawSegments)(struct _Viewport * v, 
							  Segment * seg, 
							  int nseg, 
							  Color color);
	void      (*FillPolygon)(struct _Viewport * v, 
							 Window win, 
							 GC gc, 
							 VPolygon * poly);
	void      (*FillRectangle)(struct _Viewport *v,
							   int x, 
							   int y, 
							   int w, 
							   int h, 
							   Color color);
	void      (*ExposeBuffer)(struct _Viewport * v, GC gc);
	void      (*SetClipRect)(struct _Viewport *v, 
							 int x1, int y1, 
							 int x2, int y2);
	void      (*Close)(struct _Viewport *v);
	unsigned long lastPixel;
} Viewport;

typedef struct {
	int       flags;			/* flag word */
	VColor   *VColorList;
	int       usePixmaps;
	int       nextPixel;		/* next pixel cell to allocate */
	int       depthCueSteps;	/* 0 = no haze */
	VColor   *depthCueColor;	/* haze color */
	double    visibility;		/* visibility (in feet) */
	double   *visTable;
} VWorkContext;

#define WCLocked		1		/* colors have been bound */

#define VGetPolygonPixel(p)		    (p->color->xcolor.pixel)
#define VLookupViewportColor(v,n)	(v->pixel[n])
#define VGetViewportMask(v) 		(v->mask)
#define VSetDepthCue(color,s) { _VDefaultWorkContext->depthCueColor=(color); \
	_VDefaultWorkContext->depthCueSteps=(s); }

/*
 * Are we C++?
 */

#if defined(__cplusplus)
extern    "C" {
#endif

/*
 *  Globals
 */

	extern VWorkContext _v_default_work_context;
	extern VWorkContext *_VDefaultWorkContext;
	extern VPoint _VUnitVectorI, _VUnitVectorJ, _VUnitVectorK;

#ifndef PARAMS
#if !defined(__STDC__) && !defined(__cplusplus) && !defined(WIN32)
#define PARAMS(arglist) ()
#else
#define PARAMS(arglist)	arglist
#endif
#endif

/*
 *  V macros and function prototypes
 */

#define VDestroyPoints(a)	free((char *) a)
#define VDestroyPolygon(a)	{free((char *) a->vertex); free((char *) a);}

	extern Viewport *VOpenViewport PARAMS((Display *, int, Window, Colormap,
							Visual *, double, double, double, int, int));
	extern void VResizeViewport PARAMS((Viewport *, double, double,
										double, int, int));
	extern void VCloseViewport(Viewport * v);	/* (Viewport *) */

	extern VPolygon *VClipPolygon PARAMS((VPolygon *, VPolygon *));
	extern VPolygon *VClipSidedPolygon PARAMS((Viewport *, 
											   VPolygon *, 
											   VPolygon *));
	extern VPoint *VCreatePoints(int numPts);	/* (int) */
	extern VColor *VAllocColor PARAMS((char *));
	extern VColor *VAllocDepthCueuedColor PARAMS((char *, int flag));
	extern int VBindColors PARAMS((Viewport *, char *));
	extern void VExposeBuffer PARAMS((Viewport *, GC));
	extern VObject *VReadObject PARAMS((FILE * f));
	extern VObject *VReadDepthCueuedObject PARAMS((FILE * f, int flag));
	extern int VWriteObject PARAMS((FILE *, VObject *));

	extern void VSetReadObjectScale PARAMS((VPoint *));
	extern void VComputeObjectExtent PARAMS((VObject *));

	extern int VEyeToScreen(Viewport * v, VPoint * p, int *x, int *y);
	extern int VWorldToScreen(Viewport * v, VPoint * p, int *x, int *y);

	extern int VFontWidthPixels(Viewport * v, int scale);
	extern void VDrawStrokeString(Viewport * v, int x, int y,
					  unsigned char *str, int len, int scale, ZInfo * z);
	extern void VGetStrokeString(Viewport * v, int x, int y, 
								 Segment * seg, int *nseg,
								 unsigned char *str, int len, int scale);
	extern void VDrawArc(Viewport * v, int x, int y, 
						 int width, int height, 
						 int angle1, int angle2, Color color);
	extern void VDrawSegments(Viewport * v, Segment * seg,
							  int nseg, Color color);
	extern void VFillPolygon(Viewport * v, Window win, GC gc, VPolygon * poly);
	extern void VFillRectangle(Viewport *v, int x, int y, int w, int h,
							   Color c);
	extern void VGetEyeSpace(Viewport * v, VPoint EyePt, 
							 VPoint CntrInt, VPoint up);
	extern VPolygon *VGetPlanes(VPolygon * poly);

	extern void VSetVisibility PARAMS((double d));
	extern int VComputePolygonColor PARAMS((Viewport * v, VPolygon * poly));
	extern int ViewportSetDepthCueing PARAMS((Viewport * v, int flag));
	extern VObject  * VReadDXFObject(FILE *f);
	extern VObject  * VReadDepthCueuedDXFObject(FILE *f, int flag);

	extern void releaseVResources (void);
	extern void VSetClipRect(Viewport *v, int x1, int y1, int x2, int y2);

	extern void VdbeDrawLine(Viewport *v, int x1, int y1, int x2, int y2,
							 Color color);
	extern void VdbeFillPolygon(Viewport * v, 
								Window win, GC gc, VPolygon * poly);
	extern void VdbeFillRectangle(Viewport *v,
								  int x, int y, int width, int h, Color c);
	extern void VdbeDrawSegments(Viewport * v, 
								 Segment * seg, int nseg, Color color);
	extern void VdbeSetClipRect(Viewport *v, int x1, int y1, 
								int x2, int y2);
	extern void VdbeExposeBuffer(Viewport * v, GC gc);
	extern void VdbeClose(Viewport *v);
	extern Status VdbeAllocColor(Viewport *v, 
								 Colormap colormap, XColor *c);

	extern void AlibDrawLine(Viewport *v, int x1, int y1, 
							 int x2, int y2,
							 Color color);
	extern void AlibFillPolygon(Viewport * v, 
								Window win, GC gc, VPolygon * poly);
	extern void AlibFillRectangle(Viewport *v,
								  int x, int y, int width, int h, Color c);
	extern void AlibDrawSegments(Viewport * v, 
								 Segment * seg, int nseg, Color color);
	extern void AlibSetClipRect(Viewport *v, int x1, int y1, 
								int x2, int y2);
	extern void AlibExposeBuffer(Viewport * v, GC gc);
	extern void AlibClose(Viewport *v);
	extern Status AlibAllocColor(Viewport *v, Colormap colormap, XColor *c);

#if 0
#ifndef SYSV
	extern char *strdup(const char *);
#endif

#if (defined(_AIX) && defined(_IBMR2)) && !(defined(__GNUC__) \
     && (__GNUC__ == 2 ))
/* For some reason, the RS/6000 AIX header files don't declare strdup. */
	extern char *strdup(const char *);
#endif
#endif
#if defined(__cplusplus)
};

#endif

#endif
