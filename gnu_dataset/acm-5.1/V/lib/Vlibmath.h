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

#ifndef _vlibmath_h
#define _vlibmath_h

#include <math.h>
#include <stdio.h>

#if defined(__cplusplus)
extern    "C" {
#endif

/*
 *  If M_PI has been cast to a long double, force the use of our
 *  own, double constants
 */

#ifdef M_PI_LONG_DOUBLE
#undef M_PI
#undef M_PI_2
#undef M_PI_4
#endif

/*
 *  M_PI undefined?
 */

#ifndef M_PI
#define M_PI            3.14159265358979323846
#define M_PI_2          1.57079632679489661923
#define M_PI_4          0.78539816339744830962
#endif

typedef struct _vcolor {
	char     *color_name;		/* text name */
	short     flags;
	short     cIndex;			/* index into pixel value or halftone table */
	struct _vcolor *next;
} VColor;

/*
 *  VColor flag values
 */

#define ColorEnableDepthCueing	1

typedef struct {
	double    x, y, z;			/* A point or vector in 3-space */
} VPoint;

typedef struct {
	double    s;
	VPoint    v;
} VQuaternion;

typedef struct {
	short     flags;			/* flag word */
	short     numVtces;			/* vertex count */
	short     assignedDepth;	/* hard-assigned depth cue or -1 */
	VPoint    normal;			/* normal vector plus vertex[0] */
	VPoint   *vertex;			/* pointer to array of vertices (& normal) */
	VColor   *color;			/* pointer to color descriptor */
	VColor   *backColor;		/* pointer to back's color (sometimes NULL) */
	double    cullDistance;		/* this polygon is ignored beyond this distance */
} VPolygon;

/*
 *  VPolygon Flags
 */

#define PolyClipBackface	0x01	/* Polygon is "one-sided" */
#define PolyUseBackColor	0x02	/* should be rendered using backColor */
#define PolyNormalValid		0x04	/* "normal" contains valid info */
#define PolyUseCullDistance 0x08	/* cullDistance should be tested before plotting */

typedef struct {
	char     *name;				/* object name */
	VPoint    xaxis, yaxis, zaxis;
	VPoint    center;			/* average of all the object's points */
	double    extent;			/* distance from center to most distant point */
	int       numPolys;			/* polygon count */
	VPolygon **polygon;			/* pointer to array of polygon pointers */
	unsigned short *order;		/* 3-D relative polygon depth ordering */
} VObject;

/*
 *  VObjects and VPolygons are an adequate model for defining simple
 *  objects.  If there are many vertices that are shared among an objects
 *  polygons, though, substantial time may be wasted re-transforming the
 *  same point.  Here we create the notion of a VTerseObject and terse polygon
 *  where all vertices are shared in a common array.
 */

typedef struct {
	short     flags;			/* flag word */
	short     numVtces;			/* vertex count for this polygon */
	VPoint    normal;			/* normal vector plus vertex[0] */
	struct _VSurfaceObject *parent;		/* object that owns this polygon */
	unsigned long *vertex;		/* indices the vertices in this polygon */
	VColor   *color;			/* pointer to color descriptor */
	VColor   *backColor;		/* pointer to back's color (sometimes NULL) */
} VTersePolygon;

typedef struct _VSurfaceObject {
	char     *name;
	VPoint    center;
	double    extent;
	long      numPolys;
	long      numVtces;			/* number of points in "point" array */
	VPoint   *point;			/* pts shared by all VTersePolygons in object */
	VPoint   *tpoint;			/* temporary (transformed) point storage */
	unsigned short *order;
} VTerseObject;

typedef struct {
	double    m[4][4];
} VMatrix;

#define VSetPoint(p, xi, yi, zi)	{ p.x = xi; p.y = yi; p.z = zi; }
#define VUnitVectorI()			(& _VUnitVectorI)
#define VUnitVectorJ()			(& _VUnitVectorJ)
#define VUnitVectorK()			(& _VUnitVectorK)
#define VPointToClipPlaneDistance(pt, pl) \
	(- VDotProd(pl, pt) / ((pl)->x * (pl)->x + (pl)->y * (pl)->y + \
	(pl)->z * (pl)->z))
#define Vmagnitude(pt)	(sqrt((pt)->x*(pt)->x+(pt)->y*(pt)->y+(pt)->z*(pt)->z))


#if (defined(__GNUC__) || defined(__STDC__) || defined(_WINDOWS)) && !defined(_NO_INLINE)

#if defined(_WINDOWS)
#define inline __inline
#endif

	static inline void
	          VCrossProd(VPoint * a, VPoint * b, VPoint * r) {
		r->x = a->y * b->z - a->z * b->y;
		r->y = a->z * b->x - a->x * b->z;
		r->z = a->x * b->y - a->y * b->x;
	} static inline double
	          VDotProd(VPoint * a, VPoint * b) {
		return a->x * b->x + a->y * b->y + a->z * b->z;
	}
	static inline void
	          VMatrixMult(VMatrix * Mt1, VMatrix * Mt2, VMatrix * R) {
		int       I, J, K;
		double    x;

		for       (I = 0; I < 4; ++I)
			for       (J = 0; J < 4; ++J) {
				x = 0.0;
				for (K = 0; K < 4; ++K)
					x += Mt1->m[K][J] * Mt2->m[I][K];
				R->m[I][J] = x;
			}
	}
	static inline void
	          VTransform(VPoint * pt, VMatrix * mt, VPoint * newPt) {

		newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[0][1]
		+ pt->z * mt->m[0][2] + mt->m[0][3];

		newPt->y = pt->x * mt->m[1][0] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[1][2] + mt->m[1][3];

		newPt->z = pt->x * mt->m[2][0] + pt->y * mt->m[2][1]
		+ pt->z * mt->m[2][2] + mt->m[2][3];
	} static inline void
	          VReverseTransform(VPoint * pt, VMatrix * mt, VPoint * newPt) {
		VPoint    tmp;
		          tmp.x = pt->x - mt->m[0][3];
		          tmp.y = pt->y - mt->m[1][3];
		          tmp.z = pt->z - mt->m[2][3];

		          newPt->x = tmp.x * mt->m[0][0] + tmp.y * mt->m[1][0]
		+         tmp.z * mt->m[2][0];

		          newPt->y = tmp.x * mt->m[0][1] + tmp.y * mt->m[1][1]
		+         tmp.z * mt->m[2][1];

		          newPt->z = tmp.x * mt->m[0][2] + tmp.y * mt->m[1][2]
		+         tmp.z * mt->m[2][2];
	} static inline void
	          VTransform_(VPoint * pt, VMatrix * mt, VPoint * newPt) {

		newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[0][1]
		+ pt->z * mt->m[0][2];

		newPt->y = pt->x * mt->m[1][0] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[1][2];

		newPt->z = pt->x * mt->m[2][0] + pt->y * mt->m[2][1]
		+ pt->z * mt->m[2][2];
	}							/*
								   *  Apply the reverse of a given transformation
								 */ static inline void
	          VReverseTransform_(VPoint * pt, VMatrix * mt, VPoint * newPt) {

		newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[1][0]
		+ pt->z * mt->m[2][0];

		newPt->y = pt->x * mt->m[0][1] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[2][1];

		newPt->z = pt->x * mt->m[0][2] + pt->y * mt->m[1][2]
		+ pt->z * mt->m[2][2];
	}
#else
	extern void VMatrixMult PARAMS((VMatrix *, VMatrix *, VMatrix *));
	extern double VDotProd PARAMS((VPoint *, VPoint *));
	extern void VCrossProd PARAMS((VPoint *, VPoint *, VPoint *));
	extern void VTransform PARAMS((VPoint *, VMatrix *, VPoint *));
	extern void VTransform_ PARAMS((VPoint *, VMatrix *, VPoint *));
	extern void VReverseTransform_ PARAMS((VPoint *, VMatrix *, VPoint *));
	extern unsigned long VConstantColor(Viewport *v, int cIndex);
#endif

/*
 *  Viewport flags (must be changed manually after VOpenViewport for now)
 */

#define	VPClip		1      /* polygons should be clipped before drawing */
#define VPPerspective	2      /* Z coordinate used for depth information */
#define VPMono		4      /* Monochrome environment */
#define VPPixmap	8      /* Use color Pixmap rather than double
								   buffering */
#define VPFastAnimation 16     /* frame-by-frame comparison */
#define VPDepthCueing	32     /* Perform depth cueing */
#define VPDepthCueParsed 64    /* Pixel has been assigned to dc color */
#define VPDoubleBuffer 128   /* use double buffering, if available */

/*
 *  VRotate options
 */

#define XRotation	1			/* rotate about X axis */
#define YRotation	2			/* rotate about Y axis */
#define ZRotation	3			/* rotate about Z axis */

/*
 *  Some units values for VOpenViewport()
 */

#define UNITS_METERS	1.0
#define UNITS_FEET	0.3048

/*
 *  Angle Conversions
 */

#define DEGtoRAD(a)	((a) * M_PI / 180.0)
#define RADtoDEG(a)	((a) * 180.0 / M_PI)

	extern int VComputeObjectAspect (VObject * obj, VPoint * loc);
	extern int VObjectNeedsOrdering (VObject * obj);
	extern void VComputePolygonOrdering (VObject * obj);
	extern char *VGetAspectName (int aspect);
	extern void VPrintPolygon (FILE * file, VPolygon * p);
	extern void VDestroyObject (VObject *obj);
	extern VObject *VCopyObject (VObject *);

	extern VMatrix *VTranslate (VMatrix *, double, double, double);
	extern VMatrix *VTranslatePoint (VMatrix *, VPoint);

	extern VMatrix *VRotate (VMatrix *, int axis, double theta);

	extern VPolygon *VCreatePolygon (int npts, VPoint * pts, VColor *);
	extern VPolygon *VCreatePolygonFromTemplate (int npts, 
												 VPoint * pts,
												 VPolygon * templ);
	extern VPolygon *VCopyPolygon (VPolygon *);

	extern void VIdentMatrix(VMatrix * Mtx);
	extern void VMatrixMultByRank (VMatrix *, 
								   VMatrix *,
								   VMatrix *, 
								   int);

	extern VPolygon *VTransformPolygon (VPolygon * poly, VMatrix * m);

#ifdef _CRTDBG_MAP_ALLOC
#define Vmalloc(s)	Vmalloc_dbg(s, __FILE__, __LINE__)
	extern char *Vmalloc_dbg (int, const char *, const int);
#else
	extern char *Vmalloc (int);
#endif

#if defined(__cplusplus)
}
#endif

#endif /* _vlibmath_h */
