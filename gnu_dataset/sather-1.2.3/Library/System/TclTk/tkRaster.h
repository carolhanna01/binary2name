/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#ifndef TK_RASTER_H
#define TK_RASTER_H

#include <tk.h>

/*--------------------------------------------------------------------------- 
 *
 *  A Tk_Raster is here defined as void. This was done on purpose -
 *  it is a simple way of saying that Tk_Raster should only be accessed
 *  from inside the tkRaster.c module. All other modules should 
 *  only make use of the procedures defined here.
 */
typedef void Tk_Raster;

/*---------------------------------------------------------------------------
 *
 *  Below are defined built-in drawing functions for rasters. They should
 *  be enough for extending the Raster to draw any object (see the
 *  function RasterAddPrimitive). All object coordinates are specified in
 *  "world" units (but see functions RasterToWorld and WorldToRaster). 
 *  Attributes such as color, linestyle etc using for drawing are those
 *  of the current drawing environment of the raster (see also functions 
 *  DrawEnvIndex and SetDrawEnv).
 */

EXTERN void RasterDrawPoints _ANSI_ARGS_((Tk_Raster*, double* coord,int npts));
/* 
 *  Draws the points (coord [2i], coord [2i+1]) for 0 <= i < npts
 */
EXTERN void RasterDrawPoint _ANSI_ARGS_((Tk_Raster*, int x, int y));

EXTERN void RasterDrawLines _ANSI_ARGS_((Tk_Raster*,double* coord,int npts));
/* 
 *  Draws the npts-1 line segments connecting the successive
 *  'npts' points (coord [2i], coord [2i+1]), for 0 <= i < npts. 
 */

EXTERN void RasterDrawRectangles _ANSI_ARGS_ ((Tk_Raster*, double* coord, 
					       int nrects));
/*     
 *  Draws rectangles with sides aligned with the coordinate axes and 
 *  with diagonal corners at (coord [4i], coord [4i+1]) and 
 *  (coord [4i+2], coord [4i+3]) for  0 <= i < nrects, 
 */

EXTERN void RasterFillRectangles _ANSI_ARGS_ ((Tk_Raster*, double* coord, 
					       int nrects));
/*     
 *  Draws filled rectangles with sides aligned with the coordinate axes and 
 *  with diagonal corners at (coord [4i], coord [4i+1]) and 
 *  (coord [4i+2], coord [4i+3]) for  0 <= i < nrects, 
 */


EXTERN void RasterFillPolygon _ANSI_ARGS_ ((Tk_Raster*, double* coord,
					   int npts));
/* 
 *  Fills the polygon bounded by the line segments connecting the successive
 *  'npts' points (coord [2i], coord [2i+1]), for 0 <= i < npts. 
 *  If the last point is not equal to the first point, a line segment 
 *  connecting them is added.
 */

EXTERN void RasterClear _ANSI_ARGS_ ((Tk_Raster *));

/* Clear the raster
 */

EXTERN void RasterDisplay _ANSI_ARGS_ ((Tk_Raster *));

/* Let the raster be redisplayed; this is not automatic if
 * one directly calls the C drawing functions !
 */

/*--------------------------------------------------------------------------
 *
 *  The user may specify many 'drawing environments' which are essentially 
 *  Graphics Contexts. These are assigned numbers (indices), being 0 the number
 *  of the default. DrawEnvIndex performs a search in the Raster structure
 *  for a given drawing environment index and, if successful,
 *  returns a pointer to the structure that holds that drawing environment's
 *  data (the ClientData argument). 
 *  SetDrawEnv then can be used to load the parameters of the
 *  drawing environment into the Raster's GC.
 *  GetDrawEnv is used to get the Raster's current drawing environment.
 */

EXTERN int DrawEnvIndex _ANSI_ARGS_((Tcl_Interp*, Tk_Raster*,int,ClientData*));
EXTERN int SetDrawEnv _ANSI_ARGS_ ((Tcl_Interp*, Tk_Raster*, ClientData));
EXTERN void GetDrawEnv _ANSI_ARGS_ ((Tk_Raster*, ClientData*));

/*--------------------------------------------------------------------------
 *
 *  SetRasterModifiedArea may be used by primitive implementations
 *  to let the raster know what part of the pixmap was modified. 
 *  If not called, raster will assume that all pixmap was modified.
 *  If implementing a raster command that does not actually modify the
 *  pixmap, this routine should be called with 0 0 0 0 as args.
 */
EXTERN void SetRasterModifiedArea _ANSI_ARGS_((Tk_Raster* raster,
					       int rx0, int ry0, 
					       int rx1, int ry1));

/*---------------------------------------------------------------------------
 *
 *  Utility functions for handling world-to-raster-and-back  transformations
 */

EXTERN void SetRasterCoords _ANSI_ARGS_((Tk_Raster*, 
					 double x0, double y0,
					 double x1, double y1));
/*    
 *  (x0,y0) are the coordinates of the upper-left corner and 
 *  (x1,y1) are the coordinates of the lower-right corner
 */

EXTERN void WorldToRaster _ANSI_ARGS_ ((Tk_Raster*, double wx, double wy,
					int* rx, int* ry));
/*
 *  (wx,wy) are  world coordinates
 *  (*rx,*ry) are set to the corresponding raster coordinates
 */

EXTERN void RasterToWorld _ANSI_ARGS_ ((Tk_Raster*, int rx, int ry, 
					double* wx, double* wy));
/*
 *  (rx,ry) are raster coordinates
 *  (*wx,*wy) are set to the corresponding world coordinates
 */

/*--------------------------------------------------------------------------
 * 
 *  A new primitive is to be implemented as three procedures:
 *
 *  . A RasterPrimInitProc, which sets up some storage to be maintained
 *     	 	by the raster module.
 *
 *  . A RasterPrimDrawProc, which actually draws the primitive.
 *
 *  . A RasterPrimFreeProc, which frees up the storage allocated by 
 *           	RasterPrimInitProc.
 * 
 *  The 'Init' and 'Free' procs are called just after the raster is created
 *  and just before the raster is destroyed respectively. In order
 *  to implement a new primitive, one must write these three routines (or
 *  just a RasterPrimDrawProc if the primitive does not require
 *  storage to be kept per window) and then put a call to
 *  RasterAddPrimitive in the application initialization
 *  routine (usually called Tcl_AppInit), just after the call to RasterInit.
 */

typedef int RasterPrimDrawProc _ANSI_ARGS_((Tcl_Interp * interp,
					    Tk_Raster * raster,
					    ClientData data,
					    int argc, 
					    char* argv [])); 

typedef int RasterPrimInitProc _ANSI_ARGS_((Tcl_Interp * interp,
					    Tk_Raster* raster,
					    ClientData * dataptr));

typedef void RasterPrimFreeProc _ANSI_ARGS_((Tk_Raster* raster,
					     ClientData data));
					    
EXTERN int RasterAddPrimitive _ANSI_ARGS_((Tcl_Interp * interp,
					   char * primitivename, 
					   RasterPrimDrawProc * drawproc,
					   RasterPrimInitProc * initproc,
					   RasterPrimFreeProc * freeproc));

/*---------------------------------------------------------------------------
 *
 *  RasterInit: initializes the Raster Widget module. This involves basically
 *  setting up the built in raster primitives. This procedure
 *  should be called just once (During the application initialization)
 */
EXTERN int RasterInit _ANSI_ARGS_((Tcl_Interp * interp));

/*---------------------------------------------------------------------------
 *  RasterCmd: implements the raster widget command, i.e., 
 *
 *     raster <pathName> ?option? ... ?option?
 *
 */
EXTERN int RasterCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, 
				  int argc,  char **argv));


/*---------------------------------------------------------------------------
 *
 *  The following are utility functions to access elements of a Tk_Raster.
 *  Normally, there's no need to use them - drawing should be done
 *  by calling the RasterDrawXXX functions. 
 *
 *  (The functions below are needed only if you are implementing something
 *   that has to use some weird X drawing function)
 */


EXTERN Drawable GetRasterDrawable _ANSI_ARGS_(( Tk_Raster* ));
EXTERN Display* GetRasterDisplay _ANSI_ARGS_(( Tk_Raster* ));
EXTERN Tk_Window GetRasterTkWin _ANSI_ARGS_(( Tk_Raster* ));
EXTERN GC GetRasterGC _ANSI_ARGS_(( Tk_Raster*));

char *
GetRasterColour (Tcl_Interp *interp,
		 Tk_Raster *RasterPtr,
		 int index);

#endif /* TK_RASTER_H */
