/*------------------------->  ANSI C - sourcefile  <-------------------------*/
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

#include "tkRaster.h"

Tk_Raster *raster_give_raster(Tcl_Interp *interp, char *path) {
  Tcl_CmdInfo info;
  Tk_Raster *raster;
  int result;

  result = Tcl_GetCommandInfo(interp, path, &info);
  fprintf(stderr, "CommandInfo: %d\n", result);
  if(!result) {
    fprintf(stderr, "An error occured in GetCommandInfo:%s\n", path);
    return NULL;
  }
  
  raster = (Tk_Raster*)(info.clientData);
  return raster;
}

void raster_clear(Tk_Raster *c_raster) {
  RasterClear(c_raster);
}

void raster_draw_line(Tk_Raster *c_raster, double *coords, int npoints) {
  RasterDrawLines(c_raster, coords, npoints);
}
  
void raster_draw_point(Tk_Raster *c_raster, double x, double y) {
  RasterDrawPoint(c_raster, (int)x, (int) y);
}

void raster_draw_points(Tk_Raster *c_raster, double *coords, int npoints) {
  RasterDrawPoints(c_raster, coords, npoints);
}

void raster_draw_rectangle(Tk_Raster *c_raster, double x0, double y0, double x1, double y1) {
  double coords[] = {x0, y0, x1, y1};

  RasterDrawRectangles(c_raster, coords, 1);
}

void raster_fill_polygon(Tk_Raster *c_raster, double *coords, int npoints) {
  RasterFillPolygon(c_raster, coords, npoints);
}

void raster_fill_rectangle(Tk_Raster *c_raster, double x0, double y0, double x1, double y1) {
  double coords[] = {x0, y0, x1, y1};

  RasterFillRectangles(c_raster, coords, 1);
}

void raster_set_world(Tk_Raster *c_raster, double x0, double y0, double x1, double y1) {
  SetRasterCoords(c_raster, x0, y0, x1, y1);
}

void raster_world_to_raster(Tk_Raster *c_raster, double wx, double wy, double *out_rx, double *out_ry) {
  int rx_i, ry_i;

  WorldToRaster(c_raster, wx, wy, &rx_i, &ry_i);
  *out_rx = (double)rx_i;
  *out_ry = (double)ry_i;
}

void raster_raster_to_world(Tk_Raster *c_raster, double rx, double ry, double *out_wx, double *out_wy) {
  RasterToWorld(c_raster, (int)rx, (int)ry, out_wx, out_wy);
}


void raster_display(Tk_Raster *c_raster) {
  RasterDisplay(c_raster);
}
