/*******************************************************************************
*
* DrawSymbols.c
*
* Contains functions:
*    DrawLine
*    DrawDashedLine
*    DrawDottedLine
*    DrawCircle
*    DrawTriangle
*    DrawSquare
*    DrawDiamond
*    DrawPentagon
*    DrawHexagon
*    DrawPlus
*    DrawX
*    DrawStar
*    DrawAsterisk
*    DrawBar
*    DrawMesh
*    DrawContour
*    DrawColorPlot
*
* Functions draw lines and plot symbols.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*******************************************************************************/


#include <math.h>
#include "gsegraf.h"


void DrawLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int line_width )
   {
   /* Declare variables */
   GnomeCanvasItem *line;


   /* Check linewidth */
   if ( line_width == 0 )
      return;


   /* Draw line */
   line = gnome_canvas_item_new(group,
                                GNOME_TYPE_CANVAS_LINE,
                                "points", points,
                                "fill_color_rgba", fill_color_rgba,
                                "width_pixels", line_width,
                                NULL);
   }


void DrawDashedLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int line_width )
   {
   /* Declare variables */
   int i, j, j1, j2, k, k1, k2, ipts, npoints, nlines;
   double *x, *y, *length, length_total, inc, fspace, factor,
          xinterp[2], yinterp[2], length_interp[2];
   GnomeCanvasPoints *pts;
   GnomeCanvasItem *line;


   /* Check linewidth */
   if ( line_width == 0 )
      return;


   /* Calculate line length */
   npoints = points->num_points;
   x = xmalloc(npoints*sizeof(double));
   y = xmalloc(npoints*sizeof(double));
   length = xmalloc(npoints*sizeof(double));
   x[0] = points->coords[0];
   y[0] = points->coords[1];
   length[0] = 0.0;
   for ( i=1; i<npoints; i++ )
      {
      x[i] = points->coords[2*i];
      y[i] = points->coords[2*i+1];
      length[i] = length[i-1] +
                  sqrt((x[i] - x[i-1])*(x[i] - x[i-1]) +
                       (y[i] - y[i-1])*(y[i] - y[i-1]));
      }
   length_total = length[npoints-1];


   /* Draw short lines */
   if ( length_total < 2.0 )
      {
      free(x);
      free(y);
      free(length);
      return;
      }
   else if ( length_total <= dash )
      {
      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", line_width,
                                   NULL);
      free(x);
      free(y);
      free(length);
      return;
      }


   /* Recalculate dash and space lengths */
   inc = dash + space_dash;
   fspace = space_dash/inc;
   if ( sqrt((x[npoints-1] - x[0])*(x[npoints-1] - x[0]) +
             (y[npoints-1] - y[0])*(y[npoints-1] - y[0])) >= space_dash )
      {
      /* Line ends with dash */
      nlines = roundint((length_total + space_dash)/inc);
      inc = length_total/(nlines - fspace);
      factor = length_total + fspace*inc;
      }
   else
      {
      /* Line ends with space */
      nlines = roundint(length_total/inc);
      inc = length_total/nlines;
      factor = length_total;
      }


   /* Interpolate and draw dashed line */
   j1 = 2;
   j2 = npoints - 1;
   for ( i=1; i<=nlines; i++ )
      {
      /* Find coordinates for dash beginning and end */
      length_interp[0] = (((double) i - 1.0)/nlines)*factor;
      length_interp[1] = (((double) i - fspace)/nlines)*factor;

      interp1(npoints, 2, length, x, &length_interp[0], &xinterp[0]);
      interp1(npoints, 2, length, y, &length_interp[0], &yinterp[0]);

      /* Find data points contained in dash */
      k1 = j1;
      k2 = j2;
      for ( j=j1; j<=j2; j++ )
         {
         if ( length[j-1] <= length_interp[0] )
            k1 = j + 1;
         if ( length[j-1] >= length_interp[1] )
            {
            k2 = j - 1;
            j1 = j;
            break;
            }
         }

      /* Calculate points-structure components for dash */
      pts = gnome_canvas_points_new(k2 - k1 + 3);
      ipts = 0;
      pts->coords[0] = xinterp[0];
      pts->coords[1] = yinterp[0];
      for ( k=k1; k<=k2; k++ )
         {
         ipts++;
         pts->coords[2*ipts]   = x[k-1];
         pts->coords[2*ipts+1] = y[k-1];
         }
      ipts++;
      pts->coords[2*ipts]   = xinterp[1];
      pts->coords[2*ipts+1] = yinterp[1];

      /* Draw dash */
      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", pts,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", line_width,
                                   NULL);
      }

   free(x);
   free(y);
   free(length);
   gnome_canvas_points_unref(pts);
   }


void DrawDottedLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int size )
   {
   /* Declare variables */
   int i, npoints, ndots;
   double *x, *y, *length, length_total, inc, inc_calc, length_interp, xinterp, yinterp;


   /* Check size */
   if ( size == 0 )
      return;


   /* Calculate line length */
   npoints = points->num_points;
   x = xmalloc(npoints*sizeof(double));
   y = xmalloc(npoints*sizeof(double));
   length = xmalloc(npoints*sizeof(double));
   x[0] = points->coords[0];
   y[0] = points->coords[1];
   length[0] = 0.0;
   for ( i=1; i<npoints; i++ )
      {
      x[i] = points->coords[2*i];
      y[i] = points->coords[2*i+1];
      length[i] = length[i-1] +
                  sqrt((x[i] - x[i-1])*(x[i] - x[i-1]) +
                       (y[i] - y[i-1])*(y[i] - y[i-1]));
      }
   length_total = length[npoints-1];


   /* Check line length */
   inc = space_dot + size;
   if ( length_total < inc )
      {
      free(x);
      free(y);
      free(length);
      return;
      }


   /* Calculate number of dots */
   ndots = roundint(length_total/inc + 1.0);
   inc_calc = length_total/(ndots - 1);


   /* Interpolate and draw dots */
   for ( i=1; i<=ndots; i++ )
      {
      length_interp = length[0] + (i - 1)*inc_calc;
      interp1(npoints, 1, length, x, &length_interp, &xinterp);
      interp1(npoints, 1, length, y, &length_interp, &yinterp);
      DrawCircle(xinterp, yinterp, fill_color_rgba, fill_color_rgba, size);
      }

   free(x);
   free(y);
   free(length);
   }


void DrawCircle ( double x, double y, guint32 fill_color_rgba,
                  guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r;
   GnomeCanvasItem *circle;


   /* Draw circle */
   if ( size == 0 )
      return;
   else
      {
      r = 0.5*size;

      circle = gnome_canvas_item_new(group,
                                     GNOME_TYPE_CANVAS_ELLIPSE,
                                     "x1", x - r,
                                     "x2", x + r,
                                     "y1", y - r,
                                     "y2", y + r,
                                     "fill_color_rgba", fill_color_rgba,
                                     "outline_color_rgba", outline_color_rgba,
                                     "width_pixels", 1,
                                     NULL);
      }
   }


void DrawTriangle ( double x, double y, guint32 fill_color_rgba,
                    guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx, dy;
   GnomeCanvasItem *triangle;
   GnomeCanvasPoints *points;


   /* Draw triangle */
   if ( size == 0 )
      return;
   else
      {
      r = 0.666666667*size;
      dx = 0.577350269*size;
      dy = 0.333333333*size;

      points = gnome_canvas_points_new(3);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - dx;
      points->coords[3] = y + dy;
      points->coords[4] = x + dx;
      points->coords[5] = y + dy;

      triangle = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_POLYGON,
                                       "points", points,
                                       "fill_color_rgba", fill_color_rgba,
                                       "outline_color_rgba", outline_color_rgba,
                                       "width_pixels", 1,
                                       NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawSquare ( double x, double y, guint32 fill_color_rgba,
                  guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double dx, dy;
   GnomeCanvasItem *square;


   /* Draw square */
   if ( size == 0 )
      return;
   else
      {
      dx = 0.5*size;
      dy = 0.5*size;

      square = gnome_canvas_item_new(group,
                                     GNOME_TYPE_CANVAS_RECT,
                                     "x1", x - dx,
                                     "x2", x + dx,
                                     "y1", y - dy,
                                     "y2", y + dy,
                                     "fill_color_rgba", fill_color_rgba,
                                     "outline_color_rgba", outline_color_rgba,
                                     "width_pixels", 1,
                                     NULL);
      }
   }


void DrawDiamond ( double x, double y, guint32 fill_color_rgba,
                   guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r;
   GnomeCanvasItem *diamond;
   GnomeCanvasPoints *points;


   /* Draw diamond */
   if ( size == 0 )
      return;
   else
      {
      r = 0.707106781*size;

      points = gnome_canvas_points_new(4);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - r;
      points->coords[3] = y;
      points->coords[4] = x;
      points->coords[5] = y + r;
      points->coords[6] = x + r;
      points->coords[7] = y;

      diamond = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_POLYGON,
                                      "points", points,
                                      "fill_color_rgba", fill_color_rgba,
                                      "outline_color_rgba", outline_color_rgba,
                                      "width_pixels", 1,
                                      NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawPentagon ( double x, double y, guint32 fill_color_rgba,
                    guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx1, dx2, dy1, dy2;
   GnomeCanvasItem *pentagon;
   GnomeCanvasPoints *points;


   /* Draw pentagon */
   if ( size == 0 )
      return;
   else
      {
      r = 0.618033988*size;
      dx1 = 0.587785252*size;
      dy1 = 0.190983005*size;
      dx2 = 0.363271264*size;
      dy2 = 0.5*size;

      points = gnome_canvas_points_new(5);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - dx1;
      points->coords[3] = y - dy1;
      points->coords[4] = x - dx2;
      points->coords[5] = y + dy2;
      points->coords[6] = x + dx2;
      points->coords[7] = y + dy2;
      points->coords[8] = x + dx1;
      points->coords[9] = y - dy1;

      pentagon = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_POLYGON,
                                       "points", points,
                                       "fill_color_rgba", fill_color_rgba,
                                       "outline_color_rgba", outline_color_rgba,
                                       "width_pixels", 1,
                                       NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawHexagon ( double x, double y, guint32 fill_color_rgba,
                   guint32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx, dy;
   GnomeCanvasItem *hexagon;
   GnomeCanvasPoints *points;


   /* Draw hexagon */
   if ( size == 0 )
      return;
   else
      {
      r = 0.577350269*size;
      dx = 0.288675134*size;
      dy = 0.5*size;

      points = gnome_canvas_points_new(6);
      points->coords[0]  = x - dx;
      points->coords[1]  = y - dy;
      points->coords[2]  = x - r;
      points->coords[3]  = y;
      points->coords[4]  = x - dx;
      points->coords[5]  = y + dy;
      points->coords[6]  = x + dx;
      points->coords[7]  = y + dy;
      points->coords[8]  = x + r;
      points->coords[9]  = y;
      points->coords[10] = x + dx;
      points->coords[11] = y - dy;

      hexagon = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_POLYGON,
                                      "points", points,
                                      "fill_color_rgba", fill_color_rgba,
                                      "outline_color_rgba", outline_color_rgba,
                                      "width_pixels", 1,
                                      NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawPlus ( double x, double y, guint32 fill_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r;
   GnomeCanvasItem *line;
   GnomeCanvasPoints *points;


   /* Draw plus sign */
   if ( size == 0 )
      return;
   else
      {
      r = 0.707106781*size;

      points = gnome_canvas_points_new(2);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y + r;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x - r;
      points->coords[1] = y;
      points->coords[2] = x + r;
      points->coords[3] = y;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawX ( double x, double y, guint32 fill_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double dx, dy;
   GnomeCanvasItem *line;
   GnomeCanvasPoints *points;


   /* Draw x */
   if ( size == 0 )
      return;
   else
      {
      dx = 0.5*size;
      dy = 0.5*size;

      points = gnome_canvas_points_new(2);
      points->coords[0] = x - dx;
      points->coords[1] = y - dy;
      points->coords[2] = x + dx;
      points->coords[3] = y + dy;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x + dx;
      points->coords[1] = y - dy;
      points->coords[2] = x - dx;
      points->coords[3] = y + dy;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawStar ( double x, double y, guint32 fill_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx1, dx2, dy1, dy2;
   GnomeCanvasItem *line;
   GnomeCanvasPoints *points;


   /* Draw star */
   if ( size == 0 )
      return;
   else
      {
      r = 0.618033988*size;
      dx1 = 0.587785252*size;
      dy1 = 0.190983005*size;
      dx2 = 0.363271264*size;
      dy2 = 0.5*size;

      points = gnome_canvas_points_new(3);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x - dx2;
      points->coords[5] = y + dy2;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x + dx2;
      points->coords[5] = y + dy2;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x - dx1;
      points->coords[1] = y - dy1;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x + dx1;
      points->coords[5] = y - dy1;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawAsterisk ( double x, double y, guint32 fill_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx, dy;
   GnomeCanvasItem *line;
   GnomeCanvasPoints *points;


   /* Draw asterisk */
   if ( size == 0 )
      return;
   else
      {
      r = 0.577350269*size;
      dx = 0.5*size;
      dy = 0.288675134*size;

      points = gnome_canvas_points_new(2);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y + r;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x + dx;
      points->coords[1] = y + dy;
      points->coords[2] = x - dx;
      points->coords[3] = y - dy;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      points->coords[0] = x - dx;
      points->coords[1] = y + dy;
      points->coords[2] = x + dx;
      points->coords[3] = y - dy;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);

      gnome_canvas_points_unref(points);
      }
   }


void DrawBar ( double x1, double y1, double x2, double y2,
               guint32 fill_color_rgba, guint32 outline_color_rgba )
   {
   /* Declare variables */
   GnomeCanvasItem *bar;


   /* Draw bar */
   bar = gnome_canvas_item_new(group,
                               GNOME_TYPE_CANVAS_RECT,
                               "x1", x1,
                               "x2", x2,
                               "y1", y1,
                               "y2", y2,
                               "fill_color_rgba", fill_color_rgba,
                               "outline_color_rgba", outline_color_rgba,
                               "width_pixels", 1,
                               NULL);
   }


void DrawMesh ( double x1, double y1, double x2, double y2,
                guint32 fill_color_rgba, guint32 outline_color_rgba, int flag )
   {
   /* Declare variables */
   int i;
   guint32 fill_color[] = { 0xFF000000, 0xFF800000, 0xFFFF0000, 0x80FF0000, 0x00FF0000 }, alpha_mesh;
   double dx[] = { 0.0, 6.0, 12.0, 18.0, 24.0, 30.0, 36.0, 42.0, 48.0, 54.0, 60.0 };
   GnomeCanvasItem *rectangle, *line;
   GnomeCanvasPoints *points;


   /* Draw rectangle */
   if ( flag != 7 )
      {
      rectangle = gnome_canvas_item_new(group,
                                        GNOME_TYPE_CANVAS_RECT,
                                        "x1", x1,
                                        "x2", x2,
                                        "y1", y1 - 6.0,
                                        "y2", y2 + 6.0,
                                        "fill_color_rgba", fill_color_rgba,
                                        "outline_color_rgba", outline_color_rgba,
                                        "width_pixels", 1,
                                        NULL);
      }

   else
      {
      alpha_mesh = fill_color_rgba;
      for ( i=1; i<=5; i++ )
         {
         fill_color[i-1] = fill_color[i-1] + alpha_mesh;
         rectangle = gnome_canvas_item_new(group,
                                           GNOME_TYPE_CANVAS_RECT,
                                           "x1", x1 + dx[2*(i-1)],
                                           "x2", x1 + dx[2*i],
                                           "y1", y1 - 6.0,
                                           "y2", y2 + 6.0,
                                           "fill_color_rgba", fill_color[i-1],
                                           "outline_color_rgba", outline_color_rgba,
                                           "width_pixels", 1,
                                           NULL);
         }
      }


   /* Draw horizontal line */
   points = gnome_canvas_points_new(2);

   points->coords[0] = x1;
   points->coords[1] = y1;
   points->coords[2] = x2;
   points->coords[3] = y2;

   line = gnome_canvas_item_new(group,
                                GNOME_TYPE_CANVAS_LINE,
                                "points", points,
                                "fill_color_rgba", outline_color_rgba,
                                "width_pixels", 1,
                                NULL);


   /* Draw vertical lines */
   for ( i=2; i<=10; i++ )
      {
      points->coords[0] = x1 + dx[i-1];
      points->coords[1] = y1 - 6.0;
      points->coords[2] = x1 + dx[i-1];
      points->coords[3] = y2 + 6.0;

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", outline_color_rgba,
                                   "width_pixels", 1,
                                   NULL);
      }

   gnome_canvas_points_unref(points);
   }


void DrawContour ( double x1, double y1, double x2, double y2,
                   guint32 fill_color_rgba, guint32 outline_color_rgba, int flag )
   {
   /* Declare variables */
   GnomeCanvasItem *rectangle, *ellipse1, *ellipse2;


   /* Draw filled rectangle */
   if ( strcmp(p_plot_param->axis_type, "3d" ) == 0 )
      rectangle = gnome_canvas_item_new(group,
                                        GNOME_TYPE_CANVAS_RECT,
                                        "x1", x1,
                                        "x2", x2,
                                        "y1", y1 - 6.0,
                                        "y2", y2 + 6.0,
                                        "fill_color_rgba", fill_color_rgba,
                                        "outline_color_rgba", fill_color_rgba,
                                        "width_pixels", 1,
                                        NULL);


   /* Initialize ellipses */
   ellipse1 = gnome_canvas_item_new(group,
                                    GNOME_TYPE_CANVAS_ELLIPSE,
                                    "x1", x1,
                                    "x2", x2,
                                    "y1", y1 - 6.0,
                                    "y2", y2 + 6.0,
                                    "width_pixels", 1,
                                    NULL);

   ellipse2 = gnome_canvas_item_new(group,
                                    GNOME_TYPE_CANVAS_ELLIPSE,
                                    "x1", 0.75*x1 + 0.25*x2,
                                    "x2", 0.25*x1 + 0.75*x2,
                                    "y1", y1 - 3.0,
                                    "y2", y2 + 3.0,
                                    "width_pixels", 1,
                                    NULL);


   if ( flag == 1 || flag == 3 )
      {
      /* 2d contour plot */
      gnome_canvas_item_set(ellipse1,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", fill_color_rgba,
                            NULL);

      gnome_canvas_item_set(ellipse2,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", fill_color_rgba,
                            NULL);
      }

   else if ( flag == 7 )
      {
      /* 2d contour plot */
      gnome_canvas_item_set(ellipse1,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", 0xFF8000FF,
                            NULL);

      gnome_canvas_item_set(ellipse2,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", 0xFF0000FF,
                            NULL);
      }

   else if ( flag == 2 || flag == 4 || flag == 5 || flag == 6 )
      {
      /* 3d contour plot */
      gnome_canvas_item_set(ellipse1,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", outline_color_rgba,
                            NULL);

      gnome_canvas_item_set(ellipse2,
                            "fill_color_rgba", 0xFFFFFF00,
                            "outline_color_rgba", outline_color_rgba,
                            NULL);
      }
   }


void DrawColorPlot ( double x1, double y1, double x2, double y2 )
   {
   int i, j, nx = 11, ny = 3;
   guint32 color;
   double x[11] = { -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5 },
          y[3] = { -1, 0, 1 },
          z[11][3] = { { -1.9407e-02,  3.8980e-17, -1.9407e-02 },
                       {  2.0208e-01,  2.3387e-01,  2.0208e-01 },
                       {  4.6034e-01,  5.0455e-01,  4.6034e-01 },
                       {  7.0200e-01,  7.5683e-01,  7.0200e-01 },
                       {  8.7350e-01,  9.3549e-01,  8.7350e-01 },
                       {  9.3549e-01,  1.0000e+00,  9.3549e-01 },
                       {  8.7350e-01,  9.3549e-01,  8.7350e-01 },
                       {  7.0200e-01,  7.5683e-01,  7.0200e-01 },
                       {  4.6034e-01,  5.0455e-01,  4.6034e-01 },
                       {  2.0208e-01,  2.3387e-01,  2.0208e-01 },
                       { -1.9407e-02,  3.8980e-17, -1.9407e-02 } },
          xi[61], yi[13], zi[61][13], zinterp, zmin, zmax, zscale, fraction;
   GdkPixbuf *pixbuf;
   GnomeCanvasItem *pixbuf_item;


   /* Normalize values of x and y */
   for ( i=1; i<=11; i++ )
      x[i-1] = x[i-1]*pi/5.0;
   for ( j=1; j<=3; j++ )
      y[j-1] = y[j-1]*pi/5.0;


   /* Calculate interpolated values of x, y, and z */
   for ( i=1; i<=61; i++ )
      xi[i-1] = x[0] + (i - 1)*(x[10] - x[0])/60;
   for ( j=1; j<=13; j++ )
      yi[j-1] = y[0] + (j - 1)*(y[2] - y[0])/12;

   for ( i=1; i<=61; i++ )
      for ( j=1; j<=13; j++ )
         interp2(nx, ny, 1, &x[0], &y[0], &z[0][0],
                 &xi[i-1], &yi[j-1], &zi[i-1][j-1]);


   /* Create pixbuf */
   pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, 61, 13);
   gdk_pixbuf_fill(pixbuf, 0xFFFFFF00);


   /* Draw color-plot symbol */
   zmin = min(793, &zi[0][0]);
   zmax = max(793, &zi[0][0]);
   zscale = 1.0/(zmax - zmin);
   for ( i=1; i<=61; i++ )
      for ( j=13; j>=1; j-- )
         {
         zinterp = zi[i-1][j-1];
         fraction = (zinterp - zmin)*zscale;
         color = interp_color_1(fraction);
         put_pixel(pixbuf, i-1, 13-j, color);
         }


   /* Draw pixbuf on canvas */
   pixbuf_item = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_PIXBUF,
                                       "pixbuf", pixbuf,
                                       "x", (x1 + x2)/2.0,
                                       "y", (y1 + y2)/2.0,
                                       "anchor", GTK_ANCHOR_CENTER,
                                       NULL);

   g_object_unref(pixbuf);
   }

