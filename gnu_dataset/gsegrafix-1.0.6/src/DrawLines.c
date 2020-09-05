/*******************************************************************************
*
* DrawLines.c
*
* Contains functions:
*    DrawLines2d
*    DrawLinesPolar
*    DrawLines3d
*
* Functions draw solid lines connecting data points.
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


void DrawLines2d ( int npts, double *x, double *y, double xmin, double xmax,
                   double ymin, double ymax, double xscale, double yscale,
                   guint32 color, unsigned int line_width, char *line_type )
   {
   /* Declare variables */
   int i, i1, i2, j, ipoints;
   double x1_box, x2_box, y1_box, y2_box, line_coords[4], x1, y1, x2, y2;
   GnomeCanvasPoints *points;


   /* Check for at least two points */
   if ( npts < 2 )
      return;


   /* Specify plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Draw continuous lines */
   i1 = 0;
   i2 = 0;
   for ( i=1; i<=npts; i++ )
      {
      /* Check if current point is in plotting area */
      if ( xmin <= x[i-1] && x[i-1] <= xmax &&
           ymin <= y[i-1] && y[i-1] <= ymax )
         {
         if ( (i2 = i) < npts )
            continue;
         }

      /* Draw lines between points within plotting area */
      if ( i2 > i1 )
         {
         if ( i1 > 0 )
            i1--;
         line_coords[0] = x[i1];
         line_coords[1] = y[i1];
         line_coords[2] = x[i1+1];
         line_coords[3] = y[i1+1];
         Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]);
         x1 = line_coords[0];
         y1 = line_coords[1];

         if ( i2 < npts )
            {
            line_coords[0] = x[i2-1];
            line_coords[1] = y[i2-1];
            line_coords[2] = x[i2];
            line_coords[3] = y[i2];
            Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]);
            x2 = line_coords[2];
            y2 = line_coords[3];
            }

         if ( i2 == npts )
            points = gnome_canvas_points_new(i2 - i1);
         else
            points = gnome_canvas_points_new(i2 - i1 + 1);

         ipoints = 0;
         points->coords[0] = x1_box + (x1 - xmin)*xscale;
         points->coords[1] = y2_box - (y1 - ymin)*yscale;
         for ( j=i1+1; j<=i2-1; j++ )
            {
            ipoints++;
            points->coords[2*ipoints]   = x1_box + (x[j] - xmin)*xscale;
            points->coords[2*ipoints+1] = y2_box - (y[j] - ymin)*yscale;
            }
         if ( i2 < npts )
            {
            ipoints++;
            points->coords[2*ipoints]   = x1_box + (x2 - xmin)*xscale;
            points->coords[2*ipoints+1] = y2_box - (y2 - ymin)*yscale;
            }

         if ( strcmp(line_type, "solid") == 0 )
            DrawLine(points, color, line_width);
         else if ( strcmp(line_type, "dashed") == 0 )
            DrawDashedLine(points, color, line_width);
         else if ( strcmp(line_type, "dotted") == 0 )
            DrawDottedLine(points, color, line_width);
         gnome_canvas_points_unref(points);
         }

      /* Draw lines between points outside plotting area if lines intersect plotting area */
      else if ( i2 == i1 && 0 < i1 && i1 < npts )
         {
         line_coords[0] = x[i1-1];
         line_coords[1] = y[i1-1];
         line_coords[2] = x[i1];
         line_coords[3] = y[i1];

         if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
            {
            points = gnome_canvas_points_new(2);
            points->coords[0] = x1_box + (line_coords[0] - xmin)*xscale;
            points->coords[1] = y2_box - (line_coords[1] - ymin)*yscale;
            points->coords[2] = x1_box + (line_coords[2] - xmin)*xscale;
            points->coords[3] = y2_box - (line_coords[3] - ymin)*yscale;

            if ( strcmp(line_type, "solid") == 0 )
               DrawLine(points, color, line_width);
            else if ( strcmp(line_type, "dashed") == 0 )
               DrawDashedLine(points, color, line_width);
            else if ( strcmp(line_type, "dotted") == 0 )
               DrawDottedLine(points, color, line_width);
            gnome_canvas_points_unref(points);
            }
         }

      i1 = i;
      i2 = i;
      }
   }


void DrawLinesPolar ( int npts, double *x, double *y, double xorigin, double yorigin,
                      double rmin, double rmax, double rscale,
                      guint32 color, unsigned int line_width, char *line_type )
   {
   /* Declare variables */
   int i, i1, i2, j, ipoints;
   double line_coords[4], theta, r, theta1, r1, theta2, r2;
   GnomeCanvasPoints *points;


   /* Check for at least two points */
   if ( npts < 2 )
      return;


   /* Draw continuous lines */
   i1 = 0;
   i2 = 0;
   for ( i=1; i<=npts; i++ )
      {
      /* Check if current point is in plotting area */
      if ( y[i-1] <= rmax )
         {
         if ( (i2 = i) < npts )
            continue;
         }

      /* Draw lines between points within plotting area */
      if ( i2 > i1 )
         {
         if ( i1 > 0 )
            i1--;
         line_coords[0] = x[i1];
         line_coords[1] = y[i1];
         line_coords[2] = x[i1+1];
         line_coords[3] = y[i1+1];
         ClipPolar(rmin, rmax, &line_coords[0]);
         theta1 = line_coords[0];
         r1     = line_coords[1];

         if ( i2 < npts )
            {
            line_coords[0] = x[i2-1];
            line_coords[1] = y[i2-1];
            line_coords[2] = x[i2];
            line_coords[3] = y[i2];
            ClipPolar(rmin, rmax, &line_coords[0]);
            theta2 = line_coords[2];
            r2     = line_coords[3];
            }

         if ( i2 == npts )
            points = gnome_canvas_points_new(i2 - i1);
         else
            points = gnome_canvas_points_new(i2 - i1 + 1);

         ipoints = 0;
         points->coords[0] = xorigin + (r1 - rmin)*cos(theta1)*rscale;
         points->coords[1] = yorigin - (r1 - rmin)*sin(theta1)*rscale;
         for ( j=i1+1; j<=i2-1; j++ )
            {
            ipoints++;
            theta = x[j];
            if ( (r = y[j]) < rmin )
               r = rmin;
            points->coords[2*ipoints]   = xorigin + (r - rmin)*cos(theta)*rscale;
            points->coords[2*ipoints+1] = yorigin - (r - rmin)*sin(theta)*rscale;
            }
         if ( i2 < npts )
            {
            ipoints++;
            points->coords[2*ipoints]   = xorigin + (r2 - rmin)*cos(theta2)*rscale;
            points->coords[2*ipoints+1] = yorigin - (r2 - rmin)*sin(theta2)*rscale;
            }

         if ( strcmp(line_type, "solid") == 0 )
            DrawLine(points, color, line_width);
         else if ( strcmp(line_type, "dashed") == 0 )
            DrawDashedLine(points, color, line_width);
         else if ( strcmp(line_type, "dotted") == 0 )
            DrawDottedLine(points, color, line_width);
         gnome_canvas_points_unref(points);
         }

      /* Draw lines between points outside plotting area if lines intersect plotting area */
      else if ( i2 == i1 && 0 < i1 && i1 < npts )
         {
         line_coords[0] = x[i1-1];
         line_coords[1] = y[i1-1];
         line_coords[2] = x[i1];
         line_coords[3] = y[i1];
         if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
            {
            points = gnome_canvas_points_new(2);
            theta1 = line_coords[0];
            r1     = line_coords[1];
            theta2 = line_coords[2];
            r2     = line_coords[3];
            points->coords[0] = xorigin + (r1 - rmin)*cos(theta1)*rscale;
            points->coords[1] = yorigin - (r1 - rmin)*sin(theta1)*rscale;
            points->coords[2] = xorigin + (r2 - rmin)*cos(theta2)*rscale;
            points->coords[3] = yorigin - (r2 - rmin)*sin(theta2)*rscale;
            if ( strcmp(line_type, "solid") == 0 )
               DrawLine(points, color, line_width);
            else if ( strcmp(line_type, "dashed") == 0 )
               DrawDashedLine(points, color, line_width);
            else if ( strcmp(line_type, "dotted") == 0 )
               DrawDottedLine(points, color, line_width);
            gnome_canvas_points_unref(points);
            }
         }

      i1 = i;
      i2 = i;
      }
   }


void DrawLines3d ( int npts, double *x, double *y, double *z, double *origin, double *Ryz,
                   double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
                   double xscale, double yscale, double zscale, guint32 color,
                   unsigned int line_width, char *line_type )
   {
   /* Declare variables */
   int i, i1, i2, j, k, ipoints;
   double line_coords[6], x1, y1, z1, x2, y2, z2, r[3], *p;
   GnomeCanvasPoints *points;


   /* Check for at least two points */
   if ( npts < 2 )
      return;


   /* Draw continuous lines */
   i1 = 0;
   i2 = 0;
   for ( i=1; i<=npts; i++ )
      {
      /* Check if current point is in plotting area */
      if ( xmin <= x[i-1] && x[i-1] <= xmax &&
           ymin <= y[i-1] && y[i-1] <= ymax &&
           zmin <= z[i-1] && z[i-1] <= zmax )
         {
         if ( (i2 = i) < npts )
            continue;
         }

      /* Draw lines between points within plotting area */
      if ( i2 > i1 )
         {
         if ( i1 > 0 )
            i1--;
         line_coords[0] = x[i1];
         line_coords[1] = y[i1];
         line_coords[2] = z[i1];
         line_coords[3] = x[i1+1];
         line_coords[4] = y[i1+1];
         line_coords[5] = z[i1+1];
         Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]);
         x1 = line_coords[0];
         y1 = line_coords[1];
         z1 = line_coords[2];

         if ( i2 < npts )
            {
            line_coords[0] = x[i2-1];
            line_coords[1] = y[i2-1];
            line_coords[2] = z[i2-1];
            line_coords[3] = x[i2];
            line_coords[4] = y[i2];
            line_coords[5] = z[i2];
            Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]);
            x2 = line_coords[3];
            y2 = line_coords[4];
            z2 = line_coords[5];
            }

         if ( i2 == npts )
            points = gnome_canvas_points_new(i2 - i1);
         else
            points = gnome_canvas_points_new(i2 - i1 + 1);

         r[0] = (x1 - xmin)*xscale;
         r[1] = (y1 - ymin)*yscale;
         r[2] = (z1 - zmin)*zscale;
         p = multiply_mv(Ryz, r);
         for ( k=1; k<=3; k++, p++ )
            r[k-1] = *p;
         ipoints = 0;
         points->coords[0] = origin[1] + r[1];
         points->coords[1] = origin[2] - r[2];
         for ( j=i1+1; j<=i2-1; j++ )
            {
            r[0] = (x[j] - xmin)*xscale;
            r[1] = (y[j] - ymin)*yscale;
            r[2] = (z[j] - zmin)*zscale;
            p = multiply_mv(Ryz, r);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            ipoints++;
            points->coords[2*ipoints]   = origin[1] + r[1];
            points->coords[2*ipoints+1] = origin[2] - r[2];
            }
         if ( i2 < npts )
            {
            r[0] = (x2 - xmin)*xscale;
            r[1] = (y2 - ymin)*yscale;
            r[2] = (z2 - zmin)*zscale;
            p = multiply_mv(Ryz, r);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            ipoints++;
            points->coords[2*ipoints]   = origin[1] + r[1];
            points->coords[2*ipoints+1] = origin[2] - r[2];
            }

         if ( strcmp(line_type, "solid") == 0 )
            DrawLine(points, color, line_width);
         else if ( strcmp(line_type, "dashed") == 0 )
            DrawDashedLine(points, color, line_width);
         else if ( strcmp(line_type, "dotted") == 0 )
            DrawDottedLine(points, color, line_width);
         gnome_canvas_points_unref(points);
         }

      /* Draw lines between points outside plotting area if lines intersect plotting area */
      else if ( i2 == i1 && 0 < i1 && i1 < npts )
         {
         line_coords[0] = x[i1-1];
         line_coords[1] = y[i1-1];
         line_coords[2] = z[i1-1];
         line_coords[3] = x[i1];
         line_coords[4] = y[i1];
         line_coords[5] = z[i1];
         if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
            {
            points = gnome_canvas_points_new(2);
            r[0] = (line_coords[0] - xmin)*xscale;
            r[1] = (line_coords[1] - ymin)*yscale;
            r[2] = (line_coords[2] - zmin)*zscale;
            p = multiply_mv(Ryz, r);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            points->coords[0] = origin[1] + r[1];
            points->coords[1] = origin[2] - r[2];

            r[0] = (line_coords[3] - xmin)*xscale;
            r[1] = (line_coords[4] - ymin)*yscale;
            r[2] = (line_coords[5] - zmin)*zscale;
            p = multiply_mv(Ryz, r);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            points->coords[2] = origin[1] + r[1];
            points->coords[3] = origin[2] - r[2];

            if ( strcmp(line_type, "solid") == 0 )
               DrawLine(points, color, line_width);
            else if ( strcmp(line_type, "dashed") == 0 )
               DrawDashedLine(points, color, line_width);
            else if ( strcmp(line_type, "dotted") == 0 )
               DrawDottedLine(points, color, line_width);
            gnome_canvas_points_unref(points);
            }
         }

      i1 = i;
      i2 = i;
      }
   }
