/*******************************************************************************
*
* PlotLines.c
*
* Plots lines.
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


void PlotLines ( void )
   {
   /* Declare variables */
   int i, nx, ny, nz, ncoords, window_width, window_height;
   unsigned int index, line_width;
   guint32 line_color;
   double x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
          xscale, yscale, zscale, xscalesq, yscalesq, zscalesq,
          rscale, xorigin, yorigin, radius, phi, theta,
          axis_length, *p, origin[3], Ry[9], Rz[9], Ryz[9], r1[3], r2[3], 
          line_coords[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   char *pchar, line_char, color_char;
   extern char color_string[];   /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Invalid or missing line coordinates.",
        "Invalid line color character.",
        "Invalid or missing line or color specification.",
        "Invalid line character." };
   FILE *fptr;
   GnomeCanvasPoints *points;


   /* Return if no lines to plot */
   fptr = fopen(p_param_file, "r");
   i = 0;
   while ( fgets(line, maxline, fptr) != NULL )
      if ( strncmp(line, "line_coords", 11) == 0 )
         i++;
   fclose(fptr);
   if ( i == 0 )
      return;


   /* Get minimum and maximum axis values */
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      ny = p_ticklabels->nyvalues;
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = floor(p_ticklabels->xvalues[0]);
      xmax = ceil(p_ticklabels->xvalues[nx-1]);
      nx = roundint(xmax - xmin + 1.0);
      ny = p_ticklabels->nyvalues;
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      ny = p_ticklabels->nyvalues;
      ymin = floor(p_ticklabels->yvalues[0]);
      ymax = ceil(p_ticklabels->yvalues[ny-1]);
      ny = roundint(ymax - ymin + 1.0);
      }

   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = floor(p_ticklabels->xvalues[0]);
      xmax = ceil(p_ticklabels->xvalues[nx-1]);
      nx = roundint(xmax - xmin + 1.0);
      ny = p_ticklabels->nyvalues;
      ymin = floor(p_ticklabels->yvalues[0]);
      ymax = ceil(p_ticklabels->yvalues[ny-1]);
      ny = roundint(ymax - ymin + 1.0);
      }

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      ny = p_ticklabels->nyvalues;
      rmin = p_ticklabels->yvalues[0];
      rmax = p_ticklabels->yvalues[ny-1];
      rmin = rmin - p_ticklabels->yoffset1;
      rmax = rmax + p_ticklabels->yoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      ny = p_ticklabels->nyvalues;
      nz = p_ticklabels->nzvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      zmin = p_ticklabels->zvalues[0];
      zmax = p_ticklabels->zvalues[nz-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      zmin = zmin - p_ticklabels->zoffset1;
      zmax = zmax + p_ticklabels->zoffset2;
      }


   /* Calculate axis scale factors */
   if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")    == 0 )
      {
      x1_box = p_plot_box_data->xmin;
      x2_box = p_plot_box_data->xmax;
      y1_box = p_plot_box_data->ymin;
      y2_box = p_plot_box_data->ymax;
      xscale = (x2_box - x1_box)/(xmax - xmin);
      yscale = (y2_box - y1_box)/(ymax - ymin);
      xscalesq = xscale*xscale;
      yscalesq = yscale*yscale;
      }

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      window_width  = p_window_data->width;
      window_height = p_window_data->height;
      xorigin = 0.375*window_width;
      yorigin = 0.500*window_height;
      if ( window_width >= window_height )
         radius = 0.375*window_height;
      else
         radius = 0.375*window_width;
      rscale = radius/(rmax - rmin);
      }

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      /* Get view angles */
      phi = p_plot_param_3d->phi;
      theta = p_plot_param_3d->theta;

      /* Get origin */
      if ( phi >= 0.0 && phi < 90.0 )
         {
         origin[0] = p_plot_param_3d->origin[0];
         origin[1] = p_plot_param_3d->origin[1];
         origin[2] = p_plot_param_3d->origin[2];
         }

      else if ( phi >= 90.0 && phi < 180.0 )
         {
         origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis2[0];
         origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis2[1];
         origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis2[2];
         }

      else if ( phi >= 180.0 && phi < 270.0 )
         {
         origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0] + p_plot_param_3d->axis2[0];
         origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1] + p_plot_param_3d->axis2[1];
         origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2] - p_plot_param_3d->axis2[2];
         }

      else if ( phi >= 270.0 && phi < 360.0 )
         {
         origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0];
         origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1];
         origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2];
         }

      /* Calculate rotation matrices */
      phi = phi*deg2rad;
      theta = theta*deg2rad;

      Ry[0] = cos(-theta);
      Ry[1] = 0.0;
      Ry[2] = -sin(-theta);
      Ry[3] = 0.0;
      Ry[4] = 1.0;
      Ry[5] = 0.0;
      Ry[6] = sin(-theta);
      Ry[7] = 0.0;
      Ry[8] = cos(-theta);

      Rz[0] = cos(phi);
      Rz[1] = sin(phi);
      Rz[2] = 0.0;
      Rz[3] = -sin(phi);
      Rz[4] = cos(phi);
      Rz[5] = 0.0;
      Rz[6] = 0.0;
      Rz[7] = 0.0;
      Rz[8] = 1.0;

      p = multiply_mm(Ry, Rz);
      for ( i=1; i<=9; i++, p++ )
         Ryz[i-1] = *p;

      /* Get axis length */
      axis_length = p_plot_param_3d->axis_length;

      /* Calculate axis scale factors */
      xscale = axis_length/(xmax - xmin);
      yscale = axis_length/(ymax - ymin);
      zscale = axis_length/(zmax - zmin);
      xscalesq = xscale*xscale;
      yscalesq = yscale*yscale;
      zscalesq = zscale*zscale;
      }


   /* Specify number of line coordinates */
   if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")    == 0 ||
        strcmp(p_plot_param->axis_type, "polar")     == 0 )
      ncoords = 4;

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      ncoords = 6;


   /* Draw lines */
   fptr = fopen(p_param_file, "r");
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get line coordinates */
      if ( strncmp(line, "line_coords", 11) == 0 )
         {
         if ( ncoords == 4 )
            {
            if ( sscanf(&line[11], " %lf %lf %lf %lf",
                        &line_coords[0], &line_coords[2],
                        &line_coords[1], &line_coords[3]) != 4 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            }

         else if ( ncoords == 6 )
            {
            if ( sscanf(&line[11], " %lf %lf %lf %lf %lf %lf",
                        &line_coords[0], &line_coords[3],
                        &line_coords[1], &line_coords[4],
                        &line_coords[2], &line_coords[5]) != 6 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            }

         /* Modify line coordinates for logarithmic and polar axes */
         if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
            for ( i=2; i<=4; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
            for ( i=1; i<=4; i++ )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = line_coords[i-1]*deg2rad;

         /* Get line type and color */
         line_width = 1;
         if ( fgets(line, maxline, fptr) != NULL )
            if ( strncmp(line, "line_style", 10) == 0 )
               {
               if ( sscanf(&line[10], " %c 0x%x", &line_char, (unsigned int *) &line_color) == 2 ||
                    sscanf(&line[10], " %c 0X%x", &line_char, (unsigned int *) &line_color) == 2 )
                  sscanf(&line[10], " %*c %*s %u", &line_width);

               else if ( sscanf(&line[10], " %c %c", &line_char, &color_char) == 2 )
                  {
                  if ( (pchar = strchr(color_string, color_char)) == NULL )
                     {
                     ErrorDialog(error_str[1]);
                     exit(1);
                     }

                  else
                     {
                     index = pchar - &color_string[0];   /* get index to color character */
                     line_color = color_rgba[index];
                     sscanf(line, "%*s %*c %*c %u", &line_width);
                     }
                  }

               else
                  {
                  ErrorDialog(error_str[2]);
                  exit(1);
                  }

               /* Check line type */
               if ( line_char != 'l' && line_char != 'd' && line_char != '.' )
                  {
                  ErrorDialog(error_str[3]);
                  exit(1);
                  }
               }

         /* Draw line */
         if ( line_char == 'l' )
            {
            if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")    == 0 )
               {
               if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = x1_box + (line_coords[0] - xmin)*xscale;
                  points->coords[1] = y2_box - (line_coords[1] - ymin)*yscale;
                  points->coords[2] = x1_box + (line_coords[2] - xmin)*xscale;
                  points->coords[3] = y2_box - (line_coords[3] - ymin)*yscale;
                  DrawLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
               {
               if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = xorigin + (line_coords[1] - rmin)*cos(line_coords[0])*rscale;
                  points->coords[1] = yorigin - (line_coords[1] - rmin)*sin(line_coords[0])*rscale;
                  points->coords[2] = xorigin + (line_coords[3] - rmin)*cos(line_coords[2])*rscale;
                  points->coords[3] = yorigin - (line_coords[3] - rmin)*sin(line_coords[2])*rscale;
                  DrawLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
               {
               if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
                  {
                  r1[0] = (line_coords[0] - xmin)*xscale;
                  r1[1] = (line_coords[1] - ymin)*yscale;
                  r1[2] = (line_coords[2] - zmin)*zscale;

                  r2[0] = (line_coords[3] - xmin)*xscale;
                  r2[1] = (line_coords[4] - ymin)*yscale;
                  r2[2] = (line_coords[5] - zmin)*zscale;

                  p = multiply_mv(Ryz, r1);
                  for ( i=1; i<=3; i++, p++ )
                     r1[i-1] = *p;

                  p = multiply_mv(Ryz, r2);
                  for ( i=1; i<=3; i++, p++ )
                     r2[i-1] = *p;

                  points = gnome_canvas_points_new(2);
                  points->coords[0] = origin[1] + r1[1];
                  points->coords[1] = origin[2] - r1[2];
                  points->coords[2] = origin[1] + r2[1];
                  points->coords[3] = origin[2] - r2[2];
                  DrawLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }
            }

         /* Draw dashed line */
         else if ( line_char == 'd' )
            {
            if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")    == 0 )
               {
               if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = x1_box + (line_coords[0] - xmin)*xscale;
                  points->coords[1] = y2_box - (line_coords[1] - ymin)*yscale;
                  points->coords[2] = x1_box + (line_coords[2] - xmin)*xscale;
                  points->coords[3] = y2_box - (line_coords[3] - ymin)*yscale;
                  DrawDashedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
               {
               if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = xorigin + (line_coords[1] - rmin)*cos(line_coords[0])*rscale;
                  points->coords[1] = yorigin - (line_coords[1] - rmin)*sin(line_coords[0])*rscale;
                  points->coords[2] = xorigin + (line_coords[3] - rmin)*cos(line_coords[2])*rscale;
                  points->coords[3] = yorigin - (line_coords[3] - rmin)*sin(line_coords[2])*rscale;
                  DrawDashedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
               {
               if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
                  {
                  r1[0] = (line_coords[0] - xmin)*xscale;
                  r1[1] = (line_coords[1] - ymin)*yscale;
                  r1[2] = (line_coords[2] - zmin)*zscale;

                  r2[0] = (line_coords[3] - xmin)*xscale;
                  r2[1] = (line_coords[4] - ymin)*yscale;
                  r2[2] = (line_coords[5] - zmin)*zscale;

                  p = multiply_mv(Ryz, r1);
                  for ( i=1; i<=3; i++, p++ )
                     r1[i-1] = *p;

                  p = multiply_mv(Ryz, r2);
                  for ( i=1; i<=3; i++, p++ )
                     r2[i-1] = *p;

                  points = gnome_canvas_points_new(2);
                  points->coords[0] = origin[1] + r1[1];
                  points->coords[1] = origin[2] - r1[2];
                  points->coords[2] = origin[1] + r2[1];
                  points->coords[3] = origin[2] - r2[2];
                  DrawDashedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }
            }

         else if ( line_char == '.' )
            {
            if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")    == 0 )
               {
               if ( Clip2d(xmin, xmax, ymin, ymax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = x1_box + (line_coords[0] - xmin)*xscale;
                  points->coords[1] = y2_box - (line_coords[1] - ymin)*yscale;
                  points->coords[2] = x1_box + (line_coords[2] - xmin)*xscale;
                  points->coords[3] = y2_box - (line_coords[3] - ymin)*yscale;
                  DrawDottedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
               {
               if ( ClipPolar(rmin, rmax, &line_coords[0]) == 1 )
                  {
                  points = gnome_canvas_points_new(2);
                  points->coords[0] = xorigin + (line_coords[1] - rmin)*cos(line_coords[0])*rscale;
                  points->coords[1] = yorigin - (line_coords[1] - rmin)*sin(line_coords[0])*rscale;
                  points->coords[2] = xorigin + (line_coords[3] - rmin)*cos(line_coords[2])*rscale;
                  points->coords[3] = yorigin - (line_coords[3] - rmin)*sin(line_coords[2])*rscale;
                  DrawDottedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }

            else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
               {
               if ( Clip3d(xmin, xmax, ymin, ymax, zmin, zmax, &line_coords[0]) == 1 )
                  {
                  r1[0] = (line_coords[0] - xmin)*xscale;
                  r1[1] = (line_coords[1] - ymin)*yscale;
                  r1[2] = (line_coords[2] - zmin)*zscale;

                  r2[0] = (line_coords[3] - xmin)*xscale;
                  r2[1] = (line_coords[4] - ymin)*yscale;
                  r2[2] = (line_coords[5] - zmin)*zscale;

                  p = multiply_mv(Ryz, r1);
                  for ( i=1; i<=3; i++, p++ )
                     r1[i-1] = *p;

                  p = multiply_mv(Ryz, r2);
                  for ( i=1; i<=3; i++, p++ )
                     r2[i-1] = *p;

                  points = gnome_canvas_points_new(2);
                  points->coords[0] = origin[1] + r1[1];
                  points->coords[1] = origin[2] - r1[2];
                  points->coords[2] = origin[1] + r2[1];
                  points->coords[3] = origin[2] - r2[2];
                  DrawDottedLine(points, line_color, line_width);
                  gnome_canvas_points_unref(points);
                  }
               }
            }
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   fclose(fptr);

   return;
   }
