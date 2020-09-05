/*******************************************************************************
*
* PlotSymbols.c
*
* Plots symbols.
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


#include <ctype.h>
#include <math.h>
#include "gsegraf.h"


void PlotSymbols ( void )
   {
   /* Declare variables */
   int i, ifunc, nx, ny, nz, ncoords, window_width, window_height;
   unsigned int index, symbol_size;
   guint32 symbol_color;
   double x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, zmin, zmax, rmin, rmax,
          xscale, yscale, zscale, xscalesq, yscalesq, zscalesq,
          rscale, xorigin, yorigin, radius, theta, phi,
          axis_length, *p, origin[3], Ry[9], Rz[9], Ryz[9], r[3],
          symbol_coords[3] = { 0.0, 0.0, 0.0 }, x, y;
   char *pchar, symbol_char, color_char;
   extern char symbol_string1[];   /* symbol-specification characters "cCtTsSiIpPhH" */
   extern char symbol_string2[];   /* symbol-specification characters "+xra" */
   extern char color_string[];     /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Invalid or missing symbol coordinates.",
        "Invalid symbol color character.",
        "Invalid or missing symbol or color specification.",
        "Invalid symbol character." };
   FILE *fptr;


   /* Return if no symbols to plot */
   fptr = fopen(p_param_file, "r");
   i = 0;
   while ( fgets(line, maxline, fptr) != NULL )
      if ( strncmp(line, "symbol_coords", 13) == 0 )
         i++;
   fclose(fptr);
   if ( i == 0 )
      return;


   /* Get minimum and maximum axis values */
   if ( strcmp(p_plot_param->axis_type, "linear")    == 0 )
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


   /* Specify number of symbol coordinates */
   if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")    == 0 ||
        strcmp(p_plot_param->axis_type, "polar")     == 0 )
      ncoords = 2;

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      ncoords = 3;


   /* Draw symbols */
   fptr = fopen(p_param_file, "r");
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "symbol_coords", 13) == 0 )
         {
         if ( ncoords == 2 )
            {
            if ( sscanf(&line[13], " %lf %lf",
                        &symbol_coords[0], &symbol_coords[1]) != 2 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            }

         else if ( ncoords == 3 )
            {
            if ( sscanf(&line[13], " %lf %lf %lf",
                        &symbol_coords[0], &symbol_coords[1], &symbol_coords[2]) != 3 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            }

         /* Modify symbol coordinates for logarithmic and polar axes */
         if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
            symbol_coords[0] = log10(fabs(symbol_coords[0]));

         else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
            symbol_coords[1] = log10(fabs(symbol_coords[1]));

         else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
            {
            symbol_coords[0] = log10(fabs(symbol_coords[0]));
            symbol_coords[1] = log10(fabs(symbol_coords[1]));
            }

         else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
            symbol_coords[0] = symbol_coords[0]*deg2rad;

         /* Get symbol type and color */
         symbol_size = 6;
         if ( fgets(line, maxline, fptr) != NULL )
            if ( strncmp(line, "symbol_style", 12) == 0 )
               {
               if ( sscanf(&line[12], " %c 0x%x", &symbol_char, (unsigned int *) &symbol_color) == 2 ||
                    sscanf(&line[12], " %c 0X%x", &symbol_char, (unsigned int *) &symbol_color) == 2 )
                  sscanf(&line[12], " %*c %*s %u", &symbol_size);

               else if ( sscanf(&line[12], " %c %c", &symbol_char, &color_char) == 2 )
                  {
                  if ( (pchar = strchr(color_string, color_char)) == NULL )
                     {
                     ErrorDialog(error_str[1]);
                     exit(1);
                     }

                  else
                     {
                     index = pchar - &color_string[0];   /* get index to color character */
                     symbol_color = color_rgba[index];
                     sscanf(&line[12], " %*c %*c %u", &symbol_size);
                     }
                  }

                else
                  {
                  ErrorDialog(error_str[2]);
                  exit(1);
                  }

               /* Check symbol type */
               if ( (pchar = strchr("cCtTsSiIpPhH+xra", symbol_char)) == NULL )
                  {
                  ErrorDialog(error_str[3]);
                  exit(1);
                  }
               }

         /* Draw symbol */
         if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
              strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
              strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
              strcmp(p_plot_param->axis_type, "loglog")    == 0 )
            {
            if ( xmin <= symbol_coords[0] && symbol_coords[0] <= xmax &&
                 ymin <= symbol_coords[1] && symbol_coords[1] <= ymax )
               {
               x = x1_box + (symbol_coords[0] - xmin)*xscale;
               y = y2_box - (symbol_coords[1] - ymin)*yscale;
               }
            else
               continue;
            }

         else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
            {
            if ( rmin <= symbol_coords[1] && symbol_coords[1] <= rmax )
               {
               x = xorigin + (symbol_coords[1] - rmin)*cos(symbol_coords[0])*rscale;
               y = yorigin - (symbol_coords[1] - rmin)*sin(symbol_coords[0])*rscale;
               }
            else
               continue;
            }

         else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
            {
            if ( xmin <= symbol_coords[0] && symbol_coords[0] <= xmax &&
                 ymin <= symbol_coords[1] && symbol_coords[1] <= ymax &&
                 zmin <= symbol_coords[2] && symbol_coords[2] <= zmax )
               {
               r[0] = (symbol_coords[0] - xmin)*xscale;
               r[1] = (symbol_coords[1] - ymin)*yscale;
               r[2] = (symbol_coords[2] - zmin)*zscale;

               p = multiply_mv(Ryz, r);
               for ( i=1; i<=3; i++, p++ )
                  r[i-1] = *p;

               x = origin[1] + r[1];
               y = origin[2] - r[2];
               }
            else
               continue;
            }

         /* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
         if ( (pchar = strchr(symbol_string1, symbol_char)) != NULL )
            {
            ifunc = pchar - symbol_string1;
            if ( isupper(*pchar) == 0 )
               symbol_func1[ifunc](x, y, color_rgba[3], symbol_color, symbol_size);
            else
               symbol_func1[ifunc](x, y, symbol_color, symbol_color, symbol_size);
            }

         /* Draw symbols in symbol_string2 ("+xra") */
         else if ( (pchar = strchr(symbol_string2, symbol_char)) != NULL )
            {
            ifunc = pchar - symbol_string2;
            symbol_func2[ifunc](x, y, symbol_color, symbol_size);
            }
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   fclose(fptr);

   return;
   }
