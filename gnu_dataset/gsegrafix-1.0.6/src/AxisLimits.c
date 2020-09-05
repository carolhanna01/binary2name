/*******************************************************************************
*
* AxisLimits.c
*
* Sets axis limits to specified values.
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


void AxisLimits ( int flag_ref )
   {
   /* Declare variables */
   int i, iplot, nplots, flag1, flag2, nx, ny, nz;
   double x1, x2, y1, y2, z1, z2, deltax, deltay, deltaz, epsx, epsy, epsz;
   const char *error_str[] = 
      { "Axis minimum must be less than axis maximum." };


   /* Get axis limits */
   if ( axis_limits[0] == 1 )
      p_data_min_max->xmin = p_plot_param->axis_limits[0];
   if ( axis_limits[1] == 1 )
      p_data_min_max->xmax = p_plot_param->axis_limits[1];
   if ( axis_limits[2] == 1 )
      p_data_min_max->ymin = p_plot_param->axis_limits[2];
   if ( axis_limits[3] == 1 )
      p_data_min_max->ymax = p_plot_param->axis_limits[3];
   if ( axis_limits[4] == 1 )
      p_data_min_max->zmin = p_plot_param->axis_limits[4];
   if ( axis_limits[5] == 1 )
      p_data_min_max->zmax = p_plot_param->axis_limits[5];


   /* Check axis limits */
   if ( (strcmp(p_plot_param->axis_type, "linear")    == 0 ||
         strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
         strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
         strcmp(p_plot_param->axis_type, "loglog")    == 0 ||
         strcmp(p_plot_param->axis_type, "polar")     == 0) &&
        (p_data_min_max->xmin >= p_data_min_max->xmax ||
         p_data_min_max->ymin >= p_data_min_max->ymax) )
      {
      ErrorDialog(error_str[0]);
      exit(1);
      }

   nplots = p_plot_param->nplots;
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      for ( iplot=1; iplot<=nplots; iplot++ )
         if ( (strcmp(&plot_types[(iplot-1)*10], "contour") == 0 ||
               strcmp(&plot_types[(iplot-1)*10], "color")   == 0) &&
              p_data_min_max->zmin >= p_data_min_max->zmax )
            {
            ErrorDialog(error_str[0]);
            exit(1);
            }
      }

   if ( strcmp(p_plot_param->axis_type, "3d") == 0 &&
        (p_data_min_max->xmin >= p_data_min_max->xmax ||
         p_data_min_max->ymin >= p_data_min_max->ymax ||
         p_data_min_max->zmin >= p_data_min_max->zmax) )
      {
      ErrorDialog(error_str[0]);
      exit(1);
      }


   /* Get autoscale data */
   flag1 = 0;
   flag2 = 0;
   if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")    == 0 )
      {
      for ( iplot=1; iplot<=nplots; iplot++ )
         {
         if ( strcmp(&plot_types[(iplot-1)*10], "color") == 0 )
            flag1 = 1;
         else if ( strcmp(&plot_types[(iplot-1)*10], "contour") == 0 &&
                   styleflags[iplot-1] == 7 )
            flag1 = 1;
         else if ( strcmp(&plot_types[(iplot-1)*10], "contour") == 0 &&
                   (styleflags[iplot-1] == 1 || styleflags[iplot-1] == 3) )
            flag2 = 1;
         }

      if ( flag1 == 1 )
         AutoScale(3, 7);
      else if ( flag2 == 1 )
         AutoScale(3, 10);
      else
         AutoScale(2, 10);
      }

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      AutoScale(2, 5);

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      AutoScale(3, 7);


   /* Get old tick-mark label values */
   nx = p_ticklabels->nxvalues;
   ny = p_ticklabels->nyvalues;
   nz = p_ticklabels->nzvalues;
   x1 = p_ticklabels->xvalues[0];
   x2 = p_ticklabels->xvalues[nx-1];
   y1 = p_ticklabels->yvalues[0];
   y2 = p_ticklabels->yvalues[ny-1];
   z1 = p_ticklabels->zvalues[0];
   z2 = p_ticklabels->zvalues[nz-1];
   deltax = (x2 - x1)/(nx - 1.0);
   deltay = (y2 - y1)/(ny - 1.0);
   deltaz = (z2 - z1)/(nz - 1.0);
   epsx = (x2 - x1)*1.0e-03;
   epsy = (y2 - y1)*1.0e-03;
   epsz = (z2 - z1)*1.0e-03;


   /* Get new tick-mark label values*/
   if ( axis_limits[0] == 1 &&
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 )
      {
      if ( p_plot_param->axis_limits[0] - x1 > epsx )
         {
         nx--;
         x1 = x1 + deltax;
         p_ticklabels->xoffset1 = x1 - p_plot_param->axis_limits[0];
         }
      else
         p_ticklabels->xoffset1 = 0.0;
      }

   if ( axis_limits[1] == 1 && 
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 )
      {
      if ( x2 - p_plot_param->axis_limits[1] > epsx )
         {
         nx--;
         x2 = x2 - deltax;
         p_ticklabels->xoffset2 = p_plot_param->axis_limits[1] - x2;
         }
      else
         p_ticklabels->xoffset2 = 0.0;
      }

   if ( axis_limits[2] == 1 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 )
      {
      if ( p_plot_param->axis_limits[2] - y1 > epsy )
         {
         ny--;
         y1 = y1 + deltay;
         p_ticklabels->yoffset1 = y1 - p_plot_param->axis_limits[2];
         }
      else
         p_ticklabels->yoffset1 = 0.0;
      }

   if ( axis_limits[3] == 1 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 )
      {
      if ( y2 - p_plot_param->axis_limits[3] > epsy )
         {
         ny--;
         y2 = y2 - deltay;
         p_ticklabels->yoffset2 = p_plot_param->axis_limits[3] - y2;
         }
      else
         p_ticklabels->yoffset2 = 0.0;
      }

   if ( axis_limits[4] == 1 )
      {
      if ( p_plot_param->axis_limits[4] - z1 > epsz )
         {
         nz--;
         z1 = z1 + deltaz;
         p_ticklabels->zoffset1 = z1 - p_plot_param->axis_limits[4];
         }
      else
         p_ticklabels->zoffset1 = 0.0;
      }

   if ( axis_limits[5] == 1 )
      {
      if ( z2 - p_plot_param->axis_limits[5] > epsz )
         {
         nz--;
         z2 = z2 - deltaz;
         p_ticklabels->zoffset2 = p_plot_param->axis_limits[5] - z2;
         }
      else
         p_ticklabels->zoffset2 = 0.0;
      }


   /* Save new tick-mark label values */
   p_ticklabels->nxvalues = nx;
   p_ticklabels->nyvalues = ny;
   p_ticklabels->nzvalues = nz;
   for ( i=1; i<=nx; i++ )
      p_ticklabels->xvalues[i-1] = x1 + (i - 1)*((x2 - x1)/(nx - 1.0));
   for ( i=1; i<=ny; i++ )
      p_ticklabels->yvalues[i-1] = y1 + (i - 1)*((y2 - y1)/(ny - 1.0));
   for ( i=1; i<=nz; i++ )
      p_ticklabels->zvalues[i-1] = z1 + (i - 1)*((z2 - z1)/(nz - 1.0));


   /* Save tick-mark reference data */
   if ( flag_ref == 1 )
      {
      nx = p_ticklabels->nxvalues;
      ny = p_ticklabels->nyvalues;
      nz = p_ticklabels->nzvalues;
      p_ticklabels->nxvalues_ref = nx;
      p_ticklabels->nyvalues_ref = ny;
      p_ticklabels->nzvalues_ref = nz;
      for ( i=1; i<=nx; i++ )
         p_ticklabels->xvalues_ref[i-1] = p_ticklabels->xvalues[i-1];
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues_ref[i-1] = p_ticklabels->yvalues[i-1];
      for ( i=1; i<=nz; i++ )
         p_ticklabels->zvalues_ref[i-1] = p_ticklabels->zvalues[i-1];
      p_ticklabels->xoffset1_ref = p_ticklabels->xoffset1;
      p_ticklabels->xoffset2_ref = p_ticklabels->xoffset2;
      p_ticklabels->yoffset1_ref = p_ticklabels->yoffset1;
      p_ticklabels->yoffset2_ref = p_ticklabels->yoffset2;
      p_ticklabels->zoffset1_ref = p_ticklabels->zoffset1;
      p_ticklabels->zoffset2_ref = p_ticklabels->zoffset2;

      p_plot_param_3d->phi_ref   = p_plot_param_3d->phi;
      p_plot_param_3d->theta_ref = p_plot_param_3d->theta;
      }

   return;
   }
