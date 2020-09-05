/*******************************************************************************
*
* DrawContours3d.c
*
* Plots 3-dimensional contour data for polygons that are not truncated.
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


void DrawContours3d ( int icontour, double *xpoints, double *ypoints, double *zpoints )
   {
   /* Declare variables */
   int i, j, ic, nc, nxvalues, nyvalues, nzvalues, flag;
   double axis_length, *p, origin[3], Ryz[9],
          xmin, xmax, ymin, ymax, zmin, zmax,
          xscale, yscale, zscale,
          xtest[8], ytest[8], ztest[8],
          zmin_polygon, zmax_polygon, contour,
          x1, y1, z1, x2, y2, z2, r1[3], r2[3],
          length, dx, dy;
   GnomeCanvasPoints *points;
   GnomeCanvasItem *line;


   /* Get origin */
   if ( p_plot_param_3d->quadrant == 1 )
      {
      origin[0] = p_plot_param_3d->origin[0];
      origin[1] = p_plot_param_3d->origin[1];
      origin[2] = p_plot_param_3d->origin[2];
      }

   else if ( p_plot_param_3d->quadrant == 2 )
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis2[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis2[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis2[2];
      }

   else if ( p_plot_param_3d->quadrant == 3 )
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0] + p_plot_param_3d->axis2[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1] + p_plot_param_3d->axis2[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2] - p_plot_param_3d->axis2[2];
      }

   else
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2];
      }


   /* Get rotation matrix */
   for ( i=1; i<=9; i++ )
      Ryz[i-1] = p_plot_param_3d->Ryz[i-1];


   /* Get axis length */
   axis_length = p_plot_param_3d->axis_length;


   /* Get minimum and maximum axis values */
   nxvalues = p_ticklabels->nxvalues;
   nyvalues = p_ticklabels->nyvalues;
   nzvalues = p_ticklabels->nzvalues;
   xmin = p_ticklabels->xvalues[0];
   xmax = p_ticklabels->xvalues[nxvalues-1];
   ymin = p_ticklabels->yvalues[0];
   ymax = p_ticklabels->yvalues[nyvalues-1];
   zmin = p_ticklabels->zvalues[0];
   zmax = p_ticklabels->zvalues[nzvalues-1];
   xmin = xmin - p_ticklabels->xoffset1;
   xmax = xmax + p_ticklabels->xoffset2;
   ymin = ymin - p_ticklabels->yoffset1;
   ymax = ymax + p_ticklabels->yoffset2;
   zmin = zmin - p_ticklabels->zoffset1;
   zmax = zmax + p_ticklabels->zoffset2;


   /* Calculate axis scale factors */
   xscale = axis_length/(xmax - xmin);
   yscale = axis_length/(ymax - ymin);
   zscale = axis_length/(zmax - zmin);


   /* Define polygon test points */
   for ( i=1; i<=4; i++ )
      {
      xtest[i-1] = xpoints[i-1];
      ytest[i-1] = ypoints[i-1];
      ztest[i-1] = zpoints[i-1];
      xtest[i+3] = xpoints[i-1];
      ytest[i+3] = ypoints[i-1];
      ztest[i+3] = zpoints[i-1];
      }


   /* Find polygon zmin and zmax */
   zmin_polygon = min(4, zpoints);
   zmax_polygon = max(4, zpoints);


   /* Calculate number of contour lines */
   if ( ncontours < 2 )
      nc = 2*nzvalues - 1;
   else
      nc = ncontours;


   /* Calculate contour-line coordinates */
   points = gnome_canvas_points_new(2);
   for ( ic=1; ic<=nc; ic++ )
      {
      /* Calculate contour value */
      contour = zmin + (ic-1)*(zmax - zmin)/(nc - 1);


      /* Check if all points equal */
      if ( ztest[0] == ztest[1] && ztest[0] == ztest[2] && ztest[0] == ztest[3] )
         continue;


      /* Check if contour line goes through polygon */
      if ( zmin_polygon < contour && contour < zmax_polygon )
         {
         /* Find coordinates of intersecting contour line */
         flag = 0;
         for ( i=1; i<=4; i++ )
            {
            /* Case 1: two adjacent sides */
            if ( ztest[i-1] < contour && ztest[i] > contour && ztest[i+1] < contour && ztest[i+2] < contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
               y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] > contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
               y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 2: two opposite sides */
            else if ( ztest[i-1] < contour && ztest[i] > contour && ztest[i+1] > contour && ztest[i+2] < contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] < contour && ztest[i+2] > contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 3: both pairs opposite sides */
            else if ( ztest[i-1] < contour && ztest[i] > contour && ztest[i+1] < contour && ztest[i+2] > contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] > contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] < contour )
               {
               x1 = interp_rect(ztest[i-1], ztest[i], xtest[i-1], xtest[i], contour);
               y1 = interp_rect(ztest[i-1], ztest[i], ytest[i-1], ytest[i], contour);
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 4: one point */
            else if ( ztest[i-1] == contour && ztest[i] > contour && ztest[i+1] < contour && ztest[i+2] < contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
               y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] == contour && ztest[i] < contour && ztest[i+1] > contour && ztest[i+2] > contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = interp_rect(ztest[i], ztest[i+1], xtest[i], xtest[i+1], contour);
               y2 = interp_rect(ztest[i], ztest[i+1], ytest[i], ytest[i+1], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] == contour && ztest[i] < contour && ztest[i+1] < contour && ztest[i+2] > contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }

            else if ( ztest[i-1] == contour && ztest[i] > contour && ztest[i+1] > contour && ztest[i+2] < contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = interp_rect(ztest[i+1], ztest[i+2], xtest[i+1], xtest[i+2], contour);
               y2 = interp_rect(ztest[i+1], ztest[i+2], ytest[i+1], ytest[i+2], contour);
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 5: two adjacent points */
            else if ( ztest[i-1] == contour && ztest[i] == contour && ztest[i+1] != contour && ztest[i+2] != contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = xtest[i];
               y2 = ytest[i];
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 6: two opposite points */
            else if ( ztest[i-1] == contour && ztest[i] != contour && ztest[i+1] == contour && ztest[i+2] != contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = xtest[i+1];
               y2 = ytest[i+1];
               z2 = contour;
               flag = 1;
               break;
               }


            /* Case 7: three points */
            else if ( ztest[i-1] == contour && ztest[i] == contour && ztest[i+1] == contour && ztest[i+2] != contour )
               {
               x1 = xtest[i-1];
               y1 = ytest[i-1];
               z1 = contour;
               x2 = xtest[i+1];
               y2 = ytest[i+1];
               z2 = contour;
               flag = 1;
               break;
               }
            }


         /* Check for solution */
         if ( flag == 0 )
            {
            printf("\n No solution\n\n");
            exit(1);
            }


         /* Calculate contour-line position vectors */
         r1[0] = (x1 - xmin)*xscale;
         r1[1] = (y1 - ymin)*yscale;
         r1[2] = (z1 - zmin)*zscale;

         r2[0] = (x2 - xmin)*xscale;
         r2[1] = (y2 - ymin)*yscale;
         r2[2] = (z2 - zmin)*zscale;


         /* Rotate contour-line position vectors */
         p = multiply_mv(Ryz, &r1[0]);
         for ( j=1; j<=3; j++, p++ )
            r1[j-1] = *p;

         p = multiply_mv(Ryz, &r2[0]);
         for ( j=1; j<=3; j++, p++ )
            r2[j-1] = *p;


         /* Draw contour line */
         x1 = origin[1] + r1[1];
         y1 = origin[2] - r1[2];
         x2 = origin[1] + r2[1];
         y2 = origin[2] - r2[2];
         length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1));
         dx = (x2 - x1)/length;
         dy = (y2 - y1)/length;
         points->coords[0] = x1 - dx;
         points->coords[1] = y1 - dy;
         points->coords[2] = x2 + dx;
         points->coords[3] = y2 + dy;
         line = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_LINE,
                                      "points", points,
                                      "fill_color_rgba", contourcolors[icontour-1],
                                      "width_pixels", 1,
                                      NULL);
         }
      }

   gnome_canvas_points_unref(points);
   }
