/*******************************************************************************
*
* ContourPlot3d.c
*
* Plots a two-dimensional projection of three-dimensional contour data.
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


#include "gsegraf.h"


void ContourPlot3d ( int iplot, int icontour, int xindex, int yindex, int zindex, int nx, int ny )
   {
   /* Declare variables */
   int i, j, nxvalues, nyvalues, nzvalues;
   unsigned int index;
   guint32 fill_color[2];
   double xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale, 
          axis_length, origin[3], xpoints[4], ypoints[4], zpoints[4];
   char *pchar;
   extern char color_string[];   /* color-specification characters "kaswrylqbfmogtnpx" */


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


   /* Get axis length */
   axis_length = p_plot_param_3d->axis_length;


   /* Calculate axis scale factors */
   xscale = axis_length/(xmax - xmin);
   yscale = axis_length/(ymax - ymin);
   zscale = axis_length/(zmax - zmin);


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


   /* Get fill colors */
   if ( styleflags[iplot-1] == 2 )
      {
      if ( (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )   /* get pointer to color character 1 */
         index = pchar - &color_string[0];                                 /* get index to color character 1   */
      fill_color[0] = color_rgba[index];
      if ( (pchar = strchr(color_string, stylechar2[iplot-1])) != NULL )   /* get pointer to color character 2 */
         index = pchar - &color_string[0];                                 /* get index to color character 2   */
      fill_color[1] = color_rgba[index];
      }
   else if ( styleflags[iplot-1] == 4 )
      {
      if ( (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )   /* get pointer to color character 1 */
         index = pchar - &color_string[0];                                 /* get index to color character 1   */
      fill_color[0] = color_rgba[index];
      fill_color[1] = stylecolor2[iplot-1];
      }
   else if ( styleflags[iplot-1] == 5 )
      {
      fill_color[0] = stylecolor1[iplot-1];
      if ( (pchar = strchr(color_string, stylechar2[iplot-1])) != NULL )   /* get pointer to color character 2 */
         index = pchar - &color_string[0];                                 /* get index to color character 2   */
      fill_color[1] = color_rgba[index];
      }
   else if ( styleflags[iplot-1] == 6 )
      {
      fill_color[0] = stylecolor1[iplot-1];
      fill_color[1] = stylecolor2[iplot-1];
      }


   /* Plot data */
   if ( p_plot_param_3d->quadrant == 1 )
      {
      for ( i=1; i<nx; i++ )
         {
         xpoints[0] = xcontour[xindex+i-1];
         xpoints[1] = xcontour[xindex+i];
         xpoints[2] = xcontour[xindex+i];
         xpoints[3] = xcontour[xindex+i-1];

         /* All x coordinates within range */
         if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
              (xmin <= xpoints[1] && xpoints[1] <= xmax) )
            {
            for ( j=1; j<ny; j++ )
               {
               ypoints[0] = ycontour[yindex+j-1];
               ypoints[1] = ycontour[yindex+j-1];
               ypoints[2] = ycontour[yindex+j];
               ypoints[3] = ycontour[yindex+j];

               /* All y coordinates within range */
               if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                    (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                  {
                  zpoints[0] = zcontour[zindex+ny*(i-1)+j-1];
                  zpoints[1] = zcontour[zindex+ny*i+j-1];
                  zpoints[2] = zcontour[zindex+ny*i+j];
                  zpoints[3] = zcontour[zindex+ny*(i-1)+j];

                  /* All z coordinates within range */
                  if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                       (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                       (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                       (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                     {
                     PlotNormal3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }

                  /* Not all z coordinates within range */
                  else
                     {
                     PlotInterp3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }
                  }
               }
            }
         }
      }

   else if ( p_plot_param_3d->quadrant == 2 )
      {
      for ( i=nx-1; i>0; i-- )
         {
         xpoints[0] = xcontour[xindex+i-1];
         xpoints[1] = xcontour[xindex+i];
         xpoints[2] = xcontour[xindex+i];
         xpoints[3] = xcontour[xindex+i-1];

         /* All x coordinates within range */
         if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
              (xmin <= xpoints[1] && xpoints[1] <= xmax) )
            {
            for ( j=1; j<ny; j++ )
               {
               ypoints[0] = ycontour[yindex+j-1];
               ypoints[1] = ycontour[yindex+j-1];
               ypoints[2] = ycontour[yindex+j];
               ypoints[3] = ycontour[yindex+j];

               /* All y coordinates within range */
               if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                    (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                  {
                  zpoints[0] = zcontour[zindex+ny*(i-1)+j-1];
                  zpoints[1] = zcontour[zindex+ny*i+j-1];
                  zpoints[2] = zcontour[zindex+ny*i+j];
                  zpoints[3] = zcontour[zindex+ny*(i-1)+j];

                  /* All z coordinates within range */
                  if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                       (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                       (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                       (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                     {
                     PlotNormal3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }

                  /* Not all z coordinates within range */
                  else
                     {
                     PlotInterp3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }
                  }
               }
            }
         }
      }

   else if ( p_plot_param_3d->quadrant == 3 )
      {
      for ( i=nx-1; i>0; i-- )
         {
         xpoints[0] = xcontour[xindex+i-1];
         xpoints[1] = xcontour[xindex+i];
         xpoints[2] = xcontour[xindex+i];
         xpoints[3] = xcontour[xindex+i-1];

         /* All x coordinates within range */
         if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
              (xmin <= xpoints[1] && xpoints[1] <= xmax) )
            {
            for ( j=ny-1; j>0; j-- )
               {
               ypoints[0] = ycontour[yindex+j-1];
               ypoints[1] = ycontour[yindex+j-1];
               ypoints[2] = ycontour[yindex+j];
               ypoints[3] = ycontour[yindex+j];

               /* All y coordinates within range */
               if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                    (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                  {
                  zpoints[0] = zcontour[zindex+ny*(i-1)+j-1];
                  zpoints[1] = zcontour[zindex+ny*i+j-1];
                  zpoints[2] = zcontour[zindex+ny*i+j];
                  zpoints[3] = zcontour[zindex+ny*(i-1)+j];

                  /* All z coordinates within range */
                  if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                       (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                       (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                       (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                     {
                     PlotNormal3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }

                  /* Not all z coordinates within range */
                  else
                     {
                     PlotInterp3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }
                  }
               }
            }
         }
      }

   else if ( p_plot_param_3d->quadrant == 4 )
      {
      for ( i=1; i<nx; i++ )
         {
         xpoints[0] = xcontour[xindex+i-1];
         xpoints[1] = xcontour[xindex+i];
         xpoints[2] = xcontour[xindex+i];
         xpoints[3] = xcontour[xindex+i-1];

         /* All x coordinates within range */
         if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
              (xmin <= xpoints[1] && xpoints[1] <= xmax) )
            {
            for ( j=ny-1; j>0; j-- )
               {
               ypoints[0] = ycontour[yindex+j-1];
               ypoints[1] = ycontour[yindex+j-1];
               ypoints[2] = ycontour[yindex+j];
               ypoints[3] = ycontour[yindex+j];

               /* All y coordinates within range */
               if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                    (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                  {
                  zpoints[0] = zcontour[zindex+ny*(i-1)+j-1];
                  zpoints[1] = zcontour[zindex+ny*i+j-1];
                  zpoints[2] = zcontour[zindex+ny*i+j];
                  zpoints[3] = zcontour[zindex+ny*(i-1)+j];

                  /* All z coordinates within range */
                  if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                       (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                       (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                       (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                     {
                     PlotNormal3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }

                  /* Not all z coordinates within range */
                  else
                     {
                     PlotInterp3d(iplot, -1, xmin, ymin, zmin, zmax, xscale, yscale, zscale,
                                  &origin[0], &p_plot_param_3d->Ryz[0], &fill_color[0],
                                  &xpoints[0], &ypoints[0], &zpoints[0]);
                     DrawContours3d(icontour, &xpoints[0], &ypoints[0], &zpoints[0]);
                     }
                  }
               }
            }
         }
      }

   return;
   }
