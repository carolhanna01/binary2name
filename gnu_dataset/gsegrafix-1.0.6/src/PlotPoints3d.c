/*******************************************************************************
*
* PlotPoints3d.c
*
* Plots a two-dimensional projection of three-dimensional points data.
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


void PlotPoints3d ( int iplot, int index, int npts )
   {
   /* Declare variables */
   int i, j, ifunc, nxvalues, nyvalues, nzvalues, index_stemflags;
   double phi, theta, axis_length, origin[3], Ry[9], Rz[9], Ryz[9], r[3], rstem[3], *p,
          xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale, x, y;
   char *pchar;
   extern char symbol_string1[];   /* symbol-specification characters "cCtTsSiIpPhH" */
   extern char symbol_string2[];   /* symbol-specification characters "+xra" */
   GnomeCanvasPoints *points;


   /* Specify view angles */
   phi = p_plot_param_3d->phi;       /* view-direction azimuth (deg) from x axis in x-y plane */
   theta = p_plot_param_3d->theta;   /* view-direction elevation (deg) from x-y plane */


   /* Specify origin */
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


   /* Specify axis length */
   axis_length = p_plot_param_3d->axis_length;


   /* Specify minimum and maximum axis values */
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


   /* Draw stem lines */
   index_stemflags = (iplot - 1)*4;
   if ( strcmp(&stemflags[index_stemflags],  "on") == 0 || 
        strcmp(&stemflags[index_stemflags], "num") == 0 )
      {
      points = gnome_canvas_points_new(2);

      /* Calculate z coordinate of stem point 1 */
      if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
         rstem[2] = 0.0;
      else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
         {
         if ( zmin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= zmax )
            rstem[2] = (stemvalues[iplot-1] - zmin)*zscale;
         else if ( stemvalues[iplot-1] < zmin )
            rstem[2] = 0.0;
         else if ( stemvalues[iplot-1] > zmax )
            rstem[2] = axis_length;
         }

      for ( i=1; i<=npts; i++ )
         if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
              ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax &&
              zmin <= zdata[index+i-1] && zdata[index+i-1] <= zmax )
            {
            /* Calculate coordinates of stem point 1 */
            rstem[0] = (xdata[index+i-1] - xmin)*xscale;
            rstem[1] = (ydata[index+i-1] - ymin)*yscale;

            p = multiply_mv(Ryz, rstem);
            for ( j=1; j<=3; j++, p++ )
               r[j-1] = *p;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            points->coords[0] = x;
            points->coords[1] = y;

            /* Calculate coordinates of stem point 2 */
            r[0] = (xdata[index+i-1] - xmin)*xscale;
            r[1] = (ydata[index+i-1] - ymin)*yscale;
            r[2] = (zdata[index+i-1] - zmin)*zscale;
            p = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, p++ )
               r[j-1] = *p;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            points->coords[2] = x;
            points->coords[3] = y;

            DrawLine(points, outline_colors_rgba[iplot-1], 1);
            }

      gnome_canvas_points_unref(points);
      }


   /* Draw lines */
   if ( stylechar1[iplot-1] == 'l' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, 'l');


   /* Draw dashed lines */
   else if ( stylechar1[iplot-1] == 'd' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, 'd');


   /* Draw dotted lines */
   else if ( stylechar1[iplot-1] == '.' )
      DrawLineSegments3d(iplot, index, npts, &origin[0], &Ryz[0],
                         xmin, xmax, ymin, ymax, zmin, zmax,
                         xscale, yscale, zscale, '.');


   /* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
   else if ( (pchar = strchr(symbol_string1, stylechar1[iplot-1])) != NULL )
      {
      ifunc = pchar - symbol_string1;
      for ( i=1; i<=npts; i++ )
         if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
              ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax &&
              zmin <= zdata[index+i-1] && zdata[index+i-1] <= zmax )
            {
            r[0] = (xdata[index+i-1] - xmin)*xscale;
            r[1] = (ydata[index+i-1] - ymin)*yscale;
            r[2] = (zdata[index+i-1] - zmin)*zscale;

            p = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, p++ )
               r[j-1] = *p;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            symbol_func1[ifunc](x, y, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1],
                                stylesizes[iplot-1]);
            }
      }


   /* Draw symbols in symbol_string2 ("+xra") */
   else if ( (pchar = strchr(symbol_string2, stylechar1[iplot-1])) != NULL )
      {
      ifunc = pchar - symbol_string2;
      for ( i=1; i<=npts; i++ )
         if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
              ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax &&
              zmin <= zdata[index+i-1] && zdata[index+i-1] <= zmax )
            {
            r[0] = (xdata[index+i-1] - xmin)*xscale;
            r[1] = (ydata[index+i-1] - ymin)*yscale;
            r[2] = (zdata[index+i-1] - zmin)*zscale;

            p = multiply_mv(Ryz, r);
            for ( j=1; j<=3; j++, p++ )
               r[j-1] = *p;

            x = origin[1] + r[1];
            y = origin[2] - r[2];
            symbol_func2[ifunc](x, y, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
            }
      }

   return;
   }


void DrawLineSegments3d ( int iplot, int index, int npts, double *origin, double *Ryz,
                          double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
                          double xscale, double yscale, double zscale, int linechar )
   {
   /* Declare variables */
   int idraw, iseg, nseg, npts_seg, iseg1;
   char linetype[7];


   /* Create linetype string */
   memset(linetype, 0, sizeof(linetype));
   if ( linechar == 'l' )
      strcpy(linetype, "solid");
   else if ( linechar == 'd' )
      strcpy(linetype, "dashed");
   else if ( linechar == '.' )
      strcpy(linetype, "dotted");


   /* Draw all line segments except last */
   idraw = 0;
   nseg = nlinebreaks;
   iseg1 = index;
   for (iseg=1; iseg<=nseg; iseg++ )
      {
      if ( index < nlinebreak[iseg-1] && nlinebreak[iseg-1] < index + npts )
         {
         idraw++;
         npts_seg = nlinebreak[iseg-1] - iseg1;
         DrawLines3d(npts_seg, &xdata[iseg1], &ydata[iseg1], &zdata[iseg1], origin, Ryz,
                     xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
                     fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);
         iseg1 = nlinebreak[iseg-1];
         }
      }


   /* Draw last line segment */
   if ( idraw > 0 )
      {
      npts_seg = index + npts - iseg1;
      DrawLines3d(npts_seg, &xdata[iseg1], &ydata[iseg1], &zdata[iseg1], origin, Ryz,
                  xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
                  fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);
      }

   /* Draw continuous line */
   else
      DrawLines3d(npts, &xdata[index], &ydata[index], &zdata[index], origin, Ryz,
                  xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale,
                  fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);

   return;
   }
