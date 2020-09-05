/*******************************************************************************
*
* ColorPlot3d.c
*
* Plots three-dimensional contour information as a function of color.
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


void ColorPlot3d ( int iplot, int xindex, int yindex, int zindex, int nx, int ny )
   {
   /* Declare variables */
   int width, height, nxvalues, nyvalues, nzvalues, quadrant,
       i, j, k, i11, i22, j11, j22, nxdata, nydata, nx_interp, ny_interp,
       ipixel, jpixel, index_colors, index_zi;
   guint32 *colors;
   double x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, zmin, zmax,
          xscale, yscale, zscale, axis_length, origin[3],
          *xi, *yi, *zi, dx, dy, *Ryz, *p, r[3], zscale2, fraction;
   GdkPixbuf *pixbuf_color;
   GnomeCanvasItem *pixbuf_item;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;
   width  = roundint(x2_box - x1_box);
   height = roundint(y2_box - y1_box);


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
   axis_length = p_plot_param_3d->axis_length;
   xscale = axis_length/(xmax - xmin);
   yscale = axis_length/(ymax - ymin);
   zscale = axis_length/(zmax - zmin);


   /* Get origin */
   quadrant = p_plot_param_3d->quadrant;
   if ( quadrant == 1 )
      {
      origin[0] = p_plot_param_3d->origin[0];
      origin[1] = p_plot_param_3d->origin[1];
      origin[2] = p_plot_param_3d->origin[2];
      }
   else if ( quadrant == 2 )
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis2[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis2[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis2[2];
      }
   else if ( quadrant == 3 )
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0] + p_plot_param_3d->axis2[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1] + p_plot_param_3d->axis2[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2] - p_plot_param_3d->axis2[2];
      }
   else if ( quadrant == 4 )
      {
      origin[0] = p_plot_param_3d->origin[0] + p_plot_param_3d->axis1[0];
      origin[1] = p_plot_param_3d->origin[1] + p_plot_param_3d->axis1[1];
      origin[2] = p_plot_param_3d->origin[2] - p_plot_param_3d->axis1[2];
      }


   /* Find data indices in range */
   for ( i=1; i<=nx; i++ )
      if ( xmin <= xcolor[xindex+i-1] )
         {
         i11 = i;
         break;
         }

   for ( i=nx; i>=1; i-- )
      if ( xcolor[xindex+i-1] <= xmax )
         {
         i22 = i;
         break;
         }

   for ( j=1; j<=ny; j++ )
      if ( ymin <= ycolor[yindex+j-1] )
         {
         j11 = j;
         break;
         }

   for ( j=ny; j>=1; j-- )
      if ( ycolor[yindex+j-1] <= ymax )
         {
         j22 = j;
         break;
         }


   /* Get interpolated values of x and y */
   nxdata = i22 - i11 + 1;
   nx_interp = ninterp[iplot-1]*(nxdata - 1) + 1;
   nydata = j22 - j11 + 1;
   ny_interp = ninterp[iplot-1]*(nydata - 1) + 1;
   xi = xmalloc(nx_interp*sizeof(double));
   yi = xmalloc(ny_interp*sizeof(double));
   zi = xmalloc(nx_interp*ny_interp*sizeof(double));

   xi[0] = xcolor[xindex+i11-1];
   for ( i=i11; i<i22; i++ )
      {
      dx = (xcolor[xindex+i] - xcolor[xindex+i-1])/ninterp[iplot-1];
      for ( k=1; k<=ninterp[iplot-1]; k++ )
         xi[ninterp[iplot-1]*(i-i11)+k] = xcolor[xindex+i-1] + k*dx;
      }

   yi[0] = ycolor[yindex+j11-1];
   for ( j=j11; j<j22; j++ )
      {
      dy = (ycolor[yindex+j] - ycolor[yindex+j-1])/ninterp[iplot-1];
      for ( k=0; k<ninterp[iplot-1]; k++ )
         yi[ninterp[iplot-1]*(j-j11)+k] = ycolor[yindex+j-1] + k*dy;
      }


   /* Get interpolated values of z (bilinear interpolation) */
   for ( i=1; i<=nx_interp; i++ )
      for ( j=1; j<=ny_interp; j++ )
         interp2(nx, ny, 1, &xcolor[xindex], &ycolor[yindex], &zcolor[zindex],
                 &xi[i-1], &yi[j-1], &zi[ny_interp*(i-1)+(j-1)]);


   /* Create color array */
   colors = xmalloc(width*height*sizeof(unsigned int));
   for ( i=1; i<=width; i++ )
      for ( j=1; j<=height; j++ )
         colors[height*(i-1)+(j-1)] = 0xFFFFFF00;   /* transparent */


   /* Calculate plot colors */
   Ryz = &p_plot_param_3d->Ryz[0];
   zscale2 = 1.0/(zmax - zmin);
   if ( quadrant == 1 || quadrant == 4 )
      for ( i=1; i<=nx_interp; i++ )
         for ( j=1; j<=ny_interp; j++ )
            {
            r[0] = (xi[i-1] - xmin)*xscale;
            r[1] = (yi[j-1] - ymin)*yscale;
            r[2] = (zi[ny_interp*(i-1)+(j-1)] - zmin)*zscale;
            p = multiply_mv(Ryz, &r[0]);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            ipixel = roundint(origin[1] + r[1] - x1_box);
            jpixel = roundint(origin[2] - r[2] - y1_box);

            index_colors = height*(ipixel - 1) + (jpixel - 1);
            if ( index_colors < 0 || index_colors > width*height )
               continue;

            index_zi = ny_interp*(i-1)+(j-1);
            fraction = (zi[index_zi] - zmin)*zscale2;
            if ( 0.0 <= fraction && fraction <= 1.0  )
               colors[index_colors] = interp_color_1(fraction) - 0xFF + alphacolor[iplot-1];
            }

   else if ( quadrant == 2 || quadrant == 3 )
      for ( i=nx_interp; i>1; i-- )
         for ( j=1; j<=ny_interp; j++ )
            {
            r[0] = (xi[i-1] - xmin)*xscale;
            r[1] = (yi[j-1] - ymin)*yscale;
            r[2] = (zi[ny_interp*(i-1)+(j-1)] - zmin)*zscale;
            p = multiply_mv(Ryz, &r[0]);
            for ( k=1; k<=3; k++, p++ )
               r[k-1] = *p;
            ipixel = roundint(origin[1] + r[1] - x1_box);
            jpixel = roundint(origin[2] - r[2] - y1_box);

            index_colors = height*(ipixel - 1) + (jpixel - 1);
            if ( index_colors < 0 || index_colors > width*height )
               continue;

            index_zi = ny_interp*(i-1)+(j-1);
            fraction = (zi[index_zi] - zmin)*zscale2;
            if ( 0.0 <= fraction && fraction <= 1.0  )
               colors[index_colors] = interp_color_1(fraction) - 0xFF + alphacolor[iplot-1];
            }


   /* Create pixbuf */
   pixbuf_color = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height);
   gdk_pixbuf_fill(pixbuf_color, 0xFFFFFF00);   /* transparent */


   /* Specify pixbuf colors */
   if ( quadrant == 1 )
      for ( i=1; i<=width; i++ )
         for ( j=1; j<=height; j++ )
            put_pixel(pixbuf_color, i-1, j-1, colors[height*(i-1)+(j-1)]);
   else if ( quadrant == 2 )
      for ( i=width; i>=1; i-- )
         for ( j=1; j<=height; j++ )
            put_pixel(pixbuf_color, i-1, j-1, colors[height*(i-1)+(j-1)]);
   else if ( quadrant == 3 )
      for ( i=width; i>=1; i-- )
         for ( j=height; j>=1; j-- )
            put_pixel(pixbuf_color, i-1, j-1, colors[height*(i-1)+(j-1)]);
   else if ( quadrant == 4 )
      for ( i=1; i<=width; i++ )
         for ( j=height; j>=1; j-- )
            put_pixel(pixbuf_color, i-1, j-1, colors[height*(i-1)+(j-1)]);


   /* Draw pixbuf on canvas */
   pixbuf_item = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_PIXBUF,
                                       "pixbuf", pixbuf_color,
                                       "x", (x1_box + x2_box)/2.0,
                                       "y", (y1_box + y2_box)/2.0,
                                       "anchor", GTK_ANCHOR_CENTER,
                                       NULL);
   g_object_unref(pixbuf_color);


   /* Free memory */
   free(xi);
   free(yi);
   free(zi);
   free(colors);

   return;
   }

