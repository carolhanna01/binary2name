/*******************************************************************************
*
* ContourPlot2d.c
*
* Plots two-dimensional contour lines using contour lines of the same color or
* contour lines which vary in color.
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


void ContourPlot2d ( int iplot, int icontour, int xindex, int yindex, int zindex, int nx, int ny )
   {
   /* Declare variables */
   int i, j, k, nxvalues, nyvalues, nzvalues, width, height, nc, linewidth;
   guint32 color, *color0 = NULL;
   double x1_box, x2_box, y1_box, y2_box, xmin, xmax, ymin, ymax, zmin, zmax,
          xscale, yscale, zscale, dz, width_bar, height_bar,
          x1_bar, x2_bar, y1_bar, y2_bar, x1_tick, x2_tick, y1_tick, y2_tick,
          width_ztick_labels, x1, x2, y1, y2, affine1[6], affine2[6], fraction,
          dx1, dx2, dy1, dy2, *contours, *xi, *yi, *zi, x, y, dzdx, dzdy, dr, grad, dr0;
   char string[21];
   GdkPixbuf *pixbuf_contour;
   GnomeCanvasItem *line, *text, *pixbuf_item;
   GnomeCanvasPoints *points;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get minimum and maximum axis values */
   nxvalues = p_ticklabels->nxvalues;
   xmin = p_ticklabels->xvalues[0];
   xmax = p_ticklabels->xvalues[nxvalues-1];
   nyvalues = p_ticklabels->nyvalues;
   ymin = p_ticklabels->yvalues[0];
   ymax = p_ticklabels->yvalues[nyvalues-1];
   nzvalues = p_ticklabels->nzvalues;
   zmin = p_ticklabels->zvalues[0];
   zmax = p_ticklabels->zvalues[nzvalues-1];
   xmin = xmin - p_ticklabels->xoffset1;
   xmax = xmax + p_ticklabels->xoffset2;
   ymin = ymin - p_ticklabels->yoffset1;
   ymax = ymax + p_ticklabels->yoffset2;
   zmin = zmin - p_ticklabels->zoffset1;
   zmax = zmax + p_ticklabels->zoffset2;


   /* Calculate axis scale factors */
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);


   /* Get number of contour lines */
   if ( ncontours < 2 )
      nc = 2*nzvalues - 1;
   else
      nc = ncontours;
   contours = xmalloc(nc*sizeof(double));


   /* Calculate contour-line values */
   for ( i=1; i<=nc; i++ )
      contours[i-1] = zmin + (i - 1)*(zmax - zmin)/(nc - 1);


   /* Contour plot with variable-color contour lines */
   if ( styleflags[iplot-1] == 7 )
      {
      /* Calculate contour-line colors */
      color0 = xmalloc(nc*sizeof(guint32));
      for ( i=1; i<=nc; i++ )
         {
         fraction = (double)(i - 1)/(double)(nc - 1);
         color0[i-1] = interp_color_1(fraction);   /* alpha = 0xFF */
         }


      /* Draw color bar */
      if ( icontour == 1 )
         {
         width_bar = 20.0;
         height_bar = 0.875*(y2_box - y1_box);
         x1_bar = x2_box + 20.0;
         x2_bar = x1_bar + width_bar;
         y1_bar = (y1_box + y2_box)/2.0 + height_bar/2.0;
         y2_bar = (y1_box + y2_box)/2.0 - height_bar/2.0;
         if ( strcmp(p_plot_param->plot_box, "on") == 0 )
            {
            points = gnome_canvas_points_new(2);
            for ( i=1; i<=nc; i++ )
               {
               points->coords[0] = x1_bar;
               points->coords[1] = y1_bar + (i - 1.0)*(y2_bar - y1_bar)/(nc - 1.0);
               points->coords[2] = x2_bar;
               points->coords[3] = points->coords[1];
               line = gnome_canvas_item_new(group,
                                            GNOME_TYPE_CANVAS_LINE,
                                            "points", points,
                                            "fill_color_rgba", color0[i-1],
                                            "width_pixels", 2,
                                            NULL);
               }
            gnome_canvas_points_unref(points);
            }


         /* Draw and label color-bar tick marks */
         x1_tick = x2_bar + 8.0;
         x2_tick = x2_bar + 8.0 + tick_major;
         y1_tick = y1_bar;
         y2_tick = y2_bar;
         if ( strcmp(p_plot_param->plot_box, "on") == 0 )
            {
            /* Draw vertical line */
            points = gnome_canvas_points_new(4);
            points->coords[0] = x2_bar + 8.0;
            points->coords[1] = y1_bar;
            points->coords[2] = x2_bar + 8.0 + tick_major;
            points->coords[3] = y1_bar;
            points->coords[4] = x2_bar + 8.0 + tick_major;
            points->coords[5] = y2_bar;
            points->coords[6] = x2_bar + 8.0;
            points->coords[7] = y2_bar;
            line = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_LINE,
                                         "points", points,
                                         "fill_color_rgba", canvas_fg_color,
                                         "width_pixels", 2,
                                         NULL);
            gnome_canvas_points_unref(points);

            /* Draw tick marks and tick-mark labels */
            if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 )
               {
               DrawTickMarks("linear", minor_ticks_flag, 0,
                             x2_bar + 8.0 + tick_major, y1_bar, x2_bar + 8.0 + tick_major, y2_bar,
                             p_ticklabels->nzvalues, &p_ticklabels->zvalues[0],
                             p_ticklabels->zoffset1, p_ticklabels->zoffset2,
                             180*deg2rad);

               if ( strcmp(p_plot_param->z_tick_labels, "on") == 0 )
                  {
                  zscale = height_bar/(zmax - zmin);
                  dz = (zmax - zmin)/(nzvalues - 1);
                  width_ztick_labels = 0.0;
                  for ( i=1; i<=nzvalues; i++ )
                     {
                     memset(string, 0, sizeof(string));
                     if ( fabs(p_ticklabels->zvalues[i-1]) < 0.01*dz )
                        snprintf(string, sizeof(string), "%1.0f", 0.0);
                     else
                        snprintf(string, sizeof(string), "%g", p_ticklabels->zvalues[i-1]);

                     text = gnome_canvas_item_new(group,
                                                  GNOME_TYPE_CANVAS_TEXT,
                                                  "text", string,
                                                  "x", x2_bar + 8.0 + tick_major + 8.0,
                                                  "y", y1_bar - (p_ticklabels->zvalues[i-1] - zmin)*zscale,
                                                  "anchor", GTK_ANCHOR_WEST,
                                                  "font-desc", font_tick_labels,
                                                  "fill_color_rgba", canvas_fg_color,
                                                  NULL);

                     gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
                     if ( x2 - x1 > width_ztick_labels )
                        width_ztick_labels = x2 - x1;
                     }
                  }
               }
            }
         }

      /* Label color-bar */
      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
           icontour == 1 && zlabel != NULL && pixbuf_zlabel != NULL )
         {
         /* Draw zlabel pixbuf canvas item */
         pixbuf_item = gnome_canvas_item_new(group,
                                             GNOME_TYPE_CANVAS_PIXBUF,
                                             "pixbuf", pixbuf_zlabel,
                                             "x", 0.0,
                                             "y", 0.0,
                                             "anchor", GTK_ANCHOR_NORTH,
                                             NULL);

         /* Rotate and translate zlabel pixbuf canvas item */
         art_affine_rotate(affine1, -90.0);
         gnome_canvas_item_affine_absolute(pixbuf_item, affine1);
         x = -(y1_box + y2_box)/2.0;
         if ( strcmp(p_plot_param->z_tick_marks, "on") == 0 &&
              strcmp(p_plot_param->z_tick_labels, "on") == 0 )
            y = x2_tick + 8.0 + width_ztick_labels + 8.0;
         else
            y = x2_tick + 8.0;
         art_affine_translate(affine2, x, y);
         gnome_canvas_item_affine_relative(pixbuf_item, affine2);
         }
      }


   /* Calculate pixbuf width and height */
   dx1 = 0.0;
   dx2 = 0.0;
   dy1 = 0.0;
   dy2 = 0.0;

   if ( xmin >= xcontour[xindex] )
      x1 = xmin;
   else
      {
      x1 = xcontour[xindex];
      dx1 = xcontour[xindex] - xmin;
      }

   if ( xmax <= xcontour[xindex+nx-1] )
      x2 = xmax;
   else
      {
      x2 = xcontour[xindex+nx-1];
      dx2 = xmax - xcontour[xindex+nx-1];
      }

   if ( ymin >= ycontour[yindex] )
      y1 = ymin;
   else
      {
      y1 = ycontour[yindex];
      dy1 = ycontour[yindex] - ymin;
      }

   if ( ymax <= ycontour[yindex+ny-1] )
      y2 = ymax;
   else
      {
      y2 = ycontour[yindex+ny-1];
      dy2 = ymax - ycontour[yindex+ny-1];
      }

   width  = roundint(x2_box - x1_box + 1.0 - (dx1 + dx2)*xscale);
   height = roundint(y2_box - y1_box + 1.0 - (dy1 + dy2)*yscale);


   /* Check pixbuf width and height */
   if ( width <= 0 || height <= 0 )
      return;


   /* Get interpolated values of x and y */
   xi = xmalloc(width*sizeof(double));
   yi = xmalloc(height*sizeof(double));
   zi = xmalloc(width*height*sizeof(double));
   for ( i=1; i<=width; i++ )
      xi[i-1] = x1 + (i - 1)*(x2 - x1)/(width - 1);
   for ( j=1; j<=height; j++ )
      yi[j-1] = y1 + (j - 1)*(y2 - y1)/(height - 1);


   /* Get interpolated values of z (bilinear interpolation) */
   for ( i=1; i<=width; i++ )
      for ( j=1; j<=height; j++ )
         interp2(nx, ny, 1, &xcontour[xindex], &ycontour[yindex], &zcontour[zindex],
                 &xi[i-1], &yi[j-1], &zi[height*(i-1)+(j-1)]);


   /* Create pixbuf */
   pixbuf_contour = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height);
   gdk_pixbuf_fill(pixbuf_contour, 0xFFFFFF00);


   /* Contour plot with variable-color contour lines */
   if ( styleflags[iplot-1] == 7 )
      {
      /* Draw contour lines */
      linewidth = stylesizes[iplot-1];
      dr0 = (linewidth + 1.0)/2.0;
      for ( i=2; i<width; i++ )
         for ( j=height-1; j>1; j-- )
            {
            dzdx = (zi[height*(i)+(j-1)] - zi[height*(i-2)+(j-1)])/2.0;
            dzdy = (zi[height*(i-1)+(j)] - zi[height*(i-1)+(j-2)])/2.0;
            grad = sqrt(dzdx*dzdx + dzdy*dzdy);
            for ( k=1; k<=nc; k++ )
               {
               dz = fabs(zi[height*(i-1)+(j-1)] - contours[k-1]);
               dr = dz/grad;
               if ( dr <= dr0 - 1.0 )
                  {
                  color = color0[k-1];                         /* alpha = 0xFF */
                  put_pixel(pixbuf_contour, i-1, height-j, color);
                  }
               else if ( dr <= dr0 )
                  {
                  color = color0[k-1] - (1 - dr0 + dr)*0xFF;   /* alpha = (dr0 - dr)*0xFF */
                  put_pixel(pixbuf_contour, i-1, height-j, color);
                  }
               }
            }
      }

   /* Contour plot with constant-color contour lines */
   else if ( styleflags[iplot-1] == 1 || styleflags[iplot-1] == 3 )
      {
      /* Draw contour lines */
      linewidth = stylesizes[iplot-1];
      dr0 = (linewidth + 1.0)/2.0;
      for ( i=2; i<width; i++ )
         for ( j=height-1; j>1; j-- )
            {
            dzdx = (zi[height*(i)+(j-1)] - zi[height*(i-2)+(j-1)])/2.0;
            dzdy = (zi[height*(i-1)+(j)] - zi[height*(i-1)+(j-2)])/2.0;
            grad = sqrt(dzdx*dzdx + dzdy*dzdy);
            for ( k=1; k<=nc; k++ )
               {
               dz = fabs(zi[height*(i-1)+(j-1)] - contours[k-1]);
               dr = dz/grad;
               if ( dr <= dr0 - 1.0 )
                  {
                  color = stylecolor1[iplot-1];                           /* alpha = 0xFF */
                  put_pixel(pixbuf_contour, i-1, height-j, color);
                  }
               else if ( dr <= dr0 )
                  {
                  color = stylecolor1[iplot-1] - (1.0 - dr0 + dr)*0xFF;   /* alpha = (dr0 - dr)*0xFF */
                  put_pixel(pixbuf_contour, i-1, height-j, color);
                  }
               }
            }
      }


   /* Draw pixbuf on canvas */
   x = x1_box + dx1*xscale;
   y = y2_box - dy1*yscale;
   pixbuf_item = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_PIXBUF,
                                       "pixbuf", pixbuf_contour,
                                       "x", x,
                                       "y", y,
                                       "anchor", GTK_ANCHOR_SOUTH_WEST,
                                       NULL);
   g_object_unref(pixbuf_contour);


   /* Free memory */
   free(contours);
   free(color0);
   free(xi);
   free(yi);
   free(zi);

   return;
   }
