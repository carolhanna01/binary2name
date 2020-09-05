/*******************************************************************************
*
* ColorPlot2d.c
*
* Plots two-dimensional contour information as a function of color.
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


void ColorPlot2d ( int iplot, int icolor, int xindex, int yindex, int zindex, int nx, int ny )
   {
   /* Declare variables */
   int i, j, i1, j1, in, jn, nxvalues, nyvalues, nzvalues, width, height;
   guint32 color;
   double x1_box, x2_box, y1_box, y2_box, xmin, xmax, ymin, ymax, zmin, zmax,
          xscale, yscale, zscale, dz, width_bar, height_bar,
          x1_bar, x2_bar, y1_bar, y2_bar, width_ztick_labels,
          x1, x2, y1, y2, affine1[6], affine2[6], fraction,
          dx1, dx2, dy1, dy2, *xi, *yi, *zi, x, y, z;
   char string[21];
   GdkPixbuf *pixbuf_colorbar, *pixbuf_color;
   GnomeCanvasItem *line, *text, *pixbuf_item;
   GnomeCanvasPoints *points;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


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
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);


   /* Draw color-bar pixbuf */
   if ( icolor == 1 )
      {
      width_bar = 20.0;
      height_bar = 0.875*(y2_box - y1_box);
      x1_bar = x2_box + 20.0;
      x2_bar = x1_bar + width_bar;
      y1_bar = (y1_box + y2_box)/2.0 + height_bar/2.0;
      y2_bar = (y1_box + y2_box)/2.0 - height_bar/2.0;
      if ( strcmp(p_plot_param->plot_box, "on") == 0 )
         {
         width  = roundint(width_bar);
         height = roundint(height_bar);
         pixbuf_colorbar = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height);
         gdk_pixbuf_fill(pixbuf_colorbar, 0xFFFFFF00);

         for ( j=height; j>=1; j-- )
            {
            fraction = (j - 1.0)/(height - 1.0);
            color = interp_color_1(fraction);
            for ( i=1; i<=width; i++ )
               put_pixel(pixbuf_colorbar, i-1, height-j, color);
            }

         pixbuf_item = gnome_canvas_item_new(group,
                                             GNOME_TYPE_CANVAS_PIXBUF,
                                             "pixbuf", pixbuf_colorbar,
                                             "x", x1_bar,
                                             "y", y1_bar,
                                             "anchor", GTK_ANCHOR_SOUTH_WEST,
                                             NULL);

         g_object_unref(pixbuf_colorbar);
         }

      /* Draw and label color-bar tick marks */
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
        icolor == 1 && zlabel != NULL && pixbuf_zlabel != NULL )
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
         y = x2_bar + 8.0 + tick_major + 8.0 + width_ztick_labels + 8.0;
      else
         y = x2_bar + 8.0 + tick_major + 8.0;
      art_affine_translate(affine2, x, y);
      gnome_canvas_item_affine_relative(pixbuf_item, affine2);
      }


   /* Calculate pixbuf width and height */
   dx1 = 0.0;
   dx2 = 0.0;
   dy1 = 0.0;
   dy2 = 0.0;

   if ( xmin >= xcolor[xindex] )
      x1 = xmin;
   else
      {
      x1 = xcolor[xindex];
      dx1 = xcolor[xindex] - xmin;
      }

   if ( xmax <= xcolor[xindex+nx-1] )
      x2 = xmax;
   else
      {
      x2 = xcolor[xindex+nx-1];
      dx2 = xmax - xcolor[xindex+nx-1];
      }

   if ( ymin >= ycolor[yindex] )
      y1 = ymin;
   else
      {
      y1 = ycolor[yindex];
      dy1 = ycolor[yindex] - ymin;
      }

   if ( ymax <= ycolor[yindex+ny-1] )
      y2 = ymax;
   else
      {
      y2 = ycolor[yindex+ny-1];
      dy2 = ymax - ycolor[yindex+ny-1];
      }

   width  = roundint(x2_box - x1_box - (dx1 + dx2)*xscale);
   height = roundint(y2_box - y1_box - (dy1 + dy2)*yscale);


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
   if ( styleflags[iplot-1] == 8 )
      for ( i=1; i<=width; i++ )
         for ( j=1; j<=height; j++ )
            interp2(nx, ny, 1, &xcolor[xindex], &ycolor[yindex], &zcolor[zindex],
                    &xi[i-1], &yi[j-1], &zi[height*(i-1)+(j-1)]);


   /* Get interpolated values of z (nearest-neighbor interpolation) */
   else if ( styleflags[iplot-1] == 9 )
      for ( i=1; i<=width; i++ )
         {
         if ( xi[i-1] <= xcolor[xindex] )
            i1 = 0;
         else if ( xi[i-1] >= xcolor[xindex+nx-1] )
            i1 = nx - 2;
         else
            i1 = find_indices(0, nx-1, &xcolor[xindex], xi[i-1]);

         for ( j=1; j<=height; j++ )
            {
            if ( yi[j-1] <= ycolor[yindex] )
               j1 = 0;
            else if ( yi[j-1] >= ycolor[yindex+ny-1] )
               j1 = ny - 2;
            else
               j1 = find_indices(0, ny-1, &ycolor[yindex], yi[j-1]);

            if ( fabs(xi[i-1] - xcolor[xindex+i1]) < fabs(xi[i-1] - xcolor[xindex+i1+1]) )
               in = i1;
            else
               in = i1 + 1;

            if ( fabs(yi[j-1] - ycolor[yindex+j1]) < fabs(yi[j-1] - ycolor[yindex+j1+1]) )
               jn = j1;
            else
               jn = j1 + 1;

            zi[height*(i-1)+(j-1)] = zcolor[zindex+ny*in+jn];
            }
         }


   /* Create pixbuf */
   pixbuf_color = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height);
   gdk_pixbuf_fill(pixbuf_color, 0xFFFFFF00);


   /* Draw color plot */
   zscale = 1.0/(zmax - zmin);
   for ( i=1; i<=width; i++ )
      for ( j=height; j>=1; j-- )
         {
         z = zi[height*(i-1)+(j-1)];
         fraction = (z - zmin)*zscale;
         if ( fraction < 0.0 )
            fraction = 0.0;
         else if ( fraction > 1.0 )
            fraction = 1.0;
         if ( z < zblack[iplot-1] )
            color = 0x000000FF;
         else if ( z > zwhite[iplot-1] )
            color = 0xFFFFFFFF;
         else
            color = interp_color_1(fraction);
         put_pixel(pixbuf_color, i-1, height-j, color);
         }


   /* Draw pixbuf on canvas */
   pixbuf_item = gnome_canvas_item_new(group,
                                       GNOME_TYPE_CANVAS_PIXBUF,
                                       "pixbuf", pixbuf_color,
                                       "x", (x1_box + x2_box + (dx1 - dx2)*xscale)/2.0,
                                       "y", (y2_box + y1_box + (dy2 - dy1)*yscale)/2.0,
                                       "anchor", GTK_ANCHOR_CENTER,
                                       NULL);
   g_object_unref(pixbuf_color);


   /* Draw grid */
   DrawGrid2d();
   DrawGridLog();


   /* Free memory */
   free(xi);
   free(yi);
   free(zi);

   return;
   }
