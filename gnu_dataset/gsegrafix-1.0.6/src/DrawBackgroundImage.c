/*******************************************************************************
*
* DrawBackgroundImage.c
*
* Draws plot background image.
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


void DrawBackgroundImage ( void )
   {
   /* Declare variables */
   int i, j, width_plot, height_plot, width_image, height_image,
       x_image, y_image, width_image_copied, height_image_copied,
       width_image_scaled, height_image_scaled, width, height;
   int CENTER=1, FILL=2, SCALE=3, ZOOM=4;
   double x1_box, x2_box, y1_box, y2_box, xcenter, ycenter,
          radius, ratio_plot, ratio_image, i0, j0, rsq;
   GdkPixbuf *image_file, *image_copied, *image_scaled, *image;
   GnomeCanvasItem *image_item, *rectangle_item;


   /* Check background_image_file */
   if ( background_image_file == NULL )
      return;


   /* Get plot box minimum and maximum values */
   if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      x1_box = p_plot_box_data->xmin;
      x2_box = p_plot_box_data->xmax;
      y1_box = p_plot_box_data->ymin;
      y2_box = p_plot_box_data->ymax;
      xcenter = (x1_box + x2_box)/2.0;
      ycenter = (y1_box + y2_box)/2.0;
      width_plot = roundint(x2_box - x1_box);
      height_plot = roundint(y2_box - y1_box);
      ratio_plot = (y2_box - y1_box)/(x2_box - x1_box);
      }

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      xcenter = 0.375*p_window_data->width;
      ycenter = 0.500*p_window_data->height;
      if ( p_window_data->width >= p_window_data->height )
         radius  = 0.375*p_window_data->height;
      else
         radius  = 0.375*p_window_data->width;
      width_plot = roundint(2.0*radius);
      height_plot = width_plot;
      ratio_plot = 1.0;
      }


   /* Draw background image */
   image_file = NULL;
   image_copied = NULL;
   image_scaled = NULL;
   image_file = gdk_pixbuf_new_from_file(background_image_file, NULL);
   width_image = gdk_pixbuf_get_width(image_file);
   height_image = gdk_pixbuf_get_height(image_file);
   ratio_image = (double) height_image/(double) width_image;

   if ( background_image_style == CENTER )
      {
      if ( width_image > width_plot && height_image <= height_plot )
         {
         width_image_copied = width_plot;
         height_image_copied = height_image;
         x_image = roundint((width_image - width_image_copied)/2.0);
         y_image = 0;
         }
      else if ( width_image <= width_plot && height_image > height_plot )
         {
         width_image_copied = width_image;
         height_image_copied = height_plot;
         x_image = 0;
         y_image = roundint((height_image - height_image_copied)/2.0);
         }
      else if ( width_image > width_plot && height_image > height_plot )
         {
         width_image_copied = width_plot;
         height_image_copied = height_plot;
         x_image = roundint((width_image - width_image_copied)/2.0);
         y_image = roundint((height_image - height_image_copied)/2.0);
         }
      else
         {
         width_image_copied = width_image;
         height_image_copied = height_image;
         x_image = 0;
         y_image = 0;
         }

      image_copied = gdk_pixbuf_new(GDK_COLORSPACE_RGB,
                                    TRUE,
                                    8,
                                    width_image_copied,
                                    height_image_copied);

      gdk_pixbuf_copy_area(image_file,
                           x_image,
                           y_image,
                           width_image_copied,
                           height_image_copied,
                           image_copied,
                           0,
                           0);

      width = width_image_copied;
      height = height_image_copied;
      image = image_copied;
      }

   else if ( background_image_style == FILL )
      {
      width_image_scaled = width_plot;
      height_image_scaled = height_plot;

      image_scaled = gdk_pixbuf_scale_simple(image_file,
                                             width_image_scaled,
                                             height_image_scaled,
                                             GDK_INTERP_BILINEAR);

      width = width_image_scaled;
      height = height_image_scaled;
      image = image_scaled;
      }

   else if ( background_image_style == SCALE )
      {
      if ( ratio_image <= ratio_plot )
         {
         width_image_scaled = width_plot;
         height_image_scaled = roundint(width_image_scaled*ratio_image);
         }
      else
         {
         height_image_scaled = height_plot;
         width_image_scaled = roundint(height_image_scaled/ratio_image);
         }

      image_scaled = gdk_pixbuf_scale_simple(image_file,
                                             width_image_scaled,
                                             height_image_scaled,
                                             GDK_INTERP_BILINEAR);

      width = width_image_scaled;
      height = height_image_scaled;
      image = image_scaled;
      }

   else if ( background_image_style == ZOOM )
      {
      if ( ratio_image <= ratio_plot )
         {
         height_image_copied = height_image;
         width_image_copied = roundint(height_image_copied/ratio_plot);
         x_image = roundint((width_image - width_image_copied)/2.0);
         y_image = 0;
         }
      else
         {
         width_image_copied = width_image;
         height_image_copied = roundint(width_image_copied*ratio_plot);
         x_image = 0;
         y_image = roundint((height_image - height_image_copied)/2.0);
         }

      image_copied = gdk_pixbuf_new(GDK_COLORSPACE_RGB,
                                    TRUE,
                                    8,
                                    width_image_copied,
                                    height_image_copied);

      gdk_pixbuf_copy_area(image_file,
                           x_image,
                           y_image,
                           width_image_copied,
                           height_image_copied,
                           image_copied,
                           0,
                           0);

      width_image_scaled = width_plot;
      height_image_scaled = height_plot;
      image_scaled = gdk_pixbuf_scale_simple(image_copied,
                                             width_image_scaled,
                                             height_image_scaled,
                                             GDK_INTERP_BILINEAR);

      width = width_image_scaled;
      height = height_image_scaled;
      image = image_scaled;
      }


   if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      /* Make image pixels outside plot circle transparent */
      i0 = (double) width/2.0;
      j0 = (double) height/2.0;
      for ( i=1; i<=width; i++ )
         for ( j=1; j<=height; j++ )
            {
            rsq = (i - i0)*(i - i0) + (j - j0)*(j - j0);
            if ( rsq >= radius*radius )
               put_pixel(image, i-1, j-1, 0xFFFFFF00);
            }
      }


   /* Draw image on canvas */
   image_item = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_PIXBUF,
                                      "pixbuf", image,
                                      "x", xcenter,
                                      "y", ycenter,
                                      "anchor", GTK_ANCHOR_CENTER,
                                      NULL);

   if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      rectangle_item = gnome_canvas_item_new(group,
                                             GNOME_TYPE_CANVAS_RECT,
                                             "x1", xcenter - radius,
                                             "x2", xcenter + radius,
                                             "y1", ycenter + radius,
                                             "y2", ycenter - radius,
                                             "fill_color_rgba", 0xFFFFFF00,
                                             "outline_color_rgba", canvas_bg_color,
                                             "width_pixels", 2,
                                             NULL);

   if ( G_IS_OBJECT(image_scaled) )
      gdk_pixbuf_unref(image_scaled);
   if ( G_IS_OBJECT(image_copied) )
      gdk_pixbuf_unref(image_copied);
   if ( G_IS_OBJECT(image_file) )
      gdk_pixbuf_unref(image_file);

   return;
   }

