/*******************************************************************************
*
* GetAxisLabelPixbufs.c
*
* Gets pixbufs for x-axis label, y-axis label, z-axis label, and plot title
* from a GdkDrawable objects.
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


void GetAxisLabelPixbufs ( void )
   {
   /* Declare variables */
   int ipixbuf;
   double x, y, x1, y1, x2, y2, font_size;
   char *label;
   GnomeCanvasItem *text;
   PangoFontDescription *font;


   /* Get pixbufs for xlabel, ylabel, zlabel, and title */
   if ( p_window_data->flag == 1 )
      {
      for ( ipixbuf=1; ipixbuf<=4; ipixbuf++ )
         {
         if ( ipixbuf == 1 )
            {
            label = xlabel;
            font = font_axis_labels;
            font_size = font_size_axis_labels;
            }
         else if ( ipixbuf == 2 )
            {
            label = ylabel;
            font = font_axis_labels;
            font_size = font_size_axis_labels;
            }
         else if ( ipixbuf == 3 )
            {
            label = zlabel;
            font = font_axis_labels;
            font_size = font_size_axis_labels;
            }
         else if ( ipixbuf == 4 )
            {
            label = title;
            font = font_title;
            font_size = font_size_title;
            }

         if ( label != NULL )
            {
            /* Draw label text canvas item */
            x = p_window_data->width/2.0;
            y = 10.0;
            text = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_TEXT,
                                         "text", label,
                                         "x", x,
                                         "y", y,
                                         "anchor", GTK_ANCHOR_NORTH,
                                         "justification", GTK_JUSTIFY_CENTER,
                                         "font-desc", font,
                                         "fill_color_rgba", canvas_fg_color,
                                         NULL);

            /* Update canvas */
            gnome_canvas_update_now((GnomeCanvas*) p_window_data->canvas);

            /* Get label pixbuf from canvas */
            gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
            if ( ipixbuf == 1 )
               pixbuf_xlabel = gdk_pixbuf_get_from_drawable(NULL,
                                                            (GdkDrawable *) p_window_data->canvas->window,
                                                            NULL,
                                                            roundint(x1),
                                                            roundint(y1),
                                                            0,
                                                            0,
                                                            roundint(x2 - x1) + 1,
                                                            roundint(y2 - y1));
            else if ( ipixbuf == 2 )
               pixbuf_ylabel = gdk_pixbuf_get_from_drawable(NULL,
                                                            (GdkDrawable *) p_window_data->canvas->window,
                                                            NULL,
                                                            roundint(x1),
                                                            roundint(y1),
                                                            0,
                                                            0,
                                                            roundint(x2 - x1) + 1,
                                                            roundint(y2 - y1));
            else if ( ipixbuf == 3 )
               pixbuf_zlabel = gdk_pixbuf_get_from_drawable(NULL,
                                                            (GdkDrawable *) p_window_data->canvas->window,
                                                            NULL,
                                                            roundint(x1),
                                                            roundint(y1),
                                                            0,
                                                            0,
                                                            roundint(x2 - x1) + 1,
                                                            roundint(y2 - y1));
            else if ( ipixbuf == 4 )
               pixbuf_title = gdk_pixbuf_get_from_drawable(NULL,
                                                           (GdkDrawable *) p_window_data->canvas->window,
                                                           NULL,
                                                           roundint(x1),
                                                           roundint(y1),
                                                           0,
                                                           0,
                                                           roundint(x2 - x1) + 1,
                                                           roundint(y2 - y1));

            /* Destroy label text item */
            gtk_object_destroy((GtkObject*) text);

            /* Update canvas */
            gnome_canvas_update_now((GnomeCanvas*) p_window_data->canvas);
            }
         }
      }

   return;
   }

