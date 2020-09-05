/*******************************************************************************
*
* DrawAxisLabels.c
*
* Draws plot box, axis labels, and title for two-dimensional plots.
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
#include <string.h>
#include "gsegraf.h"


void DrawAxisLabels ( void )
   {
   /* Declare variables */
   double x1_box, x2_box, y1_box, y2_box, x, y, affine1[6], affine2[6];
   GnomeCanvasItem *rectangle, *pixbuf_item;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Draw plot box rectangle */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 )
      rectangle = gnome_canvas_item_new(group,
                                        GNOME_TYPE_CANVAS_RECT,
                                        "x1", x1_box,
                                        "x2", x2_box,
                                        "y1", y1_box,
                                        "y2", y2_box,
                                        "fill_color_rgba", 0xFFFFFF00,
                                        "outline_color_rgba", canvas_fg_color,
                                        "width_pixels", 2,
                                        NULL);


   /* Draw x-axis label */
   if ( xlabel != NULL && pixbuf_xlabel != NULL )
      {
      x = (x1_box + x2_box)/2.0;
      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
           strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
           strcmp(p_plot_param->x_tick_labels, "on") == 0 )
         y = y2_box + 8.0 + font_size_tick_labels + 8.0;
      else
         y = y2_box + 8.0;
      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_xlabel,
                                          "x", x,
                                          "y", y,
                                          "anchor", GTK_ANCHOR_NORTH,
                                          NULL);
      }


   /* Draw y-axis label */
   if ( ylabel != NULL && pixbuf_ylabel != NULL )
      {
      /* Draw ylabel pixbuf canvas item */
      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_ylabel,
                                          "x", 0.0,
                                          "y", 0.0,
                                          "anchor", GTK_ANCHOR_SOUTH,
                                          NULL);

      /* Rotate and translate ylabel pixbuf canvas item */
      art_affine_rotate(affine1, -90.0);
      gnome_canvas_item_affine_absolute(pixbuf_item, affine1);
      x = -(y1_box + y2_box)/2.0;
      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
           strcmp(p_plot_param->y_tick_marks, "on") == 0 &&
           strcmp(p_plot_param->y_tick_labels, "on") == 0 )
         y = x1_box - 8.0 - width_ytick_labels - 8.0;
      else
         y = x1_box - 8.0;
      art_affine_translate(affine2, x, y);
      gnome_canvas_item_affine_relative(pixbuf_item, affine2);
      }


   /* Draw plot title */
   if ( title != NULL && pixbuf_title != NULL )
      {
      x = (x1_box + x2_box)/2.0;
      y = y1_box - 8.0;
      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_title,
                                          "x", x,
                                          "y", y,
                                          "anchor", GTK_ANCHOR_SOUTH,
                                          NULL);
      }

   return;
   }



   /********************************************************************************
   *
   *   Following rotation of gnome-canvas-text-item does not work.
   *   Text item is translated but not rotated.
   *
   *   if ( ylabel != NULL && pixbuf_ylabel != NULL )
   *      {
   *      GnomeCanvasItem *text_item;
   *      text_item = gnome_canvas_item_new(group,
   *                                        GNOME_TYPE_CANVAS_TEXT,
   *                                        "text", ylabel,
   *                                        "x", 0.0,
   *                                        "y", 0.0,
   *                                        "anchor", GTK_ANCHOR_SOUTH,
   *                                        "justification", GTK_JUSTIFY_CENTER,
   *                                        "font-desc", font_axis_labels,
   *                                        "fill_color_rgba", canvas_fg_color,
   *                                        NULL);
   *
   *      art_affine_rotate(affine1, -90.0);
   *      gnome_canvas_item_affine_absolute(text_item, affine1);
   *      x = -(y1_box + y2_box)/2.0;
   *      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
   *           strcmp(p_plot_param->y_tick_marks, "on") == 0 &&
   *           strcmp(p_plot_param->y_tick_labels, "on") == 0 )
   *         y = x1_box - 8.0 - width_ytick_labels - 8.0;
   *      else
   *         y = x1_box - 8.0;
   *      art_affine_translate(affine2, x, y);
   *      gnome_canvas_item_affine_relative(text_item, affine2);
   *      }
   *
   ********************************************************************************/

   /********************************************************************************
   *
   *   Following creation of gnome-canvas-widget-item does not work.
   *   The gnome-canvas-widget-item is not drawn on the canvas.
   *   Reference: Foundations of GTK+ Development
   *              Andrew Krause
   *              2007, pp. 224 - 227
   *
   *   if ( ylabel != NULL && pixbuf_ylabel != NULL )
   *      {
   *      GtkWidget *textview;
   *      GtkTextBuffer *buffer;
   *      GdkPixmap *pixmap;
   *      GdkRectangle clip_rect = { -1, -1, 0, 0 };
   *      GdkPixbuf *pixbuf;
   *      GnomeCanvasItem *widget_item;
   *      GnomeCanvasItem *pixbuf_item;
   *
   *      textview = gtk_text_view_new();
   *      gtk_widget_modify_font(textview, font_axis_labels);
   *      buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
   *      gtk_text_buffer_set_text(buffer, "abcdef\nghijkl\nmnopqr", -1);
   *
   *      widget_item = gnome_canvas_item_new(group,
   *                                          GNOME_TYPE_CANVAS_WIDGET,
   *                                          "widget", textview,
   *                                          "x", (x1_box + x2_box)/2.0,
   *                                          "y", (y1_box + y2_box)/2.0,
   *                                          "anchor", GTK_ANCHOR_CENTER,
   *                                          NULL);
   *
   *      pixmap = gtk_widget_get_snapshot(textview, &clip_rect);
   *
   * printf("\n   x = %d",(&clip_rect)->x);
   * printf("\n   y = %d",(&clip_rect)->y);
   * printf("\n   width  = %d",(&clip_rect)->width);
   * printf("\n   height = %d",(&clip_rect)->height);
   * printf("\n\n");
   *
   *      pixbuf = gdk_pixbuf_get_from_drawable(NULL,
   *                                            (GdkDrawable *) pixmap,
   *                                            NULL,
   *                                            (&clip_rect)->x,
   *                                            (&clip_rect)->y,
   *                                            0,
   *                                            0,
   *                                            (&clip_rect)->width,
   *                                            (&clip_rect)->height);
   *
   *      pixbuf_item = gnome_canvas_item_new(group,
   *                                          GNOME_TYPE_CANVAS_PIXBUF,
   *                                          "pixbuf", pixbuf,
   *                                          "x", (x1_box + x2_box)/2.0,
   *                                          "y", (y1_box + y2_box)/2.0,
   *                                          "anchor", GTK_ANCHOR_CENTER,
   *                                          NULL);
   *      }
   *
   ********************************************************************************/
