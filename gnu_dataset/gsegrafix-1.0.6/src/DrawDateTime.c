/*******************************************************************************
*
* DrawDateTime.c
*
* Draws date-time string.
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


void DrawDateTime ( void )
   {
   /* Declare variables */
   int x1, x2, y1, y2, anchor;
   double xposition, yposition;
   const char *error_str[] =
      { "Invalid date_time location." };
   GnomeCanvasItem *text;


   if ( strcmp(p_plot_param->date_time_anchor, "off") == 0 )
      return;


   /* Specify window minimum and maximum values */
   x1 = p_window_data->x;
   x2 = p_window_data->width;
   y1 = p_window_data->y;
   y2 = p_window_data->height - height_menu_bar;


   /* Define anchor and text position */
   if ( strcmp(p_plot_param->date_time_anchor, "north") == 0 )
      {
      anchor = GTK_ANCHOR_NORTH;
      xposition = (x1 + x2)/2.0;
      yposition = y1 + 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "northeast") == 0 )
      {
      anchor = GTK_ANCHOR_NORTH_EAST;
      xposition = x2 - 2.0;
      yposition = y1 + 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "southeast") == 0 )
      {
      anchor = GTK_ANCHOR_SOUTH_EAST;
      xposition = x2 - 2.0;
      yposition = y2 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "south") == 0 )
      {
      anchor = GTK_ANCHOR_SOUTH;
      xposition = (x1 + x2)/2.0;
      yposition = y2 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "southwest") == 0 )
      {
      anchor = GTK_ANCHOR_SOUTH_WEST;
      xposition = x1 + 2.0;
      yposition = y2 - 2.0;
      }
   else if ( strcmp(p_plot_param->date_time_anchor, "northwest") == 0 )
      {
      anchor = GTK_ANCHOR_NORTH_WEST;
      xposition = x1 + 2.0;
      yposition = y1 + 2.0;
      }
   else
      {
      ErrorDialog(error_str[0]);
      exit(1);
      }


   /* Draw text */
   text = gnome_canvas_item_new(group,
                                GNOME_TYPE_CANVAS_TEXT,
                                "text", date_time,
                                "x", xposition,
                                "y", yposition,
                                "anchor", anchor,
                                "font-desc", font_date_time,
                                "fill_color_rgba", canvas_fg_color,
                                NULL);

   return;
   }
