/*******************************************************************************
*
* DrawText.c
*
* Contains functions:
*    DrawText
*    background_text
*
* Draws text.
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


void DrawText ( void )
   {
   /* Declare variables */
   int i, nlines, index, text_anchor;
   unsigned int i1_str, i2_str, size, nchar;
   long int file_position, file_position_1;
   double x1_box, x2_box, y1_box, y2_box, height_lines,
          xtext, ytext, ztext, xanchor, yanchor,
          plot_coords[3], window_coords[2], x1, y1, x2, y2;
   char *string, *text_str = NULL, anchor[10],
        text_coords_flag[4];
   GnomeCanvasItem *text;
   FILE *fptr;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Draw text */
   fptr = fopen(p_param_file, "r");
   file_position = ftell(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get number of text lines */
      nlines = 0;
      if ( strncmp(line, "text_string", 11) == 0 )
         {
         nlines++;
         file_position_1 = file_position;
         while ( fgets(line, maxline, fptr) != NULL )
            {
            if ( strncmp(line, "text_string", 11) == 0 )
               nlines++;
            else
               break;
            }

         /* Get text-line sizes */
         fseek(fptr, file_position_1, SEEK_SET);
         nchar = 0;
         for ( i=1; i<= nlines; i++ )
            {
            fgets(line, maxline, fptr);
            size = strlen(&line[11]);
            if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 1)) != NULL )
               nchar = nchar + size + 1;
            }

         /* Read text lines */
         fseek(fptr, file_position_1, SEEK_SET);
         text_str = NULL;
         text_str = xmalloc(nchar);
         memset(text_str, 0, sizeof(text_str));
         index = 0;
         for ( i=1; i<= nlines; i++ )
            {
            fgets(line, maxline, fptr);
            size = strlen(&line[11]);
            if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 1)) != NULL )
               {
               strcpy(&text_str[index], string);
               index = index + size + 1;
               text_str[index-1] = '\n';
               }
            }
         text_str[index-1] = '\0';

         /* Get text coordinates and anchor */
         memset(text_coords_flag, 0, sizeof(text_coords_flag));
         for ( i=1; i<=2; i++ )
            if ( fgets(line, maxline, fptr) != NULL )
               {
               if ( strncmp(line, "text_coords_abs", 15) == 0 ||
                    strncmp(line, "text_coords_rel", 15) == 0 )
                  {
                  if ( (strcmp(p_plot_param->axis_type, "3d") != 0 &&
                        strncmp(line, "text_coords_abs", 15) == 0 &&
                        sscanf(&line[15], "%lf %lf", &xtext, &ytext) == 2) ||
                       (strcmp(p_plot_param->axis_type, "3d") == 0 &&
                        strncmp(line, "text_coords_abs", 15) == 0 &&
                        sscanf(&line[15], "%lf %lf %lf", &xtext, &ytext, &ztext) == 3) ||
                       (strncmp(line, "text_coords_rel", 15) == 0 &&
                        sscanf(&line[15], "%lf %lf", &xtext, &ytext) == 2) )
                     strncpy(text_coords_flag, &line[12], 3);

                  else
                     {
                     size = strlen("Invalid or incomplete text coordinates for text:\n") +
                            strlen(text_str);
                     string = xmalloc(size + 1);
                     sprintf(string, "%s%s",
                             "Invalid or incomplete text coordinates for text:\n",
                             text_str);
                     ErrorDialog(string);
                     free(string);
                     free(text_str);
                     exit(1);
                     }
                  }

               else if ( strncmp(line, "text_anchor", 11) == 0 )
                  {
                  memset(anchor, 0, sizeof(anchor));
                  size = strlen(&line[11]);
                  if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 0)) != NULL )
                     strncpy(&anchor[0], string, 9);
                  else
                     {
                     size = strlen("Text anchor not found for text:\n") +
                            strlen(text_str);
                     string = xmalloc(size + 1);
                     sprintf(string, "%s%s",
                             "Text anchor not found for text:\n", text_str);
                     ErrorDialog(string);
                     free(string);
                     free(text_str);
                     exit(1);
                     }
                  }

               else
                  {
                  size = strlen("Text coordinates or text anchor not found for text:\n") +
                                strlen(text_str);
                  string = xmalloc(size + 1);
                  sprintf(string, "%s%s",
                          "Text coordinates or text anchor not found for text:\n", text_str);
                  ErrorDialog(string);
                  free(string);
                  free(text_str);
                  exit(1);
                  }
               }

         /* Define text anchor */
         if ( strcmp(anchor, "center") == 0 )
            text_anchor = GTK_ANCHOR_CENTER;
         else if ( strcmp(anchor, "north") == 0 )
            text_anchor = GTK_ANCHOR_NORTH;
         else if ( strcmp(anchor, "northeast") == 0 )
            text_anchor = GTK_ANCHOR_NORTH_EAST;
         else if ( strcmp(anchor, "east") == 0 )
            text_anchor = GTK_ANCHOR_EAST;
         else if ( strcmp(anchor, "southeast") == 0 )
            text_anchor = GTK_ANCHOR_SOUTH_EAST;
         else if ( strcmp(anchor, "south") == 0 )
            text_anchor = GTK_ANCHOR_SOUTH;
         else if (strcmp(anchor, "southwest") == 0 )
            text_anchor = GTK_ANCHOR_SOUTH_WEST;
         else if ( strcmp(anchor, "west") == 0 )
            text_anchor = GTK_ANCHOR_WEST;
         else if ( strcmp(anchor, "northwest") == 0 )
            text_anchor = GTK_ANCHOR_NORTH_WEST;
         else
            {
            size = strlen("Invalid text anchor for text:\n") +
                   strlen(text_str);
            string = xmalloc(size + 1);
            sprintf(string, "%s%s",
                    "Invalid text anchor for text:\n", text_str);
            ErrorDialog(string);
            free(string);
            free(text_str);
            exit(1);
            }

         /* Draw text */
         if ( text_str != NULL )
            {
            if ( strcmp(text_coords_flag, "abs") == 0 )
               {
               if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
                  {
                  plot_coords[0] = xtext;
                  plot_coords[1] = ytext;
                  plot_coords[2] = ztext;
                  }
               else
                  {
                  plot_coords[0] = xtext;
                  plot_coords[1] = ytext;
                  }
               GetWindowCoords(plot_coords, window_coords);
               xanchor = window_coords[0];
               yanchor = window_coords[1];
               }
            else if ( strcmp(text_coords_flag, "rel") == 0 )
               {
               xanchor = (1.0 - xtext)*x1_box + xtext*x2_box;
               yanchor = (1.0 - ytext)*y2_box + ytext*y1_box;
               }

            /* Check text is within plot box for absolute coordinates */
            if ( strcmp(text_coords_flag, "abs") == 0 &&
                 (xanchor < 0 || yanchor < 0) )
               continue;

            text = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_TEXT,
                                         "text", text_str,
                                         "x", xanchor,
                                         "y", yanchor,
                                         "anchor", text_anchor,
                                         "font-desc", font_text,
                                         "fill_color_rgba", canvas_fg_color,
                                         NULL);

            /* Draw text background rectangles */
            gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
            height_lines = (y2 - y1)/nlines;
            background_text(text_str, height_lines, x1, y1);
            gnome_canvas_item_raise_to_top(text);
            }

         free(text_str);
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;

      else
         file_position = ftell(fptr);
      }

   fclose(fptr);

   return;
   }



void background_text ( char *text_str, double height_lines, double x1, double y1 )
   {
   /* Declare variables */
   int i, i1, nline;
   double x, y, x1rec, y1rec, x2rec, y2rec,
          x11, y11, x22, y22;
   char ch;
   GnomeCanvasItem *text, *rectangle;


   /* Get lengths of text lines and draw text background */
   x = p_window_data->width/2.0;
   y = 10.0;
   i = 0;
   i1 = 0;
   nline = 0;
   while ( 1 )
      {
      if ( (ch = text_str[i]) == '\n' || (ch = text_str[i]) == '\0' )
         {
         nline++;
         text_str[i] = '\0';

         if ( strlen(&text_str[i1]) > 0 )
            {
            text = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_TEXT,
                                         "text", &text_str[i1],
                                         "x", x,
                                         "y", y,
                                         "anchor", GTK_ANCHOR_NORTH,
                                         "font-desc", font_text,
                                         "fill_color_rgba", 0xFFFFFF00,
                                         NULL);
            gnome_canvas_item_get_bounds(text, &x1rec, &y1rec, &x2rec, &y2rec);
            gtk_object_destroy((GtkObject*) text);

            x11 = x1 - 1.0;
            x22 = x1 + x2rec - x1rec + 1.0;
            y11 = y1 + (nline - 1)*height_lines;
            y22 = y11 + height_lines + 1.0;
            rectangle = gnome_canvas_item_new(group,
                                              GNOME_TYPE_CANVAS_RECT,
                                              "x1", x11,
                                              "x2", x22,
                                              "y1", y11,
                                              "y2", y22,
                                              "fill_color_rgba", canvas_bg_color,
                                              "outline_color_rgba", canvas_bg_color,
                                              "width_pixels", 1,
                                              NULL);
            }

         if ( ch == '\0' )
            return;

         i++;
         i1 = i;
         }

      else
         i++;
      }
   }
