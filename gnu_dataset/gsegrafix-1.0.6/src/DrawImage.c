/*******************************************************************************
*
* DrawImage.c
*
* Draws image from image file.
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


void DrawImage ( void )
   {
   /* Declare variables */
   int i, image_anchor;
   unsigned int i1_str, i2_str, size;
   double x1_box, x2_box, y1_box, y2_box, ximage, yimage, zimage, xanchor, yanchor,
          plot_coords[3], window_coords[2];
   char *image_file=NULL, *string, anchor[10],
        image_coords_flag[4];
   GdkPixbuf *image;
   GnomeCanvasItem *image_item;
   FILE *fptr, *fptr_image;
 

   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get image data */
   fptr = fopen(p_param_file, "r");
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get image filename */
      if ( strncmp(line, "image_filename", 14) == 0 )
         {
         size = strlen(&line[14]);
         if ( (string = get_string(&line[14], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            image_file = NULL;
            image_file = xmalloc(size + 1);
            strcpy(image_file, string);
            if ( (fptr_image = fopen(image_file, "r")) != NULL )
               fclose(fptr_image);
            else
               {
               size = strlen("Cannot open image file:\n") + strlen(image_file);
               string = xmalloc(size + 1);
               sprintf(string, "%s%s", "Cannot open image file:\n", image_file);
               ErrorDialog(string);
               free(string);
               free(image_file);
               exit(1);
               }
            }

         /* Get image coordinates and anchor */
         memset(image_coords_flag, 0, sizeof(image_coords_flag));
         for ( i=1; i<=2; i++ )
            if ( fgets(line, maxline, fptr) != NULL )
               {
               if ( strncmp(line, "image_coords_abs", 16) == 0 ||
                    strncmp(line, "image_coords_rel", 16) == 0 )
                  {
                  if ( (strcmp(p_plot_param->axis_type, "3d") != 0 &&
                        strncmp(line, "image_coords_abs", 16) == 0 &&
                        sscanf(&line[16], "%lf %lf", &ximage, &yimage) == 2) ||
                       (strcmp(p_plot_param->axis_type, "3d") == 0 &&
                        strncmp(line, "image_coords_abs", 16) == 0 &&
                        sscanf(&line[16], "%lf %lf %lf", &ximage, &yimage, &zimage) == 3) ||
                       (strncmp(line, "image_coords_rel", 16) == 0 &&
                        sscanf(&line[16], "%lf %lf", &ximage, &yimage) == 2) )
                     strncpy(image_coords_flag, &line[13], 3);

                  else
                     {
                     size = strlen("Invalid or incomplete image coordinates for image file:\n") +
                            strlen(image_file);
                     string = xmalloc(size + 1);
                     sprintf(string, "%s%s",
                             "Invalid or incomplete image coordinates for image file:\n",
                             image_file);
                     ErrorDialog(string);
                     free(string);
                     free(image_file);
                     exit(1);
                     }
                  }

               else if ( strncmp(line, "image_anchor", 12) == 0 )
                  {
                  memset(anchor, 0, sizeof(anchor));
                  size = strlen(&line[12]);
                  if ( (string = get_string(&line[12], &i1_str, &i2_str, &size, 0)) != NULL )
                     strncpy(&anchor[0], string, 9);
                  else
                     {
                     size = strlen("Image anchor not found for image file:\n") +
                                   strlen(image_file);
                     string = xmalloc(size + 1);
                     sprintf(string, "%s%s",
                             "Image anchor not found for image file:\n", image_file);
                     ErrorDialog(string);
                     free(string);
                     free(image_file);
                     exit(1);
                     }
                  }

               else
                  {
                  size = strlen("Image coordinates or image anchor not found for image file:\n") +
                                strlen(image_file);
                  string = xmalloc(size + 1);
                  sprintf(string, "%s%s",
                          "Image coordinates or image anchor not found for image file:\n", image_file);
                  ErrorDialog(string);
                  free(string);
                  free(image_file);
                  exit(1);
                  }
               }

         /* Define image anchor */
         if ( strcmp(anchor, "center") == 0 )
            image_anchor = GTK_ANCHOR_CENTER;
         else if ( strcmp(anchor, "north") == 0 )
            image_anchor = GTK_ANCHOR_NORTH;
         else if ( strcmp(anchor, "northeast") == 0 )
            image_anchor = GTK_ANCHOR_NORTH_EAST;
         else if ( strcmp(anchor, "east") == 0 )
            image_anchor = GTK_ANCHOR_EAST;
         else if ( strcmp(anchor, "southeast") == 0 )
            image_anchor = GTK_ANCHOR_SOUTH_EAST;
         else if ( strcmp(anchor, "south") == 0 )
            image_anchor = GTK_ANCHOR_SOUTH;
         else if (strcmp(anchor, "southwest") == 0 )
            image_anchor = GTK_ANCHOR_SOUTH_WEST;
         else if ( strcmp(anchor, "west") == 0 )
            image_anchor = GTK_ANCHOR_WEST;
         else if ( strcmp(anchor, "northwest") == 0 )
            image_anchor = GTK_ANCHOR_NORTH_WEST;
         else
            {
            size = strlen("Invalid image anchor for image file:\n") +
                   strlen(image_file);
            string = xmalloc(size + 1);
            sprintf(string, "%s%s",
                    "Invalid image anchor for image file:\n", image_file);
            ErrorDialog(string);
            free(string);
            free(image_file);
            exit(1);
            }

         /* Draw image */
         if ( image_file != NULL )
            {
            if ( strcmp(image_coords_flag, "abs") == 0 )
               {
               if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
                  {
                  plot_coords[0] = ximage;
                  plot_coords[1] = yimage;
                  plot_coords[2] = zimage;
                  }
               else
                  {
                  plot_coords[0] = ximage;
                  plot_coords[1] = yimage;
                  }
               GetWindowCoords(plot_coords, window_coords);
               xanchor = window_coords[0];
               yanchor = window_coords[1];
               }
            else if ( strcmp(image_coords_flag, "rel") == 0 )
               {
               xanchor = (1.0 - ximage)*x1_box + ximage*x2_box;
               yanchor = (1.0 - yimage)*y2_box + yimage*y1_box;
               }

            /* Check image is within plot box for absolute coordinates */
            if ( strcmp(image_coords_flag, "abs") == 0 &&
                 (xanchor < 0 || yanchor < 0) )
               continue;

            image = gdk_pixbuf_new_from_file(image_file, NULL);
            image_item = gnome_canvas_item_new(group,
                                               GNOME_TYPE_CANVAS_PIXBUF,
                                               "pixbuf", image,
                                               "x", xanchor,
                                               "y", yanchor,
                                               "anchor", image_anchor,
                                               NULL);
            gdk_pixbuf_unref(image);
            }

         free(image_file);
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   fclose(fptr);

   return;
   }
