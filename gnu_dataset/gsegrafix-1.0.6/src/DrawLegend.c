/*******************************************************************************
*
* DrawLegend.c
*
* Contains functions:
*    DrawLegend
*    background_legend
*
* Draws legend.
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


void DrawLegend ( void )
   {
   /* Declare variables */
   int i, iplot, ifunc, nplots, nlines, index_plot_types, imesh, icontour, index, anchor_text;
   unsigned int i1_str, i2_str, size, nchar;
   long int file_position, file_position_1;
   double x, y, x1, y1, x2, y2, x1_box, y1_box, x2_box, y2_box,
          xlegend, ylegend, zlegend, xanchor, yanchor,
          xanchor_text, yanchor_text, xanchor_symbol, yanchor_symbol,
          width_text, height_text, width_legend, height_legend,
          height_lines, dx_text, dx_symbol, dy_symbol,
          yinc1, yinc2, plot_coords[3], window_coords[2];
   double dy_bar[] = { 0.0, 6.0, 0.0 };
   char *string, *legend_str=NULL, anchor[10], *pchar,
        legend_coords_flag[4];
   extern char symbol_string1[];   /* symbol-specification characters "cCtTsSiIpPhH" */
   extern char symbol_string2[];   /* symbol-specification characters "+xra" */
   extern char color_string[];     /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Invalid or incomplete legend coordinates.",
        "Legend anchor not found.",
        "Legend coordinates or legend anchor not found.",
        "Invalid legend anchor." };
   GnomeCanvasPoints *points;
   GnomeCanvasItem *text;
   FILE *fptr;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get number of legend lines */
   nlines = 0;
   fptr = fopen(p_param_file, "r");
   file_position = ftell(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "legend_string", 13) == 0 )
         {
         nlines++;
         file_position_1 = file_position;
         while ( fgets(line, maxline, fptr) != NULL )
            {
            if ( strncmp(line, "legend_string", 13) == 0 )
               nlines++;
            else
               break;
            }
         break;
         }
      else if ( strncmp(line, "#####", 5) == 0 )
          break;
      else
         file_position = ftell(fptr);
      }
   if ( nlines == 0 )
      return;


   /* Get legend-line sizes */
   fseek(fptr, file_position_1, SEEK_SET);
   nchar = 0;
   for ( i=1; i<= nlines; i++ )
      {
      fgets(line, maxline, fptr);
      size = strlen(&line[13]);
      if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 1)) != NULL )
         nchar = nchar + size + 1;
      }


   /* Read legend lines */
   fseek(fptr, file_position_1, SEEK_SET);
   legend_str = xmalloc(nchar);
   memset(legend_str, 0, sizeof(legend_str));
   index = 0;
   for ( i=1; i<= nlines; i++ )
      {
      fgets(line, maxline, fptr);
      size = strlen(&line[13]);
      if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 1)) != NULL )
         {
         strcpy(&legend_str[index], string);
         index = index + size + 1;
         legend_str[index-1] = '\n';
         }
      }
   legend_str[index-1] = '\0';


   /* Get legend coordinates and anchor */
   memset(legend_coords_flag, 0, sizeof(legend_coords_flag));
   for ( i=1; i<=2; i++ )
      {
      if ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "legend_coords_abs", 17) == 0 ||
              strncmp(line, "legend_coords_rel", 17) == 0 )
            {
            if ( (strcmp(p_plot_param->axis_type, "3d") != 0 &&
                  strncmp(line, "legend_coords_abs", 17) == 0 &&
                  sscanf(&line[17], "%lf %lf", &xlegend, &ylegend) == 2) ||
                 (strcmp(p_plot_param->axis_type, "3d") == 0 &&
                  strncmp(line, "legend_coords_abs", 17) == 0 &&
                  sscanf(&line[17], "%lf %lf %lf", &xlegend, &ylegend, &zlegend) == 3) ||
                 (strncmp(line, "legend_coords_rel", 17) == 0 &&
                  sscanf(&line[17], "%lf %lf", &xlegend, &ylegend) == 2) )
               strncpy(legend_coords_flag, &line[14], 3);

            else
               {
               ErrorDialog(error_str[0]);
               free(legend_str);
               fclose(fptr);
               exit(1);
               }
            }

         else if ( strncmp(line, "legend_anchor", 13) == 0 )
            {
            memset(anchor, 0, sizeof(anchor));
            size = strlen(&line[13]);
            if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 0)) != NULL )
               strncpy(&anchor[0], string, 9);
            else
               {
               ErrorDialog(error_str[1]);
               free(legend_str);
               fclose(fptr);
               exit(1);
               }
            }

         else
            {
            ErrorDialog(error_str[2]);
            free(legend_str);
            fclose(fptr);
            exit(1);
            }
         }
      }


   /* Specify legend text parent group and font properties */
   text = gnome_canvas_item_new(group,
                                GNOME_TYPE_CANVAS_TEXT,
                                "text", legend_str,
                                "font-desc", font_legend,
                                "fill_color_rgba", canvas_fg_color,
                                NULL);


   /* Calculate legend coordinates and draw legend text */
   gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
   width_text = x2 - x1;
   height_text = y2 - y1;
   width_legend = 70.0 + width_text;
   height_legend = height_text;
   if ( strcmp(anchor, "center") == 0 )
      {
      anchor_text = GTK_ANCHOR_CENTER;
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = -height_legend/2.0;
      }
   else if ( strcmp(anchor, "north") == 0 )
      {
      anchor_text = GTK_ANCHOR_NORTH;
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = 0.0;
      }
   else if ( strcmp(anchor, "northeast") == 0 )
      {
      anchor_text = GTK_ANCHOR_NORTH_EAST;
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = 0.0;
      }
   else if ( strcmp(anchor, "east") == 0 )
      {
      anchor_text = GTK_ANCHOR_EAST;
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = -height_legend/2.0;
      }
   else if ( strcmp(anchor, "southeast") == 0 )
      {
      anchor_text = GTK_ANCHOR_SOUTH_EAST;
      dx_text = 0.0;
      dx_symbol = -width_legend;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor, "south") == 0 )
      {
      anchor_text = GTK_ANCHOR_SOUTH;
      dx_text = (width_legend - width_text)/2.0;
      dx_symbol = -width_legend/2.0;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor, "southwest") == 0 )
      {
      anchor_text = GTK_ANCHOR_SOUTH_WEST;
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = -height_legend;
      }
   else if ( strcmp(anchor, "west") == 0 )
      {
      anchor_text = GTK_ANCHOR_WEST;
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = -height_legend/2.0;
      }
   else if ( strcmp(anchor, "northwest") == 0 )
      {
      anchor_text = GTK_ANCHOR_NORTH_WEST;
      dx_text = width_legend - width_text;
      dx_symbol = 0.0;
      dy_symbol = 0.0;
      }
   else
      {
      ErrorDialog(error_str[3]);
      free(legend_str);
      fclose(fptr);
      exit(1);
      }

   if ( strcmp(legend_coords_flag, "abs") == 0 )
      {
      if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         plot_coords[0] = xlegend;
         plot_coords[1] = ylegend;
         plot_coords[2] = zlegend;
         }
      else
         {
         plot_coords[0] = xlegend;
         plot_coords[1] = ylegend;
         }
      GetWindowCoords(plot_coords, window_coords);
      xanchor = window_coords[0];
      yanchor = window_coords[1];
      }
   else if ( strcmp(legend_coords_flag, "rel") == 0 )
      {
      xanchor = (1.0 - xlegend)*x1_box + xlegend*x2_box;
      yanchor = (1.0 - ylegend)*y2_box + ylegend*y1_box;
      }


   /* Check legend is within plot box for absolute coordinates */
   if ( strcmp(legend_coords_flag, "abs") == 0 &&
        (xanchor < 0 || yanchor < 0) )
      {
      free(legend_str);
      fclose(fptr);
      return;
      }

   xanchor_text = xanchor + dx_text;
   yanchor_text = yanchor;
   gnome_canvas_item_set(text,
                         "x", xanchor_text,
                         "y", yanchor_text,
                         "anchor", anchor_text,
                         NULL);


   /* Draw legend background rectangles */
   gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
   height_lines = (y2 - y1)/nlines;
   background_legend(legend_str, height_lines, x1 - 70.0, y1);
   gnome_canvas_item_raise_to_top(text);


   /* Specify legend symbol coordinate parameters */
   xanchor_symbol = xanchor + dx_symbol;
   yanchor_symbol = yanchor + dy_symbol;
   yinc1 = height_legend/nlines;
   yinc2 = yinc1/2.0;


   /* Draw symbols */
   nplots = p_plot_param->nplots;
   index_plot_types = 0;
   imesh = 0;
   icontour = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
           strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         /* Draw line */
         if ( stylechar1[iplot-1] == 'l' )
            {
            points = gnome_canvas_points_new(2);
            points->coords[0] = xanchor_symbol;
            points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            points->coords[2] = xanchor_symbol + 60.0;
            points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            DrawLine(points, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
            gnome_canvas_points_unref(points);
            }


         /* Draw dashed line */
         else if ( stylechar1[iplot-1] == 'd' )
            {
            points = gnome_canvas_points_new(2);
            points->coords[0] = xanchor_symbol;
            points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            points->coords[2] = xanchor_symbol + 60.0;
            points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            DrawDashedLine(points, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
            gnome_canvas_points_unref(points);
            }


         /* Draw dotted line */
         else if ( stylechar1[iplot-1] == '.' )
            {
            points = gnome_canvas_points_new(2);
            points->coords[0] = xanchor_symbol;
            points->coords[1] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            points->coords[2] = xanchor_symbol + 60.0;
            points->coords[3] = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
            DrawDottedLine(points, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
            gnome_canvas_points_unref(points);
            }


         /* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
         else if ( (pchar = strchr(symbol_string1, stylechar1[iplot-1])) != NULL )
            {
            ifunc = pchar - symbol_string1;
            for ( i=1; i<=3; i++ )
               {
               x = xanchor_symbol + 10.0 + (i-1)*20.0;
               y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
               symbol_func1[ifunc](x, y, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1],
                                   stylesizes[iplot-1]);
               }
            }


         /* Draw symbols in symbol_string2 ("+xra") */
         else if ( (pchar = strchr(symbol_string2, stylechar1[iplot-1])) != NULL )
            {
            ifunc = pchar - symbol_string2;
            for ( i=1; i<=3; i++ )
               {
               x = xanchor_symbol + 10.0 + (i-1)*20.0;
               y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
               symbol_func2[ifunc](x, y, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
               }
            }


         /* Draw bars */
         else if ( stylechar1[iplot-1] == 'b' || stylechar1[iplot-1] == 'B' )
            {
            for ( i=1; i<=3; i++ )
               {
               x = xanchor_symbol + 10.0 + (i-1)*20.0;
               y = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

               x1 = x - 10.0;
               x2 = x + 10.0;
               y1 = y + 6.0;
               y2 = y - dy_bar[i-1];

               DrawBar(x1+1.0, y1, x2-1.0, y2, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1]);
               DrawBar(x1, y1+1.0, x2, y2-1.0, 0xFFFFFF00, canvas_bg_color);
               }
            }
         }


      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         imesh++;
         x1 = xanchor_symbol;
         y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         x2 = xanchor_symbol + 60.0;
         y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

         if ( styleflags[iplot-1] == 2 || styleflags[iplot-1] == 4 )
            {
            if ( (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )
               {
               index = pchar - &color_string[0];
               DrawMesh(x1, y1, x2, y2, color_rgba[index], meshcolors[imesh-1], styleflags[iplot-1]);
               }
            }

         else if ( styleflags[iplot-1] == 5 || styleflags[iplot-1] == 6 )
            DrawMesh(x1, y1, x2, y2, stylecolor1[iplot-1], meshcolors[imesh-1], styleflags[iplot-1]);

         else if ( styleflags[iplot-1] == 7 )
            DrawMesh(x1, y1, x2, y2, alphacolor[iplot-1], meshcolors[imesh-1], styleflags[iplot-1]);
         }


      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;
         x1 = xanchor_symbol;
         y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         x2 = xanchor_symbol + 60.0;
         y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;

         if ( styleflags[iplot-1] == 1 )
            {
            /* 2d contour plot */
            if ( (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )
               {
               index = pchar - &color_string[0];
               DrawContour(x1, y1, x2, y2, color_rgba[index], 0xFFFFFF00, styleflags[iplot-1]);
               }
            }

         else if ( styleflags[iplot-1] == 3 )
            {
            /* 2d contour plot */
            DrawContour(x1, y1, x2, y2, stylecolor1[iplot-1], 0xFFFFFF00, styleflags[iplot-1]);
            }

         else if ( styleflags[iplot-1] == 7 )
            {
            /* 2d contour plot */
            DrawContour(x1, y1, x2, y2, color_rgba[3], 0xFFFFFF00, styleflags[iplot-1]);
            }

         else if ( styleflags[iplot-1] == 2 || styleflags[iplot-1] == 4 )
            {
            /* 3d contour plot */
            if ( (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )
               {
               index = pchar - &color_string[0];
               DrawContour(x1, y1, x2, y2, color_rgba[index], contourcolors[icontour-1], styleflags[iplot-1]);
               }
            }

         else if ( styleflags[iplot-1] == 5 || styleflags[iplot-1] == 6 )
            {
            /* 3d contour plot */
            DrawContour(x1, y1, x2, y2, stylecolor1[iplot-1], contourcolors[icontour-1], styleflags[iplot-1]);
            }
         }


      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         x1 = xanchor_symbol;
         y1 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         x2 = xanchor_symbol + 60.0;
         y2 = yanchor_symbol + yinc1*(iplot - 1) + yinc2;
         DrawColorPlot(x1, y1, x2, y2);                                            /* 2d color plot */
         }


      /* Increment indices */
      index_plot_types = index_plot_types + 10;
      }

   free(legend_str);
   fclose(fptr);

   return;
   }



void background_legend ( char *legend_str, double height_lines, double x1, double y1 )
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
      if ( (ch = legend_str[i]) == '\n' || (ch = legend_str[i]) == '\0' )
         {
         nline++;
         legend_str[i] = '\0';

         if ( strlen(&legend_str[i1]) > 0 )
            {
            text = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_TEXT,
                                         "text", &legend_str[i1],
                                         "x", x,
                                         "y", y,
                                         "anchor", GTK_ANCHOR_NORTH,
                                         "font-desc", font_legend,
                                         "fill_color_rgba", 0xFFFFFF00,
                                         NULL);
            gnome_canvas_item_get_bounds(text, &x1rec, &y1rec, &x2rec, &y2rec);
            gtk_object_destroy((GtkObject*) text);

            x11 = x1 - 1.0;
            x22 = x1 + 70.0 + x2rec - x1rec + 1.0;
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
