/*******************************************************************************
*
* PlotRectangles.c
*
* Plots rectangles.
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


void PlotRectangles ( void )
   {
   /* Declare variables */
   int i, nx, ny;
   unsigned int index, line_width;
   guint32 line_color;
   double pi, deg2rad, x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, xscale, yscale,
          x0, y0, width, height, angle, a, b,
          R[4], x[5], y[5];
   char *pchar, line_char, color_char;
   extern char color_string[];   /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Invalid or missing rectangle coordinates.",
        "Invalid rectangle color character.",
        "Invalid or missing rectangle line or color specification.",
        "Invalid rectangle line character." };
   FILE *fptr;


   /* Return if no rectangles to plot */
   fptr = fopen(p_param_file, "r");
   i = 0;
   while ( fgets(line, maxline, fptr) != NULL )
      if ( strncmp(line, "rect_coords", 11) == 0 )
         i++;
   fclose(fptr);
   if ( i == 0 )
      return;


   /* Define constants */
   pi = 4.0*atan(1.0);
   deg2rad = pi/180.0;


   /* Get plot box minimum and maximum values */
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      x1_box = p_plot_box_data->xmin;
      x2_box = p_plot_box_data->xmax;
      y1_box = p_plot_box_data->ymin;
      y2_box = p_plot_box_data->ymax;
      }


   /* Get minimum and maximum axis values */
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      ny = p_ticklabels->nyvalues;
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      }


   /* Calculate axis scale factors */
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      xscale = (x2_box - x1_box)/(xmax - xmin);
      yscale = (y2_box - y1_box)/(ymax - ymin);
      }


   /* Draw rectangles */
   fptr = fopen(p_param_file, "r");
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get rectangle shape parameters */
      if ( strncmp(line, "rect_coords", 11) == 0 )
         {
         if ( sscanf(&line[11], "%lf %lf %lf %lf %lf",
                     &x0, &y0, &width, &height, &angle) != 5 )
            {
            ErrorDialog(error_str[0]);
            exit(1);
            }

         line_width = 1;
         if ( fgets(line, maxline, fptr) != NULL )
            if ( strncmp(line, "rect_style", 10) == 0 )
               {
               if ( sscanf(&line[10], " %c 0x%x", &line_char, (unsigned int *) &line_color) == 2 ||
                    sscanf(&line[10], " %c 0X%x", &line_char, (unsigned int *) &line_color) == 2 )
                  sscanf(&line[10], " %*c %*s %u", &line_width);

               else if ( sscanf(&line[10], " %c %c", &line_char, &color_char) == 2 )
                  {
                  if ( (pchar = strchr(color_string, color_char)) == NULL )
                     {
                     ErrorDialog(error_str[1]);
                     exit(1);
                     }

                  else
                     {
                     index = pchar - &color_string[0];   /* get index to color character */
                     line_color = color_rgba[index];
                     sscanf(&line[13], " %*c %*c %u", &line_width);
                     }
                  }

               else
                  {
                  ErrorDialog(error_str[2]);
                  exit(1);
                  }

               /* Check line character */
               if ( line_char != 'l' && line_char != 'd' && line_char != '.' )
                  {
                  ErrorDialog(error_str[3]);
                  exit(1);
                  }
               }

         /* Calculate rectangle centered at offset point */
         a = width/2.0;    /* half long dimension */
         b = height/2.0;   /* half short dimension */

         if ( angle == 0.0 )
            {
            /* Calculate unrotated rectangle */
            x[0] = x0 - a;
            y[0] = y0 - b;
            x[1] = x0 + a;
            y[1] = y0 - b;
            x[2] = x0 + a;
            y[2] = y0 + b;
            x[3] = x0 - a;
            y[3] = y0 + b;
            x[4] = x[0];
            y[4] = y[0];
            }
         else
            {
            /* Calculate rotation matrix elements; */
            /* R elements labeled: [ 0 1; 2 3 ]    */
            angle = angle*deg2rad;
            R[0] =  cos(angle);
            R[1] =  sin(angle);
            R[2] = -sin(angle);
            R[3] =  cos(angle);

            /* Calculate rotated rectangle */
            x[0] = x0 + (- R[0]*a - R[1]*b);
            y[0] = y0 - (- R[2]*a - R[3]*b);
            x[1] = x0 + (+ R[0]*a - R[1]*b);
            y[1] = y0 - (+ R[2]*a - R[3]*b);
            x[2] = x0 + (+ R[0]*a + R[1]*b);
            y[2] = y0 - (+ R[2]*a + R[3]*b);
            x[3] = x0 + (- R[0]*a + R[1]*b);
            y[3] = y0 - (- R[2]*a + R[3]*b);
            x[4] = x[0];
            y[4] = y[0];
            }

         /* Draw rectangle */
         if ( line_char == 'l' )
            DrawLines2d(5, &x[0],  &y[0], xmin, xmax, ymin, ymax, xscale, yscale,
                        line_color, line_width, "solid");
         else if ( line_char == 'd' )
            DrawLines2d(5, &x[0],  &y[0], xmin, xmax, ymin, ymax, xscale, yscale,
                        line_color, line_width, "dashed");
         else if ( line_char == '.' )
            DrawLines2d(5, &x[0],  &y[0], xmin, xmax, ymin, ymax, xscale, yscale,
                        line_color, line_width, "dotted");
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   fclose(fptr);

   return;
   }

