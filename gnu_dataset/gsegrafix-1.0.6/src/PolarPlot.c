/*******************************************************************************
*
* PolarPlot.c
*
* Calculates polar plot of input data.
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


void PolarPlot ( void )
   {
   /* Declare variables */
   int i, j, ifunc, nyvalues, nrvalues, iplot, nplots, npts, index, index_stemflags,
       anchor, window_width, window_height;
   guint32 fill_color, outline_color;
   double rmin, rmax, xorigin, yorigin, radius, radius_grid, rscale,
          x, y, x1, y1, x2, y2, xtext1, xtext2, ytext1, ytext2,
          theta, theta_max, theta_minor, r, r1, dr, width_xtick_label;
   char string[21], *pchar;
   extern char symbol_string1[];   /* symbol-specification characters "cCtTsSiIpPhH" */
   extern char symbol_string2[];   /* symbol-specification characters "+xra" */
   GnomeCanvasPoints *points;
   GnomeCanvasItem *line, *circle, *rectangle, *text, *pixbuf_item;


   /* Specify plot-circle location and radius */
   window_width  = p_window_data->width;
   window_height = p_window_data->height;
   xorigin = 0.375*window_width;
   yorigin = 0.500*window_height;
   if ( window_width >= window_height )
      radius  = 0.375*window_height;
   else
      radius  = 0.375*window_width;


   /* Save plot-box data */
   p_plot_box_data->xmin = xorigin - radius;
   p_plot_box_data->xmax = xorigin + radius;
   p_plot_box_data->ymin = yorigin - radius;
   p_plot_box_data->ymax = yorigin + radius;


   /* Specify axis minimum and maximum values */
   nyvalues = p_ticklabels->nyvalues;
   nrvalues = nyvalues;
   rmin = p_ticklabels->yvalues[0];
   rmax = p_ticklabels->yvalues[nyvalues-1];
   rmin = rmin - p_ticklabels->yoffset1;
   rmax = rmax + p_ticklabels->yoffset2;
   rscale = radius/(rmax - rmin);


   /* Draw grid lines */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
        (strcmp(p_plot_param->grid, "on1") == 0 ||
         strcmp(p_plot_param->grid, "on2") == 0 ) )
      {
      /* Draw constant-theta grid lines */
      if ( strcmp(p_plot_param->x_tick_marks, "on") == 0 )
         {
         points = gnome_canvas_points_new(2);
         for ( i=1; i<=12; i++ )
            {
            theta = (i - 1.0)*30.0*deg2rad;
            points->coords[0] = xorigin;
            points->coords[1] = yorigin;
            points->coords[2] = xorigin + radius*cos(theta);
            points->coords[3] = yorigin - radius*sin(theta);

            if ( gridchar1 == 'l' )
               DrawLine(points, gridcolor, 1);
            else if ( gridchar1 == 'd' )
               DrawDashedLine(points, gridcolor, 1);
            else if ( gridchar1 == '.' )
               DrawDottedLine(points, gridcolor, 1);
            }
         gnome_canvas_points_unref(points);
         }


      /* Draw constant-r grid lines */
      if ( strcmp(p_plot_param->y_tick_marks, "on") == 0 )
         {
         for ( i=1; i<=nrvalues; i++ )
            {
            x1 = xorigin - (p_ticklabels->yvalues[i-1] - rmin)*rscale;
            x2 = xorigin + (p_ticklabels->yvalues[i-1] - rmin)*rscale;
            y1 = yorigin - (p_ticklabels->yvalues[i-1] - rmin)*rscale;
            y2 = yorigin + (p_ticklabels->yvalues[i-1] - rmin)*rscale;

            if ( gridchar1 == 'l' )
               circle = gnome_canvas_item_new(group,
                                              GNOME_TYPE_CANVAS_ELLIPSE,
                                              "x1", x1,
                                              "x2", x2,
                                              "y1", y1,
                                              "y2", y2,
                                              "fill_color_rgba", 0xFFFFFF00,
                                              "outline_color_rgba", gridcolor,
                                              "width_pixels", 1,
                                              NULL);
            else if ( gridchar1 == 'd' )
               {
               radius_grid = (p_ticklabels->yvalues[i-1] - rmin)*rscale;
               DrawDashedCircle(xorigin, yorigin, radius_grid, gridcolor);
               }
            else if ( gridchar1 == '.' )
               {
               radius_grid = (p_ticklabels->yvalues[i-1] - rmin)*rscale;
               DrawDottedCircle(xorigin, yorigin, radius_grid, gridcolor);
               }
            }
         }
      }


   /* Draw theta tick marks */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
        strcmp(p_plot_param->x_tick_marks, "on") == 0 )
      {
      points = gnome_canvas_points_new(2);
      for ( i=1; i<=12; i++ )
         {
         theta = (i - 1)*30.0*deg2rad;
         points->coords[0] = xorigin + radius*cos(theta);
         points->coords[1] = yorigin - radius*sin(theta);
         points->coords[2] = xorigin + (radius - tick_major)*cos(theta);
         points->coords[3] = yorigin - (radius - tick_major)*sin(theta);

         line = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_LINE,
                                      "points", points,
                                      "fill_color_rgba", canvas_fg_color,
                                      "width_pixels", 1,
                                      NULL);

         if ( minor_ticks_flag == 1 )
            {
            for ( j=1; j<=5; j++ )
               {
               theta_minor = ((i - 1)*30.0 + j*5.0)*deg2rad;
               points->coords[0] = xorigin + radius*cos(theta_minor);
               points->coords[1] = yorigin - radius*sin(theta_minor);
               points->coords[2] = xorigin + (radius - tick_minor)*cos(theta_minor);
               points->coords[3] = yorigin - (radius - tick_minor)*sin(theta_minor);

               line = gnome_canvas_item_new(group,
                                            GNOME_TYPE_CANVAS_LINE,
                                            "points", points,
                                            "fill_color_rgba", canvas_fg_color,
                                            "width_pixels", 1,
                                            NULL);
               }
            }
         }
      gnome_canvas_points_unref(points);
      }


   /* Draw r axis and tick marks */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
        strcmp(p_plot_param->y_tick_marks, "on") == 0 )
      {
      points = gnome_canvas_points_new(2);

      theta = 45.0*deg2rad;
      points->coords[0] = xorigin;
      points->coords[1] = yorigin;
      points->coords[2] = xorigin + radius*cos(theta);
      points->coords[3] = yorigin - radius*sin(theta);

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", canvas_fg_color,
                                   "width_pixels", 1,
                                   NULL);

      DrawTickMarks("linear", minor_ticks_flag, 1,
                    xorigin, yorigin, xorigin + radius*cos(45.0*deg2rad), yorigin - radius*sin(theta),
                    p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                    p_ticklabels->yoffset1, p_ticklabels->yoffset2,
                    135.0*deg2rad);

      gnome_canvas_points_unref(points);
      }


   /* Draw plot circle */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 )
      circle = gnome_canvas_item_new(group,
                                     GNOME_TYPE_CANVAS_ELLIPSE,
                                     "x1", xorigin - radius,
                                     "x2", xorigin + radius,
                                     "y1", yorigin - radius,
                                     "y2", yorigin + radius,
                                     "fill_color_rgba", 0xFFFFFF00,
                                     "outline_color_rgba", canvas_fg_color,
                                     "width_pixels", 2,
                                     NULL);


   /* Draw theta tick-mark labels */
   if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
        strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
        strcmp(p_plot_param->x_tick_labels, "on") == 0 )
      {
      width_xtick_label = 0.0;
      for ( i=1; i<=12; i++ )
         {
         theta = (i - 1.0)*30.0*deg2rad;
         x = xorigin + (radius + 8.0)*cos(theta);
         y = yorigin - (radius + 8.0)*sin(theta);
         memset(string, 0, sizeof(string));
         snprintf(string, sizeof(string), "%d", (i - 1)*30);

         if ( i == 1 )
            anchor = GTK_ANCHOR_WEST;
         else if ( i == 2  || i == 3 )
            anchor = GTK_ANCHOR_SOUTH_WEST;
         else if ( i == 4 )
            anchor = GTK_ANCHOR_SOUTH;
         else if ( i == 5  || i == 6 )
            anchor = GTK_ANCHOR_SOUTH_EAST;
         else if ( i == 7 )
            anchor = GTK_ANCHOR_EAST;
         else if ( i == 8  || i == 9 )
            anchor = GTK_ANCHOR_NORTH_EAST;
         else if ( i == 10 )
            anchor = GTK_ANCHOR_NORTH;
         else if ( i == 11  || i == 12 )
            anchor = GTK_ANCHOR_NORTH_WEST;

         text = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_TEXT,
                                      "text", string,
                                      "x", x,
                                      "y", y,
                                      "anchor", anchor,
                                      "font-desc", font_tick_labels,
                                      "fill_color_rgba", canvas_fg_color,
                                      NULL);

         if ( i == 1 )
            {
            gnome_canvas_item_get_bounds(text, &x1, &y1, &x2, &y2);
            width_xtick_label = x2 - x1;
            }
         }
      }


   /* Draw theta-axis label */
   if ( xlabel != NULL && pixbuf_xlabel != NULL )
      {
      if ( strcmp(p_plot_param->plot_box, "on") == 0 )
         {
         points = gnome_canvas_points_new(2);
         for ( i=1; i<=9; i++ )
            {
            theta_max = 45.0/(radius + 20.0 + width_xtick_label);
            theta = (i - 1)*theta_max/10.0;
            points->coords[0] = xorigin + (radius + 8.0 + width_xtick_label + 10.0)*cos(theta);
            points->coords[1] = yorigin - (radius + 8.0 + width_xtick_label + 10.0)*sin(theta);
            theta = i*theta_max/10.0;
            points->coords[2] = xorigin + (radius + 8.0 + width_xtick_label + 10.0)*cos(theta);
            points->coords[3] = yorigin - (radius + 8.0 + width_xtick_label + 10.0)*sin(theta);

            line = gnome_canvas_item_new(group,
                                         GNOME_TYPE_CANVAS_LINE,
                                         "points", points,
                                         "fill_color_rgba", canvas_fg_color,
                                         "width_pixels", 1,
                                         NULL);
            }

         theta = 0.9*theta_max;
         points->coords[0] = xorigin + (radius + 8.0 + width_xtick_label + 10.0)*cos(theta);
         points->coords[1] = yorigin - (radius + 8.0 + width_xtick_label + 10.0)*sin(theta);
         theta = theta_max;
         points->coords[2] = xorigin + (radius + 8.0 + width_xtick_label + 10.0)*cos(theta);
         points->coords[3] = yorigin - (radius + 8.0 + width_xtick_label + 10.0)*sin(theta);

         line = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_LINE,
                                      "points", points,
                                      "fill_color_rgba", canvas_fg_color,
                                      "width_pixels", 1,
                                      "last_arrowhead", TRUE,
                                      "arrow_shape_a", 10.0,
                                      "arrow_shape_b", 12.0,
                                      "arrow_shape_c", 4.0,
                                      NULL);
         gnome_canvas_points_unref(points);
         }

      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_xlabel,
                                          "x", xorigin + radius + 28.0 + width_xtick_label,
                                          "y", yorigin,
                                          "anchor", GTK_ANCHOR_WEST,
                                          NULL);
      }


   /* Draw r-axis label */
   if ( ylabel != NULL && pixbuf_ylabel != NULL )
      {
      if ( strcmp(p_plot_param->plot_box, "on") == 0 )
         {
         points = gnome_canvas_points_new(2);
         theta = 45.0*deg2rad;
         points->coords[0] = xorigin + (radius + 15.0)*cos(theta);
         points->coords[1] = yorigin - (radius + 15.0)*sin(theta);
         points->coords[2] = xorigin + (radius + 60.0)*cos(theta);
         points->coords[3] = yorigin - (radius + 60.0)*sin(theta);

         line = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_LINE,
                                      "points", points,
                                      "fill_color_rgba", canvas_fg_color,
                                      "width_pixels", 1,
                                      "last_arrowhead", TRUE,
                                      "arrow_shape_a", 10.0,
                                      "arrow_shape_b", 12.0,
                                      "arrow_shape_c", 4.0,
                                      NULL);
         gnome_canvas_points_unref(points);
         }

      theta = 45.0*deg2rad;
      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_ylabel,
                                          "x", xorigin + (radius + 70.0)*cos(theta),
                                          "y", yorigin - (radius + 70.0)*sin(theta),
                                          "anchor", GTK_ANCHOR_WEST,
                                          NULL);
      }


   /* Draw plot title */
   if ( title != NULL && pixbuf_title != NULL )
      {
      x = xorigin;
      if ( strcmp(p_plot_param->plot_box, "on") == 0 &&
           strcmp(p_plot_param->x_tick_marks, "on") == 0 &&
           strcmp(p_plot_param->x_tick_labels, "on") == 0 )
         y = yorigin - radius - 8.0 - font_size_tick_labels - 8.0;
      else
         y = yorigin - radius - 8.0;
      pixbuf_item = gnome_canvas_item_new(group,
                                          GNOME_TYPE_CANVAS_PIXBUF,
                                          "pixbuf", pixbuf_title,
                                          "x", x,
                                          "y", y,
                                          "anchor", GTK_ANCHOR_SOUTH,
                                          NULL);
      }


   /* Plot data */
   nplots = p_plot_param->nplots;
   index = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      npts = ndata[iplot-1];

      /* Draw stem lines */
      index_stemflags = (iplot - 1)*4;
      if ( strcmp(&stemflags[index_stemflags],  "on") == 0 ||
           strcmp(&stemflags[index_stemflags], "num") == 0 )
         {
         points = gnome_canvas_points_new(2);

         /* Calculate r coordinate of stem point 1 */
         if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
            r1 = rmin;
         else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
            {
            if ( rmin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= rmax )
               r1 = stemvalues[iplot-1];
            else if ( stemvalues[iplot-1] < rmin )
               r1 = rmin;
            else if ( stemvalues[iplot-1] > rmax )
               r1 = rmax;
            }

         for ( i=1; i<=npts; i++ )
            {
            theta = xdata[index+i-1];
            r     = ydata[index+i-1];
            if ( r <= rmax )
               {
               /* Calculate coordinates of stem point 1 */
               x = xorigin + (r1 - rmin)*cos(theta)*rscale;
               y = yorigin - (r1 - rmin)*sin(theta)*rscale;
               points->coords[0] = x;
               points->coords[1] = y;

               /* Calculate coordinates of stem point 2 */
               if ( r < rmin )
                  r = rmin;
               x = xorigin + (r - rmin)*cos(theta)*rscale;
               y = yorigin - (r - rmin)*sin(theta)*rscale;
               points->coords[2] = x;
               points->coords[3] = y;
               DrawLine(points, outline_colors_rgba[iplot-1], 1);
               }
            }

         gnome_canvas_points_unref(points);
         }


      /* Draw lines */
      if ( stylechar1[iplot-1] == 'l' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin, yorigin, rmin, rmax, rscale, 'l');


      /* Draw dashed lines */
      else if ( stylechar1[iplot-1] == 'd' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin, yorigin, rmin, rmax, rscale, 'd');


      /* Draw dashed lines */
      else if ( stylechar1[iplot-1] == '.' )
         DrawLineSegmentsPolar(iplot, index, npts, xorigin, yorigin, rmin, rmax, rscale, '.');


      /* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
      else if ( (pchar = strchr(symbol_string1, stylechar1[iplot-1])) != NULL )
         {
         ifunc = pchar - symbol_string1;
         for ( i=1; i<=npts; i++ )
            {
            theta = xdata[index+i-1];
            r     = ydata[index+i-1];
            if ( rmin <= r && r <= rmax )
               {
               x = xorigin + (r - rmin)*cos(theta)*rscale;
               y = yorigin - (r - rmin)*sin(theta)*rscale;
               symbol_func1[ifunc](x, y, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1],
                                   stylesizes[iplot-1]);
               }
            }
         }


      /* Draw symbols in symbol_string2 ("+xra") */
      else if ( (pchar = strchr(symbol_string2, stylechar1[iplot-1])) != NULL )
         {
         ifunc = pchar - symbol_string2;
         for ( i=1; i<=npts; i++ )
            {
            theta = xdata[index+i-1];
            r     = ydata[index+i-1];
            if ( rmin <= r && r <= rmax )
               {
               x = xorigin + (r - rmin)*cos(theta)*rscale;
               y = yorigin - (r - rmin)*sin(theta)*rscale;
               symbol_func2[ifunc](x, y, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
               }
            }
         }

      index = index + ndata[iplot-1];
      }


   /* Plot specified lines and symbols */
   PlotLines();
   PlotSymbols();


   /* Draw r tick-mark labels on translucent rectangles */
   if ( strcmp(p_plot_param->plot_box, "on")      == 0 &&
        strcmp(p_plot_param->y_tick_marks, "on")  == 0 &&
        strcmp(p_plot_param->y_tick_labels, "on") == 0 )
      {
      dr = (rmax - rmin)/(nrvalues - 1);
      for ( i=1; i<=nrvalues; i++ )
         {
         theta = 45.0*deg2rad;
         x = xorigin + (p_ticklabels->yvalues[i-1] - rmin)*rscale*cos(theta);
         y = yorigin - (p_ticklabels->yvalues[i-1] - rmin)*rscale*sin(theta);

         memset(string, 0, sizeof(string));
         if ( fabs(p_ticklabels->yvalues[i-1]) < 0.01*dr )
            snprintf(string, sizeof(string), "%1.0f", 0.0);
         else
            snprintf(string, sizeof(string), "%g", p_ticklabels->yvalues[i-1]);

         text = gnome_canvas_item_new(group,
                                      GNOME_TYPE_CANVAS_TEXT,
                                      "text", string,
                                      "x", x - 10.0,
                                      "y", y,
                                      "anchor", GTK_ANCHOR_EAST,
                                      "font-desc", font_tick_labels,
                                      "fill_color_rgba", canvas_fg_color,
                                      NULL);

         gnome_canvas_item_get_bounds(text, &xtext1, &ytext1, &xtext2, &ytext2);

         fill_color = canvas_bg_color - 0xFF + 0xC0;
         outline_color = canvas_bg_color - 0xFF + 0xC0;
         rectangle = gnome_canvas_item_new(group,
                                           GNOME_TYPE_CANVAS_RECT,
                                           "x1", xtext1,
                                           "x2", xtext2,
                                           "y1", ytext1,
                                           "y2", ytext2,
                                           "fill_color_rgba", fill_color,
                                           "outline_color_rgba", outline_color,
                                           "width_pixels", 1,
                                           NULL);

         gnome_canvas_item_raise_to_top(text);
         }
      }

   return;
   }


void DrawLineSegmentsPolar ( int iplot, int index, int npts,
                             double xorigin, double yorigin, double rmin, double rmax,
                             double rscale, int linechar )
   {
   /* Declare variables */
   int idraw, iseg, nseg, npts_seg, iseg1;
   char linetype[7];


   /* Create linetype string */
   memset(linetype, 0, sizeof(linetype));
   if ( linechar == 'l' )
      strcpy(linetype, "solid");
   else if ( linechar == 'd' )
      strcpy(linetype, "dashed");
   else if ( linechar == '.' )
      strcpy(linetype, "dotted");


   /* Draw all line segments except last */
   idraw = 0;
   nseg = nlinebreaks;
   iseg1 = index;
   for (iseg=1; iseg<=nseg; iseg++ )
      {
      if ( index < nlinebreak[iseg-1] && nlinebreak[iseg-1] < index + npts )
         {
         idraw++;
         npts_seg = nlinebreak[iseg-1] - iseg1;
         DrawLinesPolar(npts_seg, &xdata[iseg1], &ydata[iseg1], xorigin, yorigin, rmin, rmax, rscale,
                        fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);
         iseg1 = nlinebreak[iseg-1];
         }
      }


   /* Draw last line segment */
   if ( idraw > 0 )
      {
      npts_seg = index + npts - iseg1;
      DrawLinesPolar(npts_seg, &xdata[iseg1], &ydata[iseg1], xorigin, yorigin, rmin, rmax, rscale,
                     fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);
      }

   /* Draw continuous line */
   else
      DrawLinesPolar(npts, &xdata[index], &ydata[index], xorigin, yorigin, rmin, rmax, rscale,
                     fill_colors_rgba[iplot-1], stylesizes[iplot-1], linetype);

   return;
   }


void DrawDashedCircle ( double xorigin, double yorigin, double radius, guint32 fill_color_rgba)
   {
   /* Declare variables */
   int i, j, ndashes;
   double theta1, theta2, dtheta1, dtheta2;
   GnomeCanvasItem *line;
   GnomeCanvasPoints *points;


   /* Check radius */
   if ( radius == 0.0 )
      return;


   /* Draw dashed circle */
   ndashes = roundint(2.0*pi*radius/(dash + space_dash));
   dtheta1 = 2.0*pi/ndashes;
   dtheta2 = 0.5*pi*dash/(ndashes*(dash + space_dash));

   points = gnome_canvas_points_new(5);

   for ( i=1; i<=ndashes; i++ )
      {
      theta1 = (i - 1)*dtheta1;
      points->coords[0] = xorigin + radius*cos(theta1);
      points->coords[1] = yorigin - radius*sin(theta1);
      for ( j=1; j<=4; j++ )
         {
         theta2 = theta1 + j*dtheta2;
         points->coords[2*j]   = xorigin + radius*cos(theta2);
         points->coords[2*j+1] = yorigin - radius*sin(theta2);
         }

      line = gnome_canvas_item_new(group,
                                   GNOME_TYPE_CANVAS_LINE,
                                   "points", points,
                                   "fill_color_rgba", fill_color_rgba,
                                   "width_pixels", 1,
                                   NULL);
      }

   gnome_canvas_points_unref(points);
   }


void DrawDottedCircle ( double xorigin, double yorigin, double radius, guint32 fill_color_rgba)
   {
   /* Declare variables */
   int i, npoints, ndots;
   double theta[361], length[361], length_total, inc, inc_calc, length_interp, theta_interp;


   /* Check radius */
   if ( radius == 0 )
      return;


   /* Calculate circle length */
   npoints = 361;
   for ( i=1; i<=npoints; i++ )
      {
      theta[i-1] = (i - 1)*pi/180.0;
      length[i-1] = radius*theta[i-1];
      }
   length_total = length[npoints-1];


   /* Calculate number of dots */
   inc = space_dot + 1.0;
   ndots = roundint(length_total/inc + 1.0);
   inc_calc = length_total/(ndots - 1);


   /* Interpolate and draw dots */
   for ( i=1; i<ndots; i++ )
      {
      length_interp = length[0] + (i - 1)*inc_calc;
      interp1(npoints, 1, length, theta, &length_interp, &theta_interp);
      DrawCircle(xorigin+radius*cos(theta_interp), yorigin-radius*sin(theta_interp),
                 fill_color_rgba, fill_color_rgba, 1);
      }
   }

