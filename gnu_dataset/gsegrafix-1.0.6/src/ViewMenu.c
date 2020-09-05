/*******************************************************************************
*
* ViewMenu.c
*
* Contains functions:
*    ViewAxesHandler
*    ViewAxesCallback
*    ViewRotateHandler
*    ViewRotateCallback
*    ViewLabelHandler
*    ViewLabelCallback
*    ViewCoordsDisplayHandler
*    ViewCoordsDisplayCallback
*
* Functions for View menu items.
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


void ViewAxesHandler ( void )
   {
   /* Declare variables */
   int nxvalues, nyvalues, nzvalues, iplot, nplots, index_plot_types, flag;
   double xmin, xmax, ymin, ymax, zmin, zmax;
   char string[101];
   const char prompt[] = "Enter new axis minimum and maximum values:\n"
                         "   2d rectangular plots: xmin xmax ymin ymax\n"
                         "   2d polar plots: rmin rmax\n"
                         "   2d contour and color plots: xmin xmax ymin ymax zmin zmax\n"
                         "   3d plots: xmin xmax ymin ymax zmin zmax";


   /* Get old axis data */
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


   /* Get axis-data string */
   memset(string, 0, sizeof(string));
   if (strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      nplots = p_plot_param->nplots;
      index_plot_types = 0;
      flag = 0;
      for ( iplot=1; iplot<=nplots; iplot++ )
         {
         if ( strcmp(&plot_types[index_plot_types], "contour") == 0 ||
              strcmp(&plot_types[index_plot_types], "color")   == 0 )
            flag = 1;
         index_plot_types = index_plot_types + 10;
         }

      if ( flag == 1 )
         snprintf(string, sizeof(string), "%g %g %g %g %g %g",
                  xmin, xmax, ymin, ymax, zmin, zmax);
      else
         snprintf(string, sizeof(string), "%g %g %g %g", xmin, xmax, ymin, ymax);
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      snprintf(string, sizeof(string), "%g %g %g %g",
              pow(10.0, floor(xmin)), pow(10.0, ceil(xmax)), ymin, ymax);

   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      snprintf(string, sizeof(string), "%g %g %g %g",
              xmin, xmax, pow(10.0, floor(ymin)), pow(10.0, ceil(ymax)));

   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      snprintf(string, sizeof(string), "%g %g %g %g",
              pow(10.0, floor(xmin)), pow(10.0, ceil(xmax)),
              pow(10.0, floor(ymin)), pow(10.0, ceil(ymax)));

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      snprintf(string, sizeof(string), "%g %g", ymin, ymax);

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      snprintf(string, sizeof(string), "%g %g %g %g %g %g",
              xmin, xmax, ymin, ymax, zmin, zmax);


   /* Open dialog box */
   RequestDialog(prompt, string, ViewAxesCallback);

   return;
   }


void ViewAxesCallback ( GtkEntry *entry_box )
   {
   /* Declare variables */
   int i, iplot, nplots, index_plot_types, flag;
   double xmin = 0.0,
          xmax = 1.0,
          ymin = 0.0,
          ymax = 1.0,
          zmin = 0.0,
          zmax = 1.0;
   char *string = NULL;
   const char *error_str[] =
      { "Incorrect number of axis minimum and maximum values;\ntwo expected.",
        "Incorrect number of axis minimum and maximum values;\nfour expected.",
        "Incorrect number of axis minimum and maximum values;\nsix expected.",
        "Invalid x-axis limit.",
        "Invalid y-axis limit.",
        "Invalid axis limit.",
        "Axis minimum must be less than axis maximum." };


   /* Get entry-box string */
   g_object_get(entry_box, "text", &string, NULL);


   /* Get axes data */
   if ( string != NULL )
      {
      /* Get new axis limits */
      if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
         {
         nplots = p_plot_param->nplots;
         index_plot_types = 0;
         flag = 0;
         for ( iplot=1; iplot<=nplots; iplot++ )
            {
            if ( strcmp(&plot_types[index_plot_types], "contour") == 0 ||
                 strcmp(&plot_types[index_plot_types], "color")   == 0 )
               flag = 1;
            index_plot_types = index_plot_types + 10;
            }

         if ( flag == 0 )
            {
            if ( sscanf(string, "%lf %lf %lf %lf", &xmin, &xmax, &ymin, &ymax) != 4 )
               {
               ErrorDialog(error_str[1]);
               return;
               }
            }
         else if ( flag == 1 )
            {
            if ( sscanf(string, "%lf %lf %lf %lf %lf %lf",
                        &xmin, &xmax, &ymin, &ymax, &zmin, &zmax) != 6 )
               {
               ErrorDialog(error_str[2]);
               return;
               }
            }
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
         {
         if ( sscanf(string, "%lf %lf %lf %lf", &xmin, &xmax, &ymin, &ymax) != 4 )
            {
            ErrorDialog(error_str[1]);
            return;
            }

         if ( xmin <= 0.0 || xmax <= 0.0 )
            {
            ErrorDialog(error_str[3]);
            return;
            }

         xmin = log10(fabs(xmin));
         xmax = log10(fabs(xmax));
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
         {
         if ( sscanf(string, "%lf %lf %lf %lf", &xmin, &xmax, &ymin, &ymax) != 4 )
            {
            ErrorDialog(error_str[1]);
            return;
            }

         if ( ymin <= 0.0 || ymax <= 0.0 )
            {
            ErrorDialog(error_str[4]);
            return;
            }

         ymin = log10(fabs(ymin));
         ymax = log10(fabs(ymax));
         }

      else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
         {
         if ( sscanf(string, "%lf %lf %lf %lf", &xmin, &xmax, &ymin, &ymax) != 4 )
            {
            ErrorDialog(error_str[1]);
            return;
            }

         if ( xmin <= 0.0 || xmax <= 0.0 ||
              ymin <= 0.0 || ymax <= 0.0 )
            {
            ErrorDialog(error_str[5]);
            return;
            }

         xmin = log10(fabs(xmin));
         xmax = log10(fabs(xmax));
         ymin = log10(fabs(ymin));
         ymax = log10(fabs(ymax));
         }

      else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
         {
         if ( sscanf(string, "%lf %lf", &ymin, &ymax) != 2 )
            {
            ErrorDialog(error_str[0]);
            return;
            }
         }

      else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         if ( sscanf(string, "%lf %lf %lf %lf %lf %lf",
                     &xmin, &xmax, &ymin, &ymax, &zmin, &zmax) != 6 )
            {
            ErrorDialog(error_str[2]);
            return;
            }
         }


      /* Check new axis limits */
      if ( xmin >= xmax || ymin >= ymax || zmin >= zmax )
         {
         ErrorDialog(error_str[6]);
         return;
         }


      /* Destroy old canvas group */
      flag = 0;
      if ( coords_display_flag == 1 )
         {
         ViewCoordsDisplayHandler();
         flag = 1;
         }
      gtk_object_destroy(GTK_OBJECT (group));


      /* Create new canvas group */
      group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(group_root,
                                                       GNOME_TYPE_CANVAS_GROUP,
                                                       "x", 0.0,
                                                       "y", 0.0,
                                                       NULL));


      /* Redraw plot */
      if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
           strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
           strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
           strcmp(p_plot_param->axis_type, "loglog")   == 0 )
         {
         /* Get new axis limits */
         for ( i=1; i<=6; i++ )
            axis_limits[i-1] = 1;
         p_plot_param->axis_limits[0] = xmin;
         p_plot_param->axis_limits[1] = xmax;
         p_plot_param->axis_limits[2] = ymin;
         p_plot_param->axis_limits[3] = ymax;
         p_plot_param->axis_limits[4] = zmin;
         p_plot_param->axis_limits[5] = zmax;

         /* Draw plot */
         AxisLimits(0);
         if ( strcmp(p_plot_param->axis_type, "linear") == 0 &&
              strcmp(p_plot_param->axis_scale, "equal") == 0 )
            AxesEqual(0);
         DrawBackgroundImage();
         DrawGrid2d();
         DrawGridLog();
         PlotData2d();
         if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
            {
            PlotRectangles();
            PlotEllipses();
            }
         PlotLines();
         PlotSymbols();
         DrawTickLabels2d();
         DrawTickLabelsLog();
         DrawAxisLabels();
         DrawLegend();
         DrawText();
         DrawImage();
         DrawDateTime();

         /* Restart coordinates display */
         if ( flag == 1 )
            ViewCoordsDisplayHandler();
         }

      else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
         {
         /* Get new axis limits */
         for ( i=3; i<=4; i++ )
            axis_limits[i-1] = 1;
         p_plot_param->axis_limits[2] = ymin;
         p_plot_param->axis_limits[3] = ymax;

         /* Draw plot */
         AxisLimits(0);
         DrawBackgroundImage();
         PolarPlot();
         DrawLegend();
         DrawText();
         DrawImage();
         DrawDateTime();
         }

      else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         /* Get new axis limits */
         for ( i=1; i<=6; i++ )
            axis_limits[i-1] = 1;
         p_plot_param->axis_limits[0] = xmin;
         p_plot_param->axis_limits[1] = xmax;
         p_plot_param->axis_limits[2] = ymin;
         p_plot_param->axis_limits[3] = ymax;
         p_plot_param->axis_limits[4] = zmin;
         p_plot_param->axis_limits[5] = zmax;

         /* Draw plot */
         AxisLimits(0);
         Initialize3d();
         DrawGrid3d();
         DrawTickMarks3d();
         DrawLabels3d();
         PlotData3d();
         PlotLines();
         PlotSymbols();
         DrawLegend();
         DrawText();
         DrawImage();
         DrawDateTime();
         }

      free(string);
      }

   else
      return;
   }


void ViewRotateHandler ( void )
   {
   /* Declare variables */
   double phi, theta;
   char string[31];
   const char prompt[] = "Enter plot orientation angles:\n"
                         "   azimuth angle (deg) (direction of positive x axis)\n"
                         "   elevation angle (deg) (orientation of x-y plane)\n"
                         "   0 \xE2\x89\xA4 elevation \xE2\x89\xA4 90 degrees";


   /* Create dialog box */
   memset(string, 0, sizeof(string));
   if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      /* Get view-direction-data string */
      phi   = p_plot_param_3d->phi;
      theta = p_plot_param_3d->theta;
      snprintf(string, sizeof(string), "%g %g", phi, theta);

      /* Open dialog box */
      RequestDialog(prompt, string, ViewRotateCallback);
      }

   return;
   }


void ViewRotateCallback ( GtkEntry *entry_box )
   {
   /* Declare variables */
   double phi, theta;
   char *string = NULL;
   const char *error_str[] =
      { "Incorrect number of values;\ntwo expected.",
        "Elevation angle out of range." };


   /* Get entry-box string */
   g_object_get(entry_box, "text", &string, NULL);


   /* Get view-direction data */
   if ( string != NULL )
      {
      if ( sscanf(string, "%lf %lf", &phi, &theta) != 2 )
         {
         ErrorDialog(error_str[0]);
         free(string);
         return;
         }

      free(string);

      if ( theta >= 0.0 && theta <= 90.0 )
         {
         /* Destroy old canvas group */
         gtk_object_destroy(GTK_OBJECT(group));

         /* Create new canvas group */
         group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(group_root,
                                                          GNOME_TYPE_CANVAS_GROUP,
                                                          "x", 0.0,
                                                          "y", 0.0,
                                                          NULL));

         /* Draw plot */
         p_plot_param_3d->phi = phi;
         p_plot_param_3d->theta = theta;
         Initialize3d();
         DrawGrid3d();
         DrawTickMarks3d();
         DrawLabels3d();
         PlotData3d();
         PlotLines();
         PlotSymbols();
         DrawLegend();
         DrawText();
         DrawImage();
         DrawDateTime();
         }

      else
         {
         ErrorDialog(error_str[1]);
         return;
         }
      }

   else
      return;
   }


/* Declare external variables */
extern GtkWidget *axes_menu_item, *xy_coords_menu_item;
int ncolor_plots, ncontour_plots;


void ViewLabelHandler ( void )
   {
   /* Declare variables */
   int iplot, nplots, icount;
   GtkWidget *window;
   GdkCursor *cursor;


   /* Reset flag */
   window = (GtkWidget *) p_window_data->window;
   if ( contour_labels_flag == 0 )
      {
      /* Turn xy_coords_display off */
      if ( coords_display_flag == 1 )
         {
         coords_display_flag = 0;
         if ( gdk_pointer_is_grabbed() == TRUE )
            gdk_pointer_ungrab(GDK_CURRENT_TIME);
         if ( G_IS_OBJECT(GTK_OBJECT(text_plot_data)) )
            gtk_object_destroy(GTK_OBJECT(text_plot_data));
         if ( G_IS_OBJECT(GTK_OBJECT(text_xy_coords_data)) )
            gtk_object_destroy(GTK_OBJECT(text_xy_coords_data));
         if ( G_IS_OBJECT(GTK_OBJECT(text_z_coords_data)) )
            gtk_object_destroy(GTK_OBJECT(text_z_coords_data));
         }

      /* Get number of contour plots */
      nplots = p_plot_param->nplots;
      icount = 0;
      for ( iplot=1; iplot<=nplots; iplot++ )
         if ( strcmp(&plot_types[(iplot-1)*10], "contour") == 0 )
            icount++;
      ncontour_plots = icount;

      /* Set cursor and initialize text canvas item */
      contour_labels_flag = 1;
      icontour_plots = 1;
      cursor = gdk_cursor_new(GDK_CROSSHAIR);
      gdk_window_set_cursor((GdkWindow *) window->window, cursor);
      text_contour_data = gnome_canvas_item_new(group,
                                                GNOME_TYPE_CANVAS_TEXT,
                                                "x", 10.0,
                                                "y", 10.0,
                                                "anchor", GTK_ANCHOR_NORTH_WEST,
                                                "justification", GTK_JUSTIFY_LEFT,
                                                "font-desc", font_text,
                                                "fill_color_rgba", canvas_fg_color,
                                                NULL);

      /* Set axes and xy_coords menu-items sensitivity */
      gtk_widget_set_sensitive(axes_menu_item, FALSE);
      gtk_widget_set_sensitive(xy_coords_menu_item, FALSE);
      }

   else if ( contour_labels_flag == 1 )
      {
      icontour_plots = 1;
      contour_labels_flag = 0;
      if ( gdk_pointer_is_grabbed() == TRUE )
         gdk_pointer_ungrab(GDK_CURRENT_TIME);
      if ( G_IS_OBJECT(GTK_OBJECT(text_contour_data)) )
         gtk_object_destroy(GTK_OBJECT(text_contour_data));
      gdk_window_set_cursor((GdkWindow *) window->window, NULL);
      gtk_widget_set_sensitive(axes_menu_item, TRUE);
      gtk_widget_set_sensitive(xy_coords_menu_item, TRUE);
      }

   return;
   }


void ViewLabelCallback ( int inc )
   {
   /* Declare variables */
   int i, nxvalues, nyvalues, nzvalues, nc,
       nx, ny, xindex, yindex, zindex;
   double x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale,
          xdata_min, xdata_max, ydata_min, ydata_max,
          x, y, diff_min, diff, contour;
   static double z, contour_nearest;


   /* Initialize pointers */
   zcontour_data = NULL;
   contour_label = NULL;


   /* Check icontour_plots value */
   icontour_plots = icontour_plots + inc;
   if ( icontour_plots < 1 )
      icontour_plots = 1;
   else if ( icontour_plots > ncontour_plots )
      icontour_plots = ncontour_plots;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get axis minimum and maximum values */
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
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);


   /* Get data indices */
   xindex = 0;
   yindex = 0;
   zindex = 0;
   nx = 0;
   ny = 0;
   for ( i=1; i<=icontour_plots; i++ )
      {
      xindex = xindex + nx;
      yindex = yindex + ny;
      zindex = zindex + nx*ny;
      nx = nxcontour[i-1];
      ny = nycontour[i-1];
      }


   /* Get data minimum and maximum values */
   xdata_min = xcontour[xindex];
   xdata_max = xcontour[xindex+nx-1];
   ydata_min = ycontour[yindex];
   ydata_max = ycontour[yindex+ny-1];


   /* Get mouse coordinates */
   x = xmin + (xmouse - x1_box)/xscale;
   y = ymin - (ymouse - y2_box)/yscale;
   if ( x < xmin || x > xmax || y < ymin || y > ymax ||
        x < xdata_min || x > xdata_max ||
        y < ydata_min || y > ydata_max )
      return;


   /* Interpolate z-axis data */
   interp2(nx, ny, 1, &xcontour[xindex], &ycontour[yindex], &zcontour[zindex], &x, &y, &z);
   zcontour_data = &z;


   /* Get number of contour lines */
   if ( ncontours < 2 )
      nc = 2*nzvalues - 1;
   else
      nc = ncontours;


   /* Calculate contour-line value */
   diff_min = DBL_MAX;
   for ( i=1; i<=nc; i++ )
      {
      contour = zmin + (i - 1)*(zmax - zmin)/(nc - 1);
      diff = fabs(z - contour);
      if ( diff < diff_min )
         {
         diff_min = diff;
         contour_nearest = contour;
         }
      }
   contour_label = &contour_nearest;

   return;
   }


void ViewCoordsDisplayHandler ( void )
   {
   /* Declare variables */
   int iplot, nplots, icount_color, icount_contour;
   GtkWidget *window;
   GdkCursor *cursor;


   /* Reset flag */
   window = (GtkWidget *) p_window_data->window;
   if ( coords_display_flag == 0 )
      {
      /* Get number of color and contour plots */
      nplots = p_plot_param->nplots;
      plot_type_data = xmalloc(nplots*sizeof(int));
      icount_color = 0;
      icount_contour = 0;
      for ( iplot=1; iplot<=nplots; iplot++ )
         {
         if ( strcmp(&plot_types[(iplot-1)*10], "color") == 0 )
            {
            icount_color++;
            plot_type_data[iplot-1] = icount_color;
            }
         else if ( strcmp(&plot_types[(iplot-1)*10], "contour") == 0 )
            {
            icount_contour++;
            plot_type_data[iplot-1] = -icount_contour;
            }
         else
            plot_type_data[iplot-1] = 0;
         }

      /* Set cursor and initialize text canvas item */
      coords_display_flag = 1;
      itotal_plots = 1;
      icolor_plots = 1;
      icontour_plots = 1;
      cursor = gdk_cursor_new(GDK_CROSSHAIR);
      gdk_window_set_cursor((GdkWindow *) window->window, cursor);
      text_plot_data = gnome_canvas_item_new(group,
                                             GNOME_TYPE_CANVAS_TEXT,
                                             "x", 10.0,
                                             "y", 10.0,
                                             "anchor", GTK_ANCHOR_NORTH_WEST,
                                             "justification", GTK_JUSTIFY_LEFT,
                                             "font-desc", font_text,
                                             "fill_color_rgba", canvas_fg_color,
                                             NULL);
      text_xy_coords_data = gnome_canvas_item_new(group,
                                                  GNOME_TYPE_CANVAS_TEXT,
                                                  "x", 10.0,
                                                  "y", p_window_data->height - height_menu_bar - 10.0,
                                                  "anchor", GTK_ANCHOR_SOUTH_WEST,
                                                  "justification", GTK_JUSTIFY_LEFT,
                                                  "font-desc", font_text,
                                                  "fill_color_rgba", canvas_fg_color,
                                                  NULL);
      text_z_coords_data = gnome_canvas_item_new(group,
                                                 GNOME_TYPE_CANVAS_TEXT,
                                                 "anchor", GTK_ANCHOR_NORTH,
                                                 "justification", GTK_JUSTIFY_CENTER,
                                                 "font-desc", font_text,
                                                 "fill_color_rgba", canvas_fg_color,
                                                 NULL);

      /* Set axes menu-item sensitivity */
      gtk_widget_set_sensitive(axes_menu_item, FALSE);
      }

   else if ( coords_display_flag == 1 )
      {
      coords_display_flag = 0;
      itotal_plots = 1;
      icolor_plots = 1;
      icontour_plots = 1;
      free(plot_type_data);
      if ( gdk_pointer_is_grabbed() == TRUE )
         gdk_pointer_ungrab(GDK_CURRENT_TIME);
      if ( G_IS_OBJECT(GTK_OBJECT(text_plot_data)) )
         gtk_object_destroy(GTK_OBJECT(text_plot_data));
      if ( G_IS_OBJECT(GTK_OBJECT(text_xy_coords_data)) )
         gtk_object_destroy(GTK_OBJECT(text_xy_coords_data));
      if ( G_IS_OBJECT(GTK_OBJECT(text_z_coords_data)) )
         gtk_object_destroy(GTK_OBJECT(text_z_coords_data));
      gdk_window_set_cursor((GdkWindow *) window->window, NULL);
      gtk_widget_set_sensitive(axes_menu_item, TRUE);
      }

   return;
   }


void ViewCoordsDisplayCallback ( int inc )
   {
   int i, nplots, nxvalues, nyvalues,
       nx, ny, xindex, yindex, zindex;
   double x1_box, x2_box, y1_box, y2_box,
          xmin, xmax, ymin, ymax, xscale, yscale,
          window_width, window_height, xorigin, yorigin, radius,
          rmin, rmax, rscale, rx, ry;
   static double x, y, z, theta, r;


   /* Initialize pointers */
   xy_coords_data[0] = NULL;
   xy_coords_data[1] = NULL;
   z_coords_data = NULL;


   /* Check itotal_plots value */
   nplots = p_plot_param->nplots;
   itotal_plots = itotal_plots + inc;
   if ( itotal_plots < 1 )
      itotal_plots = 1;
   else if ( itotal_plots > nplots )
      itotal_plots = nplots;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get plot-circle location and radius */
   if ( flag_polar == 1 )
      {
      window_width  = p_window_data->width;
      window_height = p_window_data->height;
      xorigin = 0.375*window_width;
      yorigin = 0.500*window_height;
      if ( window_width >= window_height )
         radius  = 0.375*window_height;
      else
         radius  = 0.375*window_width;
      }


   if ( flag_2d_rect == 1 )
      {
      /* Get axis minimum and maximum values for 2d rectangular coordinates */
      if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
         {
         nxvalues = p_ticklabels->nxvalues;
         xmin = p_ticklabels->xvalues[0];
         xmax = p_ticklabels->xvalues[nxvalues-1];
         nyvalues = p_ticklabels->nyvalues;
         ymin = p_ticklabels->yvalues[0];
         ymax = p_ticklabels->yvalues[nyvalues-1];
         xmin = xmin - p_ticklabels->xoffset1;
         xmax = xmax + p_ticklabels->xoffset2;
         ymin = ymin - p_ticklabels->yoffset1;
         ymax = ymax + p_ticklabels->yoffset2;
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
         {
         nxvalues = p_ticklabels->nxvalues;
         xmin = floor(p_ticklabels->xvalues[0]);
         xmax = ceil(p_ticklabels->xvalues[nxvalues-1]);
         nxvalues = roundint(xmax - xmin + 1.0);
         nyvalues = p_ticklabels->nyvalues;
         ymin = p_ticklabels->yvalues[0];
         ymax = p_ticklabels->yvalues[nyvalues-1];
         ymin = ymin - p_ticklabels->yoffset1;
         ymax = ymax + p_ticklabels->yoffset2;
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
         {
         nxvalues = p_ticklabels->nxvalues;
         xmin = p_ticklabels->xvalues[0];
         xmax = p_ticklabels->xvalues[nxvalues-1];
         nyvalues = p_ticklabels->nyvalues;
         ymin = floor(p_ticklabels->yvalues[0]);
         ymax = ceil(p_ticklabels->yvalues[nyvalues-1]);
         nyvalues = roundint(ymax - ymin + 1.0);
         xmin = xmin - p_ticklabels->xoffset1;
         xmax = xmax + p_ticklabels->xoffset2;
         }

      else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
         {
         nxvalues = p_ticklabels->nxvalues;
         xmin = floor(p_ticklabels->xvalues[0]);
         xmax = ceil(p_ticklabels->xvalues[nxvalues-1]);
         nxvalues = roundint(xmax - xmin + 1.0);
         nyvalues = p_ticklabels->nyvalues;
         ymin = floor(p_ticklabels->yvalues[0]);
         ymax = ceil(p_ticklabels->yvalues[nyvalues-1]);
         nyvalues = roundint(ymax - ymin + 1.0);
         }

      /* Calculate axis scale factors */
      xscale = (x2_box - x1_box)/(xmax - xmin);
      yscale = (y2_box - y1_box)/(ymax - ymin);
      }

   else if ( flag_polar == 1 )
      {
      /* Get axis minimum and maximum values for polar coordinates */
      nyvalues = p_ticklabels->nyvalues;
      rmin = p_ticklabels->yvalues[0];
      rmax = p_ticklabels->yvalues[nyvalues-1];
      rmin = rmin - p_ticklabels->yoffset1;
      rmax = rmax + p_ticklabels->yoffset2;
      rscale = radius/(rmax - rmin);
      }


   /* Get mouse coordinates */
   if ( flag_2d_rect == 1 )
      {
      x = xmin + (xmouse - x1_box)/xscale;
      y = ymin - (ymouse - y2_box)/yscale;
      if ( x < xmin || x > xmax || y < ymin || y > ymax )
         return;
      else
         {
         if ( flag_logx == 1 )
            x = pow(10.0, x);
         else if ( flag_logy == 1 )
            y = pow(10.0, y);
         else if ( flag_loglog == 1 )
            {
            x = pow(10.0, x);
            y = pow(10.0, y);
            }
         xy_coords_data[0] = &x;
         xy_coords_data[1] = &y;
         }

      /* Interpolate z-axis data for color plots */
      if ( plot_type_data[itotal_plots-1] > 0 )
         {
         xindex = 0;
         yindex = 0;
         zindex = 0;
         nx = 0;
         ny = 0;
         icolor_plots = plot_type_data[itotal_plots-1];
         for ( i=1; i<=icolor_plots; i++ )
            {
            xindex = xindex + nx;
            yindex = yindex + ny;
            zindex = zindex + nx*ny;
            nx = nxcolor[i-1];
            ny = nycolor[i-1];
            }
         if ( xcolor[xindex] <= x && x <= xcolor[xindex+nx-1] &&
              ycolor[yindex] <= y && y <= ycolor[yindex+ny-1] )
            {
            interp2(nx, ny, 1, &xcolor[xindex], &ycolor[yindex], &zcolor[zindex], &x, &y, &z);
            z_coords_data = &z;
            }
         else
            z_coords_data = NULL;
         return;
         }

      /* Interpolate z-axis data for contour plots */
      else if ( plot_type_data[itotal_plots-1] < 0 )
         {
         xindex = 0;
         yindex = 0;
         zindex = 0;
         nx = 0;
         ny = 0;
         icontour_plots = -plot_type_data[itotal_plots-1];
         for ( i=1; i<=icontour_plots; i++ )
            {
            xindex = xindex + nx;
            yindex = yindex + ny;
            zindex = zindex + nx*ny;
            nx = nxcontour[i-1];
            ny = nycontour[i-1];
            }
         if ( xcontour[xindex] <= x && x <= xcontour[xindex+nx-1] &&
              ycontour[yindex] <= y && y <= ycontour[yindex+ny-1] )
            {
            interp2(nx, ny, 1, &xcontour[xindex], &ycontour[yindex], &zcontour[zindex], &x, &y, &z);
            z_coords_data = &z;
            }
         else
            z_coords_data = NULL;
         return;
         }
      }

   else if ( flag_polar == 1 )
      {
      rx = xmouse - xorigin;
      ry = ymouse - yorigin;
      r = rmin + sqrt(rx*rx + ry*ry)/rscale;
      if ( r > rmax )
         return;
      if ( rx == 0.0 && ry == 0.0 )
         theta = 0.0;
      else if ( rx > 0.0 && ry == 0.0 )
         theta = 0.0;
      else
         theta = atan2(-ry, rx)/deg2rad;
      if ( theta < 0.0 )
         theta = theta + 360.0;
      xy_coords_data[0] = &theta;
      xy_coords_data[1] = &r;
      return;
      }
   }

