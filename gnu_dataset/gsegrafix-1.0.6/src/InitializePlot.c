/*******************************************************************************
*
* InitializePlot.c
*
* Contains functions:
*    InitializePlot
*    PlotBox
*
* Initializes plot variables, reads plot-parameter file, checks plot-parameter
* values, calculates various plot variables, and analyzes various plot
* variables.
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


void InitializePlot ( void )
   {
   /* Declare variables */
   double window_width, window_height, max_dimension;


   /* Initialize variables */
   InitializeVariables();


   /* Read plot-parameter file */
   ReadParamFile();


   /* Check plot-parameter values */
   CheckParamData();


   /* Set axis_type flags */
   flag_linear = 0;
   flag_logx = 0;
   flag_logy = 0;
   flag_loglog = 0;
   flag_polar = 0;
   flag_3d = 0;
   flag_2d = 0;
   flag_2d_rect = 0;
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      flag_linear = 1;
   else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      flag_logx = 1;
   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      flag_logy = 1;
   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      flag_loglog = 1;
   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      flag_polar = 1;
   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      flag_3d = 1;

   if ( flag_linear == 1 ||
        flag_logx   == 1 ||
        flag_logy   == 1 ||
        flag_loglog == 1 ||
        flag_polar  == 1 )
      flag_2d = 1;

   if ( flag_linear == 1 ||
        flag_logx   == 1 ||
        flag_logy   == 1 ||
        flag_loglog == 1 )
      flag_2d_rect = 1;


   /* Calculate tick-mark and dashed-line variables */
   window_width  = p_window_data->width;
   window_height = p_window_data->height;
   if ( window_width >= window_height )
      max_dimension = window_width;
   else
      max_dimension = window_height;
   tick_major = (8.0/768.0)*max_dimension;
   tick_minor = 0.5*tick_major;
   dash = tick_major;
   space_dash = 0.5*dash;
   space_dot = space_dash - 1.0;


   /* Analyze plot data */
   if ( flag_2d_rect == 1 )
      {
      /* Read plot data files */
      FileRead();

      /* Find minimum and maximum values of plot data */
      DataMinMax();

      /* Calculate axes */
      AxisLimits(1);
      }

   else if ( flag_polar == 1 )
      {
      /* Read plot data files */
      FileRead();

      /* Find minimum and maximum values of plot data */
      DataMinMax();

      /* Adjust axes */
      AxisLimits(1);
      }

   else if ( flag_3d == 1 )
      {
      /* Read plot data file */
      FileRead3d();

      /* Find minimum and maximum values of plot data */
      DataMinMax3d();

      /* Adjust axes */
      AxisLimits(1);
      }

   return;
   }


void PlotBox ( void )
   {
   /* Declare variables */
   int iplot, nplots, flag;


   /* Specify plot box screen coordinates */
   nplots = p_plot_param->nplots;
   flag = 0;
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      for ( iplot=1; iplot<=nplots; iplot++ )
         if ( strcmp(&plot_types[(iplot-1)*10], "color") == 0 ||
              (strcmp(&plot_types[(iplot-1)*10], "contour") == 0 &&
               styleflags[iplot-1] == 7) )
            flag = 1;


   /* Plot types: points, contour, and histogram */
   if ( flag == 0 )
      {
      p_plot_box_data->xmin = 0.15625*p_window_data->width;
      p_plot_box_data->xmax = 0.90625*p_window_data->width;
      p_plot_box_data->ymin = 0.09375*p_window_data->height;
      p_plot_box_data->ymax = 0.84375*p_window_data->height;
      }

   /* Plot types: points, contour, histogram, and color */
   else if ( flag == 1 )
      {
      p_plot_box_data->xmin = 0.18750*p_window_data->width;
      p_plot_box_data->xmax = 0.75000*p_window_data->width;
      p_plot_box_data->ymin = 0.09375*p_window_data->height;
      p_plot_box_data->ymax = 0.84375*p_window_data->height;
      }
   }
