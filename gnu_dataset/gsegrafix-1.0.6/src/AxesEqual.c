/*******************************************************************************
*
* AxesEqual.c
*
* Converts axis scaling from "normal" (units on x and y axes not necessarily
* the same length) to "equal" (units on x and y axes equal in length).
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


void AxesEqual ( int flag_ref )
   {
   /* Declare variables */
   int nx, ny;
   double x1_box, y1_box, x2_box, y2_box,
          xmin, xmax, ymin, ymax, ratio,
          xdata_min, xdata_max, ydata_min, ydata_max,
          x1, y1, xmid, ymid, x2, y2;


   /* Return if equal axes not requested */
   if ( strcmp(p_plot_param->axis_scale, "equal") != 0 )
      return;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get initial tick-mark label values */
   nx = p_ticklabels->nxvalues;
   ny = p_ticklabels->nyvalues;
   xmin = p_ticklabels->xvalues[0];
   xmax = p_ticklabels->xvalues[nx-1];
   ymin = p_ticklabels->yvalues[0];
   ymax = p_ticklabels->yvalues[ny-1];
   xmin = xmin - p_ticklabels->xoffset1;
   xmax = xmax + p_ticklabels->xoffset2;
   ymin = ymin - p_ticklabels->yoffset1;
   ymax = ymax + p_ticklabels->yoffset2;


   /* Get data minimum and maximum values */
   xdata_min = p_data_min_max->xmin;
   xdata_max = p_data_min_max->xmax;
   ydata_min = p_data_min_max->ymin;
   ydata_max = p_data_min_max->ymax;


   /* Expand x data range if necessary */
   ratio = (y2_box - y1_box)/(x2_box - x1_box);
   if ( ratio*(xmax - xmin) < (ymax - ymin) )
      {
      xmid = (xdata_min + xdata_max)/2.0;
      x1 = xmid - 0.5*(ydata_max - ydata_min)/ratio;
      x2 = xmid + 0.5*(ydata_max - ydata_min)/ratio;
      axis_limits[0] = 1;
      axis_limits[1] = 1;
      p_plot_param->axis_limits[0] = x1;
      p_plot_param->axis_limits[1] = x2;
      AxisLimits(0);
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      }


   /* Calculate new y-axis limits */
   ymid = (ydata_min + ydata_max)/2.0;
   y1 = ymid - ratio*(xmax - xmin)/2.0;
   y2 = ymid + ratio*(xmax - xmin)/2.0;
   axis_limits[2] = 1;
   axis_limits[3] = 1;
   p_plot_param->axis_limits[2] = y1;
   p_plot_param->axis_limits[3] = y2;


   /* Calculate new tick-mark label values */
   AxisLimits(flag_ref);

   return;
   }
