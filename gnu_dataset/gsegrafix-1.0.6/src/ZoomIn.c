/*******************************************************************************
*
* ZoomIn.c
*
* Causes plot to zoom to indicated rectangle.
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


void ZoomIn ( double x1_window, double y1_window, double x2_window, double y2_window )
   {
   /* Declare variables */
   int i, nxvalues, nyvalues, nzvalues;
   double x1_box, y1_box, x2_box, y2_box,
          xmin, xmax, ymin, ymax, zmin, zmax,
          xscale, yscale,
          x1, y1, x2, y2;


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

   if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      xmin = floor(xmin);
      xmax = ceil(xmax);
      }

   if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      ymin = floor(ymin);
      ymax = ceil(ymax);
      }


   /* Calculate new data minimum and maximum values */
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);
   x1 = xmin + (x1_window - x1_box)/xscale;
   x2 = xmin + (x2_window - x1_box)/xscale;
   y1 = ymin - (y1_window - y2_box)/yscale;
   y2 = ymin - (y2_window - y2_box)/yscale;


   /* Save new data minimum and maximum values */
   if ( x1 < x2 )
      {
      p_data_min_max->xmin = x1;
      p_data_min_max->xmax = x2;
      }
   else
      {
      p_data_min_max->xmin = x2;
      p_data_min_max->xmax = x1;
      }

   if ( y1 < y2 )
      {
      p_data_min_max->ymin = y1;
      p_data_min_max->ymax = y2;
      }
   else
      {
      p_data_min_max->ymin = y2;
      p_data_min_max->ymax = y1;
      }

   p_data_min_max->zmin = zmin;
   p_data_min_max->zmax = zmax;


   /* Set tick-label offsets to zero */
   p_ticklabels->xoffset1 = 0.0;
   p_ticklabels->xoffset2 = 0.0;
   p_ticklabels->yoffset1 = 0.0;
   p_ticklabels->yoffset2 = 0.0;


   /* Turn axis limits off */
   for ( i=1; i<=4; i++ )
      axis_limits[i-1] = 0;
   for ( i=5; i<=6; i++ )
      axis_limits[i-1] = 1;
   p_plot_param->axis_limits[4] = zmin;
   p_plot_param->axis_limits[5] = zmax;


   /* Draw plot */
   if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
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
      }

   return;
   }
