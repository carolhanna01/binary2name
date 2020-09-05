/*******************************************************************************
*
* DrawTickLabels2d.c
*
* Draws tick-mark labels for linear axes of two-dimensional plots.
*
* Copyright � 2008, 2009, 2010, 2011 Spencer A. Buckner
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


void DrawTickLabels2d ( void )
   {
   /* Declare variables */
   double x1_box, x2_box, y1_box, y2_box;


   /* Check plot_box parameter */
   if ( strcmp(p_plot_param->plot_box, "off") == 0 )
      return;


   /* Check axis_type parameter */
   if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      return;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Draw x-axis tick-mark labels */
   if ( strcmp(p_plot_param->x_tick_marks, "on")  == 0 &&
        strcmp(p_plot_param->x_tick_labels, "on") == 0 &&
        (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
         strcmp(p_plot_param->axis_type, "semilogy") == 0) )
      {
      DrawTickLabels("linear",
                     x1_box, y2_box, x2_box, y2_box,
                     p_ticklabels->nxvalues, &p_ticklabels->xvalues[0],
                     p_ticklabels->xoffset1, p_ticklabels->xoffset2,
                     0.0, 8.0, GTK_ANCHOR_NORTH);
      }


   /* Draw y-axis tick-mark labels */
   if ( strcmp(p_plot_param->y_tick_marks, "on")  == 0 &&
        strcmp(p_plot_param->y_tick_labels, "on") == 0 &&
        (strcmp(p_plot_param->axis_type, "linear")   == 0 ||
         strcmp(p_plot_param->axis_type, "semilogx") == 0) )
      {
      width_ytick_labels = DrawTickLabels("linear",
                                          x1_box, y2_box, x1_box, y1_box,
                                          p_ticklabels->nyvalues, &p_ticklabels->yvalues[0],
                                          p_ticklabels->yoffset1, p_ticklabels->yoffset2,
                                          -8.0, 0.0, GTK_ANCHOR_EAST);
      }

   return;
   }
