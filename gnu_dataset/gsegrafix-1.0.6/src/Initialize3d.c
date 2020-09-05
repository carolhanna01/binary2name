/*******************************************************************************
*
* Initialize3d.c
*
* Calculates quadrants, view azimuth angles, rotation matrices, origin, and
* axes for 3-dimensional plots.
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


void Initialize3d ( void )
   {
   /* Declare variables */
   int i, quadrant;
   double phi, theta, *p, origin[3],
          Ry[9], Rz[9], Ryz[9], Ry1[9], Rz1[9], Ryz1[9],
          axis1[3], axis2[3], axis3[3],
          plot_width, plot_height;
   double axis_length = 0.4375*p_window_data->height;
   p_plot_param_3d->axis_length = axis_length;


   /* Get view angles */
   phi   = p_plot_param_3d->phi;     /* view-direction azimuth (deg) from x axis in x-y plane */
   theta = p_plot_param_3d->theta;   /* view-direction elevation (deg) from x-y plane */


   /* Calculate rotation matrices for general case */
   phi = phi*deg2rad;
   theta = theta*deg2rad;

   Ry[0] = cos(-theta);
   Ry[1] = 0.0;
   Ry[2] = -sin(-theta);
   Ry[3] = 0.0;
   Ry[4] = 1.0;
   Ry[5] = 0.0;
   Ry[6] = sin(-theta);
   Ry[7] = 0.0;
   Ry[8] = cos(-theta);

   Rz[0] = cos(phi);
   Rz[1] = sin(phi);
   Rz[2] = 0.0;
   Rz[3] = -sin(phi);
   Rz[4] = cos(phi);
   Rz[5] = 0.0;
   Rz[6] = 0.0;
   Rz[7] = 0.0;
   Rz[8] = 1.0;

   p = multiply_mm(Ry, Rz);
   for ( i=1; i<=9; i++, p++ )
      Ryz[i-1] = *p;


   /* Save rotation matrix for general case */
   for ( i=1; i<=9; i++ )
      p_plot_param_3d->Ryz[i-1] = Ryz[i-1];


   /* Define quadrants */
   phi   = p_plot_param_3d->phi;
   theta = p_plot_param_3d->theta;

   phi = fmod(phi, 360.0);
   if ( phi < 0.0 )
      phi = phi + 360.0;
   if ( 0.0 <= phi && phi < 90.0 )
      quadrant = 1;
   else if ( 90.0 <= phi && phi < 180.0 )
      quadrant = 2;
   else if ( 180.0 <= phi && phi < 270.0 )
      quadrant = 3;
   else
      quadrant = 4;


   /* Adjust view azimuth-angle */
   if ( quadrant == 2 )
      phi = phi - 90.0;
   else if ( quadrant == 3 )
      phi = phi - 180.0;
   else if ( quadrant == 4 )
      phi = phi - 270.0;


   /* Calculate rotation matrices for quadrant 1 */
   phi = phi*deg2rad;
   theta = theta*deg2rad;

   Ry1[0] = cos(-theta);
   Ry1[1] = 0.0;
   Ry1[2] = -sin(-theta);
   Ry1[3] = 0.0;
   Ry1[4] = 1.0;
   Ry1[5] = 0.0;
   Ry1[6] = sin(-theta);
   Ry1[7] = 0.0;
   Ry1[8] = cos(-theta);

   Rz1[0] = cos(phi);
   Rz1[1] = sin(phi);
   Rz1[2] = 0.0;
   Rz1[3] = -sin(phi);
   Rz1[4] = cos(phi);
   Rz1[5] = 0.0;
   Rz1[6] = 0.0;
   Rz1[7] = 0.0;
   Rz1[8] = 1.0;

   p = multiply_mm(Ry1, Rz1);
   for ( i=1; i<=9; i++, p++ )
      Ryz1[i-1] = *p;


   /* Calculate axes */
   axis1[0] = axis_length;
   axis1[1] = 0.0;
   axis1[2] = 0.0;
   p = multiply_mv(Ryz1, axis1);
   for ( i=1; i<=3; i++, p++ )
      axis1[i-1] = *p;

   axis2[0] = 0.0;
   axis2[1] = axis_length;
   axis2[2] = 0.0;
   p = multiply_mv(Ryz1, axis2);
   for ( i=1; i<=3; i++, p++ )
      axis2[i-1] = *p;

   axis3[0] = 0.0;
   axis3[1] = 0.0;
   axis3[2] = axis_length;
   p = multiply_mv(Ryz1, axis3);
   for ( i=1; i<=3; i++, p++ )
      axis3[i-1] = *p;


   /* Calculate origin and plot-box width and height */
   plot_width  = axis2[1] - axis1[1];
   plot_height = axis3[2] - axis1[2] - axis2[2];
   origin[0] = 0.0;
   origin[1] = (p_window_data->width - plot_width)/2.0 - axis1[1];
   origin[2] = (p_window_data->height - 30 - plot_height)/2.0 + axis3[2];


   /* Save quadrant */
   p_plot_param_3d->quadrant = quadrant;


   /* Save axis parameters */
   for ( i=1; i<=3; i++ )
      {
      p_plot_param_3d->axis1[i-1] = axis1[i-1];
      p_plot_param_3d->axis2[i-1] = axis2[i-1];
      p_plot_param_3d->axis3[i-1] = axis3[i-1];
      }


   /* Save plot width and height */
   p_plot_param_3d->plot_width  = plot_width;
   p_plot_param_3d->plot_height = plot_height;


   /* Save origin parameters */
   for ( i=1; i<=3; i++ )
      p_plot_param_3d->origin[i-1] = origin[i-1];


   /* Save plot box parameters */
   p_plot_box_data->xmin = origin[1] + axis1[1];
   p_plot_box_data->xmax = origin[1] + axis2[1];
   p_plot_box_data->ymin = origin[2] - axis3[2];
   p_plot_box_data->ymax = origin[2] - axis1[2] - axis2[2];

   return;
   }
