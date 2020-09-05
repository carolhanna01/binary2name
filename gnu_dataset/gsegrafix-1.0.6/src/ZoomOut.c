/*******************************************************************************
*
* ZoomOut.c
*
* Causes plot to zoom out to original plot.
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


void ZoomOut ( void )
   {
   /* Declare variables */
   int i, nx, ny, nz;


   /* Draw original plot */
   if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      /* Get tick-mark reference values */
      nx = p_ticklabels->nxvalues_ref;
      ny = p_ticklabels->nyvalues_ref;
      nz = p_ticklabels->nzvalues_ref;
      p_ticklabels->nxvalues = nx;
      p_ticklabels->nyvalues = ny;
      p_ticklabels->nzvalues = nz;
      for ( i=1; i<=nx; i++ )
         p_ticklabels->xvalues[i-1] = p_ticklabels->xvalues_ref[i-1];
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      for ( i=1; i<=nz; i++ )
         p_ticklabels->zvalues[i-1] = p_ticklabels->zvalues_ref[i-1];
      p_ticklabels->xoffset1 = p_ticklabels->xoffset1_ref;
      p_ticklabels->xoffset2 = p_ticklabels->xoffset2_ref;
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;
      p_ticklabels->zoffset1 = p_ticklabels->zoffset1_ref;
      p_ticklabels->zoffset2 = p_ticklabels->zoffset2_ref;

      /* Draw plot */
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

   else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
      {
      /* Get tick-mark reference values */
      ny = p_ticklabels->nyvalues_ref;
      p_ticklabels->nyvalues = ny;
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;

      /* Draw plot */
      DrawBackgroundImage();
      PolarPlot();
      DrawLegend();
      DrawText();
      DrawImage();
      DrawDateTime();
      }

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      /* Get tick-mark reference values */
      nx = p_ticklabels->nxvalues_ref;
      ny = p_ticklabels->nyvalues_ref;
      nz = p_ticklabels->nzvalues_ref;
      p_ticklabels->nxvalues = nx;
      p_ticklabels->nyvalues = ny;
      p_ticklabels->nzvalues = nz;
      for ( i=1; i<=nx; i++ )
         p_ticklabels->xvalues[i-1] = p_ticklabels->xvalues_ref[i-1];
      for ( i=1; i<=ny; i++ )
         p_ticklabels->yvalues[i-1] = p_ticklabels->yvalues_ref[i-1];
      for ( i=1; i<=nz; i++ )
         p_ticklabels->zvalues[i-1] = p_ticklabels->zvalues_ref[i-1];
      p_ticklabels->xoffset1 = p_ticklabels->xoffset1_ref;
      p_ticklabels->xoffset2 = p_ticklabels->xoffset2_ref;
      p_ticklabels->yoffset1 = p_ticklabels->yoffset1_ref;
      p_ticklabels->yoffset2 = p_ticklabels->yoffset2_ref;
      p_ticklabels->zoffset1 = p_ticklabels->zoffset1_ref;
      p_ticklabels->zoffset2 = p_ticklabels->zoffset2_ref;

      /* Get view-angle reference values */
      p_plot_param_3d->phi   = p_plot_param_3d->phi_ref;
      p_plot_param_3d->theta = p_plot_param_3d->theta_ref;

      /* Draw plot */
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

   return;
   }
