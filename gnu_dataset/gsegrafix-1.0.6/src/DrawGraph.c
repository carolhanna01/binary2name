/*******************************************************************************
*
* DrawGraph.c
*
* Draws graph.
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


void DrawGraph ( void )
   {
   /* Draw graph */
   if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
        strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
        strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
        strcmp(p_plot_param->axis_type, "loglog")   == 0 )
      {
      /* Specify plot-box coordinates */
      PlotBox();

      /* Adjust axes */
      if ( strcmp(p_plot_param->axis_type, "linear") == 0 &&
           strcmp(p_plot_param->axis_scale, "equal") == 0 )
         AxesEqual(1);

      /* Draw Plot */
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
      /* Draw Plot */
      DrawBackgroundImage();
      PolarPlot();
      DrawLegend();
      DrawText();
      DrawImage();
      DrawDateTime();
      }

   else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      {
      /* Draw Plot */
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

