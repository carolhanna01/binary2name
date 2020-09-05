/*******************************************************************************
*
* PlotData3d.c
*
* Plots a two-dimensional projection of three-dimensional data.
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


void PlotData3d ( void )
   {
   /* Declare variables */
   int iplot, nplots, ipoints, imesh, icontour, icolor, index_plot_types,
       npts, nx, ny, index, xindex_mesh, yindex_mesh, zindex_mesh,
       xindex_contour, yindex_contour, zindex_contour,
       xindex_color, yindex_color, zindex_color;


   /* Plot data */
   nplots = p_plot_param->nplots;
   index_plot_types = 0;
   ipoints = 0;
   imesh = 0;
   icontour = 0;
   icolor = 0;
   index = 0;
   xindex_mesh = 0;
   yindex_mesh = 0;
   zindex_mesh = 0;
   xindex_contour = 0;
   yindex_contour = 0;
   zindex_contour = 0;
   xindex_color = 0;
   yindex_color = 0;
   zindex_color = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         /* Increment plot counter */
         ipoints++;

         /* Plot 3d line data */
         npts = ndata[ipoints-1];
         PlotPoints3d(iplot, index, npts);

         /* Increment index */
         index = index + ndata[ipoints-1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         /* Increment plot counter */
         imesh++;

         /* Plot 3d mesh data */
         nx = nxmesh[imesh-1];
         ny = nymesh[imesh-1];
         MeshPlot3d(iplot, imesh, xindex_mesh, yindex_mesh, zindex_mesh, nx, ny);

         /* Increment indices */
         xindex_mesh = xindex_mesh + nx;
         yindex_mesh = yindex_mesh + ny;
         zindex_mesh = zindex_mesh + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         /* Increment plot counter */
         icontour++;

         /* Plot 3d contour data */
         nx = nxcontour[icontour-1];
         ny = nycontour[icontour-1];
         ContourPlot3d(iplot, icontour, xindex_contour, yindex_contour, zindex_contour, nx, ny);

         /* Increment indices */
         xindex_contour = xindex_contour + nx;
         yindex_contour = yindex_contour + ny;
         zindex_contour = zindex_contour + nx*ny;
         }


      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         /* Increment plot counter */
         icolor++;

         /* Plot 3d color data */
         nx = nxcolor[icolor-1];
         ny = nycolor[icolor-1];
         ColorPlot3d(iplot, xindex_color, yindex_color, zindex_color, nx, ny);

         /* Increment indices */
         xindex_color = xindex_color + nx;
         yindex_color = yindex_color + ny;
         zindex_color = zindex_color + nx*ny;
         }


      /* Increment indices */
      index_plot_types = index_plot_types + 10;
      }

   return;
   }
