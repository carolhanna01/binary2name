################################################################################
##
## Source-directory Makefile.am
## Processing Makefile.am with automake produces Makefile.in
##
## Copyright © 2008, 2009, 2010 Spencer A. Buckner
## http://savannah.gnu.org/projects/gsegrafix
##
## This file is part of GSEGrafix, a scientific and engineering plotting program.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
################################################################################

INCLUDES = -I$(top_srcdir)

AM_CFLAGS = \
   `pkg-config --cflags libgnomeui-2.0` \
   `pkg-config --cflags libgnomeprintui-2.2` \
   -DPREFIX=\""$(prefix)"\" \
   -DDATADIR=\""$(prefix)/share"\" \
   -DSYSCONFDIR=\""$(prefix)/etc"\" \
   -DLIBDIR=\""$(prefix)/lib"\"

AM_LDFLAGS = \
   `pkg-config --libs libgnomeui-2.0` \
   `pkg-config --libs libgnomeprintui-2.2`

bin_PROGRAMS = gsegrafix gsegraf

gsegrafix_SOURCES = Dialogs.c GSEGrafix.c HelpMenu.c gsegrafix.h

gsegraf_SOURCES = \
   AutoScale.c AxesEqual.c AxisLimits.c CheckParamData.c Clip.c ColorPlot2d.c \
   ColorPlot3d.c ContourPlot2d.c ContourPlot3d.c CreateMenuBar.c DataMinMax.c \
   DataMinMax3d.c Dialogs.c DrawAxisLabels.c DrawBackgroundImage.c \
   DrawContours3d.c DrawDateTime.c DrawGraph.c DrawGrid.c DrawGrid2d.c \
   DrawGridLog.c DrawGrid3d.c DrawImage.c DrawLabels3d.c DrawLegend.c \
   DrawLines.c DrawSymbols.c DrawText.c DrawTickLabels.c DrawTickLabels2d.c \
   DrawTickLabelsLog.c DrawTickMarks.c DrawTickMarks3d.c EditMenu.c \
   EventHandler.c FileMenu.c FileRead.c FileRead3d.c GetAxisLabelPixbufs.c \
   GetWindowCoords.c GSEGraf.c HelpMenu.c Histogram.c InitializePlot.c \
   InitializeVariables.c Initialize3d.c MeshPlot3d.c Misc.c PlotData2d.c \
   PlotData3d.c PlotEllipses.c PlotInterp3d.c PlotLines.c PlotNormal3d.c \
   PlotPoints3d.c PlotRectangles.c PlotSymbols.c PolarPlot.c ReadParamFile.c \
   ViewMenu.c ZoomIn.c ZoomOut.c gsegraf.h

