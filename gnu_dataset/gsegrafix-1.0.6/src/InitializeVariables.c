/*******************************************************************************
*
* InitializeVariables.c
*
* Initializes plot variables.
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
#include <time.h>
#include "gsegraf.h"


/* Declare external variables */
const char symbol_string[]  = "ld.cCtTsSiIpPhH+xra";   /* symbol-specification characters */
const char symbol_string1[] = "cCtTsSiIpPhH";          /* symbol-specification characters */
const char symbol_string2[] = "+xra";                  /* symbol-specification characters */
const char color_string[]   = "kaswrylqbfmogtnpx";     /* color-specification characters */


void InitializeVariables ( void )
   {
   /* Declare variables */
   int i;
   unsigned int size;
   time_t time_current;
   static window_data_type window_data;
   static plot_box_data_type plot_box_data;
   static plot_param_type plot_param;
   static data_min_max_type data_min_max;
   static ticklabels_type ticklabels;
   static plot_param_3d_type plot_param_3d;


   /* Define constants */
   pi = 4.0*atan(1.0);
   deg2rad = pi/180.0;


   /* Create pointers to structures */
   p_window_data = &window_data;
   p_plot_box_data = &plot_box_data;
   p_plot_param = &plot_param;
   p_data_min_max = &data_min_max;
   p_ticklabels = &ticklabels;
   p_plot_param_3d = &plot_param_3d;


   /* Define function arrays */
   symbol_func1[0]  = DrawCircle;
   symbol_func1[1]  = DrawCircle;
   symbol_func1[2]  = DrawTriangle;
   symbol_func1[3]  = DrawTriangle;
   symbol_func1[4]  = DrawSquare;
   symbol_func1[5]  = DrawSquare;
   symbol_func1[6]  = DrawDiamond;
   symbol_func1[7]  = DrawDiamond;
   symbol_func1[8]  = DrawPentagon;
   symbol_func1[9]  = DrawPentagon;
   symbol_func1[10] = DrawHexagon;
   symbol_func1[11] = DrawHexagon;

   symbol_func2[0] = DrawPlus;
   symbol_func2[1] = DrawX;
   symbol_func2[2] = DrawStar;
   symbol_func2[3] = DrawAsterisk;


   /* Get screen size */
   width_screen  = gdk_screen_width();
   height_screen = gdk_screen_height();


   /* Get date and time */
   memset(date_time, 0, sizeof(date_time));
   time_current = time(NULL);
   strftime(date_time,
            sizeof(date_time),
            "%d-%b-%Y %H:%M:%S",
            localtime(&time_current));


   /* Initialize plot-parameter character arrays to zero bits */
   memset(p_plot_param->axis_type,        0, sizeof(p_plot_param->axis_type));
   memset(p_plot_param->axis_scale,       0, sizeof(p_plot_param->axis_scale));
   memset(p_plot_param->grid,             0, sizeof(p_plot_param->grid));
   memset(p_plot_param->date_time_anchor, 0, sizeof(p_plot_param->date_time_anchor));
   memset(p_plot_param->plot_box,         0, sizeof(p_plot_param->plot_box));
   memset(p_plot_param->x_tick_marks,     0, sizeof(p_plot_param->x_tick_marks));
   memset(p_plot_param->y_tick_marks,     0, sizeof(p_plot_param->y_tick_marks));
   memset(p_plot_param->z_tick_marks,     0, sizeof(p_plot_param->z_tick_marks));
   memset(p_plot_param->x_tick_labels,    0, sizeof(p_plot_param->x_tick_labels));
   memset(p_plot_param->y_tick_labels,    0, sizeof(p_plot_param->y_tick_labels));
   memset(p_plot_param->z_tick_labels,    0, sizeof(p_plot_param->z_tick_labels));


   /* Set pointers to NULL */
   line                  = NULL;
   string_get            = NULL;
   nfilenames            = NULL;
   nformats              = NULL;
   filenames             = NULL;
   formats               = NULL;
   formats_mod           = NULL;
   nlinebreak            = NULL;
   plot_types            = NULL;
   styleflags            = NULL;
   stylechar1            = NULL;
   stylechar2            = NULL;
   stylecolor1           = NULL;
   stylecolor2           = NULL;
   alphacolor            = NULL;
   stylesizes            = NULL;
   fill_colors_rgba      = NULL;
   outline_colors_rgba   = NULL;
   bin_widths            = NULL;
   bin_values            = NULL;
   bin_refs              = NULL;
   ninterp               = NULL;
   stemflags             = NULL;
   stemvalues            = NULL;
   meshcolors            = NULL;
   contourcolors         = NULL;
   xlabel                = NULL;
   ylabel                = NULL;
   zlabel                = NULL;
   title                 = NULL;
   save_filename         = NULL;
   background_image_file = NULL;
   ndata                 = NULL;
   xdata                 = NULL;
   ydata                 = NULL;
   zdata                 = NULL;
   nxmesh                = NULL;
   nymesh                = NULL;
   xmesh                 = NULL;
   ymesh                 = NULL;
   zmesh                 = NULL;
   nxcontour             = NULL;
   nycontour             = NULL;
   xcontour              = NULL;
   ycontour              = NULL;
   zcontour              = NULL;
   nxcolor               = NULL;
   nycolor               = NULL;
   xcolor                = NULL;
   ycolor                = NULL;
   zcolor                = NULL;
   font_name             = NULL;
   font_date_time        = NULL;
   font_legend           = NULL;
   font_text             = NULL;
   font_tick_labels      = NULL;
   font_axis_labels      = NULL;
   font_title            = NULL;
   pixbuf_window         = NULL;
   pixbuf_xlabel         = NULL;
   pixbuf_ylabel         = NULL;
   pixbuf_zlabel         = NULL;


   /* Specify default plot parameters */
   p_window_data->x      = 0;
   p_window_data->y      = 0;
   p_window_data->width  = 768;
   p_window_data->height = 576;
   
   strcpy(p_plot_param->axis_scale,       "auto");
   strcpy(p_plot_param->grid,             "off");
   strcpy(p_plot_param->minor_ticks,      "off");
   strcpy(p_plot_param->date_time_anchor, "off");
   strcpy(p_plot_param->plot_box,         "on");
   strcpy(p_plot_param->x_tick_marks,     "on");
   strcpy(p_plot_param->y_tick_marks,     "on");
   strcpy(p_plot_param->z_tick_marks,     "on");
   strcpy(p_plot_param->x_tick_labels,    "on");
   strcpy(p_plot_param->y_tick_labels,    "on");
   strcpy(p_plot_param->z_tick_labels,    "on");

   canvas_bg_color = 0xFFFFFFFF;   /* white */
   canvas_fg_color = 0x000000FF;   /* black */
   zoom_fill_color = 0x80808080;   /* gray */
   gridchar1 = 'l';                /* solid line */
   gridchar2 = 's';                /* silver */
   gridcolor = 0xC0C0C0FF;         /* silver */
   background_image_style = 2;     /* fill */

   p_data_min_max->xmin = 0.0;
   p_data_min_max->xmax = 1.0;
   p_data_min_max->ymin = 0.0;
   p_data_min_max->ymax = 1.0;
   p_data_min_max->zmin = 0.0;
   p_data_min_max->zmax = 1.0;

   for ( i=1; i<=6; i++ )
      axis_limits[i-1] = 0;

   p_plot_param_3d->phi   = 30.0;
   p_plot_param_3d->theta = 30.0;

   minor_ticks_flag = 0;
   coords_display_flag = 0;
   contour_labels_flag = 0;
   close_flag = 0;


   /* Specify default font name */
   size = sizeof("Sans");
   font_name = xmalloc(size + 1);
   strcpy(font_name, "Sans");


   /* Specify font style and weight */
   font_date_time = pango_font_description_new();
   pango_font_description_set_style(font_date_time, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_date_time, PANGO_WEIGHT_NORMAL);

   font_legend = pango_font_description_new();
   pango_font_description_set_style(font_legend, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_legend, PANGO_WEIGHT_NORMAL);

   font_text = pango_font_description_new();
   pango_font_description_set_style(font_text, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_text, PANGO_WEIGHT_NORMAL);

   font_tick_labels = pango_font_description_new();
   pango_font_description_set_style(font_tick_labels, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_tick_labels, PANGO_WEIGHT_NORMAL);

   font_axis_labels = pango_font_description_new();
   pango_font_description_set_style(font_axis_labels, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_axis_labels, PANGO_WEIGHT_NORMAL);

   font_title = pango_font_description_new();
   pango_font_description_set_style(font_title, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_title, PANGO_WEIGHT_NORMAL);


   /* Specify default font point sizes */
   font_size_date_time   = 12;
   font_size_legend      = 14;
   font_size_text        = 14;
   font_size_tick_labels = 14;
   font_size_axis_labels = 16;
   font_size_title       = 18;


   /* Tabulate color array for colors specified by color characters */
   color_rgba[0]  = 0x000000FF;   /* k black   (black)        */
   color_rgba[1]  = 0x808080FF;   /* a gray    (gray50)       */
   color_rgba[2]  = 0xC0C0C0FF;   /* s silver  (gray75)       */
   color_rgba[3]  = 0xFFFFFFFF;   /* w white   (white)        */
   color_rgba[4]  = 0xFF0000FF;   /* r red     (red)          */
   color_rgba[5]  = 0xFFFF00FF;   /* y yellow  (yellow)       */
   color_rgba[6]  = 0x00FF00FF;   /* l lime    (green)        */
   color_rgba[7]  = 0x00FFFFFF;   /* q aqua    (cyan)         */
   color_rgba[8]  = 0x0000FFFF;   /* b blue    (blue)         */
   color_rgba[9]  = 0xFF00FFFF;   /* f fuchsia (magenta)      */
   color_rgba[10] = 0x800000FF;   /* m maroon  (dark red)     */
   color_rgba[11] = 0x808000FF;   /* o olive   (dark yellow)  */
   color_rgba[12] = 0x008000FF;   /* g green   (dark green)   */
   color_rgba[13] = 0x008080FF;   /* t teal    (dark cyan)    */
   color_rgba[14] = 0x000080FF;   /* n navy    (dark blue)    */
   color_rgba[15] = 0x800080FF;   /* p purple  (dark magenta) */
   color_rgba[16] = 0xFFFFFF00;   /* x transparent            */


   /****************************************************************************
   *
   * Calculate all colors with maximum saturation from blue to green to red
   *    used for:
   *       color plots
   *       contour-line colors of 2d contour plots with plot_style = "auto"
   *       upper-surface colors of 3d mesh plots with plot_style = "auto"
   *
   ****************************************************************************/
   n_color_spectrum_1 = 1021;
   color_spectrum_1[0] = 0x0000FFFF;                        /*               0x0000FFFF; index =           0 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[i]     = 0x0000FFFF + 0x10000*i;     /* 0x0001FFFF to 0x00FFFFFF; index =   1 to  255 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[255+i] = 0x00FFFFFF - 0x100*i;       /* 0x00FFFEFF to 0x00FF00FF; index = 256 to  510 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[510+i] = 0x00FF00FF + 0x1000000*i;   /* 0x01FF00FF to 0xFFFF00FF; index = 511 to  765 */
   for ( i=1; i<= 255; i++ )
      color_spectrum_1[765+i] = 0xFFFF00FF - 0x10000*i;     /* 0xFFFE00FF to 0xFF0000FF; index = 766 to 1020 */


   /****************************************************************************
   *
   * Calculate all colors with 75% maximum saturation from blue to green to red
   *    used for lower-surface colors of 3d mesh plots with plot_style = "auto"
   *
   ****************************************************************************/
   n_color_spectrum_2 = 769;
   color_spectrum_2[0] = 0x0000C0FF;                        /*               0x0000C0FF; index =          0 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[i]     = 0x0000C0FF + 0x10000*i;     /* 0x0001C0FF to 0x00C0C0FF; index =   1 to 192 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[192+i] = 0x00C0C0FF - 0x100*i;       /* 0x00C0BFFF to 0x00C000FF; index = 193 to 384 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[384+i] = 0x00C000FF + 0x1000000*i;   /* 0x01C000FF to 0xC0C000FF; index = 385 to 576 */
   for ( i=1; i<= 192; i++ )
      color_spectrum_2[576+i] = 0xC0C000FF - 0x10000*i;     /* 0xC0BF00FF to 0xC00000FF; index = 577 to 768 */

   return;
   }
