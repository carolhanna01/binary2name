/*******************************************************************************
*
* gsegraf.h
*
* Declares structures, external variables, and function prototypes for gsegraf
* executable.
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


#include <gnome.h>
#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-job.h>
#include <libgnomeprintui/gnome-print-dialog.h>
#include <libgnomeprintui/gnome-print-job-preview.h>


/* Define structures */
typedef struct
   {
   /* GnomeApp *window; */
   GnomeApp *window;
   GtkWidget *canvas;
   int x, y, width, height;
   int flag;
   } window_data_type;


typedef struct
   {
   double xmin, xmax, ymin, ymax;
   } plot_box_data_type;


typedef struct
   {
   int  nplots;
   char axis_type[10];
   char grid[4];
   char minor_ticks[4];
   char axis_scale[6];
   double axis_limits[6];
   char date_time_anchor[10];
   char plot_box[4];
   char x_tick_marks[4], y_tick_marks[4], z_tick_marks[4];
   char x_tick_labels[4], y_tick_labels[4], z_tick_labels[4];
   } plot_param_type;


typedef struct
   {
   double xmin, xmax, ymin, ymax, zmin, zmax;
   } data_min_max_type;


typedef struct
   {
   int nxvalues, nyvalues, nzvalues,
       nxvalues_ref, nyvalues_ref, nzvalues_ref;
   double xvalues[11], yvalues[11], zvalues[11],
          xoffset1, xoffset2, yoffset1, yoffset2, zoffset1, zoffset2,
          xvalues_ref[11], yvalues_ref[11], zvalues_ref[11],
          xoffset1_ref, xoffset2_ref, yoffset1_ref, yoffset2_ref,
          zoffset1_ref, zoffset2_ref;
   } ticklabels_type;


typedef struct
   {
   int quadrant;
   double phi, theta, phi_ref, theta_ref, axis_length, plot_width, plot_height,
          origin[3], axis1[3], axis2[3], axis3[3], Ryz[9];
   } plot_param_3d_type;


/* Declare structure pointers */
window_data_type   *p_window_data;
plot_param_type    *p_plot_param;
plot_box_data_type *p_plot_box_data;
ticklabels_type    *p_ticklabels;
data_min_max_type  *p_data_min_max;
plot_param_3d_type *p_plot_param_3d;


/* Declare constants */
double pi, deg2rad;


/* Declare screen variables */
int width_screen, height_screen;


/* Declare menu bar variables */
int height_menu_bar;
GtkMenuBar *menu_bar;


/* Declare axis_type flags */
int flag_linear, flag_logx, flag_logy, flag_loglog, flag_polar, flag_3d,
    flag_2d, flag_2d_rect;


/* Declare tick-mark and dashed-line variables */
double tick_major, tick_minor, dash, space_dash, space_dot;


/* Declare gnome canvas group pointers */
GnomeCanvasGroup *group_root, *group;


/* Declare color variables and pointers for colors specified by color characters */
guint32 color_rgba[17], zoom_fill_color, *fill_colors_rgba, *outline_colors_rgba;


/* Declare color variables for all colors with maximum saturation from blue to green to red */
int n_color_spectrum_1, n_color_spectrum_2;
guint32 color_spectrum_1[1021], color_spectrum_2[769];


/* Declare pango font-description pointers and font-size variables */
PangoFontDescription *font_date_time,
                     *font_legend,
                     *font_text,
                     *font_tick_labels,
                     *font_axis_labels,
                     *font_title;

double font_size_date_time,
       font_size_legend,
       font_size_text,
       font_size_tick_labels,
       font_size_axis_labels,
       font_size_title;


/* Declare plot-parameter variables and pointers */
int maxline, *nfilenames, *nformats, *styleflags, axis_limits[6],
    minor_ticks_flag, ncontours, *ninterp, close_flag, background_image_style;
unsigned int *stylesizes;
guint32 canvas_fg_color, canvas_bg_color, gridcolor, *stylecolor1, *stylecolor2,
        *alphacolor, *meshcolors, *contourcolors;
char *line, *string_get, *p_param_file, *p_tempfile, *filenames, *formats, *formats_mod,
     *plot_types, *stylechar1, *stylechar2, gridchar1, gridchar2, *bin_values, *bin_refs,
     *stemflags, *xlabel, *ylabel, *zlabel, *title, *save_filename, *background_image_file,
     *font_name, date_time[21];
double *zblack, *zwhite, *bin_widths, *stemvalues;


/* Declare tick-label size variables */
double width_ytick_labels,
       width_axis1_tick_labels,
       width_axis2_tick_labels,
       width_axis3_tick_labels;


/* Declare variables for 2d-contour labeling */
int contour_labels_flag, icontour_plots;
double *zcontour_data, *contour_label;
GnomeCanvasItem *text_contour_data;


/* Declare variables for x-y coordinates display */
int *plot_type_data, coords_display_flag, icolor_plots, itotal_plots;
double xmouse, ymouse, *xy_coords_data[2], *z_coords_data;
GnomeCanvasItem *text_plot_data, *text_xy_coords_data, *text_z_coords_data;


/* Declare data variables and pointers */
int *ndata,
    *nxmesh, *nymesh,
    *nxcontour, *nycontour,
    *nxcolor, *nycolor,
    *nlinebreak, nlinebreaks;
double *xdata, *ydata, *zdata,
       *xmesh, *ymesh, *zmesh,
       *xcontour, *ycontour, *zcontour,
       *xcolor, *ycolor, *zcolor;


/* Declare pixbufs */
GdkPixbuf *pixbuf_window,
          *pixbuf_logo,
          *pixbuf_xlabel,
          *pixbuf_ylabel,
          *pixbuf_zlabel,
          *pixbuf_title;


/* Function prototypes */
void     AutoScale ( int naxes, int maxlabels );

void     AxesEqual ( int flag_ref );

void     AxisLimits ( int flag_ref );

void     CheckParamData ( void );

/* Clip.c */
int      Clip2d ( double xmin, double xmax, double ymin, double ymax, double *line_coords );
int      ClipPolar ( double rmin, double rmax, double *line_coords );
int      Clip3d ( double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
                  double *line_coords );

void     ColorPlot2d ( int iplot, int icolor, int xindex, int yindex, int zindex, int nx, int ny );

void     ColorPlot3d ( int iplot, int xindex, int yindex, int zindex, int nx, int ny );

void     ContourPlot2d ( int iplot, int icontour, int xindex, int yindex, int zindex, int nx, int ny );

void     ContourPlot3d ( int iplot, int icountour, int xindex, int yindex, int zindex, int nx, int ny );

void     CreateMenuBar ( void );

void     DataMinMax ( void );

void     DataMinMax3d ( void );

/* Dialogs.c */
void     RequestDialog ( const char *prompt_str, const char *text_str, void (*callback) (GtkEntry *entry_box) );
void     ErrorDialog ( const char *message_str );
void     MessageDialog ( const char *message_str );
void     QuestionDialog ( const char *question_str, void (*callback) );

void     DrawAxisLabels ( void );

void     DrawBackgroundImage ( void );

void     DrawContours3d ( int icontour, double *xpoints, double *ypoints, double *zpoints );

void     DrawDateTime ( void );

void     DrawGraph ( void );

void     DrawGrid ( char *axis_type,
                    double x11_screen, double y11_screen, double x12_screen, double y12_screen,
                    double x21_screen, double y21_screen, int nticks, double *tick_values,
                    double offset1, double offset2 );

void     DrawGrid2d ( void );

void     DrawGridLog ( void );

void     DrawGrid3d ( void );

void     DrawImage ( void );

void     DrawLabels3d ( void );

/* DrawLegend.c */
void     DrawLegend ( void );
void     background_legend ( char *legend_str, double height_lines, double x1, double y1 );

/* DrawLines.c */
void     DrawLines2d ( int npts, double *x, double *y, double xmin, double xmax, double ymin, double ymax,
                       double xscale, double yscale, guint32 color, unsigned int line_width, char *line_type );
void     DrawLinesPolar ( int npts, double *x, double *y, double xorigin, double yorigin,
                          double rmin, double rmax, double rscale,
                          guint32 color, unsigned int line_width, char *line_type );
void     DrawLines3d ( int npts, double *x, double *y, double *z, double *origin, double *Ryz,
                       double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
                       double xscale, double yscale, double zscale, guint32 color,
                       unsigned int line_width, char *line_type );

/* DrawSymbols.c */
void     DrawLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int line_width );
void     DrawDashedLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int line_width );
void     DrawDottedLine ( GnomeCanvasPoints *points, guint32 fill_color_rgba, unsigned int size );
void     DrawCircle ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawTriangle ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawSquare ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawDiamond ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawPentagon ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawHexagon ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba, unsigned int size );
void     DrawPlus ( double x, double y, guint32 fill_color_rgba, unsigned int size );
void     DrawX ( double x, double y, guint32 fill_color_rgba, unsigned int size );
void     DrawStar ( double x, double y, guint32 fill_color_rgba, unsigned int size );
void     DrawAsterisk ( double x, double y, guint32 fill_color_rgba, unsigned int size );
void     DrawBar ( double x1, double y1, double x2, double y2,
                   guint32 fill_color_rgba, guint32 outline_color_rgba );
void     DrawMesh ( double x1, double y1, double x2, double y2,
                    guint32 fill_color_rgba, guint32 outline_color_rgba, int flag );
void     DrawContour ( double x1, double y1, double x2, double y2,
                       guint32 fill_color_rgba, guint32 outline_color_rgba, int flag );
void     DrawColorPlot ( double x1, double y1, double x2, double y2 );

/* DrawText.c */
void     DrawText ( void );
void     background_text ( char *text_str, double text_height, double x1, double y1 );

double   DrawTickLabels ( char *axis_type,
                          double x1_screen, double y1_screen, double x2_screen, double y2_screen,
                          int nticks, double *tick_values, double offset1, double offset2,
                          double xoffset, double yoffset, int anchor );

void     DrawTickLabels2d ( void );

void     DrawTickLabelsLog ( void );

void     DrawTickMarks ( char *axis_type, int minor_ticks_flag, int center_flag,
                         double x1_screen, double y1_screen, double x2_screen, double y2_screen,
                         int nticks, double *tick_values, double offset1, double offset2,
                         double tick_angle );

void     DrawTickMarks3d ( void );

/* EditMenu.c */
void     EditCopyHandler ( void );
void     GetWindowPixbuf ( void );

gboolean EventHandler ( GtkWidget *window, GdkEvent *event );

/* FileMenu.c */
void     FileSaveAsHandler ( void );
void     FileOK ( GtkWidget *file_chooser );
void     FilePrintPreviewHandler ( void );
void     FilePrintHandler ( void );
void     PrintCalculate ( GnomePrintJob *job, GnomePrintContext *context, GdkPixbuf *pixbuf );
void     FileCloseHandler ( void );
void     FreeMemory ( void );

/* FileRead.c */
void     FileRead ( void );
int      sort_compare_double ( const void *p1, const void *p2 );

void     FileRead3d ( void );

void     GetAxisLabelPixbufs ( void );

void     GetWindowCoords ( double *plot_coords, double *window_coords );

/* HelpMenu.c */
void     HelpHandler ( void );
void     HelpCallback ( GtkEntry *entry_box );
void     AboutHandler ( void );

void     Histogram ( int iplot, int ihist );

/* InitializePlot.c */
void     InitializePlot ( void );
void     PlotBox ( void );

void     InitializeVariables ( void );

void     Initialize3d ( void );

void     MeshPlot3d ( int iplot, int imesh, int xindex, int yindex, int zindex, int nx, int ny );

/* Misc.c */
void *   xmalloc ( size_t size );
double   min ( int n, double *x );
double   max ( int n, double *x );
int      roundint ( double  x );
double   dot ( double *vector1, double *vector2 );
double * cross ( double *vector1, double *vector2 );
double * multiply_mv ( double *matrix, double *vector );
double * multiply_mm ( double *matrix1, double *matrix2 );
void     interp1 ( int n, int ni, double *x, double *y, double *xi, double *yi );
void     interp2 ( int nx, int ny, int ni, double *x, double *y, double *z,
                   double *xi, double *yi, double *zi );
double   interp_rect ( double x1, double x2, double y1, double y2, double xi );
guint32  interp_color_1 ( double fraction );
guint32  interp_color_2 ( double fraction );
int      find_indices ( int n1, int n2, double *x, double xi );
int      search_compare_ascending ( const void *p1, const void *p2 );
int      search_compare_descending ( const void *p1, const void *p2 );
char *   get_string ( char *string, unsigned int *i1_str, unsigned int *i2_str,
                      unsigned int *size, int flag );
void     put_pixel ( GdkPixbuf *pixbuf, int i, int j, guint32 color );

/* PlotData2d.c */
void     PlotData2d ( void );
void     DrawLineSegments ( int iplot, int index, int npts,
                            double xmin, double xmax, double ymin, double ymax,
                            double xscale, double yscale, int linechar );

void     PlotData3d ( void );
void     PlotEllipses ( void );
void     PlotInterp3d ( int iplot, int imesh,
                        double xmin, double ymin, double zmin, double zmax,
                        double xscale, double yscale, double zscale,
                        double *origin, double *Ryz, guint32 *fill_color,
                        double *xpoints, double *ypoints, double *zpoints );

void     PlotLines ( void );

void     PlotNormal3d ( int iplot, int imesh,
                        double xmin, double ymin, double zmin, double zmax,
                        double xscale, double yscale, double zscale,
                        double *origin, double *Ryz, guint32 *fill_color,
                        double *xpoints, double *ypoints, double *zpoints );

/* PlotPoints3d.c */
void     PlotPoints3d ( int iplot, int index, int npts );
void     DrawLineSegments3d ( int iplot, int index, int npts, double *origin, double *Ryz,
                              double xmin, double xmax, double ymin, double ymax, double zmin, double zmax,
                              double xscale, double yscale, double zscale, int linechar );

void     PlotRectangles ( void );
void     PlotSymbols ( void );

/* PolarPlot.c */
void     PolarPlot ( void );
void     DrawLineSegmentsPolar ( int iplot, int index, int npts,
                                 double xorigin, double yorigin, double rmin, double rmax,
                                 double rscale, int linechar );
void     DrawDashedCircle ( double xorigin, double yorigin, double radius, guint32 fill_color_rgba );
void     DrawDottedCircle ( double xorigin, double yorigin, double radius, guint32 fill_color_rgba );

void     ReadParamFile ( void );

void     ( *symbol_func1[12] ) ( double x, double y, guint32 fill_color_rgba, guint32 outline_color_rgba,
                                 unsigned int size );

void     ( *symbol_func2[4] ) ( double x, double y, guint32 fill_color_rgba, unsigned int size );

/* ViewMenu.c */
void     ViewAxesHandler ( void );
void     ViewAxesCallback ( GtkEntry *entry_box );
void     ViewRotateHandler ( void );
void     ViewRotateCallback ( GtkEntry *entry_box );
void     ViewLabelHandler ( void );
void     ViewLabelCallback ( int inc );
void     ViewCoordsDisplayHandler ( void );
void     ViewCoordsDisplayCallback ( int inc );

void     ZoomIn ( double x1_window, double y1_window, double x2_window, double y2_window );

void     ZoomOut ( void );
