#ifndef XLOGMASTER_H
#define XLOGMASTER_H

/* 
   Include file for Xlogmaster 
*/

/*{{{  Variables  */
// This flag is used for enabling / disabling the logs...
gint display_logs;                  // Display of logs file (ENABLED/DISABLED)

Log **entry;
int syslogs;                   // Amount of "Log" objects
int active;                     // currently active "Log" object
char* read_buffer;        // Pointer to I/O-buffer for file operations
long maxtext;               // Number of bytes to be cached
struct stat status;         // stat-buffer for files


/*

  all kinds of flags to be used at startup and runtime:

 */
int request_active;      // which one should be activated...
int xpos,ypos;
int width, height;
int wordwrap;
int terse;                    // should there be a "are you sure" question for
// critical things ?
int lockmem;               // should I try to lock the Xlogmasters pages in memory ?
int small_icon;          // big or small icon to be displayed ?
int notice_follows_mouse; // should the notice window follow the mouse around ?
gint configuration_changed; // did the configuration change ?
gint buttons_shown;  // are the buttons visible right now ?



/*
  The directories that hold the configuration...
*/
gchar* config_dir;
gchar* database_dir;
gchar* private_database_dir;
gchar* lib_dir;

/*}}}*/

/*{{{  GTK variables  */
/* GtkWidget is the storage type for widgets */
GtkWidget *window;            // Main window
GtkAccelGroup* accel_group;   // and it's accelerator table

/* 
   tags attached to main window: 
*/
// scrolled window that contains box for buttons in main window
// this tag is used to know where the size needs to be monitored.
#define BUTTONBOX "xlm::main::buttonbox"


GtkWidget *textwindow;    // GtkText widget
GtkWidget *textscrollbar;   // and the attached Scrollbar

static GtkWidget* box;      // main window horizontal box
static GtkWidget *buttonbox;     // Box for Buttons
GtkTooltips *tooltips;         // for the help messages
static GtkWidget *scrolled_window;

/* GTK-Widgets for "about Xlogmaster" */
static GtkWidget *about_dialog;
GtkWidget *small_logo_xpm;

/* Standard style */
GtkStyle* stdstyle;

/* GDK variables: */
GdkColormap* cmap; // xlogmasters overall colormap
GdkColor alert;              // Alert color to start at
GdkColor base;              // Colors to fade to
GdkColor prelight;         // (prelight to fade to)
double c_alert[3] = {0,0,0};

/*}}}*/

/*{{{  commandline options  */
/* commandline option structure - see ../import/getopt.h
   for more information */

static struct option options[] =
{
  {"activate", 1, 0, 0},
  {"alertcolor", 1, 0, 0},
  {"audio", 1, 0, 0},
  {"audio-alert", 1, 0, 0},
  {"audio-notice", 1, 0, 0},
  {"audio-uniconify", 1, 0, 0},
  {"buffer", 1, 0, 0},
  {"config", 1 , 0, 0},
  {"database", 1 , 0, 0},
  {"fadeseconds", 1, 0, 0},
  {"fadesteps", 1, 0, 0},
  {"geometry", 1, 0, 0},
  {"gtkrc", 1, 0, 0},
  {"help", 0, 0, 0},
#if HAVE_MLOCKALL
  {"mlockall", 0, 0, 0},
#endif /* HAVE_MLOCKALL */
  {"notice-follows-mouse", 2, 0, 0},
  {"silent", 0, 0, 0},
  {"smallicon", 2, 0, 0},
  {"terse", 0, 0, 0},
  {"version", 0, 0, 0},
  {"wordwrap", 2, 0, 0},
  {0, 0, 0, 0}
};

/*}}}*/

/*{{{  GTK subroutines  */
gint delete_event(GtkWidget *, GdkEvent *, gpointer);
void quit (GtkWidget*, gpointer*);
void really_quit_button_pressed();
void color_init();
void create_textwidget();
void create_buttons();
void destroy_buttons();
void button_pressed (GtkWidget*, gpointer*);
void request_about();
void about_ready(GtkWidget *, gpointer *);
/*}}}*/

/*{{{  Data management  */
Log** copy_data_array(Log**);
Log** add_log_entry(Log**, gint, const gchar*, const gchar*, const gchar*, const gchar*, gint, gint);
void add_filter_to_last(Log**, const gchar*, gint, const gchar*);
void destroy_log_array(Log**);
void free_memory();
void disable();
void enable();
gint determine_filter_class(gint);
void sort_filters(Filter**);
/*}}}*/

/*{{{  Initial preparations  */
void usage();
void help_message();
void version_message();
static void eval_colorstring(char*);
long get_value(char*, int*);
void evaluate_paths();
void init(int, char**);


/*}}}*/

/*{{{  Text output subroutines  */
void output_text (GtkWidget*, char*, long);

/*}}}*/

/*{{{  REGEX matching routines  */
glong match_all(Filter**, gint, gchar*, glong, glong);
static gint match_line(Filter*, gchar*);

/*}}}*/

/*{{{  Error procedures  */
void file_error(gchar*);
void fork_error();
void execute_error(gchar*);
void plugin_overflow(gchar*);


/*}}}*/

/*{{{  About Xlogmaster  */
void create_about();
/*}}}*/

#endif /* XLOGMASTER_H */
