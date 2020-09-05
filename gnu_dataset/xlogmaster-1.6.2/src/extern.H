#ifndef EXTERN_H
#define EXTERN_H
/*
  
  All "extern" definitions for the Xlogmaster
  
  This is just an include file - if you want to know details about the
  variables and functions have a look at the specific .H or .cc files !!!!!!

*/

#ifndef XLOGMASTER_H
/*{{{  from xlogmaster.cc  */
// GTK based:
extern GtkWidget *window;            // Main window
extern GtkAccelGroup* accel_group;   // and it's accelerator table

/* 
   tags attached to main window: 
*/
//  scrolled window that contains box for buttons in main window
#define BUTTONBOX "xlm::main::buttonbox"

extern GtkWidget *textwindow;                // GtkText widget
extern GtkWidget *textscrollbar;   // and the attached Scrollbar
extern GtkWidget *small_logo_xpm;
extern GtkTooltips *tooltips;         // for the help messages
/* Standard style */
extern GtkStyle* stdstyle;

// GDK based:
extern GdkColormap* cmap; // xlogmasters overall colormap
extern GdkColor alert;           // Alert color to start at
extern GdkColor base;           // Colors to fade to
extern GdkColor prelight;      // (prelight to fade to)

// others:
extern gint display_logs;                  // Display of logs file (ENABLED/DISABLED)
extern Log **entry;
extern int syslogs;                   // Amount of "Log" objects
extern int active;                     // currently active "Log" object

extern struct stat status;         // stat-buffer for files
extern int terse;                    // should there be a "are you sure" question for
extern char* read_buffer;        // Pointer to I/O-buffer for file operations
extern long maxtext;               // Number of bytes to be cached
extern int notice_follows_mouse; // should the notice window follow the mouse around ?
extern gint configuration_changed; // did the configuration change ?
extern gint buttons_shown;  // are the buttons visible right now ?


/*
  The directories that hold the configuration...
*/
extern gchar* config_dir;
extern gchar* database_dir;
extern gchar* private_database_dir;
extern gchar* lib_dir;

// functions:
extern gint delete_event(GtkWidget *, GdkEvent *, gpointer);
extern void quit (GtkWidget*, gpointer*);
extern void request_about();
extern void disable();
extern void enable();
extern gint determine_filter_class(gint);
extern Log** copy_data_array(Log**);
extern Log** add_log_entry(Log**, gint, const gchar*, const gchar*, const gchar*, const gchar*, gint, gint);
extern void add_filter_to_last(Log**, const gchar*, gint, const gchar*);
extern void destroy_log_array(Log**);
extern void create_buttons();
extern void destroy_buttons();
extern void sort_filters(Filter**);
extern void output_text (GtkWidget*, char*, long);
extern glong match_all(Filter**, gint, gchar*, glong, glong);

/* error procedures */
extern void file_error(gchar*);
extern void fork_error();
extern void execute_error(gchar*);
extern void plugin_overflow(gchar*);

/*}}}*/
#endif /* ! XLOGMASTER_H */


#ifndef MENU_H
/*{{{  from menu.cc  */
// others:
extern gint customizing;

// functions:
extern void fire_up_menu(GtkWidget*);

/*}}}*/
#endif /* ! MENU_H */


#ifndef FILE_H
/*{{{  from file.cc  */
// GTK based:

// others:
extern char* load_path;
extern char* save_path;
extern char* log_path;

// functions:
//extern void create_form();
extern Log** read_configuration_file(gchar*);
extern int write_configuration(char*);
extern gint legalize(gint, gint);
extern int match(char*, char*, int);
extern int searchfor(char, char*, int);
extern char* getstring(char , char* , int );
extern char* clean_string(char*);


/*}}}*/
#endif /* ! FILE_H */


#ifndef ALERT_H
/*{{{  from alert.cc  */
// GTK based:


// others:
extern gint steps; // how many steps to use for fading back to original color
extern gdouble fade_base; // fade base (system dependent)
extern gdouble fade_step; // stepsize for fading back

// functions:
extern void trigger_alert(int);
extern void remove_alert(int);
extern void create_notice_window();
extern void notice_alert(gchar*, gchar*);
/*}}}*/
#endif /* ! ALERT_H */


#ifndef WATCHDOG_H
/*{{{  from watchdog.cc  */
// GTK based:


// others:
extern gint class0_filters[];
extern gint class1_filters[];


// functions:
extern void activate();
extern void deactivate();
extern void start_watchdog();
extern void stop_watchdog();


/*}}}*/
#endif /* ! WATCHDOG_H */


#ifndef AUDIO_H
/*{{{  from audio.cc  */
extern gint sound;
extern char* alert_sound;
extern char* notice_sound;
extern char* uniconify_sound;
// functions:
extern void play(gint);

/*}}}*/
#endif /* ! AUDIO_H */


#ifndef EXECUTE_H
/*{{{  from execute.cc  */

extern gint cleanup_tag;
extern gint pid_reference;
extern GList* pid_list;

// functions:
extern void execute_program(gint, gint, gchar*);
extern gchar** split_execute_line(gchar*);
extern void reference_child(gint);
extern gint check_terminate(gint);
extern gint my_setenv(const gchar*, const gchar*);


/*}}}*/
#endif /* ! EXECUTE_H */


#ifndef DIALOG_H
/*{{{  from dialog.cc  */

// functions:
extern void popup_dialog(gchar*, gchar*, GList*, GList*);

/*}}}*/
#endif /* ! DIALOG_H */


#ifndef DATABASE_H
/*{{{  from database.cc  */
extern GtkWidget* db_form;

// functions:
extern void startup_database_menu();
extern void disassemble_database_menu();

/*}}}*/
#endif /* ! DATABASE_H */


#ifndef OUTPUT_H
/*{{{  from output.cc  */


// functions:
extern glong pipe_magic(gint, gchar*, glong, glong);
extern void deactivate_plugin(gint);

/*}}}*/
#endif /* ! OUTPUT_H */


#ifndef CUSTOMIZE_H
/*{{{  from customize.cc  */
extern GtkWidget* database_button;


// functions:
extern void startup_customize(Log**);
extern void remove_all_entries();
extern void add_entry_from_database( Log* );


/*}}}*/
#endif /* ! CUSTOMIZE_H */



#endif /* EXTERN_H */
