/*

  The Xlogmaster
  
  a GTK+ based program to keep track of logfiles & system status
  Copyright (C) 1998,1999 Georg C. F. Greve
  
This file is part of GNU xlogmaster.

GNU xlogmaster is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

GNU xlogmaster is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.

  
  Contact: 
           mailto:xlogmaster-bugs@gnu.org
           http://www.gnu.org/software/xlogmaster/
  Secondary sources:
           http://www.fusebox.hanse.de/xlogmaster/
	 
*/

/*{{{  Header  */

#include "../config.h"
#include "sysinc.H"
#include "logclass.H"
#include "xlogmaster.H"
#include "extern.H"

#include "../graphics/logo.xpm"
#include "../graphics/logo_small.xpm"

/*}}}*/

/*{{{  Main  */
/* 
   this is the main function

   it bascially only coordinates the startup and then hands over the control
   to the GTK+ toolkit

 */
using namespace std;	/* jsg - added because of cout and cerr complaints and not using std::cout etc */
int 
main (int argc, char *argv[])
{
  /* Parse commandline & initialize toolkit */
  gtk_set_locale();
  gtk_init (&argc, &argv);

  /* Do my own initialization stunts */
  init( argc, argv );

  /* create main window */
  accel_group = gtk_accel_group_new ();
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_accel_group_attach (accel_group, GTK_OBJECT (window));
  gtk_widget_set_usize( GTK_WIDGET (window), width, height);
  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC (quit), NULL);
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  gtk_widget_set_uposition( window, xpos, ypos );

  /* create horizontally split box */
  box = gtk_hbox_new (FALSE, 0);
  gtk_container_add(GTK_CONTAINER (window), box);
  gtk_widget_show(box);

  /*  initialize tooltips  */
  tooltips = gtk_tooltips_new();

  /* create textwidget */
  /* if you want to know a few things about the setup of the main
     window and why I don't use some things you'd like to have
     please see comment on top of the create_textwidget function */
  create_textwidget();

  /* create scrolled box for buttons */
  buttonbox = gtk_vbox_new(FALSE, 0);
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );

  /* put box for buttons into window */
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW (scrolled_window), 
      buttonbox);
  gtk_object_set_data( GTK_OBJECT(window), BUTTONBOX, 
		       (GtkWidget*) scrolled_window );
  gtk_box_pack_start (GTK_BOX(box), scrolled_window, FALSE, FALSE, 0);
  gtk_widget_show(scrolled_window);

  /* set initial size of box for buttons */
  gtk_widget_set_usize(scrolled_window, 170, 0);
 
  /* Fire up Pop-Up Menu - the function is defined in menu.cc */
  fire_up_menu(textwindow);

  /* do color stunts */
  color_init();
  
  /* Now create the buttons */
  create_buttons(); 

  /* PopUp Window */
  gtk_widget_show(buttonbox);
  gtk_widget_show(box);
  gtk_widget_show(window);

  /* Create dialog boxes for "about" and notice */
  create_about();
  create_notice_window();

  /* start displaying first Log */
  if ( request_active > syslogs ) active = syslogs-1;
  else active = request_active;
  start_watchdog();
  activate();

  /* logs can be displayed now */
  enable();

  /* and now hand over control to toolkit */
  gtk_window_set_policy(GTK_WINDOW(window), TRUE, TRUE, FALSE);
  gtk_main ();
  return 0;
}
/*}}}*/

/*{{{  GTK toolkit subroutines  */
gint
delete_event(GtkWidget *widget, GdkEvent *, gpointer data)
{
  // we will attach this to all windows so the window managers "delete" signal
  // won't kill our windows (that's what returning TRUE means)

  /*
    we do want the user whether he really wants to quit, though...
  */
  quit((GtkWidget*) widget, (gpointer*) data);

  return (TRUE);
}
void 
quit (GtkWidget *, gpointer *)
{
  if ( terse == TRUE )
    {
      really_quit_button_pressed();
      return;
    }

  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  
  buttons = g_list_append(buttons,
			  (gpointer) "    YES    ");
  functions = g_list_append(functions,
			    (gpointer) really_quit_button_pressed );
  buttons = g_list_append(buttons,
			  (gpointer) "    NO    ");
  functions = g_list_append(functions, NULL );

  if ( configuration_changed == TRUE )
    popup_dialog
      ("Really Quit ?", 
       "\n      There are unsaved changes.      \n-\n      Do you really want to quit ?      \n", 
       buttons, functions);
  else
    popup_dialog
      ("Really Quit ?", 
       "\n      Do you really want to quit ?      \n", 
       buttons, functions);
}
void 
really_quit_button_pressed()
{
  disable();
  stop_watchdog();
  free_memory();
  gtk_main_quit();
}
void 
color_init()
{
  cmap = gdk_colormap_get_system();
  alert.pixel = 0;
  base.pixel = prelight.pixel = 0;
  stdstyle = gtk_rc_get_style(window);
  if ( ! stdstyle ) stdstyle = gtk_widget_get_style(window);
  base.red = stdstyle->bg[GTK_STATE_NORMAL].red;
  base.green = stdstyle->bg[GTK_STATE_NORMAL].green;
  base.blue = stdstyle->bg[GTK_STATE_NORMAL].blue;
  prelight.red = stdstyle->bg[GTK_STATE_PRELIGHT].red;
  prelight.green = stdstyle->bg[GTK_STATE_PRELIGHT].green;
  prelight.blue = stdstyle->bg[GTK_STATE_PRELIGHT].blue;  
  alert.red = (guint)( 65535.0 * c_alert[0] );
  alert.green = (guint)( 65535.0 * c_alert[1] );
  alert.blue = (guint)( 65535.0 * c_alert[2] );
}
/*
  This routine is called during initalization and it creates the GtkText Widget.
  
  The return value is the box that contains the textwindow and it's scrollbar
  so the box can be put into the paned window.
*/
void
create_textwidget()
{
  /*
    NOTE ABOUT THE MAIN WINDOW CONSTRUCTION AND THE TEXT WINDOW
    ESPECIALLY:

    The GTK+ toolkit still has some weaknesses and two of the main ones
    affect the Xlogmaster (unfortunately):

    - There is no horizontal scrollbar for the GtkText widget which means lines HAVE
    to be wrapped, otherwise you'd lose information

    - The paned widget (that would usually be the ultimate choice for the main window)
    is rudimental at best - there is no handler for changing the aspect ratio, no way
    totell it the RIGHT side should not get resized when resizing the main window
    and so on... PLUS it is incredibly slow when resizing. Hopefully it'll work soon...


    Just ignore this commented out code... I'll deal with it once the GTK+ toolkit
    supplies the functionality I need,

    textwindow = gtk_text_new(NULL,NULL);
    gtk_text_set_editable(GTK_TEXT(textwindow), FALSE);
    gtk_text_set_word_wrap (GTK_TEXT(textwindow), wordwrap);
    textscrollbar = gtk_vscrollbar_new (GTK_TEXT(textwindow)->vadj);
    gtk_box_pack_end(GTK_BOX(box), textscrollbar, FALSE, FALSE, 0);
    
    
    GTK_TEXT(textwindow)->line_wrap = FALSE;
    GtkWidget* text_hscrollbar= gtk_hscrollbar_new (GTK_TEXT(textwindow)->hadj);
    GtkWidget* vbox = gtk_vbox_new(FALSE,0);
    gtk_adjustment_clamp_page(GTK_ADJUSTMENT(GTK_TEXT(textwindow)->hadj),
    0, 1000 );
    gtk_box_pack_start(GTK_BOX(vbox), textwindow, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), text_hscrollbar, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(box), vbox, TRUE, TRUE, 0);
    gtk_widget_show (text_hscrollbar);
    gtk_widget_show (vbox);
    

    gtk_widget_show (textscrollbar);
    gtk_widget_show (textwindow);
  
    return;
  */

  textwindow = gtk_text_new(NULL,NULL);
  gtk_text_set_editable(GTK_TEXT(textwindow), FALSE);
  gtk_text_set_word_wrap (GTK_TEXT(textwindow), wordwrap);
  textscrollbar = gtk_vscrollbar_new (GTK_TEXT(textwindow)->vadj);
  gtk_box_pack_start(GTK_BOX(box), textwindow, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(box), textscrollbar, FALSE, FALSE, 0);
  gtk_widget_show (textscrollbar);
  gtk_widget_show(textwindow);
 
}
/*
  This function creates the buttons - 
  it is called at startup and during reinitialisation
  when the user changed the config or loaded a new one...
  
  This routine is also responsible for counting the amount
  of objects and putting the number into "syslogs" for the
  other routines. The reason is simple: before the buttons
  are up there is no routine needing the "syslogs" variable.
  
  no parameters or return values...
*/
void 
create_buttons()
{
  /* if there is no structure to display we have a problem... */
  if ( entry == NULL )
    {
      cerr << PACKAGE << ": critical problem, no configuration could be" << endl;
      cerr << "  found - this should never happen, please refer to manuals" << endl;
      cerr << "  for help." << endl;
      _exit(0);
    }

  /* count the amount of Log class objects */
  syslogs = 0;
  while ( entry[syslogs] != NULL ) syslogs++;

  guint8 mods = 0;
  guchar key = '1';
  
  for ( gint i = 0 ; i < syslogs ; i++ )
    {

      entry[i]->button = gtk_button_new_with_label (entry[i]->buttontext);

      gtk_signal_connect (GTK_OBJECT (entry[i]->button), "clicked",
			  GTK_SIGNAL_FUNC (button_pressed), (gpointer) i );

      gtk_box_pack_start(GTK_BOX(buttonbox), 
			 entry[i]->button, 
			 FALSE, 
			 FALSE, 
			 5 );

      gtk_tooltips_set_tip(tooltips, 
			   entry[i]->button, 
			   entry[i]->help,
			   entry[i]->help );

      gtk_widget_show(entry[i]->button);
      
      /* install the keyboard accelerators */
      if ( mods < ( GDK_MOD1_MASK | 
		    GDK_CONTROL_MASK ) )
	{

	  entry[i]->accel_key = key;
	  entry[i]->accel_mods = mods;

	  gtk_widget_add_accelerator ( entry[i]->button ,
				       "clicked" ,
				       accel_group ,
				       entry[i]->accel_key ,
				       entry[i]->accel_mods ,
				       GTK_ACCEL_VISIBLE );
	  
	}

      /* 
	 determine the next keyboard accelerator 
	 
	 first numbers from '1'-'0', then
	 CTRL+'1'-'0', then ALT+'1'-'0', then 
	 ALT+CTRL+'1'-'0'
      */
      if ( key == '0' )
	{
	  
	  if ( mods & GDK_CONTROL_MASK )
	    {
      	      if ( mods & GDK_MOD1_MASK )
		{
		  
		  /* alright if this wasn't enough we stop
		     assigning keyboard accelerators now */
		  mods = ( GDK_MOD1_MASK | 
			   GDK_CONTROL_MASK |
			   GDK_SHIFT_MASK );  
		}
	      else
		{
		  mods |= GDK_MOD1_MASK;
		}
	      
	      if ( ! mods & GDK_SHIFT_MASK )
		{
		  mods &= ~GDK_CONTROL_MASK;
		}
	    }
	  else
	    {
	      mods |= GDK_CONTROL_MASK;
	    }  
	}
      key++;
      if ( key > '9' )
	{
	  key = '0';
	}
      
      /* and now the final style things... */
      entry[i]->style = gtk_style_copy(stdstyle);
      gtk_widget_set_style(entry[i]->button, entry[i]->style);
      gtk_style_unref(entry[i]->style);
      
    }
  
  gtk_widget_grab_focus(entry[0]->button);
  gtk_widget_realize(window);
  
}
/*
  Function to do a clean removal of the buttons...
*/
void
destroy_buttons()
{
  gint i = 0;
  
  while ( entry != NULL && entry[i] != NULL )
    {

      /* clear alert for the button... */
      remove_alert(i);

      /* remove the button. */
      if ( entry[i]->button != NULL )
	{

	  gtk_widget_remove_accelerator ( entry[i]->button ,
					  accel_group ,
					  entry[i]->accel_key ,
					  entry[i]->accel_mods );

	  gtk_widget_destroy( entry[i]->button );

	}
      entry[i]->button = NULL;
      
      i++;
      
    }
  
  gtk_widget_realize(window);
}
void 
button_pressed(GtkWidget *, gpointer *data)
{
  if ( display_logs == DISABLED ) return;
  int pressed = (int) data;       // Which button has been pressed ?
  deactivate();                        // deactivate old "Log" object
  active = pressed;                 // set new one
  activate();                           // and activate it
}
void 
request_about()
{
  gtk_window_position(GTK_WINDOW(about_dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show(about_dialog);
}
void 
about_ready(GtkWidget *, gpointer *)
{
  gtk_widget_hide(about_dialog);
};
/*}}}*/

/*{{{  Data management  */
/*
  This routine creates a 1:1 copy of a Log Class Data Array

  The argument is the array to be copied, the return value
  is the copy.
*/
Log**
copy_data_array(Log** array)
{

  if ( array == NULL ) return NULL;
  
  gint i = 0;
  Log** copy = NULL;
  while ( array[i] != NULL )
    {

      copy = add_log_entry( copy,
			    array[i]->mode,
			    array[i]->filename,
			    array[i]->help,
			    array[i]->buttontext,
			    array[i]->plugin,
			    array[i]->interval,
			    array[i]->delay);
      
      gint j = 0;
      while ( array[i]->filter != NULL &&
	      array[i]->filter[j] != NULL )
	{
	  
	  add_filter_to_last( copy ,
			      array[i]->filter[j]->string ,
			      array[i]->filter[j]->mode ,
			      array[i]->filter[j]->execline );

	  j++;
	}
      
      i++;
      
    }
  
  return copy;
}
/*
  This function takes a data array of Log class objects and appends
  one that gets initialized with the given parameters.

  The new array is then returned...
*/
Log**
add_log_entry(Log** array, gint mode, const gchar* filename, const gchar* help, const gchar* buttontext, const gchar* plugin, gint interval, gint delay)
{
  /* how many entries are in the array right now ? */
  gint amount = 0;
  while ( array != NULL && array[amount] != NULL ) amount++;
  
  Log** target = (Log**) new void*[amount+2];
  amount = 0;
  while ( array != NULL && array[amount] != NULL )
    {
      target[amount] = array[amount];
      amount++;
    }
  /* delete old array */
  if ( array != NULL ) delete array;
  
  /* create new entry */
  target[amount] = new Log();
  target[amount+1] = NULL;

  /*
    Initialize new entry with the data we got
  */
  target[amount]->init( filename,
			help,
			buttontext,
			plugin,
			mode,
			interval,
			delay );
  
  return target;
}
/*
  This functions adds the filter specifications (strings & mode)
  to the filter array of the last element in the elements
  structure given as parameter.

  This is done via just searching the last one and adding to
  it via Log::add_filter

  no return value.
 */
void
add_filter_to_last(Log** elements, const gchar* string, gint mode, const gchar* execline)
{
  Log* element = NULL;
  gint x = 0;
  
  while ( elements != NULL && elements[x]!= NULL )
    {
      element = elements[x];
      x++;
    }

  if ( element != NULL )
    {
      element->add_filter(string, mode, execline);
    }

}
/*
  This routine cleanly kills a whole array of
  Log entries.
  
  The parameter is the address of the NULL terminated
  Log class element array, there is no return value.
  
  The checks for array != NULL are being done so the
  routine won't crash even if you give it a NULL
  parameter... this makes sure we don't do stupid things
  here.
*/
void
destroy_log_array(Log** array)
{
  gint x = 0;
  
  while ( array != NULL && array[x] != NULL )
    {
      delete array[x];
      x++;
    }
  
  if ( array != NULL )
    {
      delete array;
    }
  
}
void 
free_memory()
{

  destroy_buttons();

  if ( entry != NULL ) destroy_log_array(entry);
  entry = NULL;

  if ( load_path != NULL )
    {
      delete load_path;
    }
  if ( save_path != NULL )
    {
      delete save_path;
    }
  if ( log_path != NULL )
    {
      delete log_path;
    }
  if ( alert_sound != NULL ) 
    {
      delete alert_sound;
    }
  if ( notice_sound != NULL )
    {
      delete notice_sound;
    }
  if ( uniconify_sound != NULL )
    {
      delete uniconify_sound;
    }
  uniconify_sound = notice_sound = alert_sound = NULL;
  
#if HAVE_MLOCKALL
  if ( lockmem == TRUE ) munlockall();
#endif /* HAVE_MLOCKALL */
  
  delete read_buffer;
  read_buffer = NULL;
  
}
void 
disable()
{
  display_logs = DISABLED;
}
void 
enable()
{
  display_logs = ENABLED;
}
/* routine to determine all filter classes applicable */
gint 
determine_filter_class(gint mode)
{
  int i;
  int fclass = NO_FILTER;
  
  i = 0;
  while ( class1_filters[i] != -1 )
    if ( class1_filters[i++] & mode ) fclass |= CLASS1_FILTER;
  
  i = 0;
  while ( class0_filters[i] != -1 )
    if ( class0_filters[i++] & mode ) fclass |= CLASS0_FILTER;

  return fclass;
}
/*

  resorts the filter elements in an array non-destructive !
  
*/
void 
sort_filters(Filter** filter)
{
  if ( filter == NULL ) return;
  int amount = 0;
  while ( filter[amount] != NULL ) amount++;
  if ( amount == 0 ) return;

  Filter** tmp = (Filter**) new void*[amount+1];
  int i, j, k;
  i = k = 0;
  gint current_mode = 0;
  while ( current_mode != -1 )
    {
      j = 0;
      while ( filter[j] != NULL ){
      if (( filter[j]->mode & CLASS0_MASK ) == current_mode )
	tmp[k++] = filter[j];
      j++;
      }
      current_mode = class0_filters[i++];
    }
  tmp[k] = NULL;
  
  k = 0;
  while ( tmp[k] != NULL )
    {
      filter[k] = tmp[k];
      k++;
    }
  filter[k] = NULL;
  
  delete tmp;
  return;
}
/*}}}*/

/*{{{  Initial preparations  */
/*{{{  Subroutines  */
/*
  outputs usage message
 */
void 
usage()
{
  cout << endl << "The Xlogmaster - version " << VERSION << ", usage: " << endl << endl;
  cout << "xlogmaster [-activate <n>] [-alertcolor <r,g,b>] ";
  cout << "[-audio <on/off>] [-audio-alert <filename>/<off>] ";
  cout << "[-audio-notice <filename>/<off>] [-audio-uniconify <filename>/<off>] ";
  cout << "[-buffer <n>[bkm]] ";
  cout << "[-config <file>] [-database <directory>] ";
  cout << "[-fadeseconds <n>] [-fadesteps <n>] ";
  cout << "[-geometry widthxheight+x+y] [-gtkrc <file>] ";
  cout << "[-help ] ";
#if HAVE_MLOCKALL
  cout << "[-mlockall ] ";
#endif /* HAVE_MLOCKALL */
  cout << "[-notice-follows-mouse [YES/no]] ";
  cout << "[-silent] [-smallicon [YES/no]] [-terse] ";
  cout << "[-version ] [-wordwrap [YES/no]] ";
  cout << endl << endl;
  _exit(0);
}
/*
  outputs help message
*/
void 
help_message()
{
  cout << endl << "The Xlogmaster " << VERSION << endl << endl;
  cout << "The following options are known to the Xlogmaster:" << endl << endl;
  cout << "-help\t\t\tPrint this message and exit" << endl;
  cout << "-version\t\tPrint version info and exit" << endl << endl;
  cout << "-audio <on/off> \tTurns audio support on/off" << endl;
  cout << "-audio-alert <filename>/<off> Sets file for alert to filename or off" << endl;
  cout << "-audio-notice <filename>/<off> Sets file for notice to filename or off" << endl;
  cout << "-audio-uniconify <filename>/<off> Sets file for uniconify to filename or off" << endl;
  cout << "-buffer <n>[bkm]\tSets buffer length to n" << endl;
  cout << "-fadeseconds <n>\tSets seconds to fade from alert" << endl;
  cout << "-fadesteps <n>\t\tSets steps to fade from alert" << endl;
  cout << "-geometry widthxheight+x+y Sets geometry to come up with" << endl;
  cout << "-gtkrc <file>\t\tUses file as the gtkrc" << endl;
#if HAVE_MLOCKALL
  cout << "-mlockall\t\tTurns memory locking on (see manpage before turning on !!) " << endl;
#endif /* HAVE_MLOCKALL */
  cout << "-notice-follows-mouse [YES/no] Sets notice box to follow mouse" << endl;
  cout << "-silent \t\tsame as '-audio off'" << endl;
  cout << "-smallicon [YES/no]\tUse small icon for iconify" << endl;
  cout << "-terse\t\t\tSuppress safety dialogs" << endl;
  cout << "-wordwrap [YES/no]\tSet wordwrap for textbuffer" << endl;
  
  cout << endl << "All arguments are evaluated in the given order";
  cout << endl << "[YES/no] means that YES is default if argument is left out" << endl << endl;
  
  cout << "Report bugs to xlogmaster-bugs@gnu.org" << endl;
  cout << "For more detailed information see manpage or tutorial." << endl << endl;

  _exit(0);
}
/*
  outputs version message
*/
void 
version_message()
{
  cout << endl << "The Xlogmaster " << VERSION << endl;
  cout << "Copyright (C) 1998 by Georg C. F. Greve" << endl;
  cout << "This is a GNU program" << endl;
  cout << "The Xlogmaster comes with NO WARRANTY," << endl;
  cout << "to the extent permitted by law." << endl;
  cout << "You may redistribute copies of the Xlogmaster" << endl;
  cout << "under the terms of the GNU General Public License." << endl;
  cout << "For more information about these matters," << endl;
  cout << "see the files named COPYING." << endl << endl;
  _exit(0);
}
/*
  evaluates GTK+ colorstring (r,g,b within 0-1) each value sepatared
  by a ',' and puts the values into the c_alert array.
*/
void 
eval_colorstring(char* string)
{
  c_alert[0] = c_alert[1] = c_alert[2] = 0;
  int rgb = 0;
  int x = 0;
  double base = 1;
  while( string[x] != 0 && rgb <= 2 ){
    char a = string[x];
    if ( a == '.' ){
      base = 0.1;
    } else if ( a == ',' ){
      rgb++;
      base = 1;
    } else {
      a -= '0';
      if ( a >= 0 && a <= 9 ){
	c_alert[rgb] += base * (double) a;
	if ( base < 1 ) base /= 10;
	else base *= 10;
      }
    }
    x++;
  }
}
/*
  this routine takes a string as the first parameter and starts converting
  it to a numeric value (understanding b,k,m for bytes, kilobytes, 
  megabytes). The conversion is started at position "index" (2nd parameter)
  and the return value is the retrieved numeric value
*/
long 
get_value(char* string, int* index)
{
  long value = 0;
  int y = *index;
  char a;
  a = string[y] - '0';
  while ( ( a >= 0 && a <= 9 ) ){
    value *= 10;
    value += a;
    y++;
    a = string[y] - '0';
  }
  *index = y+1;
  if ( string[y] == 'k' ) value *= 1024;
  else if ( string[y] == 'm' ) value *= 1048576;
  else *index = y;
  return value;
}
/*
  This function takes care that the paths for the configuration
  and the database are being set correctly
*/
void
evaluate_paths()
{
  /* evaluate compile-time settings */
  config_dir = new gchar[ strlen(XLM_HOME)+2 ];
  database_dir = new gchar[ strlen(XLM_DB)+2 ];
  lib_dir = new gchar[ strlen(XLM_LIB)+2 ];
  strcpy( config_dir, XLM_HOME );
  strcpy( database_dir, XLM_DB );
  strcpy( lib_dir, XLM_LIB );

  /* fill in the path for the users private database */
  gchar* hdir = NULL;
  hdir = getenv("HOME");
  if ( hdir == NULL ) hdir = getenv("USER");
  if ( hdir == NULL ) hdir = getenv("PWD");
  if ( hdir == NULL ) hdir = ".";
  private_database_dir = 
    new gchar[ strlen( hdir ) + strlen( XLM_USER_DB ) + 2 ];
  strcpy( private_database_dir , hdir );
  strcat( private_database_dir , "/" );
  strcat( private_database_dir , XLM_USER_DB );

  /* evaluate the environment variables */
  gchar* env_conf = getenv( "XLM_HOME" );
  gchar* env_db = getenv( "XLM_DB" );
  gchar* env_user_db = getenv( "XLM_USER_DB" );
  gchar* env_lib = getenv( "XLM_LIB" );

  if ( env_conf != NULL )
    {
      delete config_dir;

      config_dir = new gchar[ strlen(env_conf)+2 ];
      strcpy( config_dir, env_conf );

    }

  if ( env_db != NULL )
    {
      delete database_dir;

      database_dir = new gchar[ strlen(env_db)+2 ];
      strcpy( database_dir, env_db );

    }

  if ( env_user_db != NULL )
    {
      delete private_database_dir;

      private_database_dir = new gchar[ strlen(env_user_db)+2 ];
      strcpy( private_database_dir, env_user_db );

    }

  if ( env_lib != NULL )
    {
      delete lib_dir;
      
      lib_dir = new gchar[ strlen(env_lib)+2 ];
      strcpy( lib_dir, env_lib );
      
    }

}
/*}}}*/
void 
init(int argc,char* argv[])
{
/*{{{  Set startup defaults  */
  /* we never want to get killed by sigpipes here, so
     we rather ignore it for the whole Xlogmaster */
  signal( SIGPIPE , SIG_IGN );

  /* first set the configuration directories to sane defaults */
  evaluate_paths();

  entry = NULL; //
  syslogs = 0; // Nothing to display (yet)

  request_active = 0; // By default the first log is being displayed  
  maxtext = MAXTEXT; // Default from config.H
  read_buffer = NULL;
  wordwrap = WORDWRAP; // Default from config.H
  small_icon = ICON_SMALL;
  xpos = ypos = -1;
  width = 400;
  height = 250;
  load_path = save_path = log_path = NULL;
  lockmem = FALSE;
  notice_follows_mouse = FALSE; // no following around is default... most people will
  // find it too annoying.
  configuration_changed = FALSE; // no change of course...
  buttons_shown = TRUE;             // the buttons are always shown at startup

  /* for execute.cc: */
  cleanup_tag = pid_reference = 0;
  pid_list = NULL;

  /* for audio.cc: */
  alert_sound = new char[ strlen( lib_dir ) + strlen("/sound/") + 20 ];
  notice_sound = new char[ strlen( lib_dir ) + strlen("/sound/") + 20 ];
  uniconify_sound = new char[ strlen( lib_dir ) + strlen("/sound/") + 20 ];
  strcpy(alert_sound, lib_dir);
  strcat(alert_sound, "/sound/");
  strcpy(notice_sound, alert_sound);
  strcpy(uniconify_sound, alert_sound);
  strcat(alert_sound, "alert");
  strcat(notice_sound, "notice");
  strcat(uniconify_sound, "uniconify");
  sound = AUDIO; // comes from autoconf
  
  /* alert color */
  char* cstring = new char[strlen(XLM_ALERT_COLOR)+2];
  strcpy(cstring, XLM_ALERT_COLOR);
  int k = 0;
  while ( cstring[k] != 0 ){
    if ( cstring[k] == '-' ) cstring[k] = ',';
    k++;
  }
  eval_colorstring(cstring);
  delete cstring;

  /* dabatase menu form */
  db_form = NULL;

/*}}}*/

/*{{{  set PATH variable  */
/*
  modify the PATH environment variable to contain the scripts
  directory - this way the scripts don't need path information for
  getting executed...
*/
 gchar* system_path = NULL;
 system_path = (gchar*) getenv("PATH");
 gchar* xlm_path;
 xlm_path = (gchar*) new char [ strlen( lib_dir ) + strlen( system_path ) + 20 ];
 strcpy(xlm_path, lib_dir);
 strcat(xlm_path, "/scripts:");
 strcat(xlm_path, system_path);
 my_setenv( "PATH", xlm_path);
 delete xlm_path;
/*}}}*/

/*{{{  my own commandline things  */
 gint  x;
 gint opt_index;
 int gtkrc = FALSE;
 terse = FALSE;
 
 x = getopt_long_only(argc, argv, "", options, &opt_index);
 while ( x != -1 && x != '?' ){
   const gchar* opt = options[opt_index].name;
   
   if ( ! strcmp( opt, "activate" ) )
     {
       if ( optarg == NULL ) usage();
       int index = 0;
       request_active = get_value( optarg, &index ) - 1 ;
     } 
   else if ( ! strcmp( opt, "alertcolor" ) )
     {
       if ( optarg == NULL ) usage();
       eval_colorstring(optarg);
     } 
   else if ( ! strcmp( opt, "audio" ))
     { 
       if ( optarg == NULL ) usage();
       if ( ! strcmp( optarg, "on" ) ) sound = TRUE;
       if ( ! strcmp( optarg, "ON" ) ) sound = TRUE;
       if ( ! strcmp( optarg, "off" ) ) sound = FALSE;
       if ( ! strcmp( optarg, "OFF" ) ) sound = FALSE;
       if ( sound == FALSE ){
	 if ( alert_sound != NULL ) delete alert_sound;
	 if ( notice_sound != NULL ) delete notice_sound;
	 if ( uniconify_sound != NULL ) delete uniconify_sound;
	 uniconify_sound = notice_sound = alert_sound = NULL;
       }
     }
   else if ( ! strcmp( opt, "audio-alert" ) )
     { 
       if ( optarg == NULL ) usage();
       if ( ! strcmp( optarg, "off" ) ) alert_sound = NULL;
       else if ( ! strcmp( optarg, "OFF" ) ) alert_sound = NULL;
       else {
	 alert_sound = new char[strlen(optarg)+1];
	 strcpy(alert_sound, optarg);
	 sound = TRUE;
       }
     }
   else if ( ! strcmp( opt, "audio-notice" ) )
     { 
       if ( optarg == NULL ) usage();
       if ( ! strcmp( optarg, "off" ) ) notice_sound = NULL;
       else if ( ! strcmp( optarg, "OFF" ) ) notice_sound = NULL;
       else {
	 notice_sound = new char[strlen(optarg)+1];
	 strcpy(notice_sound, optarg);
	 sound = TRUE;
       }
     } 
   else if ( ! strcmp( opt, "audio-uniconify" ) )
     { 
       if ( optarg == NULL ) usage();
       if ( ! strcmp( optarg, "off" ) ) uniconify_sound = NULL;
       else if ( ! strcmp( optarg, "OFF" ) ) uniconify_sound = NULL;
       else {
	 uniconify_sound = new char[strlen(optarg)+1];
	 strcpy(uniconify_sound, optarg);
	 sound = TRUE;
       }
     } 
   else if ( ! strcmp( opt, "buffer" ) )
     {
       if ( optarg == NULL ) usage();
       int index = 0;
       maxtext = get_value( optarg, &index );
     }
   else if ( ! strcmp( opt, "config" ) )
     {
       if ( optarg == NULL ) usage();
       entry = read_configuration_file(optarg);
     }
   else if ( ! strcmp( opt, "database" ) )
     {
       if ( optarg == NULL ) usage();
       delete database_dir;
       database_dir = new gchar[ strlen(optarg) + 2 ];
       strcpy( database_dir, optarg );
     }
   else if ( ! strcmp( opt, "fadeseconds" ) )
     {
       if ( optarg == NULL ) usage();
       int index = 0;
       int seconds = 0;
       seconds = get_value( optarg, &index );
       if ( seconds < 1 ) seconds = 1;
       fade_step = (-1.0 * ( (gdouble) fade_base / ( 10 * seconds )));
     }
   else if ( ! strcmp( opt, "fadesteps" ) )
     {
       if ( optarg == NULL ) usage();
       int index = 0;
       steps = get_value( optarg, &index );
     }
   else if ( ! strcmp( opt, "geometry" ) )
     {
       if ( optarg == NULL ) usage();
       int index = 0;
       char* string = optarg;
       
       if ( string[index] != 'x' )
	 width = get_value( optarg, &index );
       if ( string[index] == 'x' ){
	 index++;
	 height = get_value( optarg, &index );
       }
       if ( string[index] == '+' ){
	 index++;
	 xpos = get_value( optarg, &index );
       }
       if ( string[index] == '+' ){
	 index++;
	 ypos = get_value( optarg, &index );
       }
     }
   else if ( ! strcmp( opt, "gtkrc" ) )
     {
       if ( optarg == NULL ) usage();
       gtkrc = TRUE;
       gtk_rc_parse(optarg);
     }
   else if  ( ! strcmp( opt, "help" ) )
     {
       help_message();
     }
#if HAVE_MLOCKALL
   else if ( ! strcmp( opt, "mlockall" ) )
     { 
       lockmem = TRUE;
     }
#endif /* HAVE_MLOCKALL */
   else if ( ! strcmp( opt, "notice-follows-mouse" ) )
     { 
       if ( optarg == NULL )
	 {
	   notice_follows_mouse = TRUE;
	 }
       else
	 {
	   if ( ! strcmp( optarg, "yes" ) ) notice_follows_mouse = TRUE;
	   if ( ! strcmp( optarg, "YES" ) ) notice_follows_mouse = TRUE;
	   if ( ! strcmp( optarg, "no" ) ) notice_follows_mouse = FALSE;
	   if ( ! strcmp( optarg, "NO" ) ) notice_follows_mouse = FALSE;
	 }
     }
   else if ( ! strcmp( opt, "silent" ) )
     { 
       sound = FALSE;
     }
   else if ( ! strcmp( opt, "smallicon" ) )
     { 
       if ( optarg == NULL )
	 {
	   small_icon = TRUE;
	 }
       else
	 { 
	   if ( ! strcmp( optarg, "yes" ) ) small_icon = TRUE;
	   if ( ! strcmp( optarg, "YES" ) ) small_icon = TRUE;
	   if ( ! strcmp( optarg, "no" ) ) small_icon = FALSE;
	   if ( ! strcmp( optarg, "NO" ) ) small_icon = FALSE;
	 }
     }
   else if ( ! strcmp( opt, "terse" ) )
     { 
       terse = TRUE;
     }
   else if ( ! strcmp( opt, "version" ) )
     {
       version_message();
     }
   else if ( ! strcmp( opt, "wordwrap" ) )
     { 
       if ( optarg == NULL )
	 {
	   small_icon = TRUE;
	 }
       else
	 { 
	   if ( ! strcmp( optarg, "yes" ) ) wordwrap = TRUE;
	   if ( ! strcmp( optarg, "YES" ) ) wordwrap = TRUE;
	   if ( ! strcmp( optarg, "no" ) ) wordwrap = FALSE;
	   if ( ! strcmp( optarg, "NO" ) ) wordwrap = FALSE;
	 }
     }
   
   x = getopt_long_only(argc, argv, "", options, &opt_index);
 };
 
 if ( optind < argc || x == '?' )
   usage();
 
/*}}}*/

/*{{{  System-wide GTKRC  */
 if ( gtkrc == FALSE )
   {
     char* gtkrc_fn;
     gtkrc_fn = new char[ strlen( config_dir ) + strlen( GTKRC ) + 3 ];
     strcpy(gtkrc_fn, config_dir);
     strcat(gtkrc_fn, "/");
     strcat(gtkrc_fn, GTKRC);
     gtk_rc_parse(gtkrc_fn);
     delete gtkrc_fn;
   }
/*}}}*/

/*{{{  User specific config file  */
 /*
   read only if we didn't read an explicit config file
 */
 if (  entry == NULL )
   {
     gchar* fname;
     gchar* hdir = NULL;
     hdir = getenv("HOME");
     if ( hdir == NULL ) hdir = getenv("USER");
     if ( hdir == NULL ) hdir = getenv("PWD");
     if ( hdir == NULL ) hdir = ".";
     fname = new gchar[ strlen( hdir ) + strlen( CONFIGFILE ) + 2 ];
     strcpy ( fname, hdir );
     strcat ( fname, "/" );
     strcat ( fname, CONFIGFILE );
     entry = read_configuration_file(fname);
     delete fname;
   }
/*}}}*/

/*{{{  System wide config file  */
  /*
    read only if we didn't read an explicit or private
    config file
  */
 if (  entry == NULL )
   {
     gchar* fname;     
     fname = new gchar[ strlen( config_dir ) + strlen( SYSCONFIGFILE ) + 3 ];
     strcpy ( fname, config_dir );
     strcat ( fname, "/" );
     strcat ( fname, SYSCONFIGFILE );
     entry = read_configuration_file(fname);
     delete fname;
   }
/*}}}*/

/*{{{  Some entries for new users  */
  if ( entry == NULL )
    {
      /*
	Alright - since we don't want to pop up an empty window without
	any boxes we better find something to display... :-)

	This also prevents a critical error in display_buttons() that
	gets triggered if there is nothing to display...
      */


      /*
	/var/log/messages should be fairly standard
      */
      entry = add_log_entry( entry,
			     TAIL_FILE,
			     "/var/log/messages",
			     "Display /var/log/messages file...",
			     "System Messages",
			     NULL,
			     INTERVAL,
			     6 );

      /*
	same for /var/log/boot.msg
      */
      entry = add_log_entry( entry,
			     TAIL_FILE,
			     "/var/log/boot.msg",
			     "Display /var/log/boot.msg file...",
			     "Bootup Messages",
			     NULL,
			     10000,
			     6 );
      
      /*
	a nice one is also /proc/meminfo
      */
      entry = add_log_entry( entry,
			     CAT_FILE,
			     "/proc/meminfo",
			     "Display memory usage via /proc/meminfo",
			     "Memory Status",
			     NULL,
			     10,
			     6 );
      
      /*
	'df' should exist on all systems, too...
      */
      entry = add_log_entry( entry,
			     RUN_FILE,
			     "df",
			     "Display Disk usage via running 'df'",
			     "Harddisk Usage",
			     NULL,
			     3,
			     15 );

      
      /*
	'w' isn't too exotic, either...
      */
      entry = add_log_entry( entry,
			     RUN_FILE,
			     "w",
			     "Display user information by running 'w'",
			     "Who's Online",
			     NULL,
			     3,
			     10 );
      
      /*
	'pstree' is pretty cool, as well...
      */
      entry = add_log_entry( entry,
			     RUN_FILE,
			     "pstree",
			     "Display information about running processes via 'pstree'",
			     "Process Tree",
			     NULL,
			     3,
			     15 );
    }
/*}}}*/

#if HAVE_MLOCKALL
  // if user requests so, try to lock the xlogmaster in memory...
  if ( lockmem == TRUE )
    {
      cout << endl << "Trying to lock the Xlogmaster in memory.... ";
      
   int result = mlockall(MCL_FUTURE);  

   if ( result == -1 )
     {
       cout << "failed. Insufficient permission ?" << endl;
       cout << "see the manpage for more information !" << endl;
     } 
   else 
     {
       cout << "success." << endl;
       cout << "The Xlogmaster will not get swapped out." << endl;
     }
    }
#endif /* HAVE_MLOCKALL */
  
  /*
    check out the buffer size so it doesn't get bigger
    than SSIZE_MAX
  */
  if ( maxtext > SSIZE_MAX)
    {
      maxtext = SSIZE_MAX;
    }
  
  /*
    allocate buffer
  */
  read_buffer = new char[maxtext+2];
}
/*}}}*/

/*{{{  Text output subroutines  */
/*
  This routine adds text at the current insertion point into
  the GtkText widget while applying all CLASS0 Filters.

  The parameters are:
  o the GtkText Widget to use
  o the buffer to insert
  o the buffersize (buffer zero termination is neither used nor
  checked)

  the routine automagically applies all filters from the
  "active" entry...

*/
void 
output_text(GtkWidget* target, char* buffer, long size)
{
  /*
    If no filter has been set we can just output and bail out
   */
  if ( entry[active]->filter == NULL ||
       entry[active]->filterclass == NO_FILTER ){
    gtk_text_insert (GTK_TEXT(target),NULL,NULL,NULL,buffer,size);
    return;
  }
  
  /*
    alright - there is a filter, so we need to process the output first.
  */
  
  buffer[size] = 0;
  GdkColor *raise, *lower;
  GdkColor *raise_bg, *lower_bg;
  GtkStyle *style;
  style = gtk_widget_get_style( target );
  raise = &style->fg[GTK_STATE_PRELIGHT];
  raise_bg = &style->bg[GTK_STATE_PRELIGHT];
  lower = &style->fg[GTK_STATE_INSENSITIVE];
  lower_bg = &style->bg[GTK_STATE_INSENSITIVE];
  glong start = 0;
  glong stop = start;
  glong new_stop = stop;

  while ( stop < size )
    {
   
      new_stop = match_all( entry[active]->filter, 
			    CLASS0_FILTER, 
			    buffer, 
			    stop, 
			    size);

      gint action = NO_CHANGE;
      gint i = 0;
      while ( entry[active]->filter[i] != NULL )
	{
	  
	  if ( entry[active]->filter[i]->regex_status == 0 )
	    {
	      action |= entry[active]->filter[i]->mode;
	    }
	  
	  i++;
	}

      /*
	mask out things that are not CLASS0_FILTERS,
	afterwards actions has the bit for every applicable
	CLASS0_FILTER set.
       */
      action &= CLASS0_MASK;


      /*
	if action is not NO_CHANGE we need to do something about
	that line, so here is our routine for that:
      */
      
      if ( action != NO_CHANGE )
	{

	  /* 
	     first output the text until the matching line
	     begins (if there is any)
	  */
	  if ( start < stop )
	    {
	      gtk_text_insert (GTK_TEXT(target),NULL,NULL,NULL,
			       (char*) (buffer+start), (stop-start));
	    }

	  /*
	    now set "start" and "stop" index to match the line
	    we're just in...
	  */
	  start = stop;
	  stop = new_stop;


	  /*
	    Now process the special line,
	    the priority is HIDE over RAISE over LOWER
	  */

	  if ( action & HIDE )
	    {
	      
	      /*
		just do nothing....
	      */
	      
	    } 
	  else if ( action & RAISE )
	    {
	      
	      gtk_text_insert (GTK_TEXT(target),NULL, raise, raise_bg,
			       (char*) (buffer+start), (stop-start)+1);
	      
	    } 
	  else if ( action & LOWER )
	    {
	      
	      gtk_text_insert (GTK_TEXT(target),NULL, lower, lower_bg,
			       (char*) (buffer+start), (stop-start)+1);
	      
	    }
	  
	  stop = new_stop + 1;
	  start = stop;
	}
      
      stop = new_stop + 1;
      
    }
  
  /*
    output the remaining normal text (no special matches found)
  */
  if ( start != stop )
    gtk_text_insert ( GTK_TEXT(target),NULL,NULL,NULL,
		      (char*) (buffer+start), (stop-start) );
  

}
/*}}}*/

/*{{{  REGEX Matching Routines  */
/*
  REGEX match all filters on one line routine

  The matching result for each filter element is returned in it's
  filter->regex_status (either zero or -1)

  The parameters for this function are:
  o pointer to a NULL terminated array of filter elements.
  o the filter class that is of interest for us.
  o the pointer to the buffer
  o position of line in buffer to start search at
  o length of buffer ( the routine will also realize a zero termination,
  though)

  the return value is the position of the next line in the buffer ( =size if the 
  matching is complete)

*/
glong
match_all(Filter** filter, gint want_class, gchar* buffer, glong index, glong size)
{
  gchar* line;
  glong end = index + 1;

  /*
    search for end of current line
  */
  while ( buffer[end] != 0 && end < size &&
	  buffer[end] != '\n' && buffer[end] != '\r' ) end++;
  /*
    now go to beginning of next one
  */
  while ( buffer[end+1] != 0 && ( end + 1 ) < size &&
	  ( buffer[end+1] == '\n' || buffer[end+1] == 10 ) ) end++;
  
  /* allocate memory */
  line = new gchar[ end - index + 5 ];

  /*
    copy buffer into line
  */
  gint x = 0;
  while ( ( index + x ) < end )
    {
      line[x] = buffer[ index + x ];
      x++;
    }
  line[x] = 0; // force zero termination
   
  /*
    if we don't have a filter array we are done already...
  */
  if ( filter == NULL )
    {
      delete line;
      return end;
    }

  /*
    now start the matching
  */
  x = 0;
  while ( filter[x] != NULL )
    {
      
      /*
	only match if the filter has an element of the desired class
	otherwise it is -1 !
      */
      if ( determine_filter_class(filter[x]->mode) & want_class )
	{
	  filter[x]->regex_status = match_line(filter[x], line);
	}
      else
	{
	  filter[x]->regex_status = REG_NOMATCH;
	}
      
      x++;
    }
  
  /*
    alright, that's it... we're done. Free string element "line" and return
    our end position.
  */
  delete line;

  return end;
}
/*
  REGEX line matching routine
  
  The matching result is the returned returned value
  ( either zero for a successful match or -1 )
  
  the return glong value is the index position for the next line
  the parameters are
  o the filter entry that should be searched
  o the line to search - needs to be zero terminated !!!
  
*/
gint
match_line(Filter* filter, gchar* line)
{
  if ( filter->regex_pattern == NULL )
    {
      return REG_NOMATCH;
    }

  gint result = regexec( filter->regex_pattern,
			 line,
			 0,
			 NULL,
			 0 );
  
  if ( filter->mode & INVERT )
    {
      if ( result == 0 )
	result = REG_NOMATCH;
      else
	result = 0;
    }
  
  return result;
}
/*}}}*/

/*{{{  Error procedures  */
void 
file_error(gchar* file)
{
  /* give stderr message */
  cerr << PACKAGE << ": file access error. Could not open" << endl;
  cerr << "  " << file << endl;

  /* and now popup dialog */
  gchar* opener = "\n      Error while trying to access file      \n\n       ";
  gchar* closer = "       \n\n    please check path and permissions.      \n";
  gchar* text = new gchar[strlen(file) + strlen(opener) + strlen(closer) + 10 ];
  strcpy ( text, opener );
  strcat ( text, file );
  strcat ( text, closer );

  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  buttons = g_list_append(buttons,
			  (gpointer) "    DISMISS    ");
  functions = g_list_append(functions, NULL );

  popup_dialog("File Access Error", text, buttons, functions);
  delete text;
}
void 
fork_error()
{
  /* give stderr message */
  cerr << PACKAGE << ": fork error - could not spawn process." << endl;
  
  /* and now popup dialog */
  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  buttons = g_list_append(buttons,
			  (gpointer) "    DISMISS    ");
  functions = g_list_append(functions, NULL );
  
  popup_dialog("Fork Error", "\n     Error while trying to spawn a new process !!!      \n", buttons, functions);
}
void 
execute_error(gchar* file)
{
  /* give stderr message */
  cerr << PACKAGE << ": execution error. Could not run" << endl;
  cerr << "  " << file << endl;
  cerr << "  " << "due to fork-problems." << endl;

  /* and now popup dialog */
  gchar* opener = "\n      Error while trying to run program      \n\n       ";
  gchar* closer = "       \n\n    due to fork problems. Insufficient memory ?      \n";
  gchar* text = new gchar[strlen(file) + strlen(opener) + strlen(closer) + 10 ];
  strcpy ( text, opener );
  strcat ( text, file );
  strcat ( text, closer );

  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  buttons = g_list_append(buttons,
			  (gpointer) "    DISMISS    ");
  functions = g_list_append(functions, NULL );
  
  popup_dialog("Execution Error", text, buttons, functions);
  delete text;
}
void 
plugin_overflow(gchar* file)
{
  /* give stderr message */
  cerr << PACKAGE << ": plugin pipe overflow for pipe into" << endl;
  cerr << "  " << file << endl;

  /* and now popup dialog */
  gchar* opener = "\n      Error in pipe into plugin      \n\n       ";
  gchar* closer = "       \n\n    ,the input pipe had a buffer overflow.   \n";
  gchar* text = new gchar[strlen(file) + strlen(opener) + strlen(closer) + 10 ];
  strcpy ( text, opener );
  strcat ( text, file );
  strcat ( text, closer );

  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  buttons = g_list_append(buttons,
			  (gpointer) "    DISMISS    ");
  functions = g_list_append(functions, NULL );
  
  popup_dialog("Plugin Error", text, buttons, functions);
  delete text;
}



/*}}}*/

/*{{{  About Xlogmaster  */
void 
create_about()
{
  GtkWidget* button;
  about_dialog = gtk_window_new( GTK_WINDOW_DIALOG );
  gtk_signal_connect (GTK_OBJECT (about_dialog), "destroy",
		      GTK_SIGNAL_FUNC (about_ready), NULL);
  gtk_signal_connect (GTK_OBJECT (about_dialog), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  gtk_window_set_title (GTK_WINDOW (about_dialog), "About the Xlogmaster" );
  GtkWidget* vbox = gtk_vbox_new(FALSE, 5);
  gtk_container_add(GTK_CONTAINER (about_dialog), vbox);
  gtk_widget_show(vbox);
  button = gtk_button_new_with_label ("Resume");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      GTK_SIGNAL_FUNC (about_ready), 0 );
  gtk_box_pack_end (GTK_BOX (vbox), button, TRUE, TRUE, 5);
  gtk_tooltips_set_tip(tooltips, button, "...back to watching all those cool logfiles... :-)" ,"...back to watching all those cool logfiles... :-)");
  gtk_widget_show (button);

  GtkWidget* label;

  GdkPixmap *pixmap;
  GdkBitmap *mask;
  GtkWidget *logo_xpm;
  GtkStyle *style;
  style = gtk_widget_get_style( window );
  pixmap = (GdkPixmap*)
    gdk_pixmap_create_from_xpm_d( window->window,
				  &mask,
				  &style->bg[GTK_STATE_NORMAL],
				  logo);

  logo_xpm = gtk_pixmap_new (pixmap, mask);
  gtk_box_pack_start (GTK_BOX (vbox), logo_xpm,TRUE, TRUE, 0);
  gtk_widget_show(logo_xpm);    

  /*
    Make the pixmap the default for Iconification... :-)
  */
  gtk_widget_realize (window);
  
  GdkPixmap *pixmap_small;
  GdkBitmap *mask_small;
  
  pixmap_small = (GdkPixmap*)
    gdk_pixmap_create_from_xpm_d( window->window,
				  &mask_small,
				  &style->bg[GTK_STATE_NORMAL],
				  logo_small);

  small_logo_xpm = gtk_pixmap_new (pixmap_small, mask_small);
  
  if ( small_icon == TRUE ){
    gdk_window_set_icon (window->window, NULL,
			 pixmap_small, mask_small);
  } else {
    gdk_window_set_icon (window->window, NULL,
			 pixmap, mask);
  }
  gdk_window_set_icon_name(window->window, "The Xlogmaster");
  
#define VERS_INTRO "Version "
  char* versionlabel = new char[ strlen(VERSION) + strlen(VERS_INTRO) + 2];
  strcpy(versionlabel, VERS_INTRO);
  strcat(versionlabel, VERSION);
  label = gtk_label_new ( versionlabel );
  delete versionlabel;
  gtk_box_pack_start (GTK_BOX (vbox), label
		      ,TRUE, TRUE, 0);
  gtk_widget_show (label);

  label = gtk_label_new ( " \n     Copyright (C) 1998 Georg C. F. Greve \n     This is a GNU program \n   \n   This program is distributed in the hope that it will be useful,   \n but WITHOUT ANY WARRANTY; without even the implied warranty of \n MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \n GNU General Public License for more details. \n     \n \n If you experience any problems, discover any bugs, \n think some new feature would be great or just want to say \n how much you like this program, write an email to: \n \n   xlogmaster-bugs@gnu.org   \n \n \n Thanks to the GTK+ crew for writing this great toolkit, I like \n it a lot - and to my girlfriend Julia for liking me even when \n I'm in a coding frenzy & for the great logo... ;-) \n");
  gtk_box_pack_start (GTK_BOX (vbox), label
		      ,TRUE, TRUE, 0);
  gtk_widget_show (label);

 
}
/*}}}*/
