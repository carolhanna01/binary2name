/*

  menu.cc

  menu subroutines for xlogmaster.cc
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
#include "menu.H"
#include "extern.H"

/*}}}*/

/*{{{  Create Menu  */
/*
  This function is called during initialisation to create
  the popup menu 
*/
void 
fire_up_menu(GtkWidget* widget)
{
  // set up starting defaults for the menus:
  customizing = FALSE;

  // create menu, no need to "show" it
  menu = gtk_menu_new();

  // create entries
  load_config = gtk_menu_item_new_with_label("Load Configuration");
  save_config = gtk_menu_item_new_with_label("Save Configuration");
  customize_entry = gtk_menu_item_new_with_label("Customize Entries");
  about = gtk_menu_item_new_with_label("About Xlogmaster");
  quit_menu = gtk_menu_item_new_with_label("Quit");
  separator1 = gtk_menu_item_new();
  separator2 = gtk_menu_item_new();
  separator3 = gtk_menu_item_new();

  // and put them into the menu
  gtk_menu_append( GTK_MENU(menu), about );
  gtk_menu_append( GTK_MENU(menu), separator1 );
  gtk_menu_append( GTK_MENU(menu), customize_entry );
  gtk_menu_append( GTK_MENU(menu), separator2 );
  gtk_menu_append( GTK_MENU(menu), load_config );
  gtk_menu_append( GTK_MENU(menu), save_config );
  gtk_menu_append( GTK_MENU(menu), separator3 );
  gtk_menu_append( GTK_MENU(menu), quit_menu );

  // attach the callbacks:
  gtk_signal_connect_object( GTK_OBJECT(about), "activate",
			     GTK_SIGNAL_FUNC(about_proc), 
			     NULL );
  gtk_signal_connect_object( GTK_OBJECT(load_config), "activate",
			     GTK_SIGNAL_FUNC(load_conf_proc), 
			     NULL );
  gtk_signal_connect_object( GTK_OBJECT(save_config), "activate",
			     GTK_SIGNAL_FUNC(save_conf_proc), 
			     NULL );
  gtk_signal_connect_object( GTK_OBJECT(customize_entry), "activate",
			     GTK_SIGNAL_FUNC(customize_entry_proc), 
			     NULL );
  gtk_signal_connect_object( GTK_OBJECT(quit_menu), "activate",
			     GTK_SIGNAL_FUNC(quit), 
			     NULL );

  // and show the items:
  gtk_widget_show( about );
  gtk_widget_show( load_config );
  gtk_widget_show( save_config );
  gtk_widget_show( customize_entry );
  gtk_widget_show( quit_menu );
  gtk_widget_show( separator1 );
  gtk_widget_show( separator2 );
  gtk_widget_show( separator3 );

  /*
    Create keyboard accelerators
  */
  /* About is ALT-A */
  gtk_widget_add_accelerator ( about ,
			       "activate" ,
			       accel_group ,
			       'A' ,
			       GDK_MOD1_MASK ,
			       GTK_ACCEL_VISIBLE );

  /* Customize is ALT-C */
  gtk_widget_add_accelerator ( customize_entry ,
			       "activate" ,
			       accel_group ,
			       'C' ,
			       GDK_MOD1_MASK ,
			       GTK_ACCEL_VISIBLE );

  /* Load is ALT-L */
  gtk_widget_add_accelerator ( load_config ,
			       "activate" ,
			       accel_group ,
			       'L' ,
			       GDK_MOD1_MASK ,
			       GTK_ACCEL_VISIBLE );

  /* Save is ALT-S */
  gtk_widget_add_accelerator ( save_config ,
			       "activate" ,
			       accel_group ,
			       'S' ,
			       GDK_MOD1_MASK ,
			       GTK_ACCEL_VISIBLE );

  /* Quit is ALT-Q */
  gtk_widget_add_accelerator ( quit_menu ,
			       "activate" ,
			       accel_group ,
			       'Q' ,
			       GDK_MOD1_MASK ,
			       GTK_ACCEL_VISIBLE );

  /* attach to handler for popup */
  gtk_signal_connect (GTK_OBJECT(widget), "button_press_event", 
		      GTK_SIGNAL_FUNC (popup_handler), 
		      menu);

}
/*}}}*/

/*{{{  Menu handling subroutines  */
static gint 
popup_handler(GtkWidget *widget, GdkEventButton *event, GtkWidget *menu)
{

  if ( event->button == 2 )
    {      
      hide_show_buttons_proc();
      
      return TRUE;
    }

  if (event->button == 3) 
    { 
      gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "button_press_event");
      
      gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,
		      event->button, event->time);
      return TRUE;
    }
  return FALSE;
}
/*}}}*/

/*{{{  hide_show_buttons procedure  */
/* 
   This function toggles the buttonbox in the main window to be
   hidden or shown (toggle).
   It keeps the lower border of the text scrollbar on the same relative position
   to the end - everything else would probably be pretty annoying.
*/
void 
hide_show_buttons_proc()
{
  GtkWidget* bbox = (GtkWidget*) gtk_object_get_data( GTK_OBJECT(window), BUTTONBOX);  

  /* memorize old scrollbar position (lower border) relative to end */
  GtkAdjustment* ver_text_adj = (GtkAdjustment*) ( GTK_RANGE(textscrollbar)->adjustment );
  gfloat old_position = ver_text_adj->upper - ( ver_text_adj->value + ver_text_adj->page_size );
  
  if ( buttons_shown == TRUE )
    {
      gtk_widget_hide( bbox );
      buttons_shown = FALSE;
    } 
  else 
    {
      gtk_widget_show( bbox );
      buttons_shown = TRUE;
    }
  
  gtk_widget_realize(window);
  gtk_widget_realize(textwindow);
  gtk_widget_map(textwindow);
  gtk_widget_realize(textscrollbar);
  gtk_widget_map(textscrollbar);

  /* Now put scrollbar on same position relative to end */
  gtk_adjustment_set_value(ver_text_adj, 
			   ver_text_adj->upper - old_position - ver_text_adj->page_size);

}
/*}}}*/

/*{{{  Load Configuration  */
void 
load_conf_proc(GtkWidget*, gpointer*)
{
  if ( customizing == TRUE || filew != 0 )
    {
      GList* buttons = g_list_alloc();
      GList* functions = g_list_alloc();
      buttons = g_list_append(buttons,
			      (gpointer) "    DISMISS    ");
      functions = g_list_append(functions, NULL );
      
      popup_dialog("Erratic Request", "\n     Unable to load configuration      \n      at this point in program !      \n", buttons, functions);
      
      return;
    }
  
  if ( configuration_changed == FALSE || terse == TRUE )
    {
      do_load();
      return;
    }
  
  /* the configuration has been changed really load ? */
  
  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  
  buttons = g_list_append(buttons,
			  (gpointer) "    YES    ");
  functions = g_list_append(functions,
			    (gpointer) do_load );
  buttons = g_list_append(buttons,
			  (gpointer) "    NO    ");
  functions = g_list_append(functions, NULL );

  popup_dialog
    ("Really Load ?", 
     "\n      There are unsaved changes      \n      loading will cause a loss of changes      \n\n      Proceed ?      \n",
     buttons, functions);
}
void 
do_load()
{
  if ( customizing == TRUE || filew != 0 )
    {
      
      GList* buttons = g_list_alloc();
      GList* functions = g_list_alloc();
      buttons = g_list_append(buttons,
			      (gpointer) "    DISMISS    ");
      functions = g_list_append(functions, NULL );
      
      popup_dialog("Erratic Request", "\n     Unable to load new configuration      \n      at this point in program execution !      \n", buttons, functions);
      
      return;
    }
  
  deactivate();
  disable();
  stop_watchdog();

  /* Create a new file selection widget */
  filew = gtk_file_selection_new ("Load configuration");

  gtk_widget_realize(filew);
  
  gtk_signal_connect (GTK_OBJECT (filew), "destroy",
		      (GtkSignalFunc) file_cancel_sel, &filew);
  gtk_signal_connect (GTK_OBJECT (filew), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filew)->ok_button),
		      "clicked", (GtkSignalFunc) file_ok_sel, filew );
  
  /* Connect the cancel_button to file_cancel_sel function */
  gtk_signal_connect_object (GTK_OBJECT 
			     (GTK_FILE_SELECTION (filew)->cancel_button),
			     "clicked", (GtkSignalFunc) file_cancel_sel,
			     GTK_OBJECT (filew));
  
  char* fname = load_path;
  if ( fname == NULL ) fname = "config";
  gtk_file_selection_set_filename (GTK_FILE_SELECTION(filew),
				   fname);

  gtk_window_position(GTK_WINDOW(filew), GTK_WIN_POS_MOUSE);
  gtk_window_set_policy(GTK_WINDOW(filew), TRUE, TRUE, TRUE);
  gtk_widget_show(filew);
}
void 
file_ok_sel (GtkWidget *, GtkFileSelection *fs)
{
  char* fname = 
    new char[strlen(gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)))+2];
  strcpy(fname,gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)));
  gtk_widget_hide(filew);
  gtk_widget_destroy(filew);
  filew = 0;

  if ( load_path != NULL ) delete load_path;
  load_path = fname;

  Log** target = read_configuration_file(fname);

  if ( target == NULL )
    {
      file_error(load_path);
      enable();
      start_watchdog();
      activate();
      return;
    } 
  else 
    {
      destroy_buttons();
      destroy_log_array(entry);
      entry = target;
      create_buttons();
      active = 0;
      enable();
      start_watchdog();
      activate();
      configuration_changed = FALSE;
      return;
    }
}
void file_cancel_sel (GtkWidget *, GtkFileSelection *){
  gtk_widget_hide(filew);
  gtk_widget_destroy(filew);
  filew = 0;
  enable();
  start_watchdog();
  activate();
}
/*}}}*/

/*{{{  Save Configuration  */
void 
save_conf_proc(GtkWidget*, gpointer*)
{
  if ( customizing == TRUE || filew != 0 )
    {
      GList* buttons = g_list_alloc();
      GList* functions = g_list_alloc();
      buttons = g_list_append(buttons,
			      (gpointer) "    DISMISS    ");
      functions = g_list_append(functions, NULL );
      
      popup_dialog("Erratic Request", "\n     Unable to save configuration      \n      at this point in program !      \n", buttons, functions);
      
      return;
    }
  
  deactivate();
  disable();
  
  if ( save_path == NULL )
    {
      char* hdir = NULL;
      hdir = getenv("HOME");
      if ( hdir == NULL ) hdir = getenv("USER");
      if ( hdir == NULL ) hdir = getenv("PWD");
      if ( hdir == NULL ) hdir = ".";
      save_path = new char[ strlen(hdir) + strlen(CONFIGFILE) + 2 ];
      strcpy ( save_path, hdir );
      strcat ( save_path, "/" );
      strcat ( save_path, CONFIGFILE );
    }
  
  /* Create a new file selection widget */
  filew = gtk_file_selection_new ("Save configuration");
  
  gtk_widget_realize(filew);

  gtk_signal_connect (GTK_OBJECT (filew), "destroy",
		      (GtkSignalFunc) file_cancel_sel, &filew);
  gtk_signal_connect (GTK_OBJECT (filew), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  
  /* Connect the ok_button to file_save_ok function */
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filew)->ok_button),
		      "clicked", (GtkSignalFunc) file_save_ok, filew );
  
  /* Connect the cancel_button to file_cancel_sel function */
  gtk_signal_connect_object (GTK_OBJECT 
			     (GTK_FILE_SELECTION (filew)->cancel_button),
			     "clicked", (GtkSignalFunc) file_cancel_sel,
			     GTK_OBJECT (filew));
  
  gtk_file_selection_set_filename (GTK_FILE_SELECTION(filew),
				   save_path);
  
  gtk_window_set_policy(GTK_WINDOW(filew), TRUE, TRUE, TRUE);
  gtk_window_position(GTK_WINDOW(filew), GTK_WIN_POS_MOUSE);
  gtk_widget_show(filew);
}
void 
file_save_ok (GtkWidget *, GtkFileSelection *fs)
{
  char* fname = 
    new char[strlen(gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)))+2];
  strcpy(fname,gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)));
  gtk_widget_hide(filew);
  gtk_widget_destroy(filew);
  filew = 0;

  if ( save_path != NULL ) delete save_path;
  save_path = fname;
  
  if ( write_configuration(save_path) == FALSE ) file_error(save_path);
  else configuration_changed = FALSE;
  
  enable();
  activate();
}
/*}}}*/

/*{{{  Customize Entry  */
void customize_entry_proc(GtkWidget*, gpointer*){
  if ( customizing == TRUE ) return;
  if ( filew != 0 ) return;

  startup_customize( entry );

  customizing = TRUE;
}
/*}}}*/

/*{{{  About Xlogmaster  */
void about_proc(GtkWidget*, gpointer*){
  request_about();
}
/*}}}*/

