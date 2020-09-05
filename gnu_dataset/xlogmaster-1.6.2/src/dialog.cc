/*

  dialog.cc

  dialog box subroutines for xlogmaster.cc
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
#include "dialog.H"
#include "extern.H"

/*}}}*/

/*{{{  popup dialog  */
/* Just give this function the title of the dialog you want,
   it's text and two lists - one with the buttons and one
   with the function of the type void function(); you want to
   be called if that button is pressed (same position in list) (or
   NULL as the function if nothing should be called) and voila.
   Even the lists get freed for you in here... as comfy as it gets... :-)
*/
void 
popup_dialog(gchar* title, gchar* text, GList* button_list, GList* function_list)
{
  GtkWidget* dialog = gtk_dialog_new ();
  gtk_signal_connect (GTK_OBJECT (dialog), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  gtk_window_set_title (GTK_WINDOW (dialog), title);
  
  GtkWidget* label = gtk_label_new(text);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), 
		      label, TRUE, TRUE, 5);
  gtk_widget_show (label);

  GtkWidget* button = NULL;
  gint buttons = g_list_length(button_list);
  for ( gint x = 1 ; x < buttons ; x++ )
    {
      gchar* buttonname = (gchar*) g_list_nth_data(button_list, x);
      gpointer function = (gpointer) g_list_nth_data(function_list, x);
      button = gtk_button_new_with_label (buttonname);
      gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area), 
			  button, TRUE, TRUE, 5);
      gtk_signal_connect (GTK_OBJECT (button), "clicked",
			  GTK_SIGNAL_FUNC (dialog_button_pressed), (gpointer) dialog );
      gtk_object_set_data( GTK_OBJECT(button), "xlogmaster::dialog::function", (gpointer) function);  
      gtk_widget_show(button);

    }
  
  gtk_window_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show(dialog);
  gtk_widget_grab_focus(button);

  g_list_free(button_list);
  g_list_free(function_list);
}
/*}}}*/
/*{{{  button press routine  */
/*
  This routine handles the button "clicked" event in dialog routines.
  
  If there is a non-NULL pointer attached to the widget as it's
  function (see "xlogmaster::dialog::function" tag) it is being called
  after destruction of the notice box.
  
  Otherwise the notice box is just being destroyed and that's it.
*/
void 
dialog_button_pressed(GtkWidget *widget, gpointer *data)
{
  GtkWidget* dialog = (GtkWidget*) data;
  GtkWidget* button = widget;
  gpointer function = (gpointer) gtk_object_get_data(GTK_OBJECT(button), "xlogmaster::dialog::function");
  
  /* now destroy dialog */
  gtk_widget_destroy(dialog);
  
  /* if no function has been given just leave */
  if ( function == NULL || function == (gpointer) -1 ) return;

  void (*complete)() = (void (*)()) function;
  complete();
}
/*}}}*/
