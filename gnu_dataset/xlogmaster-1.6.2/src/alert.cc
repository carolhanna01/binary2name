/*

  alert.cc

  notifying subroutines for xlogmaster.cc
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
#include "alert.H"
#include "extern.H"

/*}}}*/

/*{{{  activate / deactivate  */
/*
  activates alert for entry number "nr" (parameter). 
*/
void 
trigger_alert(int nr)
{
  if ( entry[nr]->alert_tag != 0 )
    {
      entry[nr]->fadestep = fade_base / 3;
    } 
  else
    {
      entry[nr]->fade = 0;
      entry[nr]->last_fade = 0;
      entry[nr]->fadestep = fade_base / 3;
      entry[nr]->alert_tag = gtk_timeout_add (100,
					      *alert_interrupt,
					      (gpointer*) nr);
    }
}
/*
  removes the alert for entry number "nr" (parameter)
*/
void 
remove_alert(int nr)
{
  if ( entry[nr]->alert_tag != 0 )
    {
      gtk_timeout_remove(entry[nr]->alert_tag);
      entry[nr]->alert_tag = 0;
    }
  
  if ( entry[nr]->pixels[0] != 0 && entry[nr]->pixels[1] != 0 )
    gdk_colors_free(cmap, entry[nr]->pixels, 2, 0);
  entry[nr]->pixels[0] = entry[nr]->pixels[1] = 0;

  entry[nr]->style = gtk_style_copy(stdstyle);
  /*
    if this is the active logfile the "no alert" color is PRELIGHT
    and not NORMAL !
  */  
  if ( nr == active )
    {
      entry[nr]->style->bg[GTK_STATE_NORMAL].red = prelight.red;
      entry[nr]->style->bg[GTK_STATE_NORMAL].green = prelight.green;
      entry[nr]->style->bg[GTK_STATE_NORMAL].blue = prelight.blue;
    }  
  gtk_widget_set_style(entry[nr]->button, entry[nr]->style);
  gtk_style_unref(entry[nr]->style);
  
  entry[nr]->fade = entry[nr]->last_fade = entry[nr]->fadestep = 0;
}
/*}}}*/

/*{{{  alert_interrupt  */
/*
  the alert interrupt routine that makes the button change it's color
*/
int 
alert_interrupt(gpointer data)
{ 
  int nr = (int) data;
  entry[nr]->fade += entry[nr]->fadestep;
  if ( entry[nr]->fade > fade_base )
    {
      entry[nr]->fade = fade_base;
      entry[nr]->fadestep = fade_step;
    } 
  else if ( entry[nr]->fade < 0 ) 
    remove_alert(nr);

  if ( entry[nr]->fade - entry[nr]->last_fade > ( fade_base / steps ) ||
       entry[nr]->last_fade - entry[nr]->fade > ( fade_base / steps ) )
    {
      entry[nr]->last_fade = entry[nr]->fade;
      
      GdkColor normal = {0,0,0,0};
      GdkColor plight = {0,0,0,0};
      
      gfloat alert_fade = (gfloat) ( (gfloat) entry[nr]->fade / fade_base);
      gfloat orig_fade = (gfloat) ((fade_base - (gfloat) entry[nr]->fade) / fade_base);
      
      plight.red = (gushort) ((( alert_fade * alert.red )) + 
			      ( orig_fade * prelight.red ));
      plight.green = (gushort) ((( alert_fade * alert.green )) + 
				( orig_fade * prelight.green ));
      plight.blue = (gushort) ((( alert_fade * alert.blue )) + 
			       ( orig_fade * prelight.blue ));
      
      normal.red = (gushort) ((( alert_fade * alert.red )) + 
			      ( orig_fade * base.red ));
      normal.green = (gushort) ((( alert_fade * alert.green )) + 
				( orig_fade * base.green ));
      normal.blue = (gushort) ((( alert_fade * alert.blue )) + 
			       ( orig_fade * base.blue ));
      
      gdk_color_alloc (cmap, &normal);
      gdk_color_alloc (cmap, &plight);
      
      entry[nr]->style = gtk_style_copy(stdstyle);

      /*
	if the current entry is the active one the color we are fading back to
	is the PRELIGHT color !
      */
      if ( nr == active )
	{
	  entry[nr]->style->bg[GTK_STATE_NORMAL].red = plight.red;
	  entry[nr]->style->bg[GTK_STATE_NORMAL].green = plight.green;
	  entry[nr]->style->bg[GTK_STATE_NORMAL].blue = plight.blue;
	}
      else
	{
	  entry[nr]->style->bg[GTK_STATE_NORMAL].red = normal.red;
	  entry[nr]->style->bg[GTK_STATE_NORMAL].green = normal.green;
	  entry[nr]->style->bg[GTK_STATE_NORMAL].blue = normal.blue;
	}

      entry[nr]->style->bg[GTK_STATE_PRELIGHT].red = plight.red;
      entry[nr]->style->bg[GTK_STATE_PRELIGHT].green = plight.green;
      entry[nr]->style->bg[GTK_STATE_PRELIGHT].blue = plight.blue;
      entry[nr]->style->bg[GTK_STATE_SELECTED].red = plight.red;
      entry[nr]->style->bg[GTK_STATE_SELECTED].green = plight.green;
      entry[nr]->style->bg[GTK_STATE_SELECTED].blue = plight.blue;
      entry[nr]->style->bg[GTK_STATE_ACTIVE].red = plight.red;
      entry[nr]->style->bg[GTK_STATE_ACTIVE].green = plight.green;
      entry[nr]->style->bg[GTK_STATE_ACTIVE].blue = plight.blue;
      gtk_widget_set_style(entry[nr]->button, entry[nr]->style);
      gtk_style_unref(entry[nr]->style);
      
      if ( entry[nr]->pixels[0] != 0 && entry[nr]->pixels[1] != 0 )
	gdk_colors_free(cmap, entry[nr]->pixels, 2, 0);
    
      entry[nr]->pixels[0] = normal.pixel;
      entry[nr]->pixels[1] = plight.pixel;
    }

  return TRUE;
}
/*}}}*/

/* Notice window things */
/*{{{  create Notice window  */
void 
create_notice_window()
{
  notice_dialog = gtk_window_new( GTK_WINDOW_DIALOG);
  gtk_widget_set_usize( GTK_WIDGET (notice_dialog), 500, 250);
  gtk_window_set_policy(GTK_WINDOW(notice_dialog), TRUE, TRUE, FALSE);
  gtk_signal_connect (GTK_OBJECT (notice_dialog), "destroy",
		      GTK_SIGNAL_FUNC (notice_taken), NULL);
  gtk_signal_connect (GTK_OBJECT (notice_dialog), "delete_event",
		      GTK_SIGNAL_FUNC (delete_event), NULL);
  gtk_window_set_title (GTK_WINDOW (notice_dialog), "*** Xlogmaster Notice ***" );
  GtkWidget* vbox = gtk_vbox_new(FALSE, 5);
  gtk_container_add(GTK_CONTAINER (notice_dialog), vbox);
  gtk_widget_show(vbox);
  GtkWidget* hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
  gtk_widget_show(hbox);

  gtk_box_pack_start (GTK_BOX (hbox), small_logo_xpm, FALSE, FALSE, 5);
  gtk_widget_show(small_logo_xpm);    

  
  GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_ALWAYS,
				  GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 5);

  text_field = gtk_entry_new_with_max_length(600);
  gtk_entry_set_text(GTK_ENTRY(text_field), " ");
  gtk_entry_set_editable (GTK_ENTRY(text_field),FALSE);
  gtk_tooltips_set_tip(tooltips, text_field, "... the line that triggered the alarm ...", "... the line that triggered the alarm ...");
  gtk_scrolled_window_add_with_viewport 
    (GTK_SCROLLED_WINDOW(scrolled_window), text_field);
  gtk_widget_set_usize(text_field, 1200, -1);
  gtk_widget_show(text_field);
  gtk_widget_show(scrolled_window);
  
  notice_scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (notice_scrolled_window),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(hbox), notice_scrolled_window, TRUE, TRUE, 5);
  gtk_widget_show(notice_scrolled_window);
  notice_list =  gtk_list_new();
  gtk_widget_show(notice_list);
  gtk_list_set_selection_mode(GTK_LIST(notice_list), GTK_SELECTION_BROWSE);
  gtk_scrolled_window_add_with_viewport 
    (GTK_SCROLLED_WINDOW(notice_scrolled_window), notice_list);
  gtk_signal_connect (GTK_OBJECT (notice_list), "select_child",
		      GTK_SIGNAL_FUNC (notice_item_select), (gpointer) notice_list );

  dismiss_button = gtk_button_new_with_label ("DISMISS");
  gtk_signal_connect (GTK_OBJECT (dismiss_button), "clicked",
		      GTK_SIGNAL_FUNC (notice_taken), (gpointer) NULL );
  gtk_box_pack_end(GTK_BOX(vbox), dismiss_button, FALSE, FALSE, 5);
  gtk_tooltips_set_tip(tooltips, dismiss_button, "...dismiss notice...","...dismiss notice...");
  gtk_widget_show(dismiss_button);

  gtk_widget_hide(notice_dialog);

}
/*}}}*/

/*{{{  notice window subroutines  */
void 
notice_taken(GtkWidget *, gpointer *)
{
  gtk_widget_hide(notice_dialog);
  
  gint children = g_list_length( (GList*) GTK_LIST(notice_list)->children );

  while ( children > 0 ){
    gtk_container_remove(GTK_CONTAINER(notice_list), 
			 (GtkWidget*) GTK_LIST(notice_list)->children->data );
    children = g_list_length( (GList*) GTK_LIST(notice_list)->children );
  }

};
void 
notice_item_select(GtkList*, GtkWidget*)
{
  GtkWidget* active_item = (GtkWidget*) GTK_LIST(notice_list)->selection->data;
  gchar* string = (gchar*) gtk_object_get_data( GTK_OBJECT( active_item ), LINE_TAG );
  gtk_entry_set_text(GTK_ENTRY(text_field), string);
  gtk_entry_set_max_length(GTK_ENTRY(text_field), strlen(string));
  gtk_widget_show(text_field);
  gtk_entry_set_position(GTK_ENTRY(text_field), 0);
}
void 
destroy_notice_entry(GtkWidget *widget, gpointer *)
{
  gchar* string = (gchar*) 
    gtk_object_get_data( GTK_OBJECT( widget ), LINE_TAG );
  delete string;
}
void 
notice_alert(gchar* name, gchar* line)
{
  int x = 0;
  int a = 0;
  gchar* string = new gchar[strlen(line)+1];
  while( line[a] != 0 ) string[x++] = line[a++];
  string[x] = 0;

  x = 0;
  a = 0;
  time_t now;
  time(&now);
  char* timestring = ctime(&now);
  gchar* nstring = new gchar[ strlen(timestring) + strlen(name) + 6 ];
  nstring[x++] = '[';
  while( timestring[a] != 0 && timestring[a] != '\r' && timestring[a] != '\n' ) nstring[x++] = timestring[a++];
  nstring[x++] = ']';
  nstring[x++] = ' ';
  a = 0;
  while( name[a] != 0 ) nstring[x++] = name[a++];
  nstring[x] = 0;

  GtkWidget* list_item = gtk_list_item_new_with_label (nstring);
  gtk_widget_show (list_item);
  gtk_signal_connect (GTK_OBJECT (list_item), "destroy",
		      GTK_SIGNAL_FUNC (destroy_notice_entry), (gpointer) notice_list);
  gtk_object_set_data( GTK_OBJECT( list_item ), LINE_TAG, (gpointer) string);
  gtk_container_add (GTK_CONTAINER(notice_list), list_item); 

  gtk_list_select_child(GTK_LIST(notice_list), list_item);

  if ( notice_follows_mouse == TRUE )
    gtk_widget_hide(notice_dialog);
  gtk_window_position(GTK_WINDOW(notice_dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show(notice_dialog);

  /* This maps the window, which also de-iconifies it according to ICCCM. */
  gdk_window_show (GTK_WIDGET (notice_dialog)->window);
  gdk_window_raise (GTK_WIDGET (notice_dialog)->window);
 
  delete nstring;

  gtk_widget_grab_focus(dismiss_button);
}
/*}}}*/
