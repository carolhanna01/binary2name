/*

  customize.cc

  customizing subroutines for xlogmaster.cc
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
#include "customize.H"
#include "extern.H"

/*}}}*/

/*{{{  startup_customize  */
void
startup_customize(Log** data)
{
  
  /* create a working copy of the current configuration */
  Log** local_array = copy_data_array( data );

/*{{{  Basic Structure  */
  /* create window */
  form = gtk_window_new (GTK_WINDOW_DIALOG);
  gtk_signal_connect (GTK_OBJECT (form), "destroy",
		      GTK_SIGNAL_FUNC (form_popdown), NULL);
  gtk_signal_connect (GTK_OBJECT (form), "delete_event",
		      GTK_SIGNAL_FUNC (form_popdown), NULL);
  gtk_window_set_title (GTK_WINDOW (form), "Customize");
  gtk_container_border_width(GTK_CONTAINER(form), 5);  
  tooltips = gtk_tooltips_new();
  
  /* create box & table */
  GtkWidget* big_box =  gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER (form), big_box);
  gtk_widget_show(big_box);
  GtkWidget* button_table = gtk_table_new(1, 9, TRUE);
  gtk_box_pack_end( GTK_BOX(big_box), button_table, FALSE, FALSE, 2);
  gtk_widget_show(button_table);

  /* create paned widget */
  GtkWidget* hpaned = gtk_hpaned_new();
  gtk_box_pack_end( GTK_BOX(big_box), hpaned, TRUE, TRUE, 2);
  gtk_paned_set_handle_size ( GTK_PANED(hpaned),
			      10 );
  gtk_paned_set_gutter_size ( GTK_PANED(hpaned),
			      15 );                       
  gtk_widget_show ( hpaned );
/*}}}*/

/*{{{  Entry CList  */  
  entry_list = gtk_clist_new ( 1 );
  gtk_clist_set_selection_mode( GTK_CLIST( entry_list ),
				GTK_SELECTION_SINGLE );
  gtk_clist_set_shadow_type( GTK_CLIST( entry_list ),
			     GTK_SHADOW_ETCHED_IN );
  gtk_clist_column_titles_passive( GTK_CLIST( entry_list ) );
  gtk_clist_column_titles_hide( GTK_CLIST( entry_list ) );
  gtk_clist_set_column_justification(  GTK_CLIST( entry_list ),
				       0,
				       GTK_JUSTIFY_LEFT );
  GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      entry_list );
  gtk_paned_add1( GTK_PANED( hpaned ), scrolled_window );
  gtk_widget_show( scrolled_window );
  gtk_widget_show( entry_list );
  GtkStyle *style;
  style = gtk_widget_get_style( entry_list );
  gint default_width = gdk_string_width ( style->font ,
					  "XXXXXXXXXXXXXXXXXXXX" );
  gtk_clist_set_column_width( GTK_CLIST( entry_list ),
			      0,
			      (guint) ( default_width * 1.2 ) );
  gtk_widget_set_usize ( scrolled_window , default_width , 0 );     
  gtk_signal_connect( GTK_OBJECT( entry_list ),
		      "select_row",
		      GTK_SIGNAL_FUNC(select_entry_row_callback),
		      NULL );
  gtk_signal_connect( GTK_OBJECT( entry_list ),
		      "unselect_row",
		      GTK_SIGNAL_FUNC(unselect_entry_row_callback),
		      NULL );
/*}}}*/

/*{{{  Table  */
  GtkWidget* table = gtk_table_new(18, 16, TRUE);
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      table );
  gtk_widget_show( scrolled_window );
  gtk_paned_add2( GTK_PANED( hpaned ), scrolled_window );
  gtk_widget_show( table );
/*}}}*/

/*{{{  Buttons  */
  cancel_button = gtk_button_new_with_label ("Cancel");
  gtk_signal_connect (GTK_OBJECT (cancel_button), "clicked",
  		      GTK_SIGNAL_FUNC (customization_abort), NULL );
  gtk_table_attach(GTK_TABLE(button_table), cancel_button, 8, 9, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_tooltips_set_tip(tooltips, cancel_button, 
		       "...discard inputs...",
		       "...discard inputs...");
  gtk_widget_show(cancel_button);
  
  apply_button = gtk_button_new_with_label ("OK");
  gtk_signal_connect (GTK_OBJECT (apply_button), "clicked",
  		      GTK_SIGNAL_FUNC (customization_done), NULL );
  gtk_table_attach(GTK_TABLE(button_table), apply_button, 7, 8, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL ),
		   4, 4);
  gtk_tooltips_set_tip(tooltips, apply_button, 
		       "...apply inputs and leave customization...",
		       "...apply inputs and leave customization...");
  gtk_widget_show(apply_button);

  database_button = gtk_button_new_with_label ("Database");
  gtk_signal_connect (GTK_OBJECT (database_button), "clicked",
  		      GTK_SIGNAL_FUNC (open_database), NULL );
  gtk_table_attach(GTK_TABLE(button_table), database_button, 5, 6, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_tooltips_set_tip(tooltips, database_button, 
		       "...check out the system wide database for entries...",
		       "...check out the system wide database for entries...");
  gtk_widget_show(database_button);

  add_button = gtk_button_new_with_label ("Insert");
  gtk_signal_connect (GTK_OBJECT (add_button), "clicked",
  		      GTK_SIGNAL_FUNC (insert_new_entry), NULL );
  gtk_table_attach(GTK_TABLE(button_table), add_button, 0, 1, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL ),
		   4, 4);
  gtk_tooltips_set_tip(tooltips, add_button, 
		       "...insert entry...",
		       "...insert entry...");
  gtk_widget_show(add_button);

  move_up_button = gtk_button_new_with_label ("Move Up");
  gtk_signal_connect (GTK_OBJECT (move_up_button), "clicked",
  		      GTK_SIGNAL_FUNC (move_entry_up), NULL );
  gtk_table_attach(GTK_TABLE(button_table), move_up_button, 1, 2, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL ),
		   4, 4);
  gtk_tooltips_set_tip(tooltips, move_up_button, 
		       "...move entry one step up...",
		       "...move entry one step up...");
  gtk_widget_show(move_up_button);

  move_down_button = gtk_button_new_with_label ("Move Down");
  gtk_signal_connect (GTK_OBJECT (move_down_button), "clicked",
  		      GTK_SIGNAL_FUNC (move_entry_down), NULL );
  gtk_table_attach(GTK_TABLE(button_table), move_down_button, 2, 3, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL ),
		   4, 4);
  gtk_tooltips_set_tip(tooltips, move_down_button, 
		       "...move entry one step down...",
		       "...move entry one step down...");
  gtk_widget_show(move_down_button);
  
  delete_button = gtk_button_new_with_label ("Delete");
  gtk_signal_connect (GTK_OBJECT (delete_button), "clicked",
  		      GTK_SIGNAL_FUNC (delete_entry), NULL );
  gtk_table_attach(GTK_TABLE(button_table), delete_button, 3, 4, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL ),
		   4, 4);
  gtk_tooltips_set_tip(tooltips, delete_button, 
		       "...delete entry...",
		       "...delete entry...");
  gtk_widget_show(delete_button);  
  
/*}}}*/

/*{{{  Boxes / Widgets for Entries  */
  /* Button text */ 
  attach_label(table, "Button text:", 0, 4, 0, 1, 0.0); 
  button_text_entry = entry_box( table,
				 "*button text*",
				 0, 7, 1, 2,
				 "Text to be used on button" );
  gtk_signal_connect (GTK_OBJECT (button_text_entry), "activate",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );
  
  /* Help text */
  attach_label(table, "Help text:", 9, 13, 0, 1, 0.0);
  help_text_entry = entry_box( table,
			       "*help text*",
			       9, 16, 1, 2,
			       "Text to show up like this one..." );
  gtk_signal_connect (GTK_OBJECT (help_text_entry), "activate",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );

  /* Filename */
  attach_label(table, "Filename (absolute path) / Program:", 
	       0, 8, 2, 3, 0.0);
  filename_entry = entry_box( table,
			      "*filename*",
			      0, 13, 3, 4,
			      "you can also use the 'select' button to browse your harddisk");
  gtk_signal_connect (GTK_OBJECT (filename_entry), "activate",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );
  select_filename_button = gtk_button_new_with_label ("select");
  gtk_signal_connect (GTK_OBJECT (select_filename_button), "clicked",
		      GTK_SIGNAL_FUNC (select_file), NULL );
  gtk_tooltips_set_tip( tooltips, 
			select_filename_button, 
			"Browse for logfile/device/program",
			"Browse for logfile/device/program" );
  gtk_table_attach(GTK_TABLE(table), select_filename_button, 13, 16, 3, 4,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_widget_show(select_filename_button);
  
  /* Plugin */
  attach_label(table, "Plugin / Output Filter:", 0, 8, 4, 5, 0.0);
  plugin_entry = entry_box( table,
			    "*plugin*",
			    0, 13, 5, 6,
			    "Plugin to pipe data through (see documentation)\nyou can also use the 'select' button to browse your harddisk");
  gtk_signal_connect (GTK_OBJECT (plugin_entry), "activate",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );
  select_plugin_button = gtk_button_new_with_label ("select");
  gtk_signal_connect (GTK_OBJECT (select_plugin_button), "clicked",
		      GTK_SIGNAL_FUNC (select_plugin), NULL );
  gtk_tooltips_set_tip( tooltips, 
			select_plugin_button, 
			"Browse for plugin",
			"Browse for plugin" );
  gtk_table_attach(GTK_TABLE(table), select_plugin_button, 13, 16, 5, 6,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_widget_show(select_plugin_button);
  
  /* Interval */
  attach_label(table, "Interval:", 0, 3, 6, 7, 0.0);
  GtkAdjustment* adj = (GtkAdjustment *) gtk_adjustment_new ( 0.3 , 
							      0.1 , 
							      10000 , 
							      0.1 ,
							      1.0 , 
							      0.0 );
  interval_selection = gtk_spin_button_new( adj, 0.5, 1 );
  gtk_spin_button_set_numeric( GTK_SPIN_BUTTON( interval_selection ), TRUE );
  gtk_table_attach(GTK_TABLE(table), interval_selection, 0, 3, 7, 8,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_tooltips_set_tip(tooltips, interval_selection,
		       "the time between two calls of the interrupt function that reads the data - recommended value between 0.3 and 3. The lower it is the more smooth the updating will become, the higher it is the less processor time is being wasted. ",
		       "the time between two calls of the interrupt function that reads the data - recommended value between 0.3 and 3. The lower it is the more smooth the updating will become, the higher it is the less processor time is being wasted. ");
  gtk_widget_show(interval_selection);
  attach_label(table, "seconds", 3, 7, 7, 8, 0.0);
  gtk_signal_connect ( GTK_OBJECT (adj), "value_changed",
		       GTK_SIGNAL_FUNC (accept_values),
		       (gpointer) NULL );

  /* Delay */
  attach_label(table, "Delay:", 9, 12, 6, 7, 0.0);
  adj = (GtkAdjustment *) gtk_adjustment_new ( 0.6 ,
					       0.1 , 
					       10000 , 
					       0.1 ,
					       1.0 , 
					       0.0 );
  delay_selection = gtk_spin_button_new( adj, 0.5, 1 );
  gtk_spin_button_set_numeric( GTK_SPIN_BUTTON( delay_selection ), TRUE );
  gtk_table_attach(GTK_TABLE(table), delay_selection, 9, 12, 7, 8,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_tooltips_set_tip(tooltips, delay_selection,
		       "the delay between two program executions, used _only by *run* mode, ignored by others. \nRecommended value between 0 (immediate restart) and anything you want.",
		       "the delay between two program executions, used _only by *run* mode, ignored by others. \nRecommended value between 0 (immediate restart) and anything you want.");
  gtk_widget_show(delay_selection);
  attach_label(table, "seconds", 12, 16, 7, 8, 0.0);
  gtk_signal_connect ( GTK_OBJECT (adj), "value_changed",
		       GTK_SIGNAL_FUNC (accept_values),
		       (gpointer) NULL );

  /* Mode */
  attach_label(table, "Mode:", 0, 2, 8, 10, 0.0);
  GSList* group;
  tail_button = gtk_radio_button_new_with_label (NULL, "TAIL-Mode  ");
  gtk_tooltips_set_tip(tooltips, tail_button, 
		       "the standard 'tail -f' emulating mode", 
		       "the standard 'tail -f' emulating mode");
  gtk_table_attach(GTK_TABLE(table), tail_button, 2, 6, 8, 10,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 4, 4);
  GTK_WIDGET_SET_FLAGS (tail_button, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (tail_button);
  gtk_widget_show (tail_button);
  gtk_signal_connect (GTK_OBJECT (tail_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );
  group = gtk_radio_button_group ( GTK_RADIO_BUTTON ( tail_button ) );
  
  cat_button = gtk_radio_button_new_with_label (group, "CAT-Mode  ");
  gtk_tooltips_set_tip(tooltips, cat_button, 
		       "the cool 'cat' mode", 
		       "the cool 'cat' mode");
  gtk_table_attach(GTK_TABLE(table), cat_button, 7, 11, 8, 10,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 4, 4);
  gtk_widget_show (cat_button);
  gtk_signal_connect (GTK_OBJECT (cat_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_values), NULL );
  group = gtk_radio_button_group ( GTK_RADIO_BUTTON ( cat_button ) );

  run_button = gtk_radio_button_new_with_label (group, "RUN-Mode  ");
  gtk_tooltips_set_tip(tooltips, run_button, 
		       "the extraordinary 'run' mode", 
		       "the extraordinary 'run' mode");
  gtk_table_attach(GTK_TABLE(table), run_button, 12, 16, 8, 10,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 4, 4);
   gtk_widget_show (run_button);
   gtk_signal_connect (GTK_OBJECT (run_button), "clicked",
		       GTK_SIGNAL_FUNC (accept_values), NULL );
  

   /* Filter Stuff */
   GtkWidget* separator = gtk_hseparator_new();
   gtk_table_attach(GTK_TABLE(table), separator, 0, 16, 10, 11,
		    (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		    (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 4, 4);
   gtk_widget_show(separator);
   GtkWidget* filter_table = gtk_table_new(6, 5, TRUE);
   gtk_table_attach(GTK_TABLE(table), filter_table, 0, 16, 11, 18,
		    (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		    (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 4, 4);
   gtk_widget_show( filter_table );
/*}}}*/

/*{{{  Filter menu  */
   attach_label(filter_table, "Filter:", 0, 2, 0, 1, 0.0);

   /* Add / Delete Buttons */
   add_filter_button = gtk_button_new_with_label ("Add");
   gtk_signal_connect (GTK_OBJECT (add_filter_button), "clicked",
		       GTK_SIGNAL_FUNC (add_filter), NULL );
   gtk_tooltips_set_tip( tooltips, 
			 add_filter_button, 
			 "...add filter entry...",
			 "...add filter entry..." );
   gtk_table_attach(GTK_TABLE(filter_table), 
		    add_filter_button, 3, 4, 0, 1,
		    (GtkAttachOptions) 
		    ( GTK_EXPAND | GTK_FILL ), 
		    (GtkAttachOptions) 
		    ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		    4, 4);
   gtk_widget_show(add_filter_button);

   delete_filter_button = gtk_button_new_with_label ("Delete");
   gtk_signal_connect (GTK_OBJECT (delete_filter_button), "clicked",
		       GTK_SIGNAL_FUNC (delete_filter), NULL );
   gtk_tooltips_set_tip( tooltips,
			 delete_filter_button, 
			 "...delete filter entry...",
			 "...delete filter entry..." );
   gtk_table_attach(GTK_TABLE(filter_table), 
		    delete_filter_button, 4, 5, 0, 1,
		    (GtkAttachOptions) 
		    ( GTK_EXPAND | GTK_FILL ), 
		    (GtkAttachOptions) 
		    ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		    4, 4);
   gtk_widget_show(delete_filter_button);

   /* List of Filter Entries */
   filter_list = gtk_clist_new ( 7 );
   gtk_clist_set_selection_mode( GTK_CLIST( filter_list ),
				 GTK_SELECTION_SINGLE );
   gtk_clist_set_shadow_type( GTK_CLIST( filter_list ),
			      GTK_SHADOW_ETCHED_IN );
  gtk_clist_column_titles_passive( GTK_CLIST( filter_list ) );
  gtk_clist_column_titles_hide( GTK_CLIST( filter_list ) );
  
  style = gtk_widget_get_style( filter_list );
  default_width = gdk_string_width ( style->font ,
				     "XXXXXXXXXXXXXXXXXXXX" );
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      0,
			      gdk_string_width ( style->font , "W" ));
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      1,
			      gdk_string_width ( style->font , "A" ));
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      2,
			      gdk_string_width ( style->font , "N" ));
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      3,
			      gdk_string_width ( style->font , "U" ));
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      4,
			      gdk_string_width ( style->font , "E" ));
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      5, default_width );
  gtk_clist_set_column_width( GTK_CLIST( filter_list ),
			      6, default_width );
  gtk_clist_set_column_auto_resize( GTK_CLIST( filter_list ), 5, TRUE);
  gtk_clist_set_column_auto_resize( GTK_CLIST( filter_list ), 6, TRUE);

  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       0,
				       GTK_JUSTIFY_CENTER );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       1,
				       GTK_JUSTIFY_CENTER );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       2,
				       GTK_JUSTIFY_CENTER );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       3,
				       GTK_JUSTIFY_CENTER );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       4,
				       GTK_JUSTIFY_CENTER );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       5,
				       GTK_JUSTIFY_LEFT );
  gtk_clist_set_column_justification(  GTK_CLIST( filter_list ),
				       6,
				       GTK_JUSTIFY_LEFT );

  gtk_signal_connect( GTK_OBJECT( filter_list ),
		      "select_row",
		      GTK_SIGNAL_FUNC(select_filter_entry_row_callback),
		      NULL );
  gtk_signal_connect( GTK_OBJECT( filter_list ),
		      "unselect_row",
		      GTK_SIGNAL_FUNC(unselect_filter_entry_row_callback),
		      NULL );

  gtk_clist_set_sort_type( GTK_CLIST( filter_list ),
			   GTK_SORT_ASCENDING );
  gtk_clist_set_compare_func( GTK_CLIST( filter_list ),
			      (GtkCListCompareFunc)
			      compare_filter_clist_elements );
  
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      filter_list );
  gtk_table_attach(GTK_TABLE(filter_table), 
		   scrolled_window, 3, 5, 1, 6,
		   (GtkAttachOptions) 
		   ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) 
		   ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_widget_show( scrolled_window );
  gtk_widget_show( filter_list );


  /* the editable strings */
  filter_string = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(filter_string), "*** Filter String ***");
  gtk_entry_set_editable (GTK_ENTRY(filter_string),TRUE);
  gtk_table_attach(GTK_TABLE(filter_table), filter_string, 0, 3, 2, 3,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_tooltips_set_tip(tooltips, filter_string, "...a standard REGEX (see GNU regex manual) to be used for this filter, matching lines will trigger the filter...", "...a standard REGEX (see GNU regex manual) to be used for this filter, matching lines will trigger the filter...");
  gtk_widget_show(filter_string);
  gtk_signal_connect (GTK_OBJECT (filter_string), "activate",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );

  execute_string = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(execute_string), "*** Execute Line ***");
  gtk_entry_set_editable (GTK_ENTRY(execute_string),TRUE);
  gtk_table_attach(GTK_TABLE(filter_table), execute_string, 1, 3, 5, 6,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_tooltips_set_tip(tooltips, execute_string, "... program with parameters to execute on match. The following variables are defined:\n%F: absolute path to file [= $XLM_FILE]\n%H: helptext of entry [= $XLM_HELP]\n%L: line that triggered the execution [= $XLM_LINE]\n%M: mode of entry (TAIL/CAT) [= $XLM_MODE]\n%N: name of entry [= $XLM_NAME]\notherwise use like usual command and commandline...", "... program with parameters to execute on match. The following variables are defined:\n%F: absolute path to file [= $XLM_FILE]\n%H: helptext of entry [= $XLM_HELP]\n%L: line that triggered the execution [= $XLM_LINE]\n%M: mode of entry (TAIL/CAT) [= $XLM_MODE]\n%N: name of entry [= $XLM_NAME]\notherwise use like usual command and commandline...");
  gtk_widget_show(execute_string);
  gtk_signal_connect (GTK_OBJECT (execute_string), "activate",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  
  /* special mode buttons */
  invert_button = gtk_check_button_new_with_label ( "Invert");
  gtk_tooltips_set_tip(tooltips, invert_button, "...invert sense of matching. If activated non-matching lines will trigger filter...", "...invert sense of matching. If activated non-matching lines will trigger filter...");
  gtk_table_attach(GTK_TABLE(filter_table), invert_button, 2, 3, 1, 2,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (invert_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (invert_button);  
  
  case_button = gtk_check_button_new_with_label ( "Case-sensitive");
  gtk_tooltips_set_tip(tooltips, case_button, "...if activated matching will be performed case-sensitive...", "...if activated matching will be performed case-sensitive...");
  gtk_table_attach(GTK_TABLE(filter_table), case_button, 1, 2, 1, 2,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (invert_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (case_button);  
  
  /* radio buttons */
  raise_button = gtk_check_button_new_with_label ( "Raise");
  gtk_tooltips_set_tip(tooltips, raise_button, "... the 'raising' mode... matching lines will be displayed in PRELIGHT color... (see gtkrc) ... ", "... the 'raising' mode... matching lines will be displayed in PRELIGHT color... (see gtkrc) ...");
  gtk_table_attach(GTK_TABLE(filter_table), raise_button, 0, 1, 3, 4,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (raise_button), "clicked",
		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) RAISE );
  gtk_widget_show (raise_button);

  lower_button = gtk_check_button_new_with_label ( "Lower");
  gtk_tooltips_set_tip(tooltips, lower_button, "... the 'lowering' mode... matching lines will be displayed in INSENSITIVE color... (see gtkrc) ... ", "... the 'lowering' mode... matching lines will be displayed in INSENSITIVE color... (see gtkrc) ...");
  gtk_table_attach(GTK_TABLE(filter_table), lower_button, 1, 2, 3, 4,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (lower_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) LOWER );
  gtk_widget_show (lower_button);  

  hide_button = gtk_check_button_new_with_label ( "Hide");
  gtk_tooltips_set_tip(tooltips, hide_button, "... the 'hiding' mode, matching lines will be hidden ... ", "... the 'hiding' mode, matching lines will be hidden ...");
  gtk_table_attach(GTK_TABLE(filter_table), hide_button, 2, 3, 3, 4,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (hide_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) HIDE );
  gtk_widget_show (hide_button);

  alert_button = gtk_check_button_new_with_label ( "Alert");
  gtk_tooltips_set_tip(tooltips, alert_button, "... the 'alert' mode, matching lines will trigger alert ... ", "... the 'alert' mode, matching lines will trigger alert ...");
  gtk_table_attach(GTK_TABLE(filter_table), alert_button, 0, 1, 4, 5,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (alert_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (alert_button);

  notice_button = gtk_check_button_new_with_label ( "Notice");
  gtk_tooltips_set_tip(tooltips, notice_button, "... the 'notice' mode, matching lines will create popup ... ", "... the 'notice' mode, matching lines will create popup ... ");
  gtk_table_attach(GTK_TABLE(filter_table), notice_button, 1, 2, 4, 5,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (notice_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (notice_button);

  uniconify_button = gtk_check_button_new_with_label ( "UnIconify");
  gtk_tooltips_set_tip(tooltips, uniconify_button, "... the 'UnIconify' mode, matching lines will force Xlogmaster to uniconify... ", "... the 'UnIconify' mode, matching lines will force Xlogmaster to uniconify ... ");
  gtk_table_attach(GTK_TABLE(filter_table), uniconify_button, 2, 3, 4, 5,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (uniconify_button), "clicked",
		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (uniconify_button);

  execute_button = gtk_check_button_new_with_label ( "Execute:");
  gtk_tooltips_set_tip(tooltips, execute_button, "... the 'Execute' mode, matching lines will trigger execution of program ...", "... the 'Execute' mode, matching lines will trigger execution of program ...");
  gtk_table_attach(GTK_TABLE(filter_table), execute_button, 0, 1, 5, 6,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 0, 0);
  gtk_signal_connect (GTK_OBJECT (execute_button), "clicked",
  		      GTK_SIGNAL_FUNC (accept_filter_values), 
		      (gpointer) NULL );
  gtk_widget_show (execute_button);

/*}}}*/

/*{{{  Add Log Entries  */
  gchar* column_title[2];
  column_title[1] = NULL;
  gint i = 0;
  while ( local_array[i] != NULL )
    {
      
      column_title[0] = local_array[i]->buttontext;
      
      gtk_clist_append( GTK_CLIST( entry_list ),
			column_title );
      
      gtk_clist_set_row_data_full( GTK_CLIST( entry_list ),
				   i,
				   (gpointer) local_array[i],
				   (GtkDestroyNotify) 
				   delete_clist_log_element );
      

      i++;
    }
  delete local_array;
/*}}}*/  

/*{{{  Set startup values  */  
  block_handlers = TRUE;

  last_row = active;
  last_filter = NULL;
  last_filter_row = 0;

  gtk_clist_select_row( GTK_CLIST( entry_list ),
			last_row,
			0 );
  
  last_entry = (Log*) gtk_clist_get_row_data( GTK_CLIST ( entry_list ),
					      last_row );
  
  set_fields();

  file_selection = NULL;
  plugin_selection = NULL;
  block_handlers = FALSE;
/*}}}*/

 gtk_widget_set_usize ( form , 0 , 300 );
 gtk_widget_show( form );
}
/*{{{  subroutines for menu creation  */
/*
  This routine creates a label and attaches it to a table
*/
void
attach_label(GtkWidget* table, gchar* string, gint x1, gint x2, gint y1, gint y2, gfloat align)
{
  
  GtkWidget* label;
  label = gtk_label_new( string );
  gtk_misc_set_alignment ( GTK_MISC (label), align, 0.5 );
  gtk_table_attach(GTK_TABLE(table), label, x1, x2, y1, y2,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_widget_show(label);
  
}
/*
  This routine creates a GtkEntry box and attaches it to a table
*/
GtkWidget*
entry_box(GtkWidget* table, gchar* def, gint x1, gint x2, gint y1, gint y2, gchar* tooltip)
{
  
  GtkWidget* text;
  text = gtk_entry_new();
  gtk_entry_set_text( GTK_ENTRY(text), def );
  gtk_entry_set_editable ( GTK_ENTRY(text),TRUE );
  gtk_table_attach(GTK_TABLE(table), text, x1, x2, y1, y2,
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_SHRINK | GTK_FILL), 
		   4, 4);
  gtk_tooltips_set_tip(tooltips, text, tooltip, tooltip);
  gtk_widget_show(text);
  
  return text;
  
}
/*}}}*/
/*}}}*/

/*{{{  terminate customize  */
void 
form_popdown (GtkWidget *, gpointer *)
{


  return;
}
void 
delete_clist_log_element(gpointer data)
{
  
  Log* attached_data = (Log*) data;

  attached_data->purge();

  delete attached_data;
  
}
/*}}}*/

/*{{{  customization main routines  */
/*{{{  File Selection Subroutines  */
void 
select_file(GtkWidget*, gpointer*)
{

  if ( file_selection != NULL ) 
    {
      
      gdk_window_show (GTK_WIDGET (file_selection)->window);
      gdk_window_raise (GTK_WIDGET (file_selection)->window);
      gtk_window_position(GTK_WINDOW (file_selection), GTK_WIN_POS_MOUSE);

      return;

    }

  file_selection = do_selection( filename_entry );

}
void
select_plugin(GtkWidget*, gpointer*)
{

  if ( plugin_selection != NULL ) 
    {
      
      gdk_window_show (GTK_WIDGET (plugin_selection)->window);
      gdk_window_raise (GTK_WIDGET (plugin_selection)->window);
      gtk_window_position(GTK_WINDOW (plugin_selection), GTK_WIN_POS_MOUSE);

      return;
      
    }

  plugin_selection = do_selection( plugin_entry );

}
GtkWidget*
do_selection(GtkWidget* entry_widget)
{

  GtkWidget* file_selector;

  /* Create a new file selection widget */
  if ( entry_widget == filename_entry )
    file_selector = gtk_file_selection_new ("Find Logfile");
  else
    file_selector = gtk_file_selection_new ("Find Plugin");
  
  gtk_signal_connect ( GTK_OBJECT (file_selector), "destroy",
		       (GtkSignalFunc) filesel_cancel_selected, 
		       (gpointer) file_selector );
  
  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect ( GTK_OBJECT 
		      (GTK_FILE_SELECTION (file_selector)->ok_button),
		      "clicked", 
		      (GtkSignalFunc) filesel_ok_selected, 
		      (gpointer) file_selector );
  
  /* Connect the cancel_button to file_cancel_sel function */
  gtk_signal_connect ( GTK_OBJECT 
		      (GTK_FILE_SELECTION (file_selector)->cancel_button),
		      "clicked", 
		      (GtkSignalFunc) filesel_cancel_selected,
		      (gpointer) file_selector );
  
  gtk_file_selection_set_filename (GTK_FILE_SELECTION(file_selector),
				   gtk_entry_get_text
				   ( GTK_ENTRY( entry_widget ) ));

  gtk_window_position(GTK_WINDOW(file_selector), GTK_WIN_POS_MOUSE);
  gtk_widget_show(file_selector);
  gtk_widget_grab_focus(file_selector);

  return file_selector;

}
void 
filesel_ok_selected (GtkWidget *, GtkFileSelection *fs)
{

  /* block handlers */
  block_handlers = TRUE;  
  
  if ( file_selection != NULL &&
       fs == GTK_FILE_SELECTION ( file_selection ) )
    {

      gtk_entry_set_text( GTK_ENTRY(filename_entry), 
			  gtk_file_selection_get_filename 
			  (GTK_FILE_SELECTION (fs)) );

    }
  
  if ( plugin_selection != NULL &&
       fs == GTK_FILE_SELECTION ( plugin_selection ) )
    {
      
      gtk_entry_set_text( GTK_ENTRY(plugin_entry), 
			  gtk_file_selection_get_filename 
			  (GTK_FILE_SELECTION (fs)) );

    }

  /* now destroy it */
  filesel_cancel_selected( NULL , fs );

  /* set handlers free again */
  block_handlers = FALSE;  

  /* accept the values */
  accept_values( NULL, NULL );

}
void 
filesel_cancel_selected (GtkWidget *, GtkFileSelection *fs)
{

  if ( file_selection != NULL &&
       fs == GTK_FILE_SELECTION ( file_selection ) )
    {

      gtk_widget_hide(file_selection);
      gtk_widget_destroy(file_selection);
      
      file_selection = NULL;

    }

  if ( plugin_selection != NULL &&
       fs == GTK_FILE_SELECTION ( plugin_selection ) )
    {

      gtk_widget_hide(plugin_selection);
      gtk_widget_destroy(plugin_selection);

      plugin_selection = NULL;

    }

}
/*}}}*/
/*

  Accept the values in the Entry Widgets and set the Label
  of the CList Entry accordingly
  
*/
void 
accept_values(GtkWidget*, gpointer*)
{ 
  if ( block_handlers == TRUE )
    return;

  /* block handlers */
  block_handlers = TRUE;  

  get_fields();
  
  gtk_clist_freeze( GTK_CLIST( entry_list ) );

  gtk_clist_set_text( GTK_CLIST( entry_list ),
		      last_row,
		      0,
		      last_entry->buttontext );

  gtk_clist_thaw( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );
 
  /* set handlers free again */
  block_handlers = FALSE;

}
/*

  Select row

*/
void 
select_entry_row_callback(GtkWidget *widget, gint row, gint column, GdkEventButton *event, gpointer data)
{
  if ( block_handlers == TRUE )
    return;

  /* block handlers */
  block_handlers = TRUE;  

  /* unselect previous item */
  gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			  last_row,
			  column );
  
  /* retrieve changed values */
  get_fields();

  /* activate new selection */
  last_row = row;
  last_entry = (Log*) gtk_clist_get_row_data( GTK_CLIST ( entry_list ),
					      last_row );
  set_fields();
  
  /* set handlers free again */
  block_handlers = FALSE;
  
}
/*

  Prevent the user from unselecting a row - we need at least one
  active selection all the time.
  
*/
void 
unselect_entry_row_callback(GtkWidget *widget, gint row, gint column, GdkEventButton *event, gpointer data)
{
  if ( block_handlers == TRUE )
    return;

  /* prevent that we don't have any selection at all */
  if ( row == last_row )
    {
      
      block_handlers = TRUE;
      
      gtk_clist_select_row( GTK_CLIST( entry_list ),
			    row,
			    column );
      
      block_handlers = FALSE;
      
    }
  
}
/*

  Set the menu to the entries according to the data element

*/
void
set_fields()
{
  
  gtk_entry_set_text( GTK_ENTRY(button_text_entry), 
		      last_entry->buttontext );

  gtk_entry_set_text( GTK_ENTRY(help_text_entry), 
		      last_entry->help );

  gtk_entry_set_text( GTK_ENTRY(filename_entry), 
		      last_entry->filename );

  if ( last_entry->plugin != NULL )
    {
      gtk_entry_set_text( GTK_ENTRY(plugin_entry), 
			  last_entry->plugin );
    }
  else
    {
      gtk_entry_set_text( GTK_ENTRY(plugin_entry), 
			  "" );
    }

  gtk_spin_button_set_value( GTK_SPIN_BUTTON(interval_selection), (gfloat) 
			     ((gfloat) last_entry->interval / 10 ) );

  gtk_spin_button_set_value( GTK_SPIN_BUTTON(delay_selection), (gfloat) 
			     ((gfloat) last_entry->delay / 10 ) );

  switch ( last_entry->mode )
    {
    case TAIL_FILE:
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (tail_button), 
				    TRUE);
      break;
    case RUN_FILE:
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (run_button), 
				    TRUE);
      break;
    case CAT_FILE:
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (cat_button), 
				    TRUE);
      break;
    }

  /* Set Filter */
  setup_filter();

}
/*

  Set the data element according to the entries

*/
void
get_fields()
{

  gint mode = 0;
  if (GTK_TOGGLE_BUTTON (tail_button)->active) mode = TAIL_FILE;
  if (GTK_TOGGLE_BUTTON (run_button)->active) mode = RUN_FILE;
  if (GTK_TOGGLE_BUTTON (cat_button)->active) mode = CAT_FILE;
  last_entry->init( gtk_entry_get_text( GTK_ENTRY(filename_entry) ),
		    gtk_entry_get_text( GTK_ENTRY(help_text_entry) ),
		    gtk_entry_get_text( GTK_ENTRY(button_text_entry) ),
		    gtk_entry_get_text( GTK_ENTRY(plugin_entry) ),
		    mode,
		    (gint) ( 10 * gtk_spin_button_get_value_as_float 
			     ( GTK_SPIN_BUTTON(interval_selection) ) ),
		    (gint) ( 10 * gtk_spin_button_get_value_as_float 
			     ( GTK_SPIN_BUTTON(delay_selection) ) ) );

  /* and don't forget the filter settings ! */
  retrieve_filter();

}
/*

  Clear all data fields
  
*/
void
clear_all_fields()
{

  block_handlers = block_filter_handlers = TRUE;

  if ( file_selection != NULL )
    {
      gtk_widget_destroy( file_selection );
      file_selection = NULL;
    }  

  if ( plugin_selection != NULL )
    {
      gtk_widget_destroy( plugin_selection );
      plugin_selection = NULL;
    }

  gtk_entry_set_text( GTK_ENTRY( button_text_entry ), "" );
  gtk_entry_set_text( GTK_ENTRY( help_text_entry ), "" );
  gtk_entry_set_text( GTK_ENTRY( filename_entry ), "" );
  gtk_entry_set_text( GTK_ENTRY( plugin_entry ), "" );

  gtk_spin_button_set_value( GTK_SPIN_BUTTON(interval_selection), 
			     (gfloat) 0.1);
  gtk_spin_button_set_value( GTK_SPIN_BUTTON(delay_selection), 
			     (gfloat) 0.1 );

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (tail_button), 
				TRUE);
  
  clear_all_filter_fields();
  last_filter = NULL;
  last_filter_row = -1;

  block_handlers = block_filter_handlers = FALSE;

}
/*}}}*/

/*{{{  Main Buttons   */
void
set_general_sensitivity(gboolean status)
{

  gtk_widget_set_sensitive( move_up_button , status );
  gtk_widget_set_sensitive( move_down_button , status );
  gtk_widget_set_sensitive( delete_button , status );
  gtk_widget_set_sensitive( entry_list , status );
  gtk_widget_set_sensitive( button_text_entry , status );
  gtk_widget_set_sensitive( help_text_entry , status );
  gtk_widget_set_sensitive( plugin_entry , status );
  gtk_widget_set_sensitive( select_plugin_button , status );
  gtk_widget_set_sensitive( filename_entry , status );
  gtk_widget_set_sensitive( select_filename_button , status );
  gtk_widget_set_sensitive( interval_selection , status );
  gtk_widget_set_sensitive( delay_selection , status ); 
  gtk_widget_set_sensitive( tail_button , status );
  gtk_widget_set_sensitive( cat_button , status );
  gtk_widget_set_sensitive( run_button , status );
  gtk_widget_set_sensitive( apply_button , status );
  gtk_widget_set_sensitive( add_filter_button , status );
  
  if ( status == TRUE )
    return;
  
  if ( file_selection != NULL )
    gtk_widget_destroy( file_selection );
  
  if ( plugin_selection != NULL )
    gtk_widget_destroy( plugin_selection );
  
  file_selection = plugin_selection = NULL;
  
}
void
insert_new_entry(GtkWidget*, gpointer)
{

  block_handlers = TRUE;

  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  if ( GTK_CLIST( entry_list )->rows > 0 )
    {
      
      get_fields();

      gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			      last_row,
			      0 );

    }
  else
    {

      last_row = 0;

    }

  Log* new_entry = new Log;
  new_entry->init( "logfile/device/program" ,
		   "" ,
		   "NEW ENTRY" ,
		   NULL ,
		   TAIL_FILE ,
		   3 ,
		   6 );
  
  gchar* array[2];
  array[1] = NULL;
  array[0] = new_entry->buttontext;
  
  gtk_clist_insert ( GTK_CLIST ( entry_list ),
		     last_row,
		     array );

  gtk_clist_set_row_data_full( GTK_CLIST( entry_list ),
			       last_row,
			       (gpointer) new_entry,
			       (GtkDestroyNotify) 
			       delete_clist_log_element );

  last_entry = new_entry;

  last_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( entry_list ) , last_entry );

  gtk_clist_select_row( GTK_CLIST( entry_list ),
			last_row,
			0 );
  
  set_fields();

  set_general_sensitivity( TRUE );
  
  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  

  block_handlers = FALSE;

}
void
move_entry_up(GtkWidget*, gpointer)
{

  if ( last_row == 0 ) return;

  block_handlers = TRUE;

  gint new_last_row = last_row--;
  if ( new_last_row < 0 ) 
    new_last_row = 0;

  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			  last_row,
			  0 );  
  
  gtk_clist_row_move ( GTK_CLIST( entry_list ),
		       last_row ,
		       new_last_row );

  last_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( entry_list ) , last_entry );

  gtk_clist_select_row( GTK_CLIST( entry_list ),
			  last_row,
			  0 );

  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  

  block_handlers = FALSE;
}
void
move_entry_down(GtkWidget*, gpointer)
{

  if ( last_row == ( GTK_CLIST( entry_list )->rows - 1 )) 
    return;

  block_handlers = TRUE;

  gint new_last_row = last_row++;
  if ( new_last_row >= GTK_CLIST( entry_list )->rows ) 
    new_last_row = GTK_CLIST( entry_list )->rows - 1;
  
  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			  last_row,
			  0 );  
  
  gtk_clist_row_move ( GTK_CLIST( entry_list ),
		       last_row ,
		       new_last_row );

  last_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( entry_list ) , last_entry );

  gtk_clist_select_row( GTK_CLIST( entry_list ),
			last_row,
			0 );

  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  

  block_handlers = FALSE;

}
void
delete_entry(GtkWidget*, gpointer)
{

  if ( GTK_CLIST( entry_list )->rows == 0 )
    return;

  block_handlers = block_filter_handlers = TRUE;
  
  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			  last_row,
			  0 );  

  clear_all_filter_fields();
  set_filter_sensitive( FALSE );

  get_fields();

  gtk_clist_remove ( GTK_CLIST( entry_list ),
		     last_row );

  last_row--;
  if ( last_row < 0 ) last_row = 0;

  if ( GTK_CLIST( entry_list )->rows == 0 )
    {

      clear_all_fields();
      set_general_sensitivity( FALSE );

    }
  else
    {
     
      gtk_clist_select_row( GTK_CLIST( entry_list ),
			    last_row,
			    0 ); 

      last_entry = (Log*) gtk_clist_get_row_data( GTK_CLIST( entry_list ), 
						  last_row );
      
      set_fields();

    }

  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  

  block_handlers = block_filter_handlers = FALSE;

}
void
customization_done(GtkWidget*, gpointer)
{

  block_handlers = block_filter_handlers = TRUE;
  
  gtk_clist_freeze ( GTK_CLIST( entry_list ) );
  
  if ( GTK_CLIST( entry_list )->rows == 0 )
    customization_abort( NULL, NULL );


  get_fields();

  gtk_clist_unselect_row( GTK_CLIST( entry_list ),
			  last_row,
			  0 );

  Log** new_array = NULL;
  gint i = 0;
  Log* pointer;
  pointer = (Log*) gtk_clist_get_row_data( GTK_CLIST( entry_list ), i );
  while ( pointer != NULL )
    {
      
      new_array = add_log_entry( new_array ,
				 pointer->mode ,
				 pointer->filename ,
				 pointer->help ,
				 pointer->buttontext ,
				 pointer->plugin ,
				 pointer->interval ,
				 pointer->delay );
      
      gint j = 0;
      while ( pointer->filter != NULL &&
	      pointer->filter[j] != NULL )
	{

	  add_filter_to_last( new_array ,
			      pointer->filter[j]->string,
			      pointer->filter[j]->mode,
			      pointer->filter[j]->execline );

	  j++;
	}

      i++;
      pointer = (Log*) gtk_clist_get_row_data( GTK_CLIST( entry_list ), i );
    }

  if ( file_selection != NULL )
    gtk_widget_destroy( file_selection );
  
  if ( plugin_selection != NULL )
    gtk_widget_destroy( plugin_selection );
  
  file_selection = plugin_selection = NULL;

  disassemble_database_menu();
  gtk_widget_destroy( form );
  form = NULL;
  block_handlers = block_filter_handlers = FALSE;

  /* stop everything for reinitialisation */
  deactivate();
  disable();
  stop_watchdog();

  /* setup stuff anew */
  destroy_buttons();
  destroy_log_array(entry);
  entry = new_array;
  create_buttons();
  if ( active >= syslogs ) 
    active = syslogs-1;
  enable();
  start_watchdog();
  activate();
  
  configuration_changed = TRUE;
  customizing = FALSE;
}
void
customization_abort(GtkWidget*, gpointer)
{

  if ( file_selection != NULL )
    gtk_widget_destroy( file_selection );
  
  if ( plugin_selection != NULL )
    gtk_widget_destroy( plugin_selection );
  
  file_selection = plugin_selection = NULL;

  disassemble_database_menu();
  gtk_widget_destroy( form );
  form = NULL;

  customizing = FALSE;

}
/*}}}*/

/*{{{  Database Interaction  */
void
open_database(GtkWidget*, gpointer)
{

  startup_database_menu();

}
void 
remove_all_entries()
{

  block_handlers = block_filter_handlers = TRUE;
  
  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  gtk_clist_unselect_row( GTK_CLIST( entry_list ), last_row, 0 );  

  clear_all_filter_fields();
  set_filter_sensitive( FALSE );

  gtk_clist_clear( GTK_CLIST ( entry_list ) );  
  
  clear_all_fields();
  set_general_sensitivity( FALSE );

  last_entry = NULL;
  last_row = 0;

  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  

  block_handlers = block_filter_handlers = FALSE;

}
void 
add_entry_from_database( Log* entry )
{

  block_handlers = TRUE;
  
  gtk_clist_freeze ( GTK_CLIST( entry_list ) );

  if ( last_row >= ( GTK_CLIST( entry_list )->rows - 1 ) )
    last_row = ( GTK_CLIST( entry_list )->rows - 1);
  last_row++;

  Log* new_entry = new Log;
  new_entry->init( entry->filename ,
		   entry->help ,
		   entry->buttontext ,
		   entry->plugin ,
		   entry->mode ,
		   entry->interval ,
		   entry->delay );
  
  gint j = 0;
  while ( entry->filter != NULL &&
	  entry->filter[j] != NULL )
    {
      
      new_entry->add_filter( entry->filter[j]->string ,
			     entry->filter[j]->mode ,
			     entry->filter[j]->execline );
      
      j++;
    }

  gchar* array[2];
  array[1] = NULL;
  array[0] = new_entry->buttontext;
  
  gtk_clist_insert ( GTK_CLIST ( entry_list ),
		     last_row,
		     array );

  gtk_clist_set_row_data_full( GTK_CLIST( entry_list ),
			       last_row,
			       (gpointer) new_entry,
			       (GtkDestroyNotify) 
			       delete_clist_log_element );
  
  last_entry = new_entry;

  last_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( entry_list ) , last_entry );
  
  gtk_clist_select_row( GTK_CLIST( entry_list ),
			last_row,
			0 );

  set_fields();

  set_general_sensitivity( TRUE );
  
  gtk_clist_thaw ( GTK_CLIST( entry_list ) );
  gtk_widget_draw( entry_list , NULL );  
  
  block_handlers = FALSE;
  
}
/*}}}*/

/*{{{  Filter Initialisation & Evaluation  */
void
set_filter_sensitive(gboolean status)
{

  gtk_widget_set_sensitive( delete_filter_button , status );
  gtk_widget_set_sensitive( filter_string , status );
  gtk_widget_set_sensitive( execute_string , status );
  gtk_widget_set_sensitive( invert_button , status );
  gtk_widget_set_sensitive( case_button , status );
  gtk_widget_set_sensitive( raise_button , status );
  gtk_widget_set_sensitive( lower_button , status );
  gtk_widget_set_sensitive( hide_button , status );
  gtk_widget_set_sensitive( alert_button , status );
  gtk_widget_set_sensitive( notice_button , status );
  gtk_widget_set_sensitive( uniconify_button , status );
  gtk_widget_set_sensitive( execute_button , status );
  gtk_widget_set_sensitive( filter_list , status );
  
}
gint 
compare_filter_clist_elements (GtkCList *clist, gconstpointer ptr1, gconstpointer ptr2)
{

  GtkCListRow *row1 = (GtkCListRow *) ptr1;
  GtkCListRow *row2 = (GtkCListRow *) ptr2;

  gint x = 0;
  gint y = 0;
  while ( y == 0 && x < 7 )
    {
      
      gchar* string1 = GTK_CELL_TEXT (row1->cell[x])->text;
      gchar* string2 = GTK_CELL_TEXT (row2->cell[x])->text; 

      if ( string1 != NULL &&
	   string2 != NULL )
	y = strcmp( string1 , string2 );
      else
	y = 0;

      x++;
      
    }

  return y;

}
void 
delete_clist_filter_element(gpointer data)
{  

  Filter* filter_element = (Filter*) data;

  delete filter_element;

}
void
setup_filter()
{

  /* block the filter handlers */
  block_filter_handlers = TRUE;

  gtk_clist_freeze( GTK_CLIST ( filter_list ) );

  gtk_clist_clear( GTK_CLIST ( filter_list ) );

  if ( last_entry->filter == NULL )
    {

      clear_all_filter_fields();
      last_filter = NULL;
      last_filter_row = -1;

      set_filter_sensitive( FALSE );
      return;
      
    }

  set_filter_sensitive( TRUE );

  gint x = 0;
  while ( last_entry->filter != NULL &&
	  last_entry->filter[x] != NULL )
    {

      insert_entry( last_entry->filter[x], x );

      x++;

    }

  delete last_entry->filter;
  last_entry->filter = NULL;
  last_entry->filterclass = NO_FILTER;
  
  /* sort the list */
  gtk_clist_sort ( GTK_CLIST( filter_list ) );
  
  if ( x > 0 )
    {

      last_filter_row = 0;
      
      gtk_clist_select_row( GTK_CLIST( filter_list ),
			    last_filter_row,
			    0 );

      last_filter = (Filter*) gtk_clist_get_row_data
	( GTK_CLIST ( filter_list ),
	  last_filter_row );

      set_filter_fields();

    }
  else
    {
      
      /* we should never get here, but stranger things have happened.. */

      clear_all_filter_fields();
      last_filter = NULL;
      last_filter_row = -1;
      
      set_filter_sensitive( FALSE );
      
    }

  gtk_clist_thaw( GTK_CLIST ( filter_list ) );
  gtk_widget_draw( filter_list , NULL );

  /* unblock them again */
  block_filter_handlers = FALSE;

}
void
insert_entry(Filter* filter, gint i)
{

  gchar* array[7];
  determine_columns( filter, array );
  
  gtk_clist_insert ( GTK_CLIST ( filter_list ),
		     i,
		     array );

  gtk_clist_set_row_data_full ( GTK_CLIST ( filter_list ),
				i,
				(gpointer) filter,
				(GtkDestroyNotify) 
				delete_clist_filter_element );

}
void
determine_columns(Filter* filter, gchar** flags)
{
  
  flags[0] = " ";
  switch ( filter->mode & CLASS0_MASK )
    {
    case RAISE:
      flags[0] = "R";
      break;
    case LOWER:
      flags[0] = "L";
      break;
    case HIDE:
      flags[0] = "H";
      break;
    }
  
  flags[1] = " ";
  if ( filter->mode & ALERT )
    flags[1] = "A";
  
  flags[2] = " ";
  if ( filter->mode & NOTICE )
    flags[2] = "N";
  
  flags[3] = " ";
  if ( filter->mode & UNICONIFY )
    flags[3] = "U";
  
  flags[4] = " ";
  if ( filter->mode & EXECUTE )
    flags[4] = "E";
  
  flags[5] = filter->string;
  
  flags[6] = filter->execline;
  
}
void
retrieve_filter()
{
  /* block the filter handlers */
  block_filter_handlers = TRUE;

  if ( GTK_CLIST ( filter_list )->rows == 0 || last_filter == NULL ) 
    return;

  get_filter_fields( last_filter->mode & CLASS0_MASK );

  gtk_clist_freeze( GTK_CLIST ( filter_list ) );

  /* make sure the active element does not have a
     filter at the moment */
  if ( last_entry->filter != NULL )
    {

      gint k = 0;
      while ( last_entry->filter[k] != NULL )
	{
	  delete last_entry->filter[k];
	  k++;
	}

      delete last_entry->filter;
      last_entry->filter = NULL;
      last_entry->filterclass = NO_FILTER;
    }
  
  gint i = 0;
  Filter* pointer;
  pointer = (Filter*) gtk_clist_get_row_data( GTK_CLIST( filter_list ), i );
  while ( pointer != NULL )
    {

      last_entry->add_filter( pointer->string,
			      pointer->mode,
			      pointer->execline );
      
      i++;
      pointer = (Filter*) gtk_clist_get_row_data
	( GTK_CLIST( filter_list ), i );
    }

  gtk_clist_thaw( GTK_CLIST ( filter_list ) );
  gtk_widget_draw( filter_list , NULL );

  /* unblock them again */
  block_filter_handlers = FALSE;

}
/*}}}*/

/*{{{  Filter Menu Maintenance Routines  */
/*

  Select row

*/
void 
select_filter_entry_row_callback(GtkWidget *widget, gint row, gint column, GdkEventButton *event, gpointer data)
{
  if ( block_filter_handlers == TRUE )
    return;

  /* block handlers */
  block_filter_handlers = TRUE;  

  /* unselect previous item */
  gtk_clist_unselect_row( GTK_CLIST( filter_list ),
			  last_filter_row,
			  column );
  
  /* retrieve changed values */
  get_filter_fields( last_filter->mode & CLASS0_MASK );

  /* activate new selection */
  last_filter_row = row;
  last_filter = (Filter*) gtk_clist_get_row_data( GTK_CLIST ( filter_list ),
						  last_filter_row );
  set_filter_fields();
  
  /* set handlers free again */
  block_filter_handlers = FALSE;
  
}
/*

  Prevent the user from unselecting a row - we need at least one
  active selection all the time.
  
*/
void 
unselect_filter_entry_row_callback(GtkWidget *widget, gint row, gint column, GdkEventButton *event, gpointer data)
{
  if ( block_filter_handlers == TRUE )
    return;

  /* prevent that we don't have any selection at all */
  if ( row == last_filter_row )
    {
      
      block_filter_handlers = TRUE;
      
      gtk_clist_select_row( GTK_CLIST( filter_list ),
			    row,
			    column );
      
      block_filter_handlers = FALSE;
      
    }
  
}
/*

  Accept the values in the Filter Widgets and set the Label
  of the Filter CList Entry accordingly
  
*/
void 
accept_filter_values(GtkWidget*, gpointer data)
{ 
  if ( block_filter_handlers == TRUE )
    return;

  gint default_class0_filter = NO_FILTER;
  if ( data != NULL )
    default_class0_filter = (gint) data;

  /* block handlers */
  block_filter_handlers = TRUE;  

  get_filter_fields( default_class0_filter );
  
  gtk_clist_freeze( GTK_CLIST( filter_list ) );

  gchar* array[7];
  determine_columns( last_filter, array );  

  for ( gint i = 0 ; i < 7 ; i++ )
    gtk_clist_set_text( GTK_CLIST( filter_list ),
			last_filter_row,
			i,
			array[i] );

  /* sort the list */
  gtk_clist_sort ( GTK_CLIST( filter_list ) );
  last_filter_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( filter_list ) , last_filter );
  
  gtk_clist_thaw( GTK_CLIST( filter_list ) );
  gtk_widget_draw( filter_list , NULL );
  
  /* set the fields again */
  set_filter_fields();
  
  /* set handlers free again */
  block_filter_handlers = FALSE;
  
}
/*

  Set the menu to the entries according to the filter element

*/
void
set_filter_fields()
{
  
  if ( last_filter->mode & RAISE )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (raise_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (raise_button), 
				  FALSE);

  if ( last_filter->mode & LOWER )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (lower_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (lower_button), 
				  FALSE);

  if ( last_filter->mode & HIDE )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (hide_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (hide_button), 
				  FALSE);
  
  if ( last_filter->mode & ALERT )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (alert_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (alert_button), 
				  FALSE);
  
  if ( last_filter->mode & NOTICE )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (notice_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (notice_button), 
				  FALSE);
  
  if ( last_filter->mode & UNICONIFY )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (uniconify_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (uniconify_button), 
				  FALSE);

  if ( last_filter->mode & EXECUTE )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (execute_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (execute_button), 
				  FALSE);
  
  if ( last_filter->mode & INVERT )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (invert_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (invert_button), 
				  FALSE);
  
  if ( last_filter->mode & CASE_SENSITIVE )
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (case_button), 
				  TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (case_button), 
				  FALSE);
  
  gtk_entry_set_text( GTK_ENTRY(filter_string), 
		      last_filter->string );
  
  if ( last_filter->execline != NULL )
    gtk_entry_set_text( GTK_ENTRY(execute_string), 
			last_filter->execline );
  else
    gtk_entry_set_text( GTK_ENTRY(execute_string), 
			"" );
  
}
/*

  Set the data element according to the filter data

*/
void
get_filter_fields( gint default_class0_mode )
{

  gint mode = NO_FILTER;
  if (GTK_TOGGLE_BUTTON ( raise_button )->active)
      mode |= RAISE;  
  if (GTK_TOGGLE_BUTTON ( lower_button )->active)
      mode |= LOWER;
  if (GTK_TOGGLE_BUTTON ( hide_button )->active)
      mode |= HIDE;

  mode = legalize( mode, default_class0_mode );

  if (GTK_TOGGLE_BUTTON ( alert_button )->active)
    mode |= ALERT;
  
  if (GTK_TOGGLE_BUTTON ( notice_button )->active)
    mode |= NOTICE;
  
  if (GTK_TOGGLE_BUTTON ( uniconify_button )->active)
    mode |= UNICONIFY;
  
  if (GTK_TOGGLE_BUTTON ( execute_button )->active)
    mode |= EXECUTE;

  if (GTK_TOGGLE_BUTTON ( invert_button )->active)
    mode |= INVERT;
  
  if (GTK_TOGGLE_BUTTON ( case_button )->active)
    mode |= CASE_SENSITIVE;

  gchar* execline = NULL;
  if ( mode & EXECUTE )
    execline = gtk_entry_get_text( GTK_ENTRY(execute_string) );
 
  gchar* regexline = gtk_entry_get_text( GTK_ENTRY(filter_string) );
  
  if ( last_filter != NULL )
    {

      last_filter->purge();
      last_filter->init( regexline , mode | COMPILE_REGEX );
      
      if ( execline != NULL &&
	   strlen(execline) > 0 )
	{
	  
	  last_filter->execline = new gchar[ strlen( execline ) + 2 ];
	  strcpy( last_filter->execline , execline );
	  
	}
      
    }
  
}
void
clear_all_filter_fields()
{

  gtk_clist_clear( GTK_CLIST ( filter_list ) );
  gtk_entry_set_text( GTK_ENTRY( filter_string ), "" );
  gtk_entry_set_text( GTK_ENTRY( execute_string ), "" );

  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (invert_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (case_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (raise_button), 
FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (lower_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (hide_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (alert_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (notice_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (uniconify_button), 
				FALSE );
  gtk_toggle_button_set_state ( GTK_TOGGLE_BUTTON (execute_button), 
				FALSE );
  
}
/*}}}*/

/*{{{  Filter Buttons  */
void
add_filter(GtkWidget*, gpointer)
{

  block_filter_handlers = TRUE;

  gtk_clist_freeze ( GTK_CLIST( filter_list ) );

  if ( GTK_CLIST( filter_list )->rows > 0 )
    {

      /* retrieve changed values */
      get_filter_fields( last_filter->mode & CLASS0_MASK );

    }

  Filter* new_filter = new Filter;
  new_filter->init( "enter REGEX here", RAISE );

  insert_entry( new_filter, 0 );
 
  /* sort the list */
  gtk_clist_sort ( GTK_CLIST( filter_list ) );
  
  last_filter_row = gtk_clist_find_row_from_data
    ( GTK_CLIST( filter_list ) , new_filter );
  last_filter = new_filter;

  gtk_clist_select_row( GTK_CLIST( filter_list ),
			last_filter_row,
			0 );
  
  set_filter_fields();
  
  set_filter_sensitive( TRUE );
  
  gtk_clist_thaw ( GTK_CLIST( filter_list ) );
  gtk_widget_draw( filter_list , NULL );

  block_filter_handlers = FALSE;
  
}
void
delete_filter(GtkWidget*, gpointer)
{
  
  if ( GTK_CLIST( filter_list )->rows == 0 )
    return;

   block_filter_handlers = TRUE;

   gtk_clist_freeze ( GTK_CLIST( filter_list ) );

   gint kill_row = last_filter_row;
   last_filter_row--;
   if ( last_filter_row < 0 )
     last_filter_row = 0;

   gtk_clist_remove( GTK_CLIST( filter_list ) ,
		     kill_row );

   if ( GTK_CLIST( filter_list )->rows == 0 )
     {

       clear_all_filter_fields();

       set_filter_sensitive( FALSE );

     }
   else
     {

       /* sort the list */
       gtk_clist_sort ( GTK_CLIST( filter_list ) );
       
       gtk_clist_select_row( GTK_CLIST( filter_list ),
			     last_filter_row,
			     0 );

       last_filter = (Filter*) gtk_clist_get_row_data
	 ( GTK_CLIST( filter_list ), last_filter_row );
  
       set_filter_fields();

     }

   gtk_clist_thaw ( GTK_CLIST( filter_list ) );
   gtk_widget_draw( filter_list , NULL );

   block_filter_handlers = FALSE;

}
/*}}}*/
