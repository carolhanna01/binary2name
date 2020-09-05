/*

  database.cc

  database handling subroutines for xlogmaster.cc

  Copyright (C) 1998,1999 Georg C. F. Greve
  Portions Copyright (C) 2005 Free Software Foundation, Inc.
  

This file is part of GNU xlogmaster.

GNU xlogmaster is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

GNU xlogmaster is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.

  Contact: 
           mailto:xlogmaster-bugs@gnu.org
           http://www.gnu.org/software/xlogmaster/
  
  Last modification: Portion of groupify function re-written to fix
                     segmentation faults.
                     September 2005 - John S. Gaythorpe - Maintainer
	 
*/

/*{{{  Header  */

#include "../config.h"
#include "sysinc.H"
#include "logclass.H"
#include "database.H"
#include "extern.H"

/*}}}*/

using namespace std;	/* jsg - added to fix cerr decl using gcc */

/*{{{  Database Menu  */
void
startup_database_menu()
{
  if ( db_form != NULL ) return;

  /* create window */
  db_form = gtk_window_new (GTK_WINDOW_DIALOG);

  gtk_signal_connect (GTK_OBJECT (db_form), "destroy",
		      GTK_SIGNAL_FUNC (disassemble_database_menu), NULL);
  gtk_signal_connect (GTK_OBJECT (db_form), "delete_event",
		      GTK_SIGNAL_FUNC (disassemble_database_menu), NULL);
  gtk_window_set_title (GTK_WINDOW (db_form), "Database");
  gtk_container_border_width(GTK_CONTAINER(db_form), 5);
  
  /* create vbox and tables */
  GtkWidget* vbox = gtk_vbox_new( FALSE, 0 );
  gtk_container_add(GTK_CONTAINER (db_form), vbox);
  gtk_widget_show(vbox);
  GtkWidget* table = gtk_table_new(6, 6, TRUE);
  gtk_box_pack_start( GTK_BOX(vbox), table, TRUE, TRUE, 2);
  gtk_widget_show(table);
  GtkWidget* buttontable = gtk_table_new(1, 1, TRUE);
  gtk_box_pack_start( GTK_BOX(vbox), buttontable, FALSE, FALSE, 2);
  gtk_widget_show(buttontable);

  /* insert labels */
  GtkWidget* label = gtk_label_new ( "Sources:" );
  gtk_misc_set_alignment( GTK_MISC(label), 0, 0.7);
  gtk_table_attach(GTK_TABLE(table), label, 0, 6, 0, 2, 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 5, 5);
  gtk_widget_show( label );
  label = gtk_label_new ( "Selection:" );
  gtk_misc_set_alignment( GTK_MISC(label), 0, 0.7);
  gtk_table_attach(GTK_TABLE(table), label, 0, 6, 12, 14, 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 5, 5);
  gtk_widget_show( label );

  gchar* titles[7];
  titles[0] = "Button text ";
  titles[1] = "Mode";
  titles[2] = "Filename     ";
  titles[3] = "Filter";
  titles[4] = "Help text           ";
  /* create file list */
  sourcelist = gtk_clist_new_with_titles(5, titles);
  gtk_clist_set_selection_mode(GTK_CLIST(sourcelist), 
			       GTK_SELECTION_SINGLE);
  gtk_clist_set_column_justification(GTK_CLIST(sourcelist), 0, 
				     GTK_JUSTIFY_LEFT); 
  gtk_clist_set_column_auto_resize( GTK_CLIST( sourcelist ), 0, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(sourcelist), 1, 
				     GTK_JUSTIFY_CENTER);
  gtk_clist_set_column_auto_resize( GTK_CLIST( sourcelist ), 1, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(sourcelist), 2, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( sourcelist ), 2, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(sourcelist), 3, 
				     GTK_JUSTIFY_CENTER);
  gtk_clist_set_column_auto_resize( GTK_CLIST( sourcelist ), 3, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(sourcelist), 4, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( sourcelist ), 4, TRUE);
  gtk_clist_column_titles_show(GTK_CLIST(sourcelist));
  gtk_clist_column_titles_passive(GTK_CLIST(sourcelist));
  gtk_clist_set_shadow_type(GTK_CLIST(sourcelist), GTK_SHADOW_ETCHED_OUT);
  gtk_clist_set_sort_type( GTK_CLIST( sourcelist ),
			   GTK_SORT_ASCENDING );
  gtk_clist_set_sort_column( GTK_CLIST( sourcelist ), 0 );
  gtk_clist_set_auto_sort ( GTK_CLIST( sourcelist ), FALSE );
  GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      sourcelist);
  gtk_widget_show( scrolled_window );
  gtk_table_attach(GTK_TABLE(table), scrolled_window, 0, 4, 2, 12, 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 5, 5);
  gtk_widget_show(sourcelist);


  /* create target list */
  targetlist = gtk_clist_new_with_titles(5, titles);
  gtk_clist_set_selection_mode(GTK_CLIST(targetlist), 
			       GTK_SELECTION_SINGLE);
  gtk_clist_set_column_justification(GTK_CLIST(targetlist), 0, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( targetlist ), 0, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(targetlist), 1, 
				     GTK_JUSTIFY_CENTER);
  gtk_clist_set_column_auto_resize( GTK_CLIST( targetlist ), 1, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(targetlist), 2, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( targetlist ), 2, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(targetlist), 3, 
				     GTK_JUSTIFY_CENTER);
  gtk_clist_set_column_auto_resize( GTK_CLIST( targetlist ), 3, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(targetlist), 4, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( targetlist ), 4, TRUE);
  gtk_clist_column_titles_show(GTK_CLIST(targetlist));
  gtk_clist_column_titles_passive(GTK_CLIST(targetlist));
  gtk_clist_set_shadow_type(GTK_CLIST(targetlist), GTK_SHADOW_ETCHED_OUT);
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      targetlist );
  gtk_widget_show( scrolled_window );
  gtk_table_attach(GTK_TABLE(table), scrolled_window, 0, 6, 14, 25, 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 5, 5);
 
  gtk_widget_show(targetlist);

  
  /* create group list */
  titles[0] = "Group";
  titles[1] = "Description";
  grouplist = gtk_clist_new_with_titles(2, titles);
  gtk_clist_set_selection_mode(GTK_CLIST(grouplist), 
			       GTK_SELECTION_SINGLE);
  gtk_clist_set_column_justification(GTK_CLIST(grouplist), 0, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( grouplist ), 0, TRUE);
  gtk_clist_set_column_justification(GTK_CLIST(grouplist), 1, 
				     GTK_JUSTIFY_LEFT);
  gtk_clist_set_column_auto_resize( GTK_CLIST( grouplist ), 1, TRUE);
  gtk_clist_column_titles_show(GTK_CLIST(grouplist));
  gtk_clist_column_titles_passive(GTK_CLIST(grouplist));
  gtk_clist_set_shadow_type(GTK_CLIST(grouplist), GTK_SHADOW_ETCHED_OUT);
  gtk_clist_set_sort_type( GTK_CLIST( grouplist ),
			   GTK_SORT_ASCENDING );
  gtk_clist_set_sort_column( GTK_CLIST( grouplist ), 0 );
  gtk_clist_set_auto_sort ( GTK_CLIST( grouplist ), FALSE );
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, 
				  GTK_POLICY_AUTOMATIC );
  gtk_scrolled_window_add_with_viewport 
    ( GTK_SCROLLED_WINDOW ( scrolled_window ), 
      grouplist );
  gtk_widget_show( scrolled_window );
  gtk_table_attach(GTK_TABLE(table), scrolled_window, 4, 6, 2, 12,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 5, 5);
  gtk_tooltips_set_tip(tooltips, grouplist, "...list of predefined groups - single click to select...","...list of predefined groups - single click to select...");
  gtk_widget_show(grouplist);

  /* create buttons */
  addbutton = gtk_button_new_with_label ("    Add    ");
  gtk_tooltips_set_tip(tooltips, addbutton, "...add selected entries to existing ones...","...add selected entries to existing ones...");
  gtk_signal_connect (GTK_OBJECT (addbutton), "clicked",
			GTK_SIGNAL_FUNC (add_button_pressed), NULL );
  gtk_table_attach(GTK_TABLE(buttontable), addbutton, 3, 4, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		   4, 4);
  gtk_widget_show(addbutton);
  
  replacebutton = gtk_button_new_with_label ("   Replace   ");
  gtk_tooltips_set_tip(tooltips, replacebutton, "...replace existing entries with selected ones...","...replace existing entries with selected ones...");
  gtk_signal_connect (GTK_OBJECT (replacebutton), "clicked",
		      GTK_SIGNAL_FUNC (replace_button_pressed), NULL );
  gtk_table_attach(GTK_TABLE(buttontable), replacebutton, 4, 5, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		   4, 4);
  gtk_widget_show(replacebutton);
  
  GtkWidget* button = gtk_button_new_with_label ("   Cancel   ");
  gtk_tooltips_set_tip(tooltips, button, "...leave database without touching anything...","...leave database without touching anything...");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      GTK_SIGNAL_FUNC (disassemble_database_menu), NULL );
  gtk_table_attach(GTK_TABLE(buttontable), button, 5, 6, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ) , 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL | GTK_SHRINK ), 
		   4, 4);
  gtk_widget_show(button);

  helpbutton = gtk_button_new_with_label ("   Help   ");
  gtk_tooltips_set_tip(tooltips, helpbutton, "...give a little help about this menu...","...give a little help about this menu...");
  gtk_signal_connect (GTK_OBJECT (helpbutton), "clicked",
		      GTK_SIGNAL_FUNC (help_button_pressed), NULL );
  gtk_table_attach(GTK_TABLE(buttontable), helpbutton, 0, 1, 0, 1,
                   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL ), 
		   (GtkAttachOptions) ( GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		   4, 4);
  gtk_widget_show(helpbutton);

  /*
    Without user selection you can't add or replace anything
  */
  gtk_widget_set_sensitive( addbutton, FALSE );
  gtk_widget_set_sensitive( replacebutton, FALSE );

  /*
    And we only want one database window for sure, so
    disable the button in the menu
  */
  gtk_widget_set_sensitive( database_button, FALSE );

  /*
    realize the form now
  */
  gtk_widget_realize(db_form);

  
/*{{{  read database  */  

  /* get system-wide database */
  get_database( database_dir );
  
  /* get users private database */
  get_database( private_database_dir );

  /* did the reading give us *anything* ??? */
  gint failed = TRUE;
  if ( GTK_CLIST(sourcelist)->rows > 0 )
    {
      failed = FALSE;
    }

  if ( GTK_CLIST(grouplist)->rows > 0 )
    {
      failed = FALSE;
    }

  /* if not then exit after popping up error message */
  if ( failed == TRUE )
    {

      /* give stderr message */
      cerr << PACKAGE << ": database access error. Failed to read" << endl;
      cerr << "  " << database_dir << endl;
      cerr << "  " << "directory either empty, not existing or has" << endl;
      cerr << "  " << "wrong permissions." << endl;

      /* and now popup dialog */
      gchar* opener = "\n      Error while trying to access database at      \n\n       ";
      gchar* closer = "       \n\n    please check path, permissions and content.      \n";
      gchar* text = new gchar[strlen(database_dir) + 
			     strlen(opener) + 
			     strlen(closer) + 10 ];
      strcpy ( text, opener );
      strcat ( text, database_dir );
      strcat ( text, closer );
      
      GList* buttons = g_list_alloc();
      GList* functions = g_list_alloc();
      buttons = g_list_append(buttons,
			      (gpointer) "    DISMISS    ");
      functions = g_list_append(functions, NULL );
      
      popup_dialog("Database Access Error", text, buttons, functions);
      delete text;

      disassemble_database_menu();

      return;

    }

/*}}}*/


  /*
    attach the CList handlers
  */
  gtk_signal_connect(GTK_OBJECT(sourcelist),
                     "select_row",
                     GTK_SIGNAL_FUNC(sourcelist_callback),
                     (gpointer) sourcelist);
  gtk_signal_connect(GTK_OBJECT(grouplist),
                     "select_row",
                     GTK_SIGNAL_FUNC(grouplist_callback),
                     (gpointer) grouplist);
  gtk_signal_connect(GTK_OBJECT(targetlist),
                     "select_row",
                     GTK_SIGNAL_FUNC(targetlist_callback),
                     (gpointer) targetlist);

  /* now pop up the window */
  gtk_widget_show(db_form);

}
void
disassemble_database_menu()
{
  if ( db_form == NULL ) 
    return;
  
  gtk_widget_hide(db_form);
  gtk_widget_destroy(db_form);
  
  /*
    And and we enable the database button again
  */
  gtk_widget_set_sensitive( database_button, TRUE );
  
  db_form = NULL;
  
  return;
}
/*}}}*/

/*{{{  GTK+ functionality subroutines  */
/*
  This function just pops up a little help window so the user
  knows how to handle the database. Nothing big but might prove
  helpful.
 */
void 
help_button_pressed()
{
  
  GList* buttons = g_list_alloc();
  GList* functions = g_list_alloc();
  buttons = g_list_append(buttons,
			  (gpointer) "    DISMISS    ");
  functions = g_list_append(functions, NULL );
      
  popup_dialog("Database Help", "Selecting an item (left list) or group (right list) is\n done by clicking on the according entry in the upper lists.\n\n You can move an entry by clicking on it and then clicking on\n the position you'd like it to be. Deleting an entry from the\n targetlist (the one on the bottom) is done by double-clicking.\n\n If the sourcelists are empty, your Xlogmaster is not set up\n properly and couldn't find the database. Please contact your\n distributor / system administrator.\n",
	       buttons, functions);

}
void 
add_button_pressed()
{
  gint pages = GTK_CLIST( targetlist )->rows;

  for ( gint i = 0 ; i < pages ; i++ )
    {

      Log* entry = (Log*) gtk_clist_get_row_data( GTK_CLIST(targetlist), i);

      add_entry_from_database( entry );

    }
  
  
  disassemble_database_menu();
  
}
void
replace_button_pressed()
{

  remove_all_entries();

  gint pages = GTK_CLIST( targetlist )->rows;
  
  for ( gint i = 0 ; i < pages ; i++ )
    {

      Log* entry = (Log*) gtk_clist_get_row_data( GTK_CLIST(targetlist), i);

      add_entry_from_database( entry );

    }
  

  disassemble_database_menu();
  
}
/*}}}*/

/*{{{  GTK+ CList subs  */
/*
  Routine to be called upon destruction of a normal
  CList entry
*/
void
delete_clist_entry(gpointer data)
{

  Log* attached_data = (Log*) data;
  
  if ( attached_data == NULL ) return;
  
  attached_data->purge();
  
  delete attached_data;
  
}
/*
  Routine to be called upon destruction of a CList 
  group entry
*/
void
delete_clist_group(gpointer data)
{
  Log** array = (Log**) data;

  gint x = 0;
  while ( array != NULL &&
	  array[x] != NULL )
    {

      array[x]->purge();
      
      delete array[x];

      x++;
    }

  if ( array != NULL )
    delete array;

}
/*

  callback for the source entry CList

*/
void 
sourcelist_callback(GtkWidget*, gint row, gint, GdkEventButton*, gpointer)
{

  Log* entry = (Log*) gtk_clist_get_row_data( GTK_CLIST ( sourcelist ),
					      row );

  append_entry_to_list( targetlist , entry );

  set_button_status();

}
/*

  callback for the group entry CList

*/
void 
grouplist_callback(GtkWidget*, gint row, gint, GdkEventButton*, gpointer)
{

  Log** array = (Log**) gtk_clist_get_row_data( GTK_CLIST ( grouplist ),
						row );
  
  gint x = 0;
  while ( array != NULL && 
	  array[x] != NULL )
    {
      
      append_entry_to_list( targetlist , array[x] );
      
      x++;

    }

  set_button_status();

}
/*
  
  callback for the target list

*/
void 
targetlist_callback(GtkWidget *, gint row, gint column, GdkEventButton *, gpointer)
{
  static gint last_selection = -1;
  
  if ( column < 0 ) return;
  
  if ( last_selection == -1 )
    {

      last_selection = row;

    }
  else if ( row == last_selection )
    {

      gtk_clist_remove( GTK_CLIST(targetlist), row );
      last_selection = -1;

    }
  else 
    {
      
      gtk_clist_row_move( GTK_CLIST(targetlist), 
			  last_selection ,
			  row );

      last_selection = -1;

    }
  
  set_button_status();

  return;
}
/*}}}*/

/*{{{  GTK based subroutines  */
void 
set_button_status()
{

  if ( GTK_CLIST(targetlist)->rows > 0 )
    {
      gtk_widget_set_sensitive( addbutton, TRUE );
      gtk_widget_set_sensitive( replacebutton, TRUE );
    }
  else
    {
      gtk_widget_set_sensitive( addbutton, FALSE );
      gtk_widget_set_sensitive( replacebutton, FALSE );
    }
  
}
void
append_entry_to_list(GtkWidget* list, Log* source_entry, gint where)
{

  if ( source_entry == NULL )
    {

      cerr << PACKAGE << ": internal database error," << endl;
      cerr << "  " << "NULL pointer detected !" << endl;

      return;

    }

  /* copy the entry element that is about to be added */
  Log* entry = new Log;
  entry->init( source_entry->filename ,
	       source_entry->help ,
	       source_entry->buttontext ,
	       source_entry->plugin ,
	       source_entry->mode ,
	       source_entry->interval ,
	       source_entry->delay );
  
  gint j = 0;
  while ( source_entry->filter != NULL &&
	  source_entry->filter[j] != NULL )
    {
      
      entry->add_filter( source_entry->filter[j]->string ,
			 source_entry->filter[j]->mode ,
			 source_entry->filter[j]->execline );
      
      j++;

    }

  

  gchar* list_entry[6];    
  /* create structure for filling it into the list */
  list_entry[0] = entry->buttontext;
  
  switch ( entry->mode )
    {
    case TAIL_FILE:
      list_entry[1] = " TAIL ";
      break;
    case RUN_FILE:
      list_entry[1] = " RUN ";
      break;
    case CAT_FILE:
      list_entry[1] = " CAT ";
      break;
    default:
      list_entry[1] = "[empty]";
      break;
    }
  
  list_entry[2] = entry->filename;
  
  if ( entry->filterclass & CLASS1_FILTER )
    {
      list_entry[3] = "Class1";
    }
  else  if ( entry->filterclass & CLASS0_FILTER )
    {
      list_entry[3] = "Class0";
    }
  else
    {
      list_entry[3] = "[none]";
    }
  
  list_entry[4] = entry->help;
  
  if ( where == -1 )
    {

      gtk_clist_append(GTK_CLIST(list), list_entry );
      gint last = GTK_CLIST( list )->rows - 1;
      gtk_clist_set_row_data_full( GTK_CLIST( list ),
				   last,
				   (gpointer) entry,
				   (GtkDestroyNotify) 
				   delete_clist_entry );
      
    }
  else
    {
      gtk_clist_insert(GTK_CLIST(list), where, list_entry );
      gtk_clist_set_row_data_full( GTK_CLIST( list ),
				   where,
				   (gpointer) entry ,
				   (GtkDestroyNotify) 
				   delete_clist_entry );
    }

}
/*}}}*/

/*{{{  File subroutines  */
/*
  This routine reads a directory containing a xlogmaster
  database and fills the data into the database structures...
*/
void
get_database(const gchar* directory)
{
  DIR *dir;
  dirent* dirinfo;

  /* try to open directory - return if not successful */
  dir = opendir( directory );
  if ( dir == NULL )
    {
      return;
    }

  /* create REGEX arrays for finding the files &
     interpreting the group files */  
  entry_regex = new regex_t;
  group_regex = new regex_t;
  gname_regex = new regex_t;
  gdesc_regex = new regex_t;
  regcomp( entry_regex,
	   entry_string,
	   REG_EXTENDED );
  regcomp( group_regex,
	   group_string,
	   REG_EXTENDED );
  regcomp( gname_regex,
	   gname_string,
	   REG_EXTENDED | REG_ICASE );
  regcomp( gdesc_regex,
	   gdesc_string,
	   REG_EXTENDED | REG_ICASE );
  
  /*
    Main functionality loop
  */  
  dirinfo = readdir(dir );
  while ( dirinfo != NULL )
    {

      /* create the absolute filename because we are going to 
	 need it for both subroutines... */
      gchar* absolute_filename =
	new gchar[ strlen(directory) + strlen(dirinfo->d_name) + 5 ];
      strcpy( absolute_filename, directory );
      strcat( absolute_filename, "/" );
      strcat( absolute_filename, dirinfo->d_name );

/*{{{  Usual files  */
      if ( regexec( entry_regex,
		    dirinfo->d_name,
		    0,
		    NULL,
		    0 ) 
	   == 0 )
	{

	  /*
	    File is ENTRY file
	  */

	  Log** content = read_configuration_file( absolute_filename );

	  gint k = 0;
	  while ( content != NULL &&
		  content[k] != NULL )
	    {

	      append_entry_to_list( sourcelist , content[k] );
	      
	      gtk_clist_sort ( GTK_CLIST( sourcelist ) );

	      k++;

	    }

	  destroy_log_array( content );

	}
/*}}}*/

/*{{{  Group files  */
      if ( regexec( group_regex,
		    dirinfo->d_name,
		    0,
		    NULL,
		    0 )
	   == 0 )
	{
	  /*
	    File is GROUP file
	  */
	  gchar* buffer = NULL;
	  glong length = 0;
	  /* try to read it */
	  gint fd = open( absolute_filename, O_RDONLY);
	  if ( fd != -1 )
	    {

	      fstat( fd,  &status );
	      glong want = (glong) status.st_size;
	      buffer = new gchar[ want+10 ];
	      length = (glong) read( fd, buffer, want);
	      close ( fd );
	      buffer[length] = 0;

	      /*
		Now we got the file in memory... let's interpret it
	      */

	      groupify(buffer, length, directory);
	      
	    }
	  
	  if ( buffer != NULL ) delete buffer;
	  buffer = NULL;
	}
/*}}}*/

      /* destroy the absolute filename again */
      delete absolute_filename;
      absolute_filename = NULL;

      dirinfo = readdir( dir );
    }
  
  /*
    free & delete the REGEX array again...
  */
  regfree(entry_regex);
  delete entry_regex;
  entry_regex = NULL;
  regfree(group_regex);
  delete group_regex;
  group_regex = NULL;
  regfree(gname_regex);
  delete gname_regex;
  gname_regex = NULL;
  regfree(gdesc_regex);
  delete gdesc_regex;
  gdesc_regex = NULL;

  /* close the directory */
  closedir( dir );
  
  return;
}
/*

  This function takes a pointer to a buffer, it's length and
  the directory of the database as arguments and then interprets
  the buffer as if it were a Xlogmaster Database Group file.

  The routine automatically fills in the entry Group CList

*/
void
groupify(gchar* buffer, gint length, const gchar* base_dir)
{
  gint x = 0;
  gchar* line = buffer;
  Log** array = NULL;
  gchar* group_list_entry[2];
  gchar fillstring[80];
  group_list_entry[0] 
    = group_list_entry[1] 
    = NULL;
  string bufferstuff = buffer;
  string namef = "NAME";
  string descf = "DESC";
  string ldelf = "{";
  string rdelf = "}";
  string blankit (80,0);
  string getdata = blankit;
 std::string::size_type posit;
 std::string::size_type lposit;
 std::string::size_type rposit;
 std::string::size_type y = 0;
  delete group_list_entry[0];
  group_list_entry[0] = new gchar[20];
  delete group_list_entry[1];
  group_list_entry[1] = new gchar[80];
  while ( x < length )
    {
      
      while ( x < length &&
	      buffer[x] != '\r' &&
	      buffer[x] != '\n' ) x++;
      
      buffer[x] = 0;
      
	if (string(bufferstuff,y).find(namef) != string::npos) 
	{
	  /* This is a "NAME" line */	  
	  if ( group_list_entry[0] != NULL )
	    {
	      delete group_list_entry[0];
	      group_list_entry[0] = new gchar[20];
	    }
	  blankit.copy(group_list_entry[0],20,0);
	  lposit = string(bufferstuff,y).find(ldelf);
	  rposit = string(bufferstuff,y).find(rdelf);
	  getdata = bufferstuff.substr(lposit+1,rposit-lposit-1);
	  y = rposit+2;
	  getdata.copy(group_list_entry[0],rposit-lposit-1,0);
	  getdata = blankit;
	}
	else if (string(bufferstuff,y).find(descf) != string::npos)
	{
	  /* This is a "DESCRIPTION" line */
	  if ( group_list_entry[1] != NULL )
	    {
	      delete group_list_entry[1];
	      group_list_entry[1] = new gchar[80];
	    }
	  blankit.copy(group_list_entry[1],80,0);
	  lposit = string(bufferstuff,y).find(ldelf);
          rposit = string(bufferstuff,y).find(rdelf);
	  getdata = bufferstuff.substr(y+lposit+1,rposit-lposit-1);
	  y = rposit+1;
	  getdata.copy(group_list_entry[1],rposit-lposit-1,0); 
	  getdata = blankit;
	}
        else
	{
	  /* It is a FILENAME line, read the file... */
	  gchar* fname = new gchar[ strlen(line) + 2 ];
	  strcpy( fname, line );
	  fname = clean_string( fname );
	  /* make it into an absolute one... */
	  gchar* filename = new gchar [ strlen(base_dir)
				      + strlen(fname)
				      + 10 ];
	  strcpy( filename, base_dir );
	  strcat( filename, "/" );
	  strcat( filename, fname );
	  delete fname;

	  Log** entries = read_configuration_file( filename );

	  /* if reading was successful, append it to the ones we got
	     already */
	  gint k=0;
	  while ( entries != NULL &&
		  entries[k] != NULL )
	    {
	      

	      array = add_log_entry( array ,
				     entries[k]->mode ,
				     entries[k]->filename ,
				     entries[k]->help ,
				     entries[k]->buttontext ,
				     entries[k]->plugin ,
				     entries[k]->interval ,
				     entries[k]->delay );

	      gint j = 0;

	      while ( entries[k]->filter != NULL &&
		      entries[k]->filter[j] != NULL )
		{
		  
		  add_filter_to_last( array ,
				      entries[k]->filter[j]->string ,
				      entries[k]->filter[j]->mode ,
				      entries[k]->filter[j]->execline );
		  
		  j++;

		}

	      k++;

	    }

	  if ( filename != NULL ) delete filename;
	}
      
      /*
	Now step to the beginning of the next line
      */
      
      while ( x < length && 
	      ( buffer[x] == 0 ||
		buffer[x] == '\r' ||
		buffer[x] == '\n'  )) x++;
      line = &buffer[x];
      
    }

  
  if ( group_list_entry[0] == NULL )
    {

      group_list_entry[0] = new gchar[20];
      strcpy( group_list_entry[0] , "[no name]" );

    }

  if ( group_list_entry[1] == NULL )
    {
      
      group_list_entry[1] = new gchar[80];
      strcpy( group_list_entry[1] , "*** no description ***" );
      
    }

  if ( array != NULL )
    {
      
      gtk_clist_append(GTK_CLIST(grouplist), group_list_entry );
      gint last = GTK_CLIST( grouplist )->rows - 1;
      gtk_clist_set_row_data_full( GTK_CLIST( grouplist ),
				   last,
				   (gpointer) array,
				   (GtkDestroyNotify) 
				   delete_clist_group );
      gtk_clist_sort (GTK_CLIST(grouplist));

      /* The array is being deleted by the menu disassembling
	 routine, leave it alone here !!! */
      array = NULL;
      
    }

  if ( group_list_entry[0] != NULL )
    delete group_list_entry[0];
  if ( group_list_entry[1] != NULL )
    delete group_list_entry[1];
}

/*}}}*/

