#ifndef DATABASE_H
#define DATABASE_H

/* 
   database.H
   include file for xlogmaster database handling
*/

/*{{{  GTK+ based variables  */

GtkWidget* db_form;
static GtkWidget* sourcelist;
static GtkWidget* targetlist;
static GtkWidget* grouplist;
static GtkWidget* addbutton;
static GtkWidget* helpbutton;
static GtkWidget* replacebutton;

/*}}}*/

/*{{{  Variables  */

/*
  REGEXes
*/
static regex_t* entry_regex = NULL;
static gchar* entry_string = "\\.xlmrc";
static regex_t* group_regex = NULL;
static gchar* group_string = "\\.xlmgrp";
static regex_t* gname_regex = NULL;
static gchar* gname_string = "NAME.*{.*}";
static regex_t* gdesc_regex = NULL;
static gchar* gdesc_string = "DESC.*{.*}";
/*}}}*/

/*{{{  Subroutines  */
/* Database Menu */
void startup_database_menu();
void disassemble_database_menu();

/* GTK+ functionality subroutines */
void help_button_pressed();
void add_button_pressed();
void replace_button_pressed();

/* GTK+ CList subs */
static void delete_clist_entry(gpointer);
static void delete_clist_group(gpointer);
static void sourcelist_callback(GtkWidget*,gint,gint,GdkEventButton*,gpointer);
static void grouplist_callback(GtkWidget*,gint,gint,GdkEventButton*,gpointer);
static void targetlist_callback(GtkWidget*,gint,gint,GdkEventButton*,gpointer);

/* GTK based subroutines */
static void set_button_status();
static void append_entry_to_list(GtkWidget*,Log*, gint = -1);

/* File Subroutines */
static void get_database(const gchar*);
static void groupify(gchar*, gint, const gchar*);

/*}}}*/


#endif /* DATABASE_H */
