/* Ggradebook 0.91 (student.c)
 * Copyright (C) 2000 Free Software Foundation, Inc.
 * Author: Norbert de Jonge <hack@altavista.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

/* I'm sorry for using the Hungarian notation...      */
/* ...but it was useful when working with the grades. */

#include "global.h"
#include "student.h"
#include "gg.h"
#include "to.h"
#include "message.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu5[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoNewStudent, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu5[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu5),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu9[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoNewStudent, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu9[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu9),
  GNOMEUIINFO_END
};
#endif

void Students (void)
{
  GtkWidget *gStudentsBox;
  GtkWidget *gStudentsTable;
  GtkWidget *gStudentsLabel;
  GtkWidget *gStudentsSep;
  GtkWidget *gStudentsSep2;
  GtkWidget *gStudentsButton;
  GtkWidget *gStudentsButton2;
  GtkWidget *gStudentsButton3;
  GtkWidget *gStudentsButton4;
  GtkWidget *gStudentsButton5;

  if (gStudentsWindow != NULL)
  {
    gtk_widget_destroy (gStudentsWindow);
    gStudentsWindow = NULL;
  }

#ifdef USE_GNOME
  gStudentsWindow = gnome_app_new ("Students", "Students");
#else
  gStudentsWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gStudentsWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gStudentsWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gStudentsWindow), "Students");
  gtk_signal_connect (GTK_OBJECT (gStudentsWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoStudents), NULL);
  gtk_window_set_policy (GTK_WINDOW (gStudentsWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gStudentsWindow), 0);
  gtk_widget_realize (gStudentsWindow);

  gStudentsBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gStudentsBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gStudentsWindow), gStudentsBox);
#else
  gtk_container_add (GTK_CONTAINER (gStudentsWindow), gStudentsBox);
#endif
  gtk_widget_show (gStudentsBox);

  gStudentsTable = gtk_table_new (4, 5, FALSE);
  gtk_widget_show (gStudentsTable);
  gtk_box_pack_start (GTK_BOX (gStudentsBox), gStudentsTable,
    TRUE, TRUE, 0);

  gStudentsLabel = gtk_label_new ("Select an action to perform on the "
    "students:");
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsLabel, 0, 4, 0,
    1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gStudentsLabel);

  gStudentsSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsSep, 0, 4, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsSep);

  gStudentsButton = gtk_button_new_with_label ("Add");
  gtk_widget_set_usize (GTK_WIDGET (gStudentsButton), 75, 0);
  gtk_signal_connect (GTK_OBJECT (gStudentsButton), "clicked",
    GTK_SIGNAL_FUNC (AddStudent), (gpointer) 0);
  GTK_WIDGET_SET_FLAGS (gStudentsButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gStudentsWindow), gStudentsButton);
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsButton, 0, 1, 2,
    3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsButton);

  gStudentsButton2 = gtk_button_new_with_label ("Edit");
  gtk_widget_set_usize (GTK_WIDGET (gStudentsButton2), 75, 0);
  gtk_signal_connect (GTK_OBJECT (gStudentsButton2), "clicked",
    GTK_SIGNAL_FUNC (EditStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gStudentsButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsButton2, 1, 2,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsButton2);

  gStudentsButton3 = gtk_button_new_with_label ("Delete");
  gtk_widget_set_usize (GTK_WIDGET (gStudentsButton3), 75, 0);
  gtk_signal_connect (GTK_OBJECT (gStudentsButton3), "clicked",
    GTK_SIGNAL_FUNC (DeleteStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gStudentsButton3, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsButton3, 2, 3,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsButton3);

  gStudentsButton4 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gStudentsButton4), 75, 0);
  gtk_signal_connect (GTK_OBJECT (gStudentsButton4), "clicked",
    GTK_SIGNAL_FUNC (NoStudents), NULL);
  GTK_WIDGET_SET_FLAGS (gStudentsButton4, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsButton4, 3, 4,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsButton4);

  gStudentsSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsSep2, 0, 4, 3, 4,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsSep2);

  gStudentsButton5 = gtk_button_new_with_label ("Add all students from another"
    " class.");
  gtk_widget_set_usize (GTK_WIDGET (gStudentsButton5), 300, 0);
  gtk_signal_connect (GTK_OBJECT (gStudentsButton5), "clicked",
    GTK_SIGNAL_FUNC (AddFromAnother), NULL);
  GTK_WIDGET_SET_FLAGS (gStudentsButton5, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gStudentsTable), gStudentsButton5, 0, 4,
    4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gStudentsButton5);

  gtk_widget_show (gStudentsWindow);
}

void AddStudent (GtkWidget *asW, gpointer *asData)
{
  GtkWidget *gNewStudentBox;
  GtkWidget *gNewStudentTable;
  GtkWidget *gNewStudentTable2;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar5;
#endif
  GtkWidget *gNewStudentLabel;
  GtkWidget *gNewStudentLabel2;
  GtkWidget *gNewStudentLabel3;
  GtkWidget *gNewStudentLabel4;
  GtkWidget *gNewStudentLabel5;
  GtkWidget *gNewStudentSep;
  GtkWidget *gNewStudentSep2;
  GtkWidget *gNewStudentFrame;
  GtkWidget *gNewStudentButton;
  GtkWidget *gNewStudentButton2;
  GtkWidget *gNewStudentStatBar;
  GtkWidget *gNewStudentScroll;
  GtkWidget *gNewStudentOptionMenu;
  GtkWidget *gNewStudentOptionMenu1;
  GtkWidget *gNewStudentMenuItem;
  GtkWidget *gNewStudentMenuItem2;
  int iEdit;

  if (gNewStudentWindow != NULL)
  {
    gtk_widget_destroy (gNewStudentWindow);
    gNewStudentWindow = NULL;
  }
  iEdit = (int)asData;

#ifdef USE_GNOME
  if (iEdit == 0)
  {
    gNewStudentWindow = gnome_app_new ("Add Student", "Add Student");
  }
  else
  {
    gNewStudentWindow = gnome_app_new ("Edit Student", "Edit Student");
  }
#else
  gNewStudentWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gNewStudentWindow), 450, 0);
  gtk_window_position (GTK_WINDOW (gNewStudentWindow), GTK_WIN_POS_CENTER);
  if (iEdit == 0)
  {
    gtk_window_set_title (GTK_WINDOW (gNewStudentWindow), "Add Student");
  }
  else
  {
    gtk_window_set_title (GTK_WINDOW (gNewStudentWindow), "Edit Student");
  }
  gtk_signal_connect (GTK_OBJECT (gNewStudentWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoNewStudent), NULL);
  gtk_window_set_policy (GTK_WINDOW (gNewStudentWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gNewStudentWindow), 0);
  gtk_widget_realize (gNewStudentWindow);

  gNewStudentBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gNewStudentBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gNewStudentWindow), gNewStudentBox);
#else
  gtk_container_add (GTK_CONTAINER (gNewStudentWindow), gNewStudentBox);
#endif
  gtk_widget_show (gNewStudentBox);

  gNewStudentTable = gtk_table_new (2, 7, FALSE);
  gtk_widget_show (gNewStudentTable);
  gtk_box_pack_start (GTK_BOX (gNewStudentBox), gNewStudentTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  if (iEdit == 0)
  {
    gnome_app_create_menus (GNOME_APP (gNewStudentWindow), menu5);
  }
  else
  {
    gnome_app_create_menus (GNOME_APP (gNewStudentWindow), menu9);
  }
#else
  if (iEdit == 0)
  {
    GetMenu (gNewStudentWindow, &GMMenuBar5, 5);
  }
  else
  {
    GetMenu (gNewStudentWindow, &GMMenuBar5, 9);
  }
  gtk_table_attach (GTK_TABLE (gNewStudentTable), GMMenuBar5, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar5);
#endif

  if (iEdit == 0)
  {
    snprintf (sLabelText, MAX_LABEL, "Enter data for the new student #%i "
      "below:", iStudents + 1);
  }
  else
  {
    snprintf (sLabelText, MAX_LABEL, "Change data for student #%i below:",
      iEdit);
  }
  gNewStudentLabel = gtk_label_new (sLabelText);
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentLabel, 0, 2, 1, 2,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentLabel);

  gNewStudentSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentSep, 0, 2,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewStudentSep);

  gNewStudentFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentFrame, 0, 2, 3, 4,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentFrame);

  gNewStudentTable2 = gtk_table_new (3, 4, FALSE);
  gtk_widget_show (gNewStudentTable2);
  gtk_container_add (GTK_CONTAINER (gNewStudentFrame), gNewStudentTable2);

  gNewStudentLabel2 = gtk_label_new ("Full name:");
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentLabel2, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentLabel2);

  gNewStudentEntry = gtk_entry_new_with_max_length (MAX_NAME);
  gtk_signal_connect (GTK_OBJECT (gNewStudentEntry), "activate",
    GTK_SIGNAL_FUNC (ToCode), NULL);
  if (iEdit != 0)
  {
    gtk_entry_set_text (GTK_ENTRY (gNewStudentEntry), sStudentNames[iEdit]);
  }
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentEntry, 1, 3,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gNewStudentEntry);
  gtk_widget_show (gNewStudentEntry);

  gNewStudentLabel3 = gtk_label_new ("Code:");
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentLabel3, 0, 1,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentLabel3);

  gNewStudentEntry2 = gtk_entry_new_with_max_length (MAX_CODE);
  gtk_signal_connect (GTK_OBJECT (gNewStudentEntry2), "activate",
    GTK_SIGNAL_FUNC (ToExtra), NULL);
  if (iEdit != 0)
  {
    gtk_entry_set_text (GTK_ENTRY (gNewStudentEntry2), sStudentCodes[iEdit]);
  }
  else
  {
    gtk_entry_set_text (GTK_ENTRY (gNewStudentEntry2), "Optional.");
  }
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentEntry2, 1, 3,
    1, 2, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentEntry2);

  gNewStudentLabel4 = gtk_label_new ("Sex:");
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentLabel4, 0, 1,
    2, 3, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentLabel4);

  gNewStudentOptionMenu = gtk_option_menu_new ();
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentOptionMenu, 1, 3,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentOptionMenu);
  gNewStudentOptionMenu1 = gtk_menu_new ();
  gNewStudentMenuItem = gtk_menu_item_new_with_label ("Male");
  gtk_signal_connect (GTK_OBJECT (gNewStudentMenuItem), "activate",
    GTK_SIGNAL_FUNC (SexMale), NULL);
  gtk_widget_show (gNewStudentMenuItem);
  gtk_menu_append (GTK_MENU (gNewStudentOptionMenu1), gNewStudentMenuItem);
  gNewStudentMenuItem2 = gtk_menu_item_new_with_label ("Female");
  gtk_signal_connect (GTK_OBJECT (gNewStudentMenuItem2), "activate",
    GTK_SIGNAL_FUNC (SexFemale), NULL);
  gtk_widget_show (gNewStudentMenuItem2);
  gtk_menu_append (GTK_MENU (gNewStudentOptionMenu1), gNewStudentMenuItem2);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (gNewStudentOptionMenu),
    gNewStudentOptionMenu1);

  if (((strcmp (sStudentSex[iEdit], "male")) == 0) || (iEdit == 0))
  {
    iSex = 0;
    gtk_option_menu_set_history (GTK_OPTION_MENU (gNewStudentOptionMenu), 0);
  }
  else
  {
    iSex = 1;
    gtk_option_menu_set_history (GTK_OPTION_MENU (gNewStudentOptionMenu), 1);
  }

  gNewStudentLabel5 = gtk_label_new ("Extra:");
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentLabel5, 0, 1,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentLabel5);

  gNewStudentText = gtk_text_new (NULL, NULL);
  gtk_text_set_editable (GTK_TEXT (gNewStudentText), TRUE);
  gtk_text_set_word_wrap (GTK_TEXT (gNewStudentText), TRUE);
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentText, 1, 2,
    3, 4, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentText);

  gNewStudentScroll = gtk_vscrollbar_new (GTK_TEXT (gNewStudentText)->vadj);
  gtk_table_attach (GTK_TABLE (gNewStudentTable2), gNewStudentScroll, 2, 3,
    3, 4, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewStudentScroll);

  if (iEdit == 0)
  {
    gtk_text_insert (GTK_TEXT (gNewStudentText), NULL, NULL, NULL, "Enter "
      "additional info here.", -1);
  }
  else
  {
    gtk_text_insert (GTK_TEXT (gNewStudentText), NULL, NULL, NULL,
      sStudentExtra[iEdit], -1);
  }

  gNewStudentSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentSep2, 0, 2, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewStudentSep2);

  if (iEdit == 0)
  {
    gNewStudentButton = gtk_button_new_with_label ("Add");
  }
  else
  {
    gNewStudentButton = gtk_button_new_with_label ("Change");
  }
  gtk_widget_set_usize (GTK_WIDGET (gNewStudentButton), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gNewStudentButton), "clicked",
    GTK_SIGNAL_FUNC (YesNewStudent), (gpointer) iEdit);
  GTK_WIDGET_SET_FLAGS (gNewStudentButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gNewStudentWindow), gNewStudentButton);
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentButton, 0, 1, 5, 6,
    GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gNewStudentButton);

  gNewStudentButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gNewStudentButton2), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gNewStudentButton2), "clicked",
    GTK_SIGNAL_FUNC (NoNewStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gNewStudentButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentButton2, 1, 2,
    5, 6, GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gNewStudentButton2);

  gNewStudentStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gNewStudentTable), gNewStudentStatBar, 0, 2,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewStudentStatBar);
  if (iEdit != 0)
  {
    gtk_statusbar_push (GTK_STATUSBAR (gNewStudentStatBar), 1,
      " Edit Student");
  }
  else
  {
    gtk_statusbar_push (GTK_STATUSBAR (gNewStudentStatBar), 1, " Add Student");
  }

  gtk_widget_show (gNewStudentWindow);
  if (iEdit == 0)
  {
    if (gStudentsWindow != NULL)
    {
      gtk_widget_destroy (gStudentsWindow);
      gStudentsWindow = NULL;
    }
  }
}

void SexMale (void)
{
  iSex = 0;
}

void SexFemale (void)
{
  iSex = 1;
}

void EditStudent (void)
{
  GtkWidget *gEditStudentBox;
  GtkWidget *gEditStudentTable;
  GtkWidget *gEditStudentLabel;
  GtkWidget *gEditStudentSep;
  GtkWidget *gEditStudentSep2;
  GtkWidget *gEditStudentButton;
  GtkWidget *gEditStudentButton2;

  if (gStudentsWindow != NULL)
  {
    gtk_widget_destroy (gStudentsWindow);
    gStudentsWindow = NULL;
  }
  if (gEditStudentWindow != NULL)
  {
    gtk_widget_destroy (gEditStudentWindow);
    gEditStudentWindow = NULL;
  }

#ifdef USE_GNOME
  gEditStudentWindow = gnome_app_new ("Edit Student", "Edit Student");
#else
  gEditStudentWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gEditStudentWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gEditStudentWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gEditStudentWindow), "Edit Student");
  gtk_signal_connect (GTK_OBJECT (gEditStudentWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoEditStudent), NULL);
  gtk_window_set_policy (GTK_WINDOW (gEditStudentWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gEditStudentWindow), 0);
  gtk_widget_realize (gEditStudentWindow);

  gEditStudentBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gEditStudentBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gEditStudentWindow), gEditStudentBox);
#else
  gtk_container_add (GTK_CONTAINER (gEditStudentWindow), gEditStudentBox);
#endif
  gtk_widget_show (gEditStudentBox);

  gEditStudentTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gEditStudentTable);
  gtk_box_pack_start (GTK_BOX (gEditStudentBox), gEditStudentTable,
    TRUE, TRUE, 0);

  gEditStudentLabel = gtk_label_new ("Enter the number of the student you "
    "want\nto edit below:");
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentLabel, 0, 2,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gEditStudentLabel);

  gEditStudentSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentSep, 0, 2,
    1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditStudentSep);

  gEditStudentEntry = gtk_entry_new_with_max_length (MAX_SNUMBER);
  gtk_signal_connect (GTK_OBJECT (gEditStudentEntry), "activate",
    GTK_SIGNAL_FUNC (YesEditStudent), NULL);
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentEntry, 0, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gEditStudentEntry);
  gtk_widget_show (gEditStudentEntry);

  gEditStudentSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentSep2,
    0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditStudentSep2);

  gEditStudentButton = gtk_button_new_with_label ("Edit");
  gtk_widget_set_usize (GTK_WIDGET (gEditStudentButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gEditStudentButton), "clicked",
    GTK_SIGNAL_FUNC (YesEditStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gEditStudentButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gEditStudentWindow),
    gEditStudentButton);
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentButton,
    0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditStudentButton);

  gEditStudentButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gEditStudentButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gEditStudentButton2), "clicked",
    GTK_SIGNAL_FUNC (NoEditStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gEditStudentButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditStudentTable), gEditStudentButton2,
    1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditStudentButton2);

  gtk_widget_show (gEditStudentWindow);
}

void NoEditStudent (void)
{
  if (gEditStudentWindow != NULL)
  {
    gtk_widget_destroy (gEditStudentWindow);
    gEditStudentWindow = NULL;
  }
}

void YesEditStudent (void)
{
  int iStudent;

  iStudent = atoi (gtk_entry_get_text (GTK_ENTRY (gEditStudentEntry)));
  if ((iStudent > 0) && (iStudent <= iStudents))
  {
    AddStudent (NULL, (gpointer)iStudent);
  }
  else
  {
    Message ("Wrong student number!");
  }
  if (gEditStudentWindow != NULL)
  {
    gtk_widget_destroy (gEditStudentWindow);
    gEditStudentWindow = NULL;
  }
}

void DeleteStudent (void)
{
  GtkWidget *gDeleteStudentBox;
  GtkWidget *gDeleteStudentTable;
  GtkWidget *gDeleteStudentLabel;
  GtkWidget *gDeleteStudentSep;
  GtkWidget *gDeleteStudentSep2;
  GtkWidget *gDeleteStudentButton;
  GtkWidget *gDeleteStudentButton2;

  if (gStudentsWindow != NULL)
  {
    gtk_widget_destroy (gStudentsWindow);
    gStudentsWindow = NULL;
  }
  if (gDeleteStudentWindow != NULL)
  {
    gtk_widget_destroy (gDeleteStudentWindow);
    gDeleteStudentWindow = NULL;
  }

#ifdef USE_GNOME
  gDeleteStudentWindow = gnome_app_new ("Delete Student", "Delete Student");
#else
  gDeleteStudentWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gDeleteStudentWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gDeleteStudentWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gDeleteStudentWindow), "Delete Student");
  gtk_signal_connect (GTK_OBJECT (gDeleteStudentWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoDeleteStudent), NULL);
  gtk_window_set_policy (GTK_WINDOW (gDeleteStudentWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gDeleteStudentWindow), 0);
  gtk_widget_realize (gDeleteStudentWindow);

  gDeleteStudentBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gDeleteStudentBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gDeleteStudentWindow), gDeleteStudentBox);
#else
  gtk_container_add (GTK_CONTAINER (gDeleteStudentWindow), gDeleteStudentBox);
#endif
  gtk_widget_show (gDeleteStudentBox);

  gDeleteStudentTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gDeleteStudentTable);
  gtk_box_pack_start (GTK_BOX (gDeleteStudentBox), gDeleteStudentTable,
    TRUE, TRUE, 0);

  gDeleteStudentLabel = gtk_label_new ("Enter the number of the student you "
    "want\nto delete below:");
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentLabel, 0, 2,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gDeleteStudentLabel);

  gDeleteStudentSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentSep, 0, 2,
    1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteStudentSep);

  gDeleteStudentEntry = gtk_entry_new_with_max_length (MAX_SNUMBER);
  gtk_signal_connect (GTK_OBJECT (gDeleteStudentEntry), "activate",
    GTK_SIGNAL_FUNC (YesDeleteStudent), NULL);
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentEntry, 0, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gDeleteStudentEntry);
  gtk_widget_show (gDeleteStudentEntry);

  gDeleteStudentSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentSep2,
    0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteStudentSep2);

  gDeleteStudentButton = gtk_button_new_with_label ("Delete");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteStudentButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteStudentButton), "clicked",
    GTK_SIGNAL_FUNC (YesDeleteStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteStudentButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gDeleteStudentWindow),
    gDeleteStudentButton);
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentButton,
    0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteStudentButton);

  gDeleteStudentButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteStudentButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteStudentButton2), "clicked",
    GTK_SIGNAL_FUNC (NoDeleteStudent), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteStudentButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gDeleteStudentTable), gDeleteStudentButton2,
    1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteStudentButton2);

  gtk_widget_show (gDeleteStudentWindow);
}

void NoDeleteStudent (void)
{
  if (gDeleteStudentWindow != NULL)
  {
    gtk_widget_destroy (gDeleteStudentWindow);
    gDeleteStudentWindow = NULL;
  }
}

void YesDeleteStudent (void)
{
  GtkWidget *gYesDeleteStudentBox;
  GtkWidget *gYesDeleteStudentTable;
  GtkWidget *gYesDeleteStudentLabel;
  GtkWidget *gYesDeleteStudentLabel2;
  GtkWidget *gYesDeleteStudentSep;
  GtkWidget *gYesDeleteStudentSep2;
  GtkWidget *gYesDeleteStudentButton;
  GtkWidget *gYesDeleteStudentButton2;
  int iStudent;

  iStudent = atoi (gtk_entry_get_text (GTK_ENTRY (gDeleteStudentEntry)));
  if (gDeleteStudentWindow != NULL)
  {
    gtk_widget_destroy (gDeleteStudentWindow);
    gDeleteStudentWindow = NULL;
  }

  if ((iStudent > 0) && (iStudent <= iStudents))
  {
    if (gYesDeleteStudentWindow != NULL)
    {
      gtk_widget_destroy (gYesDeleteStudentWindow);
      gYesDeleteStudentWindow = NULL;
    }

#ifdef USE_GNOME
    gYesDeleteStudentWindow = gnome_app_new ("Question", "Question");
#else
    gYesDeleteStudentWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteStudentWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gYesDeleteStudentWindow),
      GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (gYesDeleteStudentWindow), "Question");
    gtk_signal_connect (GTK_OBJECT (gYesDeleteStudentWindow), "delete_event",
      GTK_SIGNAL_FUNC (DontDeleteStudent), NULL);
    gtk_window_set_policy (GTK_WINDOW (gYesDeleteStudentWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gYesDeleteStudentWindow), 0);
    gtk_widget_realize (gYesDeleteStudentWindow);

    gYesDeleteStudentBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gYesDeleteStudentBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gYesDeleteStudentWindow),
      gYesDeleteStudentBox);
#else
    gtk_container_add (GTK_CONTAINER (gYesDeleteStudentWindow),
      gYesDeleteStudentBox);
#endif
    gtk_widget_show (gYesDeleteStudentBox);

    gYesDeleteStudentTable = gtk_table_new (2, 5, FALSE);
    gtk_widget_show (gYesDeleteStudentTable);
    gtk_box_pack_start (GTK_BOX (gYesDeleteStudentBox), gYesDeleteStudentTable,
      TRUE, TRUE, 0);

    gYesDeleteStudentLabel = gtk_label_new ("Are you sure you want to delete "
      "the\nstudent below:");
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable),
      gYesDeleteStudentLabel, 0, 2, 0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gYesDeleteStudentLabel);

    gYesDeleteStudentSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable), gYesDeleteStudentSep,
      0, 2, 1, 2, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteStudentSep);

    gYesDeleteStudentLabel2 = gtk_label_new (sStudentNames[iStudent]);
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable),
      gYesDeleteStudentLabel2, 0, 2, 2, 3, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gYesDeleteStudentLabel2);

    gYesDeleteStudentSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable),
      gYesDeleteStudentSep2, 0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteStudentSep2);

    gYesDeleteStudentButton = gtk_button_new_with_label ("Delete");
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteStudentButton), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gYesDeleteStudentButton), "clicked",
      GTK_SIGNAL_FUNC (DoDeleteStudent), (gpointer) iStudent);
    GTK_WIDGET_SET_FLAGS (gYesDeleteStudentButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gYesDeleteStudentWindow),
      gYesDeleteStudentButton);
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable),
      gYesDeleteStudentButton, 0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteStudentButton);

    gYesDeleteStudentButton2 = gtk_button_new_with_label ("Cancel");
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteStudentButton2), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gYesDeleteStudentButton2), "clicked",
      GTK_SIGNAL_FUNC (DontDeleteStudent), NULL);
    GTK_WIDGET_SET_FLAGS (gYesDeleteStudentButton2, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gYesDeleteStudentTable),
      gYesDeleteStudentButton2, 1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteStudentButton2);

    gtk_widget_show (gYesDeleteStudentWindow);
  }
  else
  {
    Message ("Wrong student number!");
  }
}

void DontDeleteStudent (void)
{
  if (gYesDeleteStudentWindow != NULL)
  {
    gtk_widget_destroy (gYesDeleteStudentWindow);
    gYesDeleteStudentWindow = NULL;
  }
}

void DoDeleteStudent (GtkWidget *ddsW, gpointer *ddsData)
{
  int iStudent;
  int iTemp;
  int iTemp2;

  iStudent = (int)ddsData;

  for (iTemp = iStudent; iTemp <= iStudents; iTemp++)
  {
    snprintf (sStudentNames[iTemp], MAX_NAME, "%s", sStudentNames[iTemp + 1]);
    snprintf (sStudentCodes[iTemp], MAX_CODE, "%s", sStudentCodes[iTemp + 1]);
    snprintf (sStudentSex[iTemp], MAX_SEX, "%s", sStudentSex[iTemp + 1]);
    snprintf (sStudentExtra[iTemp], MAX_TEXT, "%s", sStudentExtra[iTemp + 1]);
    for (iTemp2 = 1; iTemp2 <= iWorks[iPeriod]; iTemp2++)
    {
      snprintf (sWorkGrades[iPeriod][iTemp2][iTemp], MAX_GRADE_L, "%s",
        sWorkGrades[iPeriod][iTemp2][iTemp + 1]);
    }
  }
  iStudents--;
  if (gYesDeleteStudentWindow != NULL)
  {
    gtk_widget_destroy (gYesDeleteStudentWindow);
    gYesDeleteStudentWindow = NULL;
  }
  UpdateList ();
}

void NoNewStudent (void)
{
  if (gNewStudentWindow != NULL)
  {
    gtk_widget_destroy (gNewStudentWindow);
    gNewStudentWindow = NULL;
  }
}

void YesNewStudent (GtkWidget *ynsW, gpointer *ynsData)
{
  char sName[MAX_NAME];
  int iRet;
  int iEdit;

  iEdit = (int)ynsData;
  if (iEdit == 0)
  {
    iStudents++;
  }

  snprintf (sName, MAX_NAME, "%s",
    gtk_entry_get_text (GTK_ENTRY (gNewStudentEntry)));
  if (iEdit == 0)
  {
    snprintf (sStudentNames[iStudents], MAX_NAME, "%s", sName);
  }
  else
  {
    snprintf (sStudentNames[iEdit], MAX_NAME, "%s", sName);
  }

  if (iEdit == 0)
  {
    snprintf (sStudentCodes[iStudents], MAX_CODE, "%s",
      gtk_entry_get_text (GTK_ENTRY (gNewStudentEntry2)));
  }
  else
  {
    snprintf (sStudentCodes[iEdit], MAX_CODE, "%s",
      gtk_entry_get_text (GTK_ENTRY (gNewStudentEntry2)));
  }

  if (iSex == 0)
  {
    if (iEdit == 0)
    {
      snprintf (sStudentSex[iStudents], MAX_SEX, "%s", "male");
    }
    else
    {
      snprintf (sStudentSex[iEdit], MAX_SEX, "%s", "male");
    }
  }
  else
  {
    if (iEdit == 0)
    {
      snprintf (sStudentSex[iStudents], MAX_SEX, "%s", "female");
    }
    else
    {
      snprintf (sStudentSex[iEdit], MAX_SEX, "%s", "female");
    }
  }

  if (iEdit == 0)
  {
    iRet = snprintf (sStudentExtra[iStudents], MAX_TEXT, "%s",
      gtk_editable_get_chars (GTK_EDITABLE (gNewStudentText), 0, -1));
  }
  else
  {
    iRet = snprintf (sStudentExtra[iEdit], MAX_TEXT, "%s",
      gtk_editable_get_chars (GTK_EDITABLE (gNewStudentText), 0, -1));
  }
  if ((iRet == -1) || (iRet > MAX_TEXT))
  {
    snprintf (sLabelText, MAX_LABEL, "Only saved the first %i characters\nof "
      "the extra info!", MAX_TEXT);
    Message (sLabelText);
  }

  if (gNewStudentWindow != NULL)
  {
    gtk_widget_destroy (gNewStudentWindow);
    gNewStudentWindow = NULL;
  }
  UpdateList ();
}

void NoStudents (void)
{
  if (gStudentsWindow != NULL)
  {
    gtk_widget_destroy (gStudentsWindow);
    gStudentsWindow = NULL;
  }
}

void QuitAdd (void)
{
  if (gAddWindow != NULL)
  {
    gtk_widget_destroy (gAddWindow);
    gAddWindow = NULL;
  }
}

void AddFromAnother (void)
{
  GtkWidget *gAddBox;
  GtkWidget *gAddTable;
  GtkWidget *gAddLabel;
  GtkWidget *gAddLabel2;
  GtkWidget *gAddSep;
  GtkWidget *gAddSep2;
  GtkWidget *gAddButton;
  GtkWidget *gAddClassButton[MAX_CLASSES];

  DIR *sClassDir;
  struct dirent *dir;
  char sLabelTemp[MAX_CLASSES][MAX_LABEL];
  int iTemp;
  int iClassesTemp;
  GdkCursor *gAddCursor;

  if (gStudentsWindow != NULL)
  {
    gtk_widget_destroy (gStudentsWindow);
    gStudentsWindow = NULL;
  }

  if ((sClassDir = opendir (sClassPath)) == NULL)
  {
    Message ("Could not open directory with\nclass files!");
  }
  else
  {
    iClassesTemp = 0;
    while ((dir = readdir (sClassDir)) != NULL)
    {
      if ((strcmp (dir->d_name, ".") != 0) && (strcmp (dir->d_name, "..")
        != 0) && (strcmp (dir->d_name, sClasses[iSelected]) != 0))
      {
        snprintf (sClassesTemp[iClassesTemp], MAX_FILE, "%s", dir->d_name);
        snprintf (sLabelTemp[iClassesTemp], MAX_LABEL, "%s", "");
        for (iTemp = 0; iTemp < strlen (dir->d_name) - 4; iTemp++)
        {
          if (dir->d_name[iTemp] == '_')
          {
            snprintf (sTempC, MAX_STRING, "%s", sLabelTemp[iClassesTemp]);
            snprintf (sLabelTemp[iClassesTemp], MAX_LABEL, "%s (", sTempC);
          }
          else
          {
            snprintf (sTempC, MAX_STRING, "%s", sLabelTemp[iClassesTemp]);
            snprintf (sLabelTemp[iClassesTemp], MAX_LABEL, "%s%c", sTempC,
              dir->d_name[iTemp]);
          }
        }
        snprintf (sTempC, MAX_STRING, "%s", sLabelTemp[iClassesTemp]);
        snprintf (sLabelTemp[iClassesTemp], MAX_LABEL, "%s)", sTempC);
        iClassesTemp++;
      }
    }
    closedir (sClassDir);

    if (gAddWindow != NULL)
    {
      gtk_widget_destroy (gAddWindow);
      gAddWindow = NULL;
    }

#ifdef USE_GNOME
    gAddWindow = gnome_app_new ("Select Class", "Select Class");
#else
    gAddWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gAddWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gAddWindow), GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (gAddWindow), "Select Class");
    gtk_signal_connect (GTK_OBJECT (gAddWindow), "delete_event",
      GTK_SIGNAL_FUNC (QuitAdd), NULL);
    gtk_window_set_policy (GTK_WINDOW (gAddWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gAddWindow), 0);
    gtk_widget_realize (gAddWindow);

    gAddBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gAddBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gAddWindow), gAddBox);
#else
    gtk_container_add (GTK_CONTAINER (gAddWindow), gAddBox);
#endif
    gtk_widget_show (gAddBox);

    gAddTable = gtk_table_new (1, MAX_CLASSES + 4, FALSE);
    gtk_widget_show (gAddTable);
    gtk_box_pack_start (GTK_BOX (gAddBox), gAddTable, TRUE, TRUE, 0);

    gAddLabel = gtk_label_new ("Select the class to add all students\nfrom "
      "below:");
    gtk_table_attach (GTK_TABLE (gAddTable), gAddLabel, 0, 1, 0, 1,
      GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gAddLabel);

    gAddSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gAddTable), gAddSep, 0, 1, 1, 2,
      GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gAddSep);

    if (iClassesTemp > 0)
    {
      for (iTemp = 0; iTemp < iClassesTemp; iTemp++)
      {
        gAddClassButton[iTemp] = gtk_button_new_with_label
          (sLabelTemp[iTemp]);
        gtk_widget_set_usize (GTK_WIDGET (gAddClassButton[iTemp]), 200, 30);
        gtk_table_attach (GTK_TABLE (gAddTable), gAddClassButton[iTemp],
          0, 1, iTemp + 2, iTemp + 3, GTK_SHRINK, GTK_SHRINK, 10, 10);
        gtk_signal_connect (GTK_OBJECT (gAddClassButton[iTemp]), "clicked",
          GTK_SIGNAL_FUNC (YesAddFromAnother), (gpointer) iTemp);
        gAddCursor = gdk_cursor_new (GDK_ARROW);
        gtk_widget_realize (gAddClassButton[iTemp]);
        if (gAddCursor != NULL)
        {
          gdk_window_set_cursor (gAddClassButton[iTemp]->window,
            gAddCursor);
        }
        gtk_widget_show (gAddClassButton[iTemp]);
      }
    }
    else
    {
      iClassesTemp = 1;
      gAddLabel2 = gtk_label_new ("No class to add from.");
      gtk_table_attach (GTK_TABLE (gAddTable), gAddLabel2, 0, 1, 2, 3,
        GTK_FILL, GTK_FILL, 10, 10);
      gtk_widget_show (gAddLabel2);
    }

    gAddSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gAddTable), gAddSep2, 0, 1, 2 + iClassesTemp,
      3 + iClassesTemp, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gAddSep2);

    gAddButton = gtk_button_new_with_label ("Close");
    gtk_signal_connect (GTK_OBJECT (gAddButton), "clicked",
      GTK_SIGNAL_FUNC (QuitAdd), NULL);
    GTK_WIDGET_SET_FLAGS (gAddButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gAddWindow), gAddButton);
    gtk_table_attach (GTK_TABLE (gAddTable), gAddButton, 0, 1,
      3 + iClassesTemp, 4 + iClassesTemp, GTK_EXPAND | GTK_FILL, GTK_FILL,
      0, 0);
    gtk_widget_show (gAddButton);

    gtk_widget_show (gAddWindow);
  }
}

void YesAddFromAnother (GtkWidget *yafaW, gpointer *yafaData)
{
  int iData;
  int iFd;
  int iStudentsTemp;
  int iTemp;
  int iTemp2;
  int iTemp3;
  char sFileNameTemp[MAX_FILE];

  iData = (int)yafaData;

  if (gAddWindow != NULL)
  {
    gtk_widget_destroy (gAddWindow);
    gAddWindow = NULL;
  }

  snprintf (sFileNameTemp, MAX_FILE, "%s%s", sClassPath, sClassesTemp[iData]);
  if ((iFd = open (sFileNameTemp, O_RDONLY, 0600)) == -1)
  {
    Message ("Could not open other class!");
  }
  else
  {
    Read (iFd, 0, "students: ");
    iStudentsTemp = atoi (sReturns);

    if (iStudentsTemp > 0)
    {
      for (iTemp = 1; iTemp <= iStudentsTemp; iTemp++)
      {
        if (iStudents + iTemp <= MAX_STUDENTS)
        {
          Read (iFd, 0, "student name: ");
          snprintf (sStudentNames[iStudents + iTemp], MAX_NAME, "%s", sReturns);
          Read (iFd, 0, "student code: ");
          snprintf (sStudentCodes[iStudents + iTemp], MAX_CODE, "%s", sReturns);
          Read (iFd, 0, "student sex: ");
          snprintf (sStudentSex[iStudents + iTemp], MAX_SEX, "%s", sReturns);
          Read (iFd, 1, "student extra:\n");
          snprintf (sStudentExtra[iStudents + iTemp], MAX_TEXT, "%s", sReturns);
          for (iTemp2 = 1; iTemp2 <= iPeriods; iTemp2++)
          {
            for (iTemp3 = 1; iTemp3 <= iWorks[iTemp2]; iTemp3++)
            {
              snprintf (sWorkGrades[iTemp2][iTemp3][iTemp], MAX_GRADE_L,
                "%s", "");
            }
          }
        }
        else
        {
          Message ("Too many students!");
        }
      }

      if (iStudents + iStudentsTemp <= MAX_STUDENTS)
      {
        iStudents += iStudentsTemp;
      }
      else
      {
        iStudents = MAX_STUDENTS;
      }
      UpdateList ();
    }
    else
    {
      Message ("No students in that class!");
    }
  }
}
