/* Ggradebook 0.91 (grades.c)
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
#include "grades.h"
#include "period.h"
#include "to.h"
#include "gg.h"
#include "message.h"
#include "preferences.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu8[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoAddGrades, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu8[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu8),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu13[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoAddGrades, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu13[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu13),
  GNOMEUIINFO_END
};
#endif

void Grades (void)
{
  GtkWidget *gGradesBox;
  GtkWidget *gGradesTable;
  GtkWidget *gGradesLabel;
  GtkWidget *gGradesSep;
  GtkWidget *gGradesSep2;
  GtkWidget *gGradesButton;
  GtkWidget *gGradesButton2;
  GtkWidget *gGradesButton3;
  GtkWidget *gGradesButton4;
  GtkWidget *gGradesButton5;
  GtkWidget *gGradesButton6;
  GtkWidget *gGradesButton7;

  if (gGradesWindow != NULL)
  {
    gtk_widget_destroy (gGradesWindow);
    gGradesWindow = NULL;
  }

#ifdef USE_GNOME
  gGradesWindow = gnome_app_new ("Grades & Periods", "Grades & Periods");
#else
  gGradesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gGradesWindow), 450, 0);
  gtk_window_position (GTK_WINDOW (gGradesWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gGradesWindow), "Grades & Periods");
  gtk_signal_connect (GTK_OBJECT (gGradesWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoGrades), NULL);
  gtk_window_set_policy (GTK_WINDOW (gGradesWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gGradesWindow), 0);
  gtk_widget_realize (gGradesWindow);

  gGradesBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gGradesBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gGradesWindow), gGradesBox);
#else
  gtk_container_add (GTK_CONTAINER (gGradesWindow), gGradesBox);
#endif
  gtk_widget_show (gGradesBox);

  gGradesTable = gtk_table_new (12, 5, FALSE);
  gtk_widget_show (gGradesTable);
  gtk_box_pack_start (GTK_BOX (gGradesBox), gGradesTable,
    TRUE, TRUE, 0);

  snprintf (sLabelText, MAX_LABEL, "Select an action to perform on the grades "
    "in period %i:", iPeriod);
  gGradesLabel = gtk_label_new (sLabelText);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesLabel, 0, 12, 0,
    1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gGradesLabel);

  gGradesSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesSep, 0, 12, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesSep);

  gGradesButton = gtk_button_new_with_label ("Add");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton), 114, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton), "clicked",
    GTK_SIGNAL_FUNC (AddGrades), (gpointer) 0);
  GTK_WIDGET_SET_FLAGS (gGradesButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gGradesWindow), gGradesButton);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton, 0, 3, 2,
    3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton);

  gGradesButton2 = gtk_button_new_with_label ("Edit");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton2), 112, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton2), "clicked",
    GTK_SIGNAL_FUNC (EditGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gGradesButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton2, 3, 6,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton2);

  gGradesButton3 = gtk_button_new_with_label ("Delete");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton3), 112, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton3), "clicked",
    GTK_SIGNAL_FUNC (DeleteGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gGradesButton3, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton3, 6, 9,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton3);

  gGradesButton4 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton4), 112, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton4), "clicked",
    GTK_SIGNAL_FUNC (NoGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gGradesButton4, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton4, 9, 12,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton4);

  gGradesSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesSep2, 0, 12, 3, 4,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesSep2);

  gGradesButton5 = gtk_button_new_with_label ("Previous Period");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton5), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton5), "clicked",
    GTK_SIGNAL_FUNC (PrevPeriod), NULL);
  GTK_WIDGET_SET_FLAGS (gGradesButton5, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton5, 0, 4,
    4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton5);

  gGradesButton6 = gtk_button_new_with_label ("Edit Current Period");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton6), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton6), "clicked",
    GTK_SIGNAL_FUNC (NextPeriod), (gpointer) 1);
  GTK_WIDGET_SET_FLAGS (gGradesButton6, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton6, 4, 8,
    4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton6);

  gGradesButton7 = gtk_button_new_with_label ("Next Period");
  gtk_widget_set_usize (GTK_WIDGET (gGradesButton7), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gGradesButton7), "clicked",
    GTK_SIGNAL_FUNC (NextPeriod), (gpointer) 0);
  GTK_WIDGET_SET_FLAGS (gGradesButton7, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gGradesTable), gGradesButton7, 8, 12,
    4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gGradesButton7);

  gtk_widget_show (gGradesWindow);
}

void NoGrades (void)
{
  if (gGradesWindow != NULL)
  {
    gtk_widget_destroy (gGradesWindow);
    gGradesWindow = NULL;
  }
}

void Grades1 (void)
{
  iGradesTemp = 1;
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesButton4), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesButton5), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesScrolled), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesOptionMenu), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton2), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton3), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton4), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton5), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry3), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry4), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry8), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry9), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton5), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton6), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton7), 0);
  InsertLettersList ();
  InsertLetters ();
}

void Grades2 (void)
{
  iGradesTemp = 2;
  InsertLettersList ();
  InsertLetters ();
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesButton4), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesButton5), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesScrolled), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesOptionMenu), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton2), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton3), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton4), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton5), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry3), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry4), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry8), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry9), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton5), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton6), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesRButton7), 1);
}

void AddGrades (GtkWidget *agW, gpointer *agData)
{
  GtkWidget *gAddGradesBox;
  GtkWidget *gAddGradesTable;
  GtkWidget *gAddGradesTable2;
  GtkWidget *gAddGradesTable3;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar8;
#endif
  GtkWidget *gAddGradesLabel;
  GtkWidget *gAddGradesLabel2;
  GtkWidget *gAddGradesLabel3;
  GtkWidget *gAddGradesLabel4;
  GtkWidget *gAddGradesLabel5;
  GtkWidget *gAddGradesLabel6;
  GtkWidget *gAddGradesLabelS[MAX_STUDENTS + 1];
  GtkWidget *gAddGradesSep;
  GtkWidget *gAddGradesSep2;
  GtkWidget *gAddGradesSep3;
  GtkWidget *gAddGradesSep4;
  GtkWidget *gAddGradesFrame;
  GtkWidget *gAddGradesButton;
  GtkWidget *gAddGradesButton2;
  GtkWidget *gAddGradesStatBar;
  GtkWidget *gAddGradesScroll;
  GtkObject *gAddGradesSpinButtonAdj;
  GtkWidget *gAddGradesScrolled;
  GtkWidget *gAddGradesScrolled2;
  GtkWidget *gAddGradesHandleBox;
  int iTemp;
  int iEdit;

  if (gAddGradesWindow != NULL)
  {
    gtk_widget_destroy (gAddGradesWindow);
    gAddGradesWindow = NULL;
  }
  iEdit = (int)agData;

#ifdef USE_GNOME
  if (iEdit == 0)
  {
    gAddGradesWindow = gnome_app_new ("Add Grades", "Add Grades");
  }
  else
  {
    gAddGradesWindow = gnome_app_new ("Edit Grades", "Edit Grades");
  }
#else
  gAddGradesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gAddGradesWindow), 450, 0);
  gtk_window_position (GTK_WINDOW (gAddGradesWindow), GTK_WIN_POS_CENTER);
  if (iEdit == 0)
  {
    gtk_window_set_title (GTK_WINDOW (gAddGradesWindow), "Add Grades");
  }
  else
  {
    gtk_window_set_title (GTK_WINDOW (gAddGradesWindow), "Edit Grades");
  }
  gtk_signal_connect (GTK_OBJECT (gAddGradesWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoAddGrades), NULL);
  gtk_window_set_policy (GTK_WINDOW (gAddGradesWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gAddGradesWindow), 0);
  gtk_widget_realize (gAddGradesWindow);

  gAddGradesBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gAddGradesBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gAddGradesWindow), gAddGradesBox);
#else
  gtk_container_add (GTK_CONTAINER (gAddGradesWindow), gAddGradesBox);
#endif
  gtk_widget_show (gAddGradesBox);

  gAddGradesTable = gtk_table_new (2, 7, FALSE);
  gtk_widget_show (gAddGradesTable);
  gtk_box_pack_start (GTK_BOX (gAddGradesBox), gAddGradesTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  if (iEdit == 0)
  {
    gnome_app_create_menus (GNOME_APP (gAddGradesWindow), menu8);
  }
  else
  {
    gnome_app_create_menus (GNOME_APP (gAddGradesWindow), menu13);
  }
#else
  if (iEdit == 0)
  {
    GetMenu (gAddGradesWindow, &GMMenuBar8, 8);
  }
  else
  {
    GetMenu (gAddGradesWindow, &GMMenuBar8, 13);
  }
  gtk_table_attach (GTK_TABLE (gAddGradesTable), GMMenuBar8, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar8);
#endif

  if (iEdit == 0)
  {
    snprintf (sLabelText, MAX_LABEL, "Enter data for the new work #%i below:",
      iWorks[iPeriod] + 1);
  }
  else
  {
    snprintf (sLabelText, MAX_LABEL, "Change data for work #%i below:", iEdit);
  }
  gAddGradesLabel = gtk_label_new (sLabelText);
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesLabel, 0, 2, 1, 2,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesLabel);

  gAddGradesSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesSep, 0, 2,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gAddGradesSep);

  gAddGradesFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesFrame, 0, 2, 3, 4,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesFrame);

  gAddGradesTable2 = gtk_table_new (3, 8, FALSE);
  gtk_widget_show (gAddGradesTable2);
  gtk_container_add (GTK_CONTAINER (gAddGradesFrame), gAddGradesTable2);

  gAddGradesLabel2 = gtk_label_new ("Work name:");
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesLabel2, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesLabel2);

  gAddGradesEntry = gtk_entry_new_with_max_length (MAX_NAME);
  gtk_signal_connect (GTK_OBJECT (gAddGradesEntry), "activate",
    GTK_SIGNAL_FUNC (ToText), NULL);
  if (iEdit != 0)
  {
    gtk_entry_set_text (GTK_ENTRY (gAddGradesEntry),
      sWorkNames[iPeriod][iEdit]);
  }
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesEntry, 1, 3,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gAddGradesEntry);
  gtk_widget_show (gAddGradesEntry);

  gAddGradesLabel3 = gtk_label_new ("Weight:");
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesLabel3, 0, 1,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesLabel3);

  if (iEdit == 0)
  {
    gAddGradesSpinButtonAdj = gtk_adjustment_new (1, 1, 10, 1, 5, 0);
  }
  else
  {
    gAddGradesSpinButtonAdj = gtk_adjustment_new (iWorkWeights[iPeriod][iEdit],
      1, 10, 1, 5, 0);
  }
  gAddGradesSpinButton = gtk_spin_button_new (GTK_ADJUSTMENT
    (gAddGradesSpinButtonAdj), 1, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gAddGradesSpinButton), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gAddGradesSpinButton),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesSpinButton, 1, 3,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesSpinButton);

  gAddGradesLabel4 = gtk_label_new ("Extra:");
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesLabel4, 0, 1,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesLabel4);

  gAddGradesText = gtk_text_new (NULL, NULL);
  gtk_text_set_editable (GTK_TEXT (gAddGradesText), TRUE);
  gtk_text_set_word_wrap (GTK_TEXT (gAddGradesText), TRUE);
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesText, 1, 2,
    3, 4, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesText);

  gAddGradesScroll = gtk_vscrollbar_new (GTK_TEXT (gAddGradesText)->vadj);
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesScroll, 2, 3,
    3, 4, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesScroll);

  if (iEdit == 0)
  {
    gtk_text_insert (GTK_TEXT (gAddGradesText), NULL, NULL, NULL, "Enter "
      "additional info here.", -1);
  }
  else
  {
    gtk_text_insert (GTK_TEXT (gAddGradesText), NULL, NULL, NULL,
      sWorkExtra[iPeriod][iEdit], -1);
  }

  gAddGradesSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesSep2, 0, 3, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gAddGradesSep2);

  if (iEdit == 0)
  {
    gAddGradesLabel5 = gtk_label_new ("Enter the grades below:\nUse the left "
      "handle to expand the view...");
  }
  else
  {
    gAddGradesLabel5 = gtk_label_new ("Change the grades below:\nUse the left "
      "handle to expand the view...");
  }
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesLabel5, 0, 3,
    5, 6, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gAddGradesLabel5);

  gAddGradesSep3 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesSep3, 0, 3, 6, 7,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gAddGradesSep3);

  if (iStudents > 0)
  {
    gAddGradesScrolled = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_set_usize (GTK_WIDGET (gAddGradesScrolled), 0, 100);
    gtk_container_set_border_width (GTK_CONTAINER (gAddGradesScrolled), 10);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (gAddGradesScrolled),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesScrolled, 0, 3,
      7, 8, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gAddGradesScrolled);

    gAddGradesHandleBox = gtk_handle_box_new ();
    gtk_widget_show (gAddGradesHandleBox);
    gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW
      (gAddGradesScrolled), gAddGradesHandleBox);

    gAddGradesScrolled2 = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_set_usize (GTK_WIDGET (gAddGradesScrolled2), 450, 450);
    gtk_container_set_border_width (GTK_CONTAINER (gAddGradesScrolled2), 10);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (gAddGradesScrolled2),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    gtk_container_add (GTK_CONTAINER (gAddGradesHandleBox),
      gAddGradesScrolled2);
    gtk_widget_show (gAddGradesScrolled2);

    gAddGradesTable3 = gtk_table_new (2, MAX_STUDENTS, FALSE);
    gtk_widget_show (gAddGradesTable3);
    gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW
      (gAddGradesScrolled2), gAddGradesTable3);

    for (iTemp = 1; iTemp <= iStudents; iTemp++)
    {
      gAddGradesLabelS[iTemp] = gtk_label_new (sStudentNames[iTemp]);
      gtk_table_attach (GTK_TABLE (gAddGradesTable3), gAddGradesLabelS[iTemp],
        0, 1, iTemp, iTemp + 1, GTK_FILL, GTK_FILL, 10, 10);
      gtk_widget_show (gAddGradesLabelS[iTemp]);

      gAddGradesEntryS[iTemp] = gtk_entry_new_with_max_length (MAX_GRADE_L);
      if (iTemp != iStudents)
      {
        gtk_signal_connect (GTK_OBJECT (gAddGradesEntryS[iTemp]), "activate",
          GTK_SIGNAL_FUNC (ToNextAddEntry), (gpointer) iTemp + 1);
      }
      else
      {
        gtk_signal_connect (GTK_OBJECT (gAddGradesEntryS[iTemp]), "activate",
          GTK_SIGNAL_FUNC (YesAddGrades), (gpointer) iEdit);
      }
      if (iEdit != 0)
      {
        gtk_entry_set_text (GTK_ENTRY (gAddGradesEntryS[iTemp]),
          sWorkGrades[iPeriod][iEdit][iTemp]);
      }
      gtk_table_attach (GTK_TABLE (gAddGradesTable3), gAddGradesEntryS[iTemp],
        1, 2, iTemp, iTemp + 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
      gtk_widget_show (gAddGradesEntryS[iTemp]);
    }
  }
  else
  {
    gAddGradesLabel6 = gtk_label_new ("Add a student first.");
    gtk_table_attach (GTK_TABLE (gAddGradesTable2), gAddGradesLabel6, 0, 3,
      7, 8, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gAddGradesLabel6);
  }

  gAddGradesSep4 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesSep4, 0, 2, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gAddGradesSep4);

  if (iEdit == 0)
  {
    gAddGradesButton = gtk_button_new_with_label ("Add");
  }
  else
  {
    gAddGradesButton = gtk_button_new_with_label ("Change");
  }
  gtk_widget_set_usize (GTK_WIDGET (gAddGradesButton), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gAddGradesButton), "clicked",
    GTK_SIGNAL_FUNC (YesAddGrades), (gpointer) iEdit);
  GTK_WIDGET_SET_FLAGS (gAddGradesButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gAddGradesWindow), gAddGradesButton);
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesButton, 0, 1, 5, 6,
    GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gAddGradesButton);

  gAddGradesButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gAddGradesButton2), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gAddGradesButton2), "clicked",
    GTK_SIGNAL_FUNC (NoAddGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gAddGradesButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesButton2, 1, 2,
    5, 6, GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gAddGradesButton2);

  gAddGradesStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gAddGradesTable), gAddGradesStatBar, 0, 2,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gAddGradesStatBar);
  if (iEdit == 0)
  {
    gtk_statusbar_push (GTK_STATUSBAR (gAddGradesStatBar), 1, " Add Grades");
  }
  else
  {
    gtk_statusbar_push (GTK_STATUSBAR (gAddGradesStatBar), 1, " Edit Grades");
  }

  gtk_widget_show (gAddGradesWindow);
  if (iEdit == 0)
  {
    if (gGradesWindow != NULL)
    {
      gtk_widget_destroy (gGradesWindow);
      gGradesWindow = NULL;
    }
  }
}

void NoAddGrades (void)
{
  if (gAddGradesWindow != NULL)
  {
    gtk_widget_destroy (gAddGradesWindow);
    gAddGradesWindow = NULL;
  }
}

void YesAddGrades (GtkWidget *yagW, gpointer *yagData)
{
  int iRet;
  int iTemp;
  int iTemp2;
  int iEdit;
  int iWrongGrades;
  char sGradeUpper[MAX_STRING];

  iEdit = (int)yagData;
  if (iEdit == 0)
  {
    iWorks[iPeriod]++;
  }

  if (iEdit == 0)
  {
    snprintf (sWorkNames[iPeriod][iWorks[iPeriod]], MAX_NAME, "%s",
      gtk_entry_get_text (GTK_ENTRY (gAddGradesEntry)));
    iWorkWeights[iPeriod][iWorks[iPeriod]] = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON (gAddGradesSpinButton));
    iRet = snprintf (sWorkExtra[iPeriod][iWorks[iPeriod]], MAX_TEXT, "%s",
      gtk_editable_get_chars (GTK_EDITABLE (gAddGradesText), 0, -1));
  }
  else
  {
    snprintf (sWorkNames[iPeriod][iEdit], MAX_NAME, "%s",
      gtk_entry_get_text (GTK_ENTRY (gAddGradesEntry)));
    iWorkWeights[iPeriod][iEdit] = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON (gAddGradesSpinButton));
    iRet = snprintf (sWorkExtra[iPeriod][iEdit], MAX_TEXT, "%s",
      gtk_editable_get_chars (GTK_EDITABLE (gAddGradesText), 0, -1));
  }
  if ((iRet == -1) || (iRet > MAX_TEXT))
  {
    snprintf (sLabelText, MAX_LABEL, "Only saved the first %i characters\nof "
      "the extra info!", MAX_TEXT);
    Message (sLabelText);
  }
  if (iStudents > 0)
  {
    iWrongGrades = 0;
    for (iTemp = 1; iTemp <= iStudents; iTemp++)
    {
      snprintf (sWriteText, MAX_STRING, "%s", gtk_entry_get_text (GTK_ENTRY
        (gAddGradesEntryS[iTemp])));
      snprintf (sGradeUpper, MAX_STRING, "%s", "");
      for (iTemp2 = 0; iTemp2 < strlen (sWriteText); iTemp2++)
      {
        snprintf (sTempC, MAX_STRING, "%s", sGradeUpper);
        snprintf (sGradeUpper, MAX_STRING, "%s%c", sTempC, toupper
          (sWriteText[iTemp2]));
      }
      snprintf (sWriteText, MAX_STRING, "%s", sGradeUpper);
      if (IsCorrectGrade (iGrades, sWriteText) == 0)
      {
        snprintf (sWriteText, MAX_STRING, "%s", "");
        iWrongGrades = 1;
      }
      if (iEdit == 0)
      {
        snprintf (sWorkGrades[iPeriod][iWorks[iPeriod]][iTemp], MAX_GRADE_L,
          "%s", sWriteText);
      }
      else
      {
        snprintf (sWorkGrades[iPeriod][iEdit][iTemp], MAX_GRADE_L, "%s",
          sWriteText);
      }
      gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
        (BEFORE_GR - 1) + iWorks[iPeriod], sWriteText);
    }
    if (iWrongGrades == 1)
    {
      Message ("Ignored some incorrect grades!");
    }
  }
  gtk_clist_set_column_visibility (GTK_CLIST (gEditClassList), (BEFORE_GR - 1)
    + iWorks[iPeriod], TRUE);
  if (gAddGradesWindow != NULL)
  {
    gtk_widget_destroy (gAddGradesWindow);
    gAddGradesWindow = NULL;
  }
  UpdateList ();
}

void EditGrades (void)
{
  GtkWidget *gEditGradesBox;
  GtkWidget *gEditGradesTable;
  GtkWidget *gEditGradesLabel;
  GtkWidget *gEditGradesSep;
  GtkWidget *gEditGradesSep2;
  GtkWidget *gEditGradesButton;
  GtkWidget *gEditGradesButton2;

  if (gGradesWindow != NULL)
  {
    gtk_widget_destroy (gGradesWindow);
    gGradesWindow = NULL;
  }
  if (gEditGradesWindow != NULL)
  {
    gtk_widget_destroy (gEditGradesWindow);
    gEditGradesWindow = NULL;
  }

#ifdef USE_GNOME
  gEditGradesWindow = gnome_app_new ("Edit Grades", "Edit Grades");
#else
  gEditGradesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gEditGradesWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gEditGradesWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gEditGradesWindow), "Edit Grades");
  gtk_signal_connect (GTK_OBJECT (gEditGradesWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoEditGrades), NULL);
  gtk_window_set_policy (GTK_WINDOW (gEditGradesWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gEditGradesWindow), 0);
  gtk_widget_realize (gEditGradesWindow);

  gEditGradesBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gEditGradesBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gEditGradesWindow), gEditGradesBox);
#else
  gtk_container_add (GTK_CONTAINER (gEditGradesWindow), gEditGradesBox);
#endif
  gtk_widget_show (gEditGradesBox);

  gEditGradesTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gEditGradesTable);
  gtk_box_pack_start (GTK_BOX (gEditGradesBox), gEditGradesTable,
    TRUE, TRUE, 0);

  gEditGradesLabel = gtk_label_new ("Enter the number of the work you "
    "want\nto edit below:");
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesLabel, 0, 2,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gEditGradesLabel);

  gEditGradesSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesSep, 0, 2,
    1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditGradesSep);

  gEditGradesEntry = gtk_entry_new_with_max_length (MAX_SNUMBER);
  gtk_signal_connect (GTK_OBJECT (gEditGradesEntry), "activate",
    GTK_SIGNAL_FUNC (YesEditGrades), NULL);
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesEntry, 0, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gEditGradesEntry);
  gtk_widget_show (gEditGradesEntry);

  gEditGradesSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesSep2,
    0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditGradesSep2);

  gEditGradesButton = gtk_button_new_with_label ("Edit");
  gtk_widget_set_usize (GTK_WIDGET (gEditGradesButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gEditGradesButton), "clicked",
    GTK_SIGNAL_FUNC (YesEditGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gEditGradesButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gEditGradesWindow),
    gEditGradesButton);
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesButton,
    0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditGradesButton);

  gEditGradesButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gEditGradesButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gEditGradesButton2), "clicked",
    GTK_SIGNAL_FUNC (NoEditGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gEditGradesButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditGradesTable), gEditGradesButton2,
    1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditGradesButton2);

  gtk_widget_show (gEditGradesWindow);
}

void NoEditGrades (void)
{
  if (gEditGradesWindow != NULL)
  {
    gtk_widget_destroy (gEditGradesWindow);
    gEditGradesWindow = NULL;
  }
}

void YesEditGrades (void)
{
  int iWork;

  iWork = atoi (gtk_entry_get_text (GTK_ENTRY (gEditGradesEntry)));
  if ((iWork > 0) && (iWork <= iWorks[iPeriod]))
  {
    AddGrades (NULL, (gpointer)iWork);
  }
  else
  {
    Message ("Wrong work number!");
  }
  if (gEditGradesWindow != NULL)
  {
    gtk_widget_destroy (gEditGradesWindow);
    gEditGradesWindow = NULL;
  }
}

void DeleteGrades (void)
{
  GtkWidget *gDeleteGradesBox;
  GtkWidget *gDeleteGradesTable;
  GtkWidget *gDeleteGradesLabel;
  GtkWidget *gDeleteGradesSep;
  GtkWidget *gDeleteGradesSep2;
  GtkWidget *gDeleteGradesButton;
  GtkWidget *gDeleteGradesButton2;

  if (gGradesWindow != NULL)
  {
    gtk_widget_destroy (gGradesWindow);
    gGradesWindow = NULL;
  }
  if (gDeleteGradesWindow != NULL)
  {
    gtk_widget_destroy (gDeleteGradesWindow);
    gDeleteGradesWindow = NULL;
  }

#ifdef USE_GNOME
  gDeleteGradesWindow = gnome_app_new ("Delete Grades", "Delete Grades");
#else
  gDeleteGradesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gDeleteGradesWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gDeleteGradesWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gDeleteGradesWindow), "Delete Grades");
  gtk_signal_connect (GTK_OBJECT (gDeleteGradesWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoDeleteGrades), NULL);
  gtk_window_set_policy (GTK_WINDOW (gDeleteGradesWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gDeleteGradesWindow), 0);
  gtk_widget_realize (gDeleteGradesWindow);

  gDeleteGradesBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gDeleteGradesBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gDeleteGradesWindow), gDeleteGradesBox);
#else
  gtk_container_add (GTK_CONTAINER (gDeleteGradesWindow), gDeleteGradesBox);
#endif
  gtk_widget_show (gDeleteGradesBox);

  gDeleteGradesTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gDeleteGradesTable);
  gtk_box_pack_start (GTK_BOX (gDeleteGradesBox), gDeleteGradesTable,
    TRUE, TRUE, 0);

  gDeleteGradesLabel = gtk_label_new ("Enter the number of the work you "
    "want\nto delete below:");
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesLabel, 0, 2,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gDeleteGradesLabel);

  gDeleteGradesSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesSep, 0, 2,
    1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteGradesSep);

  gDeleteGradesEntry = gtk_entry_new_with_max_length (MAX_SNUMBER);
  gtk_signal_connect (GTK_OBJECT (gDeleteGradesEntry), "activate",
    GTK_SIGNAL_FUNC (YesDeleteGrades), NULL);
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesEntry, 0, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gDeleteGradesEntry);
  gtk_widget_show (gDeleteGradesEntry);

  gDeleteGradesSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesSep2,
    0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteGradesSep2);

  gDeleteGradesButton = gtk_button_new_with_label ("Delete");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteGradesButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteGradesButton), "clicked",
    GTK_SIGNAL_FUNC (YesDeleteGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteGradesButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gDeleteGradesWindow),
    gDeleteGradesButton);
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesButton,
    0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteGradesButton);

  gDeleteGradesButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteGradesButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteGradesButton2), "clicked",
    GTK_SIGNAL_FUNC (NoDeleteGrades), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteGradesButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gDeleteGradesTable), gDeleteGradesButton2,
    1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteGradesButton2);

  gtk_widget_show (gDeleteGradesWindow);
}

void NoDeleteGrades (void)
{
  if (gDeleteGradesWindow != NULL)
  {
    gtk_widget_destroy (gDeleteGradesWindow);
    gDeleteGradesWindow = NULL;
  }
}

void YesDeleteGrades (void)
{
  GtkWidget *gYesDeleteGradesBox;
  GtkWidget *gYesDeleteGradesTable;
  GtkWidget *gYesDeleteGradesLabel;
  GtkWidget *gYesDeleteGradesLabel2;
  GtkWidget *gYesDeleteGradesSep;
  GtkWidget *gYesDeleteGradesSep2;
  GtkWidget *gYesDeleteGradesButton;
  GtkWidget *gYesDeleteGradesButton2;
  int iWork;

  iWork = atoi (gtk_entry_get_text (GTK_ENTRY (gDeleteGradesEntry)));
  if (gDeleteGradesWindow != NULL)
  {
    gtk_widget_destroy (gDeleteGradesWindow);
    gDeleteGradesWindow = NULL;
  }

  if ((iWork > 0) && (iWork <= iWorks[iPeriod]))
  {
    if (gYesDeleteGradesWindow != NULL)
    {
      gtk_widget_destroy (gYesDeleteGradesWindow);
      gYesDeleteGradesWindow = NULL;
    }

#ifdef USE_GNOME
    gYesDeleteGradesWindow = gnome_app_new ("Question", "Question");
#else
    gYesDeleteGradesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteGradesWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gYesDeleteGradesWindow),
      GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (gYesDeleteGradesWindow), "Question");
    gtk_signal_connect (GTK_OBJECT (gYesDeleteGradesWindow), "delete_event",
      GTK_SIGNAL_FUNC (DontDeleteGrades), NULL);
    gtk_window_set_policy (GTK_WINDOW (gYesDeleteGradesWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gYesDeleteGradesWindow), 0);
    gtk_widget_realize (gYesDeleteGradesWindow);

    gYesDeleteGradesBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gYesDeleteGradesBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gYesDeleteGradesWindow),
      gYesDeleteGradesBox);
#else
    gtk_container_add (GTK_CONTAINER (gYesDeleteGradesWindow),
      gYesDeleteGradesBox);
#endif
    gtk_widget_show (gYesDeleteGradesBox);

    gYesDeleteGradesTable = gtk_table_new (2, 5, FALSE);
    gtk_widget_show (gYesDeleteGradesTable);
    gtk_box_pack_start (GTK_BOX (gYesDeleteGradesBox), gYesDeleteGradesTable,
      TRUE, TRUE, 0);

    gYesDeleteGradesLabel = gtk_label_new ("Are you sure you want to delete "
      "the\nwork below:");
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable), gYesDeleteGradesLabel,
      0, 2, 0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gYesDeleteGradesLabel);

    gYesDeleteGradesSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable), gYesDeleteGradesSep,
      0, 2, 1, 2, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteGradesSep);

    gYesDeleteGradesLabel2 = gtk_label_new (sWorkNames[iPeriod][iWork]);
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable),
      gYesDeleteGradesLabel2, 0, 2, 2, 3, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gYesDeleteGradesLabel2);

    gYesDeleteGradesSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable), gYesDeleteGradesSep2,
      0, 2, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteGradesSep2);

    gYesDeleteGradesButton = gtk_button_new_with_label ("Delete");
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteGradesButton), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gYesDeleteGradesButton), "clicked",
      GTK_SIGNAL_FUNC (DoDeleteGrades), (gpointer) iWork);
    GTK_WIDGET_SET_FLAGS (gYesDeleteGradesButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gYesDeleteGradesWindow),
      gYesDeleteGradesButton);
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable),
      gYesDeleteGradesButton, 0, 1, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteGradesButton);

    gYesDeleteGradesButton2 = gtk_button_new_with_label ("Cancel");
    gtk_widget_set_usize (GTK_WIDGET (gYesDeleteGradesButton2), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gYesDeleteGradesButton2), "clicked",
      GTK_SIGNAL_FUNC (DontDeleteGrades), NULL);
    GTK_WIDGET_SET_FLAGS (gYesDeleteGradesButton2, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gYesDeleteGradesTable),
      gYesDeleteGradesButton2, 1, 2, 4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gYesDeleteGradesButton2);

    gtk_widget_show (gYesDeleteGradesWindow);
  }
  else
  {
    Message ("Wrong work number!");
  }
}

void DontDeleteGrades (void)
{
  if (gYesDeleteGradesWindow != NULL)
  {
    gtk_widget_destroy (gYesDeleteGradesWindow);
    gYesDeleteGradesWindow = NULL;
  }
}

void DoDeleteGrades (GtkWidget *ddgW, gpointer *ddgData)
{
  int iWork;
  int iTemp;
  int iTemp2;

  iWork = (int)ddgData;

  for (iTemp = iWork; iTemp <= iWorks[iPeriod]; iTemp++)
  {
    snprintf (sWorkNames[iPeriod][iTemp], MAX_NAME, "%s",
      sWorkNames[iPeriod][iTemp + 1]);
    iWorkWeights[iPeriod][iTemp] = iWorkWeights[iPeriod][iTemp + 1];
    snprintf (sWorkExtra[iPeriod][iTemp], MAX_TEXT, "%s",
      sWorkExtra[iPeriod][iTemp + 1]);
    for (iTemp2 = 1; iTemp2 <= iStudents; iTemp2++)
    {
      snprintf (sWorkGrades[iPeriod][iTemp][iTemp2], MAX_GRADE_L, "%s",
        sWorkGrades[iPeriod][iTemp + 1][iTemp2]);
    }
  }
  iWorks[iPeriod]--;
  if (gYesDeleteGradesWindow != NULL)
  {
    gtk_widget_destroy (gYesDeleteGradesWindow);
    gYesDeleteGradesWindow = NULL;
  }
  UpdateList ();
}

void Lowest (double dGrade, int iWeight)
{
  if (dGrade < dLowestGrade)
  {
    dLowestGrade = dGrade;
    iLowestWeight = iWeight;
  }
  else if (dGrade == dLowestGrade)
  {
    if (iWeight > iLowestWeight)
    {
      dLowestGrade = dGrade;
      iLowestWeight = iWeight;
    }
  }
}

void Averages (void)
{
  int iTemp;
  int iTemp2;
  int iTemp3;
  double dGradesTotal;
  int iWeightsTotal;
  int iWeight;
  char sPeriodAvgTmp[20];
  char sPeriodAvgTmp2[20];
  char sPeriodAvgTmp3[20];
  char sFinalAvgTmp[20];
  char sFinalAvgTmp2[20];
  char sFinalAvgTmp3[20];
  int iDot;
  int iNrWorks;
  int iIndex;
  char cTheChar;
  int dSmallestDif;
  int iDifGrade;
  int iPlus;
  int iMinus;

  for (iTemp = 1; iTemp <= iPeriods; iTemp++)
  {
    for (iTemp2 = 1; iTemp2 <= iStudents; iTemp2++)
    {
      if (iGrades == 1)
      {
        dGradesTotal = 0;
        iWeightsTotal = 0;
        dLowestGrade = 10000;
        iLowestWeight = 10;
        for (iTemp3 = 1; iTemp3 <= iWorks[iTemp]; iTemp3++)
        {
          if ((sWorkGrades[iTemp][iTemp3][iTemp2][0] >= 65) &&
            (sWorkGrades[iTemp][iTemp3][iTemp2][0] <= 90))
          {
            cTheChar = sWorkGrades[iTemp][iTemp3][iTemp2][0];
            iIndex = cTheChar - 65;
            iWeight = iWorkWeights[iTemp][iTemp3];
            iWeightsTotal += iWeight;
            dGradesTotal += atof (sGrades[iIndex]) * iWeight;
            if (sWorkGrades[iTemp][iTemp3][iTemp2][1] == '+')
            {
              dGradesTotal += dPlus * iWeight;
              Lowest (atof (sGrades[iIndex]) + dPlus, iWeight);
            }
            else if (sWorkGrades[iTemp][iTemp3][iTemp2][1] == '-')
            {
              dGradesTotal += dMinus * iWeight;
              Lowest (atof (sGrades[iIndex]) + dMinus, iWeight);
            }
            else
            {
              Lowest (atof (sGrades[iIndex]), iWeight);
            }
          }
          else
          {
            iWeight = iWorkWeights[iTemp][iTemp3];
            iWeightsTotal += iWeight;
            dGradesTotal += atof (sWorkGrades[iTemp][iTemp3][iTemp2])
              * iWeight;
            Lowest (atof (sWorkGrades[iTemp][iTemp3][iTemp2]), iWeight);
          }
        }
        if ((iDrop == 1) && (iWorks[iTemp] > 1))
        {
          dGradesTotal -= (dLowestGrade * iLowestWeight);
          iWeightsTotal -= iLowestWeight;
        }
        dSmallestDif = dHighestPos - dLowestPos;
        iDifGrade = 0;
        for (iTemp3 = 0; iTemp3 < ALPH; iTemp3++)
        {
          if ((Difference (atof (sGrades[iTemp3]), (dGradesTotal /
            iWeightsTotal))) < dSmallestDif)
          {
            dSmallestDif = Difference (atof (sGrades[iTemp3]), (dGradesTotal /
              iWeightsTotal));
            iDifGrade = iTemp3;
          }
        }
        snprintf (sPeriodAvgData[iTemp][iTemp2], MAX_GRADE_L, "%c",
          iDifGrade + 65);
      }
      else
      {
        dGradesTotal = 0;
        iWeightsTotal = 0;
        dLowestGrade = 10000;
        iLowestWeight = 10;
        iNrWorks = 0;
        for (iTemp3 = 1; iTemp3 <= iWorks[iTemp]; iTemp3++)
        {
          iPlus = 0;
          iMinus = 0;
          iWeight = iWorkWeights[iTemp][iTemp3];
          if (sWorkGrades[iTemp][iTemp3][iTemp2]
            [strlen (sWorkGrades[iTemp][iTemp3][iTemp2]) - 1] == '+')
          {
            iPlus = 1;
          }
          if (sWorkGrades[iTemp][iTemp3][iTemp2]
            [strlen (sWorkGrades[iTemp][iTemp3][iTemp2]) - 1] == '-')
          {
            iMinus = 1;
          }
          if (atof (sWorkGrades[iTemp][iTemp3][iTemp2]) != 0)
          {
            iNrWorks++;
            iWeightsTotal += iWeight;
            dGradesTotal += atof (sWorkGrades[iTemp][iTemp3][iTemp2])
              * iWeight;
            if (iPlus == 1)
            {
              dGradesTotal += dPlus * iWeight;
              Lowest (atof (sWorkGrades[iTemp][iTemp3][iTemp2]) + iPlus,
                iWeight);
            }
            else if (iMinus == 1)
            {
              dGradesTotal += dMinus * iWeight;
              Lowest (atof (sWorkGrades[iTemp][iTemp3][iTemp2]) + iMinus,
                iWeight);
            }
            else
            {
              Lowest (atof (sWorkGrades[iTemp][iTemp3][iTemp2]), iWeight);
            }
          }
        }
        if ((iDrop == 1) && (iNrWorks > 1))
        {
          dGradesTotal -= (dLowestGrade * iLowestWeight);
          iWeightsTotal -= iLowestWeight;
        }

        if (iWeightsTotal > 0)
        {
          if (iPeriodAvg[iTemp] == 1)
          {
            snprintf (sPeriodAvgData[iTemp][iTemp2], MAX_GRADE_L, "%.0f",
              rint (dGradesTotal / iWeightsTotal));
          }
          else if (iPeriodAvg[iTemp] == 2)
          {
            snprintf (sPeriodAvgTmp, 20, "%.2f", dGradesTotal / iWeightsTotal);
            iDot = 0;
            snprintf (sPeriodAvgTmp2, 20, "%s", "");
            snprintf (sPeriodAvgTmp3, 20, "%s", "");
            for (iTemp3 = 0; iTemp3 < strlen (sPeriodAvgTmp); iTemp3++)
            {
              if (iDot == 1)
              {
                snprintf (sTempC, MAX_STRING, "%s", sPeriodAvgTmp2);
                snprintf (sPeriodAvgTmp2, 20, "%s%c", sTempC,
                  sPeriodAvgTmp[iTemp3]);
              }
              else
              {
                if (sPeriodAvgTmp[iTemp3] == '.')
                {
                  iDot = 1;
                }
                else
                {
                  snprintf (sTempC, MAX_STRING, "%s", sPeriodAvgTmp3);
                  snprintf (sPeriodAvgTmp3, 20, "%s%c", sTempC,
                    sPeriodAvgTmp[iTemp3]);
                }
              }
            }
            if ((atoi (sPeriodAvgTmp2) >= 25) && (atoi (sPeriodAvgTmp2) < 75))
            {
              snprintf (sTempC, MAX_STRING, "%s", sPeriodAvgTmp3);
              snprintf (sPeriodAvgTmp3, 20, "%s.5", sTempC);
            }
            if (atoi (sPeriodAvgTmp2) >= 75)
            {
              snprintf (sTempC, MAX_STRING, "%s", sPeriodAvgTmp3);
              snprintf (sPeriodAvgTmp3, 20, "%i", atoi (sTempC) + 1);
            }
            snprintf (sPeriodAvgData[iTemp][iTemp2], MAX_GRADE_L, "%s",
              sPeriodAvgTmp3);
          }
          else
          {
            snprintf (sPeriodAvgData[iTemp][iTemp2], MAX_GRADE_L, "%.2f",
              dGradesTotal / iWeightsTotal);
          }
        }
        else
        {
          snprintf (sPeriodAvgData[iTemp][iTemp2], MAX_GRADE_L, "%s", "?");
        }
      }
    }
  }
  for (iTemp = 1; iTemp <= iStudents; iTemp++)
  {
    if (iGrades == 1)
    {
      dGradesTotal = 0;
      iWeightsTotal = 0;
      for (iTemp2 = 1; iTemp2 <= iPeriods; iTemp2++)
      {
        cTheChar = sPeriodAvgData[iTemp2][iTemp][0];
        iIndex = cTheChar - 65;
        iWeight = iPeriodWeights[iTemp2];
        iWeightsTotal += iWeight;
        dGradesTotal += atof (sGrades[iIndex]) * iWeight;
      }
      dSmallestDif = dHighestPos - dLowestPos;
      iDifGrade = 0;
      for (iTemp2 = 0; iTemp2 < ALPH; iTemp2++)
      {
        if ((Difference (atof (sGrades[iTemp2]), (dGradesTotal /
          iWeightsTotal))) < dSmallestDif)
        {
          dSmallestDif = Difference (atof (sGrades[iTemp2]), (dGradesTotal /
            iWeightsTotal));
          iDifGrade = iTemp2;
        }
      }
      snprintf (sFinalAvgData[iTemp], MAX_GRADE_L, "%c", iDifGrade + 65);
    }
    else
    {
      dGradesTotal = 0;
      iWeightsTotal = 0;
      for (iTemp2 = 1; iTemp2 <= iPeriods; iTemp2++)
      {
        iWeight = iPeriodWeights[iTemp2];
        if (atof (sPeriodAvgData[iTemp2][iTemp]) != 0)
        {
          iWeightsTotal += iWeight;
          dGradesTotal += atof (sPeriodAvgData[iTemp2][iTemp]) * iWeight;
        }
      }
      if (iWeightsTotal > 0)
      {
        if (iFinalAvg == 1)
        {
          snprintf (sFinalAvgData[iTemp], MAX_GRADE_L, "%.0f",
            rint (dGradesTotal / iWeightsTotal));
        }
        else if (iFinalAvg == 2)
        {
          snprintf (sFinalAvgTmp, 20, "%.2f", dGradesTotal / iWeightsTotal);
          iDot = 0;
          snprintf (sFinalAvgTmp2, 20, "%s", "");
          snprintf (sFinalAvgTmp3, 20, "%s", "");
          for (iTemp2 = 0; iTemp2 < strlen (sFinalAvgTmp); iTemp2++)
          {
            if (iDot == 1)
            {
              snprintf (sTempC, MAX_STRING, "%s", sFinalAvgTmp2);
              snprintf (sFinalAvgTmp2, 20, "%s%c", sTempC,
                sFinalAvgTmp[iTemp2]);
            }
            else
            {
              if (sFinalAvgTmp[iTemp2] == '.')
              {
                iDot = 1;
              }
              else
              {
                snprintf (sTempC, MAX_STRING, "%s", sFinalAvgTmp3);
                snprintf (sFinalAvgTmp3, 20, "%s%c", sTempC,
                  sFinalAvgTmp[iTemp2]);
              }
            }
          }
          if ((atoi (sFinalAvgTmp2) >= 25) && (atoi (sFinalAvgTmp2) < 75))
          {
            snprintf (sTempC, MAX_STRING, "%s", sFinalAvgTmp3);
            snprintf (sFinalAvgTmp3, 20, "%s.5", sTempC);
          }
          if (atoi (sFinalAvgTmp2) >= 75)
          {
            snprintf (sTempC, MAX_STRING, "%s", sFinalAvgTmp3);
            snprintf (sFinalAvgTmp3, 20, "%i", atoi (sTempC) + 1);
          }
          snprintf (sFinalAvgData[iTemp], MAX_GRADE_L, "%s", sFinalAvgTmp3);
        }
        else
        {
          snprintf (sFinalAvgData[iTemp], MAX_GRADE_L, "%.2f",
            dGradesTotal / iWeightsTotal);
        }
      }
      else
      {
        snprintf (sFinalAvgData[iTemp], MAX_GRADE_L, "%s", "?");
      }
    }
  }
}

int IsCorrectGrade (int iGradeType, char *sGrade)
{
  char cTheChar;

  if (iGradeType == 1)
  {
    cTheChar = sGrade[0];
    if (strcmp (sGrades[cTheChar - 65], "-200") == 0)
    {
      return (0);
    }
    else
    {
      if (sGrade[strlen (sGrade) - 1] == '+')
      {
        if ((atof (sGrades[cTheChar - 65]) + dPlus > dHighestPos) ||
          (atof (sGrades[cTheChar - 65]) + dPlus < dLowestPos))
        {
          return (0);
        }
        else
        {
          return (1);
        }
      }
      else if (sGrade[strlen (sGrade) - 1] == '-')
      {
        if ((atof (sGrades[cTheChar - 65]) + dMinus > dHighestPos) ||
          (atof (sGrades[cTheChar - 65]) + dMinus < dLowestPos))
        {
          return (0);
        }
        else
        {
          return (1);
        }
      }
      else
      {
        if ((atof (sGrades[cTheChar - 65]) > dHighestPos) ||
          (atof (sGrades[cTheChar - 65]) < dLowestPos))
        {
          return (0);
        }
        else
        {
          return (1);
        }
      }
    }
  }
  else
  {
    cTheChar = sGrade[strlen (sGrade) - 1];
    if (cTheChar == '+')
    {
      if ((atof (sGrade) + dPlus > dHighestPos) ||
        (atof (sGrade) + dPlus < dLowestPos))
      {
        return (0);
      }
      else
      {
        return (1);
      }
    }
    else if (cTheChar == '-')
    {
      if ((atof (sGrade) + dMinus > dHighestPos) ||
        (atof (sGrade) + dMinus < dLowestPos))
      {
        return (0);
      }
      else
      {
        return (1);
      }
    }
    else
    {
      if ((atof (sGrade) > dHighestPos) ||
        (atof (sGrade) < dLowestPos))
      {
        return (0);
      }
      else
      {
        return (1);
      }
    }
  }
}

void ToggleDrop (void)
{
  if (iDropTemp == 0)
  {
    iDropTemp = 1;
  }
  else
  {
    iDropTemp = 0;
  }
}

double Difference (float fOne, float fTwo)
{
  if (fOne > fTwo)
  {
    return (fOne - fTwo);
  }
  else
  {
    return (fTwo - fOne);
  }
}
