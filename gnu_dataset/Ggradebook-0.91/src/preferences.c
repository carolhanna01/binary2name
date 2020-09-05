/* Ggradebook 0.91 (preferences.c)
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
#include "preferences.h"
#include "gg.h"
#include "to.h"
#include "period.h"
#include "grades.h"
#include "message.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu6[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoPreferences, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu6[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu6),
  GNOMEUIINFO_END
};
#endif

void Preferences (GtkWidget *pW, gpointer pData)
{
#ifndef USE_GNOME
  GtkWidget *GMMenuBar6;
#endif
  GtkWidget *gPreferencesBox;
  GtkWidget *gPreferencesTable;
  GtkWidget *gPreferencesTable2;
  GtkWidget *gPreferencesTable3;
  GtkWidget *gPreferencesTable4;
  GtkWidget *gPreferencesTable5;
  GtkWidget *gPreferencesTable6;
  GtkWidget *gPreferencesLabel;
  GtkWidget *gPreferencesLabel2;
  GtkWidget *gPreferencesLabel3;
  GtkWidget *gPreferencesLabel4;
  GtkWidget *gPreferencesLabel5;
  GtkWidget *gPreferencesLabel6;
  GtkWidget *gPreferencesLabel7;
  GtkWidget *gPreferencesLabel12;
  GtkWidget *gPreferencesLabel13;
  GtkWidget *gPreferencesLabel14;
  GtkWidget *gPreferencesLabel15;
  GtkWidget *gPreferencesBLabel;
  GtkWidget *gPreferencesBLabel2;
  GtkWidget *gPreferencesBLabel3;
  GtkWidget *gPreferencesSep;
  GtkWidget *gPreferencesSep2;
  GtkWidget *gPreferencesSep3;
  GtkWidget *gPreferencesSep4;
  GtkWidget *gPreferencesSep5;
  GtkWidget *gPreferencesFrame;
  GtkWidget *gPreferencesButton;
  GtkWidget *gPreferencesButton2;
  GtkWidget *gPreferencesButton3;
  GtkWidget *gPreferencesStatBar;
  GtkWidget *gPreferencesBook;
  GtkWidget *gPreferencesRButton;
  GtkWidget *gPreferencesRButton2;
  GSList    *gPreferencesRGroup;
  GSList    *gPreferencesRGroup2;
  GtkWidget *gPreferencesCButton;
  GtkObject *gPreferencesSpinButtonAdj;
  GtkObject *gPreferencesSpinButtonAdj2;
  GtkObject *gPreferencesSpinButtonAdj3;
  GtkObject *gPreferencesSpinButtonAdj4;
  GtkObject *gPreferencesSpinButtonAdj5;
  GtkObject *gPreferencesSpinButtonAdj6;

  int iTemp;
  int iData;

  gchar *PercList[2] = {
    "Letter",
    "Percentage",
  };

  iData = (int)pData;

  if (gPreferencesWindow != NULL)
  {
    gtk_widget_destroy (gPreferencesWindow);
    gPreferencesWindow = NULL;
  }

#ifdef USE_GNOME
  if (iData == 1)
  {
    gPreferencesWindow = gnome_app_new ("Default Preferences",
      "Default Preferences");
  }
  else
  {
    gPreferencesWindow = gnome_app_new ("Preferences", "Preferences");
  }
#else
  gPreferencesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesWindow), 500, 0);
  gtk_window_position (GTK_WINDOW (gPreferencesWindow), GTK_WIN_POS_CENTER);
  if (iData == 1)
  {
    gtk_window_set_title (GTK_WINDOW (gPreferencesWindow),
      "Default Preferences");
  }
  else
  {
    gtk_window_set_title (GTK_WINDOW (gPreferencesWindow), "Preferences");
  }
  gtk_signal_connect (GTK_OBJECT (gPreferencesWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoPreferences), NULL);
  gtk_window_set_policy (GTK_WINDOW (gPreferencesWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gPreferencesWindow), 0);
  gtk_widget_realize (gPreferencesWindow);

  gPreferencesBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gPreferencesBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gPreferencesWindow), gPreferencesBox);
#else
  gtk_container_add (GTK_CONTAINER (gPreferencesWindow), gPreferencesBox);
#endif
  gtk_widget_show (gPreferencesBox);

  gPreferencesTable = gtk_table_new (2, 7, FALSE);
  gtk_widget_show (gPreferencesTable);
  gtk_box_pack_start (GTK_BOX (gPreferencesBox), gPreferencesTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gPreferencesWindow), menu6);
#else
  GetMenu (gPreferencesWindow, &GMMenuBar6, 6);
  gtk_table_attach (GTK_TABLE (gPreferencesTable), GMMenuBar6, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar6);
#endif

  if (iData == 1)
  {
    gPreferencesLabel = gtk_label_new
      ("Change the default preferences below:");
  }
  else
  {
    gPreferencesLabel = gtk_label_new ("Change the preferences below:");
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesLabel, 0, 2, 1,
    2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel);

  gPreferencesSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesSep, 0, 2, 2, 3,
    GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesSep);

  gPreferencesFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesFrame, 0, 2,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesFrame);

  gPreferencesTable2 = gtk_table_new (1, 1, FALSE);
  gtk_widget_show (gPreferencesTable2);
  gtk_container_add (GTK_CONTAINER (gPreferencesFrame), gPreferencesTable2);

  gPreferencesBook = gtk_notebook_new ();
  gtk_notebook_set_tab_pos (GTK_NOTEBOOK (gPreferencesBook), GTK_POS_TOP);
  gtk_table_attach (GTK_TABLE (gPreferencesTable2), gPreferencesBook, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesBook);

  if (iData == 0)
  {
    gPreferencesBLabel3 = gtk_label_new ("General");
    gtk_widget_show (gPreferencesBLabel3);

    gPreferencesTable3 = gtk_table_new (2, 3, FALSE);
    gtk_widget_show (gPreferencesTable3);
    gtk_notebook_append_page (GTK_NOTEBOOK (gPreferencesBook),
      gPreferencesTable3, gPreferencesBLabel3);
  }
  else
  {
    gPreferencesTable3 = NULL;
  }

  gPreferencesBLabel = gtk_label_new ("Grades");
  gtk_widget_show (gPreferencesBLabel);

  gPreferencesTable4 = gtk_table_new (3, 8, FALSE);
  gtk_widget_show (gPreferencesTable4);
  gtk_notebook_append_page (GTK_NOTEBOOK (gPreferencesBook),
    gPreferencesTable4, gPreferencesBLabel);

  gPreferencesBLabel2 = gtk_label_new ("Specific");
  gtk_widget_show (gPreferencesBLabel2);

  gPreferencesTable5 = gtk_table_new (3, 7, FALSE);
  gtk_widget_show (gPreferencesTable5);
  gtk_notebook_append_page (GTK_NOTEBOOK (gPreferencesBook),
    gPreferencesTable5, gPreferencesBLabel2);
  gtk_notebook_set_page (GTK_NOTEBOOK (gPreferencesBook), 0);

  if (iData == 0)
  {
    gPreferencesLabel2 = gtk_label_new ("Teacher:");
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesLabel2, 0, 1,
      0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPreferencesLabel2);

    gPreferencesEntry = gtk_entry_new_with_max_length (MAX_TEACHER);
    gtk_signal_connect (GTK_OBJECT (gPreferencesEntry), "activate",
      GTK_SIGNAL_FUNC (ToMentorP), NULL);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry), sTeacher);
    gtk_entry_select_region (GTK_ENTRY (gPreferencesEntry), 0, GTK_ENTRY
      (gPreferencesEntry)->text_length);
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesEntry, 1, 2,
      0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_grab_focus (gPreferencesEntry);
    gtk_widget_show (gPreferencesEntry);

    gPreferencesLabel3 = gtk_label_new ("Mentor:");
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesLabel3, 0, 1,
      1, 2, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPreferencesLabel3);

    gPreferencesEntry2 = gtk_entry_new_with_max_length (MAX_MENTOR);
    gtk_signal_connect (GTK_OBJECT (gPreferencesEntry2), "activate",
      GTK_SIGNAL_FUNC (YesPreferences), (gpointer) iData);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry2), sMentor);
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesEntry2, 1, 2,
      1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPreferencesEntry2);

    gPreferencesLabel15 = gtk_label_new ("Periods:");
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesLabel15,
      0, 1, 2, 3, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPreferencesLabel15);

    gPreferencesButton3 = gtk_button_new_with_label ("Edit Current Period");
    gtk_signal_connect (GTK_OBJECT (gPreferencesButton3), "clicked",
      GTK_SIGNAL_FUNC (NextPeriod), (gpointer) 1);
    gtk_table_attach (GTK_TABLE (gPreferencesTable3), gPreferencesButton3,
      1, 2, 2, 3, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPreferencesButton3);
  }

  gPreferencesLabel4 = gtk_label_new ("Use:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesLabel4, 0, 1,
    0, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel4);

  gPreferencesRButton = gtk_radio_button_new_with_label (NULL,
    "Letters and percentages.");
  gtk_signal_connect (GTK_OBJECT (gPreferencesRButton), "clicked",
    GTK_SIGNAL_FUNC (Grades1), NULL);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesRButton, 1, 3,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesRButton);

  gPreferencesRGroup = gtk_radio_button_group (GTK_RADIO_BUTTON
    (gPreferencesRButton));

  gPreferencesRButton2 = gtk_radio_button_new_with_label (gPreferencesRGroup,
    "Numeric grades.");
  gtk_signal_connect (GTK_OBJECT (gPreferencesRButton2), "clicked",
    GTK_SIGNAL_FUNC (Grades2), NULL);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesRButton2, 1, 3,
    1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesRButton2);

  gPreferencesSep3 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesSep3, 0, 3,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesSep3);

  gPreferencesButton4 = gtk_button_new_with_label ("Remove");
  gtk_signal_connect (GTK_OBJECT (gPreferencesButton4), "clicked",
    GTK_SIGNAL_FUNC (RemoveLetter), NULL);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesButton4, 0, 1,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesButton4);

  gPreferencesButton5 = gtk_button_new_with_label ("Add");
  gtk_signal_connect (GTK_OBJECT (gPreferencesButton5), "clicked",
    GTK_SIGNAL_FUNC (AddLetter), NULL);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesButton5, 0, 1,
    4, 5, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesButton5);

  gPreferencesScrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (gPreferencesScrolled), 0);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (gPreferencesScrolled),
    GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesScrolled, 1, 3,
    3, 5, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesScrolled);

  gPreferencesList = gtk_clist_new_with_titles (2, PercList);
  gtk_signal_connect (GTK_OBJECT (gPreferencesList), "select_row",
    GTK_SIGNAL_FUNC (SelectedRow), NULL);
  gtk_clist_set_shadow_type (GTK_CLIST (gPreferencesList), GTK_SHADOW_IN);
  gtk_clist_set_use_drag_icons (GTK_CLIST (gPreferencesList), FALSE);
  gtk_clist_set_column_resizeable (GTK_CLIST (gPreferencesList), 0, FALSE);
  for (iTemp = 0; iTemp < 2; iTemp++)
  {
    gtk_clist_set_column_auto_resize (GTK_CLIST (gPreferencesList), iTemp,
      TRUE);
  }
  gtk_clist_column_titles_passive (GTK_CLIST (gPreferencesList));
  gtk_container_add (GTK_CONTAINER (gPreferencesScrolled), gPreferencesList);
  gtk_widget_show (gPreferencesList);

  if (iData == 1)
  {
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      snprintf (sGradesTemp[iTemp], MAX_GRADE_L, "%s", sDefaultGrades[iTemp]);
    }
    iLettersTemp = iDefaultLetters;
  }
  if (iData == 0)
  {
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      snprintf (sGradesTemp[iTemp], MAX_GRADE_L, "%s", sGrades[iTemp]);
    }
    iLettersTemp = iLetters;
  }
  InsertLettersList ();

  gPreferencesOptionMenu = gtk_option_menu_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesOptionMenu,
    1, 2, 5, 6, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesOptionMenu);

  InsertLetters ();

  gPreferencesSpinButtonAdj3 = gtk_adjustment_new
    (0.0, -100.0, 100.0, 0.1, 100.0, 0.0);
  gPreferencesSpinButton3 = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj3), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton3), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton3), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton3),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesSpinButton3,
    2, 3, 5, 6, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesSpinButton3);

  gPreferencesSep4 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesSep4, 0, 3,
    6, 7, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesSep4);

  gPreferencesTable6 = gtk_table_new (3, 3, FALSE);
  gtk_table_attach (GTK_TABLE (gPreferencesTable4), gPreferencesTable6, 0, 3,
    7, 8, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesTable6);

  gPreferencesLabel5 = gtk_label_new ("Lowest possible perc./grade:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesLabel5, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel5);

  if (iData == 1)
  {
    gPreferencesSpinButtonAdj4 = gtk_adjustment_new
      (dDefaultLowestPos, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  else
  {
    gPreferencesSpinButtonAdj4 = gtk_adjustment_new
      (dLowestPos, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  gPreferencesSpinButton4 = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj4), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton4), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton4), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton4),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesSpinButton4,
    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPreferencesSpinButton4);

  gPreferencesEntry3 = gtk_entry_new_with_max_length (MAX_GRADE_L);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesEntry3), 70, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesEntry3), "activate",
    GTK_SIGNAL_FUNC (ToHighest), NULL);
  if (iData == 0)
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dLowestPos);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry3), sWriteText);
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dDefaultLowestPos);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry3), sWriteText);
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesEntry3, 2, 3,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesEntry3);

  gPreferencesLabel6 = gtk_label_new ("Highest possible perc./grade:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesLabel6, 0, 1,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel6);

  if (iData == 1)
  {
    gPreferencesSpinButtonAdj5 = gtk_adjustment_new
      (dDefaultHighestPos, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  else
  {
    gPreferencesSpinButtonAdj5 = gtk_adjustment_new
      (dHighestPos, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  gPreferencesSpinButton5 = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj5), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton5), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton5), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton5),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesSpinButton5,
    1, 2, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPreferencesSpinButton5);

  gPreferencesEntry4 = gtk_entry_new_with_max_length (MAX_GRADE_L);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesEntry4), 70, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesEntry4), "activate",
    GTK_SIGNAL_FUNC (YesPreferences), (gpointer) iData);
  if (iData == 0)
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dHighestPos);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry4), sWriteText);
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dDefaultHighestPos);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry4), sWriteText);
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesEntry4, 2, 3,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesEntry4);

  gPreferencesLabel7 = gtk_label_new ("Highlight below perc./grade:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesLabel7, 0, 1,
    2, 3, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel7);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesLabel7), 0);

  if (iData == 1)
  {
    gPreferencesSpinButtonAdj6 = gtk_adjustment_new
      (dDefaultHighlightBelow, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  else
  {
    gPreferencesSpinButtonAdj6 = gtk_adjustment_new
      (dHighlightBelow, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  gPreferencesSpinButton6 = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj6), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton6), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton6), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton6),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesSpinButton6,
    1, 2, 2, 3, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPreferencesSpinButton6);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesSpinButton6), 0);

  gPreferencesEntry5 = gtk_entry_new_with_max_length (MAX_GRADE_L);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesEntry5), 70, 0);
  if (iData == 0)
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dHighlightBelow);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry5), sWriteText);
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dDefaultHighlightBelow);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry5), sWriteText);
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable6), gPreferencesEntry5, 2, 3,
    2, 3, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesEntry5);
  gtk_widget_set_sensitive (GTK_WIDGET (gPreferencesEntry5), 0);

  gPreferencesLabel12 = gtk_label_new ("Plus (+) perc./grade:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesLabel12, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel12);

  if (iData == 1)
  {
    gPreferencesSpinButtonAdj = gtk_adjustment_new
      (dDefaultPlus, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  else
  {
    gPreferencesSpinButtonAdj = gtk_adjustment_new
      (dPlus, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  gPreferencesSpinButton = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesSpinButton,
    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPreferencesSpinButton);

  gPreferencesEntry8 = gtk_entry_new_with_max_length (MAX_PERC);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesEntry8), 70, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesEntry8), "activate",
    GTK_SIGNAL_FUNC (ToMinusP), NULL);
  if (iData == 0)
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dPlus);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry8), sWriteText);
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dDefaultPlus);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry8), sWriteText);
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesEntry8, 2, 3,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesEntry8);

  gPreferencesLabel13 = gtk_label_new ("Minus (-) perc./grade:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesLabel13, 0, 1,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel13);

  if (iData == 1)
  {
    gPreferencesSpinButtonAdj2 = gtk_adjustment_new
      (dDefaultMinus, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  else
  {
    gPreferencesSpinButtonAdj2 = gtk_adjustment_new
      (dMinus, -100.0, 100.0, 0.1, 100.0, 0.0);
  }
  gPreferencesSpinButton2 = gtk_spin_button_new (GTK_ADJUSTMENT
    (gPreferencesSpinButtonAdj2), 1, 2);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesSpinButton2), 70, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gPreferencesSpinButton2), TRUE);
  gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gPreferencesSpinButton2),
    GTK_UPDATE_IF_VALID);
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesSpinButton2,
    1, 2, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPreferencesSpinButton2);

  gPreferencesEntry9 = gtk_entry_new_with_max_length (MAX_PERC);
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesEntry9), 70, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesEntry9), "activate",
    GTK_SIGNAL_FUNC (YesPreferences), (gpointer) iData);
  if (iData == 0)
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dMinus);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry9), sWriteText);
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "%.2f", dDefaultMinus);
    gtk_entry_set_text (GTK_ENTRY (gPreferencesEntry9), sWriteText);
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesEntry9, 2, 3,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesEntry9);

  gPreferencesSep5 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesSep5, 0, 3,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesSep5);

  gPreferencesLabel14 = gtk_label_new ("Final Averages in:");
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesLabel14, 0, 1,
    3, 6, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesLabel14);

  gPreferencesRButton5 = gtk_radio_button_new_with_label (NULL, "Wholes");
  gtk_signal_connect (GTK_OBJECT (gPreferencesRButton5), "clicked",
    GTK_SIGNAL_FUNC (SetAvg), (gpointer) 1);
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesRButton5, 1, 3,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesRButton5);

  gPreferencesRGroup2 = gtk_radio_button_group (GTK_RADIO_BUTTON
    (gPreferencesRButton5));
  gPreferencesRButton6 = gtk_radio_button_new_with_label (gPreferencesRGroup2,
    "Halves");
  gtk_signal_connect (GTK_OBJECT (gPreferencesRButton6), "clicked",
    GTK_SIGNAL_FUNC (SetAvg), (gpointer) 2);
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesRButton6, 1, 3,
    4, 5, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesRButton6);

  gPreferencesRGroup2 = gtk_radio_button_group (GTK_RADIO_BUTTON
    (gPreferencesRButton6));
  gPreferencesRButton7 = gtk_radio_button_new_with_label (gPreferencesRGroup2,
    "Decimals");
  gtk_signal_connect (GTK_OBJECT (gPreferencesRButton7), "clicked",
    GTK_SIGNAL_FUNC (SetAvg), (gpointer) 3);
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesRButton7, 1, 3,
    5, 6, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPreferencesRButton7);

  if (iData == 1)
  {
    switch (iDefaultFinalAvg)
    {
      case 1: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton5), TRUE); break;
      case 2: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton6), TRUE); break;
      case 3: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton7), TRUE); break;
    }
  }
  else
  {
    switch (iFinalAvg)
    {
      case 1: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton5), TRUE); break;
      case 2: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton6), TRUE); break;
      case 3: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton7), TRUE); break;
    }
  }

  gPreferencesCButton = gtk_check_button_new_with_label ("Drop lowest grades "
    "from periods when calculating\nthe period averages.");
  if (iData == 1)
  {
    if (iDefaultDrop == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPreferencesCButton),
        FALSE);
    }
    else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPreferencesCButton),
        TRUE);
    }
  }
  else
  {
    if (iDrop == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPreferencesCButton),
        FALSE);
    }
    else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPreferencesCButton),
        TRUE);
    }
  }
  gtk_table_attach (GTK_TABLE (gPreferencesTable5), gPreferencesCButton, 0, 3,
    6, 7, GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gPreferencesCButton), "clicked",
    GTK_SIGNAL_FUNC (ToggleDrop), NULL);
  gtk_widget_show (gPreferencesCButton);

  if (iData == 1)
  {
    iDropTemp = iDefaultDrop;
    iFinalAverageTemp = iDefaultFinalAvg;
  }
  else
  {
    iDropTemp = iDrop;
    iFinalAverageTemp = iFinalAvg;
  }

  gPreferencesSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesSep2, 0, 2, 4,
    5, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesSep2);

  gPreferencesButton = gtk_button_new_with_label ("Apply");
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesButton), 250, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesButton), "clicked",
    GTK_SIGNAL_FUNC (YesPreferences), (gpointer) iData);
  GTK_WIDGET_SET_FLAGS (gPreferencesButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gPreferencesWindow), gPreferencesButton);
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesButton, 0, 1, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesButton);

  gPreferencesButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gPreferencesButton2), 250, 0);
  gtk_signal_connect (GTK_OBJECT (gPreferencesButton2), "clicked",
    GTK_SIGNAL_FUNC (NoPreferences), NULL);
  GTK_WIDGET_SET_FLAGS (gPreferencesButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesButton2, 1, 2,
    5, 6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesButton2);

  gPreferencesStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gPreferencesTable), gPreferencesStatBar, 0, 2,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPreferencesStatBar);
  if (iData == 1)
  {
    gtk_statusbar_push (GTK_STATUSBAR (gPreferencesStatBar), 1,
      " Default Preferences");
  }
  else
  {
    gtk_statusbar_push (GTK_STATUSBAR (gPreferencesStatBar), 1,
      " Preferences");
  }

  if (iData == 0)
  {
    switch (iGrades)
    {
      case 1: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton), TRUE); Grades1 (); break;
      case 2: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton2), TRUE); Grades2 (); break;
    }
  }
  else
  {
    switch (iDefaultGrades)
    {
      case 1: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton), TRUE); Grades1 (); break;
      case 2: gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
        (gPreferencesRButton2), TRUE); Grades2 (); break;
    }
  }
  gtk_widget_show (gPreferencesWindow);
}

void NoPreferences (void)
{
  if (gPreferencesWindow != NULL)
  {
    gtk_widget_destroy (gPreferencesWindow);
    gPreferencesWindow = NULL;
  }
}

void YesPreferences (GtkWidget *ypW, gpointer ypData)
{
  int iTemp;
  int iTemp2;
  int iTemp3;
  int iWrongGrades;
  int iData;

  iData = (int)ypData;

  if (iData == 0)
  {
    snprintf (sTeacher, MAX_TEACHER, "%s",
      gtk_entry_get_text (GTK_ENTRY (gPreferencesEntry)));
    snprintf (sMentor, MAX_MENTOR, "%s",
      gtk_entry_get_text (GTK_ENTRY (gPreferencesEntry2)));
    iGrades = iGradesTemp;
    iLetters = iLettersTemp;
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      snprintf (sGrades[iTemp], MAX_GRADE_L, "%s", sGradesTemp[iTemp]);
    }
    if (iGrades == 1)
    {
      dLowestPos = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton4));
      dHighestPos = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton5));
      dHighlightBelow = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton6));
      dPlus = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton));
      dMinus = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton2));
    }
    else
    {
      dLowestPos = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry3)));
      dHighestPos = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry4)));
      dHighlightBelow = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry5)));
      dPlus = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry8)));
      dMinus = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry9)));
      iFinalAvg = iFinalAverageTemp;
    }
    iDrop = iDropTemp;
  }
  else
  {
    iDefaultGrades = iGradesTemp;
    iDefaultLetters = iLettersTemp;
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      snprintf (sDefaultGrades[iTemp], MAX_GRADE_L, "%s",
        sGradesTemp[iTemp]);
    }
    if (iDefaultGrades == 1)
    {
      dDefaultLowestPos = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton4));
      dDefaultHighestPos = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton5));
      dDefaultHighlightBelow = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton6));
      dDefaultPlus = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton));
      dDefaultMinus = gtk_spin_button_get_value_as_float
        (GTK_SPIN_BUTTON (gPreferencesSpinButton2));
    }
    else
    {
      dDefaultLowestPos = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry3)));
      dDefaultHighestPos = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry4)));
      dDefaultHighlightBelow = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry5)));
      dDefaultPlus = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry8)));
      dDefaultMinus = atof (gtk_entry_get_text
        (GTK_ENTRY (gPreferencesEntry9)));
      iDefaultFinalAvg = iFinalAverageTemp;
    }
    iDefaultDrop = iDropTemp;
    SaveDefaults ();
  }

  if (iData == 0)
  {
    iWrongGrades = 0;
    for (iTemp = 1; iTemp <= iPeriods; iTemp++)
    {
      for (iTemp2 = 1; iTemp2 <= iWorks[iTemp]; iTemp2++)
      {
        for (iTemp3 = 1; iTemp3 <= iStudents; iTemp3++)
        {
          if ((IsCorrectGrade (iGrades, sWorkGrades[iTemp][iTemp2][iTemp3]))
            == 0)
          {
            snprintf (sWorkGrades[iTemp][iTemp2][iTemp3], MAX_GRADE_L, "%s",
              "");
            iWrongGrades = 1;
          }
        }
      }
    }
    if (iWrongGrades == 1)
    {
      Message ("Removed some incorrect grades!");
    }
    Averages ();
    UpdateList ();
  }

  if (gPreferencesWindow != NULL)
  {
    gtk_widget_destroy (gPreferencesWindow);
    gPreferencesWindow = NULL;
  }
}

void SelectedRow (GtkWidget *gList, int iRow, int iColumn, GdkEventButton
  *gEvent, gpointer gData)
{
  iSelectedRow = iRow;
}

void RemoveLetter (void)
{
  int iTemp;
  int iTemp2;

  if (iLettersTemp == 0)
  {
    Message ("There are no letters to remove!");
  }
  else
  {
    iTemp2 = 0;
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      if (strcmp (sGradesTemp[iTemp], "-200") != 0)
      {
        if (iTemp2 == iSelectedRow)
        {
          snprintf (sGradesTemp[iTemp], MAX_GRADE_L, "%s", "-200");
        }
        iTemp2++;
      }
    }
    gtk_clist_remove (GTK_CLIST (gPreferencesList), iSelectedRow);
    iLettersTemp--;
    gtk_widget_destroy (gPreferencesOptionMenu1);
    InsertLetters ();
  }
}

void AddLetter (void)
{
  if (iLettersTemp == 26)
  {
    Message ("No more letters to add!");
  }
  else
  {
    snprintf (sGradesTemp[iNewLetter], MAX_GRADE_L, "%.2f",
      gtk_spin_button_get_value_as_float
      (GTK_SPIN_BUTTON (gPreferencesSpinButton3)));
    iLettersTemp++;
    InsertLettersList ();
    InsertLetters ();
  }
}

void ChangeLetter (GtkWidget *clW, gpointer pData)
{
  int iData;
  iData = (int)pData;

  iNewLetter = iData;
}

void InsertLetters (void)
{
  GtkWidget *gPreferencesMenuItem[ALPH];

  char sLabelTemp[MAX_LABEL];
  int iTemp;
  int iTemp2;

  gPreferencesOptionMenu1 = gtk_menu_new ();
  if (iGradesTemp == 1)
  {
    iTemp2 = 0;
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      if (strcmp (sGradesTemp[iTemp], "-200") == 0)
      {
        if (iTemp2 == 0)
        {
          iNewLetter = iTemp;
          iTemp2 = 1;
        }
        snprintf (sLabelTemp, MAX_LABEL, "%c", iTemp + 65);
        gPreferencesMenuItem[iTemp] = gtk_menu_item_new_with_label
          (sLabelTemp);
        gtk_signal_connect (GTK_OBJECT (gPreferencesMenuItem[iTemp]),
          "activate", GTK_SIGNAL_FUNC (ChangeLetter), (gpointer) iTemp);
        gtk_widget_show (gPreferencesMenuItem[iTemp]);
        gtk_menu_append (GTK_MENU (gPreferencesOptionMenu1),
          gPreferencesMenuItem[iTemp]);
      }
    }
  }
  gtk_option_menu_set_menu (GTK_OPTION_MENU (gPreferencesOptionMenu),
    gPreferencesOptionMenu1);
  iSelectedRow = 0;
}

void SaveDefaults (void)
{
  int iTemp;
  int iDefaultFd;

  if ((iDefaultFd = open (sGgConfig, O_WRONLY | O_CREAT | O_TRUNC, 0600))
    == -1)
  {
    Message ("Could not save defaults!");
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "classpath: %s\n", sClassPath);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "default grades: %i\n", iDefaultGrades);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    if (iDefaultGrades == 1)
    {
      snprintf (sWriteText, MAX_STRING, "default letters: %i\n",
        iDefaultLetters);
      write (iDefaultFd, sWriteText, strlen (sWriteText));
      for (iTemp = 0; iTemp < ALPH; iTemp++)
      {
        if (strcmp (sDefaultGrades[iTemp], "-200") != 0)
        {
          snprintf (sWriteText, MAX_STRING, "%c: %s\n", iTemp + 65,
            sDefaultGrades[iTemp]);
          write (iDefaultFd, sWriteText, strlen (sWriteText));
        }
      }
    }
    snprintf (sWriteText, MAX_STRING, "lowest pos: %.2f\n",
      dDefaultLowestPos);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "highest pos: %.2f\n",
      dDefaultHighestPos);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "highlight below: %.2f\n",
      dDefaultHighlightBelow);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "plus: %.2f\n",
      dDefaultPlus);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "minus: %.2f\n",
      dDefaultMinus);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    if (iDefaultGrades == 2)
    {
      snprintf (sWriteText, MAX_STRING, "final averages: %i\n",
        iDefaultFinalAvg);
      write (iDefaultFd, sWriteText, strlen (sWriteText));
    }
    snprintf (sWriteText, MAX_STRING, "drop lowest: %i\n", iDefaultDrop);
    write (iDefaultFd, sWriteText, strlen (sWriteText));
    close (iDefaultFd);
  }
}

void InsertLettersList (void)
{
  GdkPixmap *gPreferencesPixmap;
  GdkBitmap *gPreferencesMask;
  GtkStyle  *gPreferencesStyle;

  int iTempLetters;
  int iTemp;
  gchar *sInsert[2];

  gtk_clist_freeze (GTK_CLIST (gPreferencesList));
  gtk_clist_clear (GTK_CLIST (gPreferencesList));
  if (iGradesTemp == 1)
  {
    iTempLetters = 0;
    for (iTemp = 0; iTemp < ALPH; iTemp++)
    {
      if (strcmp (sGradesTemp[iTemp], "-200") != 0)
      {
        sInsert[0] = "";
        sInsert[1] = "";
        gtk_clist_insert (GTK_CLIST (gPreferencesList), iTempLetters, sInsert);
        if (iColor == 1)
        {
          gtk_clist_set_background (GTK_CLIST (gPreferencesList), iTempLetters,
            &color);
          gtk_clist_set_foreground (GTK_CLIST (gPreferencesList), iTempLetters,
            &color2);
        }
        snprintf (sWriteText, MAX_STRING, "%c", iTemp + 65);
        gtk_clist_set_text (GTK_CLIST (gPreferencesList), iTempLetters, 0,
          sWriteText);
        gPreferencesStyle = gtk_widget_get_style (gPreferencesWindow);
        gPreferencesPixmap = gdk_pixmap_create_from_xpm
          (gPreferencesWindow->window, &gPreferencesMask,
          &gPreferencesStyle->bg[GTK_STATE_NORMAL], PKGDATADIR"pix/arrow.xpm");
        if (gPreferencesPixmap != NULL)
        {
          gtk_clist_set_pixtext (GTK_CLIST (gPreferencesList), iTempLetters, 1,
            sGradesTemp[iTemp], 5, gPreferencesPixmap, gPreferencesMask);
        }
        else
        {
          gtk_clist_set_text (GTK_CLIST (gPreferencesList), iTempLetters, 1,
            sGradesTemp[iTemp]);
        }
        gtk_clist_set_selectable (GTK_CLIST (gPreferencesList), iTempLetters,
          TRUE);
        iTempLetters++;
      }
    }
  }
  gtk_clist_thaw (GTK_CLIST (gPreferencesList));
}
