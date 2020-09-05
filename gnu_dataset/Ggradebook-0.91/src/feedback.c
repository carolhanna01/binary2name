/* Ggradebook 0.91 (feedback.c)
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
#include "feedback.h"
#include "gg.h"
#include "helpabout.h"
#include "to.h"
#include "message.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu10[] = {
  {GNOME_APP_UI_ITEM, N_("_Close"), 0, QuitFeedback, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu10[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu10),
  GNOMEUIINFO_END
};
#endif

void Feedback (void)
{
  GtkWidget *gFeedbackBox;
  GtkWidget *gFeedbackBox2;
  GtkWidget *gFeedbackTable;
  GtkWidget *gFeedbackTable2;
  GtkWidget *gFeedbackTable3;
  GtkWidget *gFeedbackLabel;
  GtkWidget *gFeedbackLabel2;
  GtkWidget *gFeedbackLabel3;
  GtkWidget *gFeedbackLabel4;
  GtkWidget *gFeedbackSep;
  GtkWidget *gFeedbackSep2;
  GtkWidget *gFeedbackSep3;
  GtkWidget *gFeedbackVSep;
  GtkWidget *gFeedbackButton;
  GtkWidget *gFeedbackButton2;
  GtkWidget *gFeedbackButton3;
  GtkWidget *gFeedbackButton4;
  GtkWidget *gFeedbackFrame;
  GtkWidget *gFeedbackScroll;
  GtkWidget *gFeedbackStatBar;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar10;
#endif

  if (gFeedbackWindow != NULL)
  {
    gtk_widget_destroy (gFeedbackWindow);
    gFeedbackWindow = NULL;
  }
  iFeelFree = 0;

#ifdef USE_GNOME
  gFeedbackWindow = gnome_app_new ("Feedback", "Feedback");
#else
  gFeedbackWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gFeedbackWindow), 450, 0);
  gtk_window_position (GTK_WINDOW (gFeedbackWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gFeedbackWindow), "Feedback");
  gtk_signal_connect (GTK_OBJECT (gFeedbackWindow), "delete_event",
    GTK_SIGNAL_FUNC (QuitFeedback), NULL);
  gtk_window_set_policy (GTK_WINDOW (gFeedbackWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gFeedbackWindow), 0);
  gtk_widget_realize (gFeedbackWindow);

  gFeedbackBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gFeedbackBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gFeedbackWindow), gFeedbackBox);
#else
  gtk_container_add (GTK_CONTAINER (gFeedbackWindow), gFeedbackBox);
#endif
  gtk_widget_show (gFeedbackBox);

  gFeedbackTable = gtk_table_new (1, 9, FALSE);
  gtk_widget_show (gFeedbackTable);
  gtk_box_pack_start (GTK_BOX (gFeedbackBox), gFeedbackTable, TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gFeedbackWindow), menu10);
#else
  GetMenu (gFeedbackWindow, &GMMenuBar10, 10);
  gtk_table_attach (GTK_TABLE (gFeedbackTable), GMMenuBar10, 0, 1, 0, 1,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar10);
#endif

  gFeedbackButton4 = gtk_button_new_with_label ("Please read the Help first!");
  gtk_widget_set_usize (GTK_WIDGET (gFeedbackButton4), 300, 0);
  gtk_signal_connect (GTK_OBJECT (gFeedbackButton4), "clicked",
    GTK_SIGNAL_FUNC (Help), NULL);
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackButton4, 0, 1, 1, 2,
    GTK_SHRINK, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackButton4);

  gFeedbackLabel = gtk_label_new ("If Ggradebook crashes or you found a bug, "
    "send an e-mail\nto <hack@altavista.net>. Make sure to describe exactly "
    "what actions\ntriggered the bug and what the precise symptoms of the bug "
    "are.\n\nAnd of course... feel free to send other comments too!");
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackLabel, 0, 1, 2, 3,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackLabel);

  gFeedbackLabel2 = gtk_label_new ("If your computer is connected to the "
    "Internet, you can also use the form\nbelow to send me (Norbert de Jonge) "
    "feedback:");
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackLabel2, 0, 1, 3, 4,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackLabel2);

  gFeedbackSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackSep, 0, 1, 4, 5,
    GTK_FILL | GTK_EXPAND , GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackSep);

  gFeedbackFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackFrame, 0, 1, 5, 6,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackFrame);

  gFeedbackBox2 = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gFeedbackBox2), 0);
  gtk_container_add (GTK_CONTAINER (gFeedbackFrame), gFeedbackBox2);
  gtk_widget_show (gFeedbackBox2);

  gFeedbackTable2 = gtk_table_new (3, 4, FALSE);
  gtk_widget_show (gFeedbackTable2);
  gtk_container_add (GTK_CONTAINER (gFeedbackBox2), gFeedbackTable2);

  gFeedbackTable3 = gtk_table_new (3, 1, FALSE);
  gtk_widget_show (gFeedbackTable3);
  gtk_container_add (GTK_CONTAINER (gFeedbackBox2), gFeedbackTable3);

  gFeedbackCButton = gtk_check_button_new_with_label ("Feel free to publish "
    "my feedback.");
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gFeedbackCButton), FALSE);
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackCButton, 0, 3,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gFeedbackCButton), "clicked",
    GTK_SIGNAL_FUNC (ToggleFeedback), NULL);
  gtk_widget_show (gFeedbackCButton);

  gFeedbackLabel3 = gtk_label_new ("Subject:");
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackLabel3, 0, 1, 1, 2,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackLabel3);

  gFeedbackEntry = gtk_entry_new_with_max_length (MAX_SUBJECT);
  gtk_signal_connect (GTK_OBJECT (gFeedbackEntry), "activate",
    GTK_SIGNAL_FUNC (ToBody), NULL);
  gtk_entry_set_text (GTK_ENTRY (gFeedbackEntry), "Ggradebook 0.91");
  gtk_entry_select_region (GTK_ENTRY (gFeedbackEntry), 0, GTK_ENTRY
    (gFeedbackEntry)->text_length);
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackEntry, 1, 3, 1, 2,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gFeedbackEntry);
  gtk_widget_show (gFeedbackEntry);

  gFeedbackLabel4 = gtk_label_new ("Body:");
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackLabel4, 0, 1, 2, 3,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackLabel4);

  gFeedbackText = gtk_text_new (NULL, NULL);
  gtk_text_set_editable (GTK_TEXT (gFeedbackText), TRUE);
  gtk_text_set_word_wrap (GTK_TEXT (gFeedbackText), TRUE);
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackText, 1, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackText);

  gFeedbackScroll = gtk_vscrollbar_new (GTK_TEXT (gFeedbackText)->vadj);
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackScroll, 2, 3,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackScroll);

  gFeedbackSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gFeedbackTable2), gFeedbackSep2, 0, 3, 3, 4,
    GTK_FILL | GTK_EXPAND , GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackSep2);

  gFeedbackButton = gtk_button_new_with_label ("Send");
  gtk_widget_set_usize (GTK_WIDGET (gFeedbackButton), 130, 0);
  gtk_signal_connect (GTK_OBJECT (gFeedbackButton), "clicked",
    GTK_SIGNAL_FUNC (SendFeedback), NULL);
  gtk_table_attach (GTK_TABLE (gFeedbackTable3), gFeedbackButton, 0, 1, 0, 1,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackButton);

  gFeedbackVSep = gtk_vseparator_new ();
  gtk_table_attach (GTK_TABLE (gFeedbackTable3), gFeedbackVSep, 1, 2, 0, 1,
    GTK_FILL | GTK_EXPAND , GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackVSep);

  gFeedbackButton2 = gtk_button_new_with_label ("Clear");
  gtk_widget_set_usize (GTK_WIDGET (gFeedbackButton2), 130, 0);
  gtk_signal_connect (GTK_OBJECT (gFeedbackButton2), "clicked",
    GTK_SIGNAL_FUNC (ClearFeedback), NULL);
  gtk_table_attach (GTK_TABLE (gFeedbackTable3), gFeedbackButton2, 2, 3, 0, 1,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 10, 10);
  gtk_widget_show (gFeedbackButton2);

  gFeedbackSep3 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackSep3, 0, 1, 6, 7,
    GTK_FILL | GTK_EXPAND , GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackSep3);

  gFeedbackButton3 = gtk_button_new_with_label ("Close");
  gtk_signal_connect (GTK_OBJECT (gFeedbackButton3), "clicked",
    GTK_SIGNAL_FUNC (QuitFeedback), NULL);
  GTK_WIDGET_SET_FLAGS (gFeedbackButton3, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gFeedbackWindow), gFeedbackButton3);
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackButton3, 0, 1, 7, 8,
    GTK_FILL | GTK_EXPAND, GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackButton3);

  gFeedbackStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gFeedbackTable), gFeedbackStatBar, 0, 1,
    8, 9, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gFeedbackStatBar);
  gtk_statusbar_push (GTK_STATUSBAR (gFeedbackStatBar), 1, " Feedback");

  gtk_widget_show (gFeedbackWindow);
}

void QuitFeedback (void)
{
  if (gFeedbackWindow != NULL)
  {
    gtk_widget_destroy (gFeedbackWindow);
    gFeedbackWindow = NULL;
  }
}

void ToggleFeedback (void)
{
  if (iFeelFree == 0)
  {
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gFeedbackCButton), TRUE);
    iFeelFree = 1;
  }
  else
  {
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gFeedbackCButton), FALSE);
    iFeelFree = 0;
  }
}

void ClearFeedback (void)
{
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gFeedbackCButton), FALSE);
  iFeelFree = 0;
  gtk_entry_set_text (GTK_ENTRY (gFeedbackEntry), "Ggradebook 0.91");
  gtk_entry_select_region (GTK_ENTRY (gFeedbackEntry), 0, GTK_ENTRY
    (gFeedbackEntry)->text_length);
  gtk_text_set_point (GTK_TEXT (gFeedbackText), 0);
  gtk_text_forward_delete (GTK_TEXT (gFeedbackText), gtk_text_get_length
    (GTK_TEXT (gFeedbackText)));
}

void SendFeedback (void)
{
  FILE *fMail;

  if ((gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gFeedbackCButton)))
    == TRUE)
  {
    snprintf (sWriteText, MAX_STRING, "mail %s -s \"YES_%s\"", E_MAIL,
      gtk_entry_get_text (GTK_ENTRY (gFeedbackEntry)));
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "mail %s -s \"NO_%s\"", E_MAIL,
      gtk_entry_get_text (GTK_ENTRY (gFeedbackEntry)));
  }
  fMail = popen (sWriteText, "w");
  if (fMail != NULL)
  {
    fprintf (fMail, gtk_editable_get_chars (GTK_EDITABLE (gFeedbackText),
      0, -1));
    fprintf (fMail, "\n.\n\n");
    if ((pclose (fMail)) == -1)
    {
      Message ("Sending may have failed!");
    }
  }
  else
  {
    Message ("Could not send feedback!");
  }
  if (gFeedbackWindow != NULL)
  {
    gtk_widget_destroy (gFeedbackWindow);
    gFeedbackWindow = NULL;
  }
  Message ("Feedback has been send!\nThanks!");
}
