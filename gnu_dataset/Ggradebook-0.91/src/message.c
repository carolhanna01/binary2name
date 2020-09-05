/* Ggradebook 0.91 (message.c)
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
#include "message.h"

void Message (char *sMessage)
{
  GtkWidget *gMessageBox;
  GtkWidget *gMessageTable;
  GtkWidget *gMessageLabel;
  GtkWidget *gMessageSep;
  GtkWidget *gMessageButton;

  if (gMessageWindow != NULL)
  {
    gtk_widget_destroy (gMessageWindow);
    gMessageWindow = NULL;
  }

#ifdef USE_GNOME
  gMessageWindow = gnome_app_new ("Message", "Message");
#else
  gMessageWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_window_position (GTK_WINDOW (gMessageWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gMessageWindow), "Message");
  gtk_signal_connect (GTK_OBJECT (gMessageWindow), "delete_event",
    GTK_SIGNAL_FUNC (QuitMessage), NULL);
  gtk_window_set_policy (GTK_WINDOW (gMessageWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gMessageWindow), 0);
  gtk_widget_realize (gMessageWindow);

  gMessageBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gMessageBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gMessageWindow), gMessageBox);
#else
  gtk_container_add (GTK_CONTAINER (gMessageWindow), gMessageBox);
#endif
  gtk_widget_show (gMessageBox);

  gMessageTable = gtk_table_new (1, 3, FALSE);
  gtk_widget_show (gMessageTable);
  gtk_box_pack_start (GTK_BOX (gMessageBox), gMessageTable, TRUE, TRUE, 0);

  gMessageLabel = gtk_label_new (sMessage);
  gtk_table_attach (GTK_TABLE (gMessageTable), gMessageLabel, 0, 1, 0, 1,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gMessageLabel);

  gMessageSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gMessageTable), gMessageSep, 0, 1, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gMessageSep);

  gMessageButton = gtk_button_new_with_label ("OK");
  gtk_signal_connect (GTK_OBJECT (gMessageButton), "clicked",
    GTK_SIGNAL_FUNC (QuitMessage), NULL);
  GTK_WIDGET_SET_FLAGS (gMessageButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gMessageWindow), gMessageButton);
  gtk_table_attach (GTK_TABLE (gMessageTable), gMessageButton, 0, 1, 2, 3,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gMessageButton);

  gtk_widget_show (gMessageWindow);
}

void QuitMessage (void)
{
  if (gMessageWindow != NULL)
  {
    gtk_widget_destroy (gMessageWindow);
    gMessageWindow = NULL;
  }
}
