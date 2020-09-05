/* Ggradebook 0.91 (helpabout.c)
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
#include "helpabout.h"

void QuitHelp (void)
{
  if (gHelpWindow != NULL)
  {
    gtk_widget_destroy (gHelpWindow);
    gHelpWindow = NULL;
  }
}

void Help (void)
{
  GtkWidget *gHelpBox;
  GtkWidget *gHelpTable;
  GtkWidget *gHelpLabel;
  GtkWidget *gHelpSep;
  GtkWidget *gHelpSep2;
  GtkWidget *gHelpButton;
  GtkWidget *gHelpText;
  GtkWidget *gHelpScroll;

  int iFd;
  int iChars;
  int iSpecial;
  char sBuffer[1];

  if (gHelpWindow != NULL)
  {
    gtk_widget_destroy (gHelpWindow);
    gHelpWindow = NULL;
  }

#ifdef USE_GNOME
  gHelpWindow = gnome_app_new ("Help", "Help");
#else
  gHelpWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gHelpWindow), 750, 500);
  gtk_window_position (GTK_WINDOW (gHelpWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gHelpWindow), "Help");
  gtk_signal_connect (GTK_OBJECT (gHelpWindow), "delete_event",
    GTK_SIGNAL_FUNC (QuitHelp), NULL);
  gtk_window_set_policy (GTK_WINDOW (gHelpWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gHelpWindow), 0);
  gtk_widget_realize (gHelpWindow);

  gHelpBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gHelpBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gHelpWindow), gHelpBox);
#else
  gtk_container_add (GTK_CONTAINER (gHelpWindow), gHelpBox);
#endif
  gtk_widget_show (gHelpBox);

  gHelpTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gHelpTable);
  gtk_box_pack_start (GTK_BOX (gHelpBox), gHelpTable, TRUE, TRUE, 0);

  gHelpLabel = gtk_label_new ("If you want to send feedback, select\nFeedback "
    "from the Help-menu.");
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpLabel, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gHelpLabel);

  gHelpSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpSep, 0, 2, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gHelpSep);

  gHelpText = gtk_text_new (NULL, NULL);
  gtk_text_set_editable (GTK_TEXT (gHelpText), FALSE);
  gtk_text_set_word_wrap (GTK_TEXT (gHelpText), TRUE);
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpText, 0, 1,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gHelpText);

  gHelpScroll = gtk_vscrollbar_new (GTK_TEXT (gHelpText)->vadj);
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpScroll, 1, 2,
    2, 3, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gHelpScroll);

  gHelpSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpSep2, 0, 2, 3, 4,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gHelpSep2);

  gHelpButton = gtk_button_new_with_label ("Close");
  gtk_signal_connect (GTK_OBJECT (gHelpButton), "clicked",
    GTK_SIGNAL_FUNC (QuitHelp), NULL);
  GTK_WIDGET_SET_FLAGS (gHelpButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gHelpWindow), gHelpButton);
  gtk_table_attach (GTK_TABLE (gHelpTable), gHelpButton, 0, 2, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gHelpButton);

  gtk_text_freeze (GTK_TEXT (gHelpText));
  iFd = open (PKGDATADIR"help/README", O_RDONLY, 0600);
  if (iFd == -1)
  {
    gtk_text_insert (GTK_TEXT (gHelpText), NULL, NULL, NULL, "Could not open "
      "the README file! Sorry.\nDid you `make install'?", -1);
  }
  else
  {
    iSpecial = 0;
    while (1)
    {
      iChars = read (iFd, sBuffer, 1);
      if (sBuffer[0] == '[')
      {
        iSpecial = 1;
      }
      else if (sBuffer[0] == ']')
      {
        iSpecial = 0;
      }
      else if ((iSpecial == 0) || (iColor == 0))
      {
        gtk_text_insert (GTK_TEXT (gHelpText), NULL, NULL, NULL, sBuffer,
          iChars);
      }
      else
      {
        if (gHelpFont == NULL)
        {
          gtk_text_insert (GTK_TEXT (gHelpText), NULL, &color2, NULL, sBuffer,
            iChars);
        }
        else
        {
          gtk_text_insert (GTK_TEXT (gHelpText), gHelpFont, &color2, NULL,
            sBuffer, iChars);
        }
      }
      if (iChars < 1)
      {
        break;
      }
    }
    close (iFd);
  }
  gtk_text_thaw (GTK_TEXT (gHelpText));

  gtk_widget_show (gHelpWindow);
}

#ifndef USE_GNOME
void QuitAbout (void)
{
  if (gAboutWindow != NULL)
  {
    gtk_widget_destroy (gAboutWindow);
    gAboutWindow = NULL;
  }
}
#endif

void About (void)
{
#ifdef USE_GNOME
  const gchar *author[] = {
    "Norbert de Jonge <hack@altavista.net>", 0
  };

  gAboutWindow = gnome_about_new ("Ggradebook", VERSION,
    "Copyright (C) 2000 Free Software Foundation, Inc.", author,
    "Ggradebook is the fully-featured GNU gradebook. It uses GTK+ and can "
    "optionally be compiled to use GNOME.", PKGDATADIR"pix/Ggradebook.xpm");
  gtk_widget_show (gAboutWindow);
#else
  GtkWidget *gAboutLabel;
  GtkWidget *gAboutLabel2;
  GtkWidget *gAboutButton;
  GdkPixmap *gAboutPixmap;
  GdkBitmap *gAboutMask;
  GtkStyle  *gAboutStyle;
  GtkWidget *gAboutPixmapWid;

  if (gAboutWindow != NULL)
  {
    gtk_widget_destroy (gAboutWindow);
    gAboutWindow = NULL;
  }

  gAboutWindow = gtk_dialog_new ();
  gtk_window_position (GTK_WINDOW (gAboutWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gAboutWindow), "About");
  gtk_signal_connect (GTK_OBJECT (gAboutWindow), "delete_event",
    GTK_SIGNAL_FUNC (QuitAbout), NULL);
  gtk_window_set_policy (GTK_WINDOW (gAboutWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (GTK_DIALOG (gAboutWindow)->vbox),
    10);
  gtk_widget_realize (gAboutWindow);

  gAboutStyle = gtk_widget_get_style (gAboutWindow);
  gAboutPixmap = gdk_pixmap_create_from_xpm
    (gAboutWindow->window, &gAboutMask,
    &gAboutStyle->bg[GTK_STATE_NORMAL], PKGDATADIR"pix/Ggradebook.xpm");
  if (gAboutPixmap != NULL)
  {
    gAboutPixmapWid = gtk_pixmap_new (gAboutPixmap, gAboutMask);
    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (gAboutWindow)->vbox),
      gAboutPixmapWid, TRUE, TRUE, 10);
    gtk_widget_show (gAboutPixmapWid);
  }

  gAboutLabel = gtk_label_new ("Ggradebook 0.91\n\n"
    "Copyright (C) 2000 Free Software Foundation, Inc.\n"
    "Author: Norbert de Jonge <hack@altavista.net>");
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (gAboutWindow)->vbox), gAboutLabel,
    TRUE, TRUE, 10);
  gtk_widget_show (gAboutLabel);

  gAboutLabel2 = gtk_label_new ("Ggradebook is the fully-featured GNU "
    "gradebook. It uses\nGTK+ and can optionally be compiled to use GNOME.");
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (gAboutWindow)->vbox), gAboutLabel2,
    TRUE, TRUE, 10);
  gtk_widget_show (gAboutLabel2);

  gAboutButton = gtk_button_new_with_label ("OK");
  gtk_signal_connect (GTK_OBJECT (gAboutButton), "clicked",
    GTK_SIGNAL_FUNC (QuitAbout), NULL);
  GTK_WIDGET_SET_FLAGS (gAboutButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gAboutWindow), gAboutButton);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (gAboutWindow)->action_area),
    gAboutButton, TRUE, TRUE, 10);
  gtk_widget_show (gAboutButton);

  gtk_widget_show (gAboutWindow);
#endif
}
