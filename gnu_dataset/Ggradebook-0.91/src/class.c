/* Ggradebook 0.91 (class.c)
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
#include "class.h"
#include "gg.h"
#include "to.h"
#include "message.h"
#include "grades.h"
#include "student.h"
#include "print.h"
#include "preferences.h"
#include "feedback.h"
#include "helpabout.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu[] = {
  {GNOME_APP_UI_ITEM, N_("_Add Class..."), 0, NewClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW, 'A', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Quit"), 0, Quit, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_QUIT, 'Q', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo helpmenu[] = {
  {GNOME_APP_UI_ITEM, N_("_Feedback..."), 0, Feedback, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_MAIL_SND, 'F', GDK_CONTROL_MASK,
    0},
  {GNOME_APP_UI_ITEM, N_("_Help..."), 0, Help, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_BOOK_OPEN, 'H', GDK_CONTROL_MASK,
    0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("A_bout..."), 0, About, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 'B', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu),
  GNOMEUIINFO_SUBTREE (N_("_Help"), helpmenu),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu2[] = {
  {GNOME_APP_UI_ITEM, N_("_Add"), 0, AddClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW, 'A', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoNewClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu2[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu2),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu3[] = {
  {GNOME_APP_UI_ITEM, N_("_Save"), 0, SaveEditClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE, 'S', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("S_tudents..."), 0, Students, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP, 'T', GDK_CONTROL_MASK, 0},
  {GNOME_APP_UI_ITEM, N_("_Grades & Periods..."), 0, Grades, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SCORES, 'G', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Print..."), 0, Print, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PRINT, 'P', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoEditClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo helpmenu3[] = {
  {GNOME_APP_UI_ITEM, N_("_Feedback..."), 0, Feedback, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_MAIL_SND, 'F', GDK_CONTROL_MASK,
    0},
  {GNOME_APP_UI_ITEM, N_("_Help..."), 0, Help, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_BOOK_OPEN, 'H', GDK_CONTROL_MASK,
    0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("A_bout..."), 0, About, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 'B', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu3[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu3),
  GNOMEUIINFO_SUBTREE (N_("_Help"), helpmenu3),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu4[] = {
  {GNOME_APP_UI_ITEM, N_("_Rename"), 0, YesRenameClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_REFRESH, 'R', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoRenameClass, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu4[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu4),
  GNOMEUIINFO_END
};
#endif

void AskClassPath (void)
{
  GtkWidget *gPathBox;
  GtkWidget *gPathTable;
  GtkWidget *gPathLabel;
  GtkWidget *gPathSep;
  GtkWidget *gPathSep2;
  GtkWidget *gPathButton;
  char sDefaultPath[MAX_PATH];

  if (gPathWindow != NULL)
  {
    gtk_widget_destroy (gPathWindow);
    gPathWindow = NULL;
  }

#ifdef USE_GNOME
  gPathWindow = gnome_app_new ("Question", "Question");
#else
  gPathWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gPathWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gPathWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gPathWindow), "Question");
  gtk_signal_connect (GTK_OBJECT (gPathWindow), "delete_event",
    GTK_SIGNAL_FUNC (Quit), NULL);
  gtk_window_set_policy (GTK_WINDOW (gPathWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gPathWindow), 0);
  gtk_widget_realize (gPathWindow);

  gPathBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gPathBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gPathWindow), gPathBox);
#else
  gtk_container_add (GTK_CONTAINER (gPathWindow), gPathBox);
#endif
  gtk_widget_show (gPathBox);

  gPathTable = gtk_table_new (1, 5, FALSE);
  gtk_widget_show (gPathTable);
  gtk_box_pack_start (GTK_BOX (gPathBox), gPathTable, TRUE, TRUE, 0);

  gPathLabel = gtk_label_new ("Enter a directory where you wish to store\n"
    "your class files:");
  gtk_table_attach (GTK_TABLE (gPathTable), gPathLabel, 0, 1, 0, 1,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPathLabel);

  gPathSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPathTable), gPathSep, 0, 1, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPathSep);

  gPathEntry = gtk_entry_new_with_max_length (MAX_PATH);
  gtk_signal_connect (GTK_OBJECT (gPathEntry), "activate",
    GTK_SIGNAL_FUNC (PathQuit), NULL);
  snprintf (sDefaultPath, MAX_PATH, "%s/gg/", getenv ("HOME"));
  gtk_entry_set_text (GTK_ENTRY (gPathEntry), sDefaultPath);
  gtk_entry_select_region (GTK_ENTRY (gPathEntry), 0, GTK_ENTRY
    (gPathEntry)->text_length);
  gtk_table_attach (GTK_TABLE (gPathTable), gPathEntry, 0, 1, 2, 3,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gPathEntry);
  gtk_widget_show (gPathEntry);

  gPathSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPathTable), gPathSep2, 0, 1, 3, 4,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPathSep2);

  gPathButton = gtk_button_new_with_label ("OK");
  gtk_signal_connect (GTK_OBJECT (gPathButton), "clicked",
    GTK_SIGNAL_FUNC (PathQuit), NULL);
  GTK_WIDGET_SET_FLAGS (gPathButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gPathWindow), gPathButton);
  gtk_table_attach (GTK_TABLE (gPathTable), gPathButton, 0, 1, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPathButton);

  gtk_widget_show (gPathWindow);
}

void PathQuit (void)
{
  snprintf (sClassPath, MAX_PATH, "%s", gtk_entry_get_text (GTK_ENTRY
    (gPathEntry)));
  if (sClassPath[strlen (sClassPath) - 1] != '/')
  {
    snprintf (sTempC, MAX_STRING, "%s", sClassPath);
    snprintf (sClassPath, MAX_PATH, "%s/", sTempC);
  }

  if (gPathWindow != NULL)
  {
    gtk_widget_destroy (gPathWindow);
    gPathWindow = NULL;
  }

  if ((mkdir (sClassPath, 0755)) == -1)
  {
    g_print ("[FAILED] Could not create directory! Directory must not "
      "exist!\n");
    gtk_exit (EXIT_ERROR);
  }
  else
  {
    if ((iConfigFd = open (sGgConfig, O_WRONLY | O_CREAT | O_TRUNC, 0600)) ==
      -1)
    {
      g_print ("[FAILED] Could not open file!\n");
      gtk_exit (EXIT_ERROR);
    }
    else
    {
      snprintf (sWriteText, MAX_STRING, "classpath: %s\n", sClassPath);
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "%s", "default grades: 1\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      iDefaultGrades = 1;
      snprintf (sWriteText, MAX_STRING, "%s", "default letters: 5\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      iDefaultLetters = 5;
      snprintf (sWriteText, MAX_STRING, "%s", "A: 90.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sDefaultGrades[0], MAX_GRADE_L, "%s", "90.00");
      snprintf (sWriteText, MAX_STRING, "%s", "B: 80.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sDefaultGrades[1], MAX_GRADE_L, "%s", "80.00");
      snprintf (sWriteText, MAX_STRING, "%s", "C: 70.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sDefaultGrades[2], MAX_GRADE_L, "%s", "70.00");
      snprintf (sWriteText, MAX_STRING, "%s", "D: 60.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sDefaultGrades[3], MAX_GRADE_L, "%s", "60.00");
      snprintf (sWriteText, MAX_STRING, "%s", "F: 0.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      snprintf (sDefaultGrades[5], MAX_GRADE_L, "%s", "0.00");
      snprintf (sWriteText, MAX_STRING, "%s", "lowest pos: 0.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      dDefaultLowestPos = 0.00;
      snprintf (sWriteText, MAX_STRING, "%s", "highest pos: 95.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      dDefaultHighestPos = 95.00;
      snprintf (sWriteText, MAX_STRING, "%s", "highlight below: 70.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      dDefaultHighlightBelow = 70.00;
      snprintf (sWriteText, MAX_STRING, "%s", "plus: 5.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      dDefaultPlus = 5.00;
      snprintf (sWriteText, MAX_STRING, "%s", "minus: -5.00\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      dDefaultMinus = -5.00;
      snprintf (sWriteText, MAX_STRING, "%s", "drop lowest: 0\n");
      write (iConfigFd, sWriteText, strlen (sWriteText));
      iDefaultDrop = 0;
      close (iConfigFd);
      LoadClasses ();
    }
  }
}

void LoadClasses (void)
{
  DIR *sClassDir;
  struct dirent *dir;
  GtkWidget *gClassesBox;
  GtkWidget *gClassesBox2;
  GtkWidget *gClassesTable;
  GtkWidget *gClassesTable2;
  GtkWidget *gClassesFrame;
  GtkWidget *gClassesLabel;
  GtkWidget *gClassesLabel2;
  int iClassNames;
  GtkWidget *gClassButton[MAX_CLASSES];
  GtkWidget *gClassesSep;
  GtkWidget *gClassesSep2;
  GtkWidget *gClassesButton;
  GtkWidget *gClassesButton2;
  GtkWidget *gClassesStatBar;
  int iTemp;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar;
#endif
  GtkWidget *gClassesHandleBox;
  GdkCursor *gCursor;

  iClasses = 0;
  if ((sClassDir = opendir (sClassPath)) == NULL)
  {
    g_print ("[FAILED] Could not open directory! You may want to delete "
      "~/.gg\n");
    gtk_exit (EXIT_ERROR);
  }
  else
  {
    while ((dir = readdir (sClassDir)) != NULL)
    {
      if ((strcmp (dir->d_name, ".") != 0) && (strcmp (dir->d_name, "..")
        != 0))
      {
        snprintf (sClasses[iClasses], MAX_FILE, "%s", dir->d_name);
        iClasses++;
      }
    }
    closedir (sClassDir);

    if (gClassesWindow != NULL)
    {
      gtk_widget_destroy (gClassesWindow);
      gClassesWindow = NULL;
    }

#ifdef USE_GNOME
    gClassesWindow = gnome_app_new ("Ggradebook", "Ggradebook");
#else
    gClassesWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gClassesWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gClassesWindow), GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (gClassesWindow), "Ggradebook");
    gtk_signal_connect (GTK_OBJECT (gClassesWindow), "delete_event",
      GTK_SIGNAL_FUNC (Quit), NULL);
    gtk_window_set_policy (GTK_WINDOW (gClassesWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gClassesWindow), 0);
    gtk_widget_realize (gClassesWindow);

    gClassesBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gClassesBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gClassesWindow), gClassesBox);
#else
    gtk_container_add (GTK_CONTAINER (gClassesWindow), gClassesBox);
#endif
    gtk_widget_show (gClassesBox);

    gClassesTable = gtk_table_new (2, 7, FALSE);
    gtk_widget_show (gClassesTable);
    gtk_box_pack_start (GTK_BOX (gClassesBox), gClassesTable, TRUE, TRUE, 0);

#ifdef USE_GNOME
    gnome_app_create_menus (GNOME_APP (gClassesWindow), menu);
    gtk_menu_item_right_justify (GTK_MENU_ITEM (menu[1].widget));
#else
    GetMenu (gClassesWindow, &GMMenuBar, 1);
    gtk_table_attach (GTK_TABLE (gClassesTable), GMMenuBar, 0, 2, 0, 1,
      GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (GMMenuBar);
#endif

    gClassesLabel = gtk_label_new ("Welcome to Ggradebook!\n\nSelect a "
      "class from the list below\nor add a class:");
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesLabel, 0, 2, 1, 2,
      GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gClassesLabel);

    gClassesSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesSep, 0, 2,
      2, 3, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gClassesSep);

    gClassesFrame = gtk_aspect_frame_new ("classes", 0.5, 0.5, 1, TRUE);
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesFrame, 0, 2, 3, 4,
      GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gClassesFrame);

    gClassesBox2 = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gClassesBox2), 10);
    gtk_container_add (GTK_CONTAINER (gClassesFrame), gClassesBox2);
    gtk_widget_show (gClassesBox2);

    gClassesHandleBox = gtk_handle_box_new ();
    gtk_widget_show (gClassesHandleBox);
    gtk_container_add (GTK_CONTAINER (gClassesBox2), gClassesHandleBox);

    gClassesTable2 = gtk_table_new (1, MAX_CLASSES, FALSE);
    gtk_widget_show (gClassesTable2);
    gtk_container_add (GTK_CONTAINER (gClassesHandleBox), gClassesTable2);

    if (iClasses == 0)
    {
      gClassesLabel2 = gtk_label_new ("Add a class first.");
      gtk_table_attach (GTK_TABLE (gClassesTable2), gClassesLabel2, 0, 1, 0, 1,
        GTK_FILL, GTK_FILL, 10, 10);
      gtk_widget_show (gClassesLabel2);
    }
    else for (iClassNames = 0; iClassNames < iClasses; iClassNames++)
    {
      snprintf (sButtonLabel[iClassNames], MAX_LABEL, "%s", "");
      for (iTemp = 0; iTemp < strlen (sClasses[iClassNames]) - 4; iTemp++)
      {
        if (sClasses[iClassNames][iTemp] == '_')
        {
          snprintf (sTempC, MAX_STRING, "%s", sButtonLabel[iClassNames]);
          snprintf (sButtonLabel[iClassNames], MAX_LABEL, "%s (",
            sTempC);
        }
        else
        {
          snprintf (sTempC, MAX_STRING, "%s", sButtonLabel[iClassNames]);
          snprintf (sButtonLabel[iClassNames], MAX_LABEL, "%s%c",
            sTempC, sClasses[iClassNames][iTemp]);
        }
      }
      snprintf (sTempC, MAX_STRING, "%s", sButtonLabel[iClassNames]);
      snprintf (sButtonLabel[iClassNames], MAX_LABEL, "%s)",
        sTempC);
      gClassButton[iClassNames] = gtk_button_new_with_label
        (sButtonLabel[iClassNames]);
      gtk_widget_set_usize (GTK_WIDGET (gClassButton[iClassNames]), 200, 30);
      gtk_table_attach (GTK_TABLE (gClassesTable2), gClassButton[iClassNames],
        0, 1, iClassNames, iClassNames + 1, GTK_SHRINK, GTK_SHRINK, 10, 10);
      gtk_signal_connect (GTK_OBJECT (gClassButton[iClassNames]), "clicked",
        GTK_SIGNAL_FUNC (SelectClass), (gpointer) iClassNames);
      gCursor = gdk_cursor_new (GDK_ARROW);
      gtk_widget_realize (gClassButton[iClassNames]);
      if (gCursor != NULL)
      {
        gdk_window_set_cursor (gClassButton[iClassNames]->window,
          gCursor);
      }
      gtk_widget_show (gClassButton[iClassNames]);
    }

    gClassesSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesSep2, 0, 2,
      4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gClassesSep2);

    gClassesButton = gtk_button_new_with_label ("Add Class");
    gtk_widget_set_usize (GTK_WIDGET (gClassesButton), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gClassesButton), "clicked",
      GTK_SIGNAL_FUNC (NewClass), NULL);
    GTK_WIDGET_SET_FLAGS (gClassesButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gClassesWindow), gClassesButton);
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesButton, 0, 1,
      5, 6, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gClassesButton);

    gClassesButton2 = gtk_button_new_with_label ("Quit");
    gtk_widget_set_usize (GTK_WIDGET (gClassesButton2), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gClassesButton2), "clicked",
      GTK_SIGNAL_FUNC (Quit), NULL);
    GTK_WIDGET_SET_FLAGS (gClassesButton2, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesButton2, 1, 2,
      5, 6, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gClassesButton2);

    gClassesStatBar = gtk_statusbar_new ();
    gtk_table_attach (GTK_TABLE (gClassesTable), gClassesStatBar, 0, 2,
      6, 7, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gClassesStatBar);
    gtk_statusbar_push (GTK_STATUSBAR (gClassesStatBar), 1, " Select Class");

    gtk_widget_show (gClassesWindow);
  }
}

void NewClass (void)
{
  GtkWidget *gNewClassBox;
  GtkWidget *gNewClassTable;
  GtkWidget *gNewClassTable2;
  GtkWidget *gNewClassLabel;
  GtkWidget *gNewClassLabel2;
  GtkWidget *gNewClassLabel3;
  GtkWidget *gNewClassLabel4;
  GtkWidget *gNewClassLabel5;
  GtkWidget *gNewClassSep;
  GtkWidget *gNewClassSep2;
  GtkWidget *gNewClassSep3;
  GtkWidget *gNewClassButton;
  GtkWidget *gNewClassButton2;
  GtkWidget *gNewClassButton3;
  GtkWidget *gNewClassFrame;
  GtkWidget *gNewClassStatBar;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar2;
#endif

  if (gNewClassWindow != NULL)
  {
    gtk_widget_destroy (gNewClassWindow);
    gNewClassWindow = NULL;
  }

#ifdef USE_GNOME
  gNewClassWindow = gnome_app_new ("Add Class", "Add Class");
#else
  gNewClassWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gNewClassWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gNewClassWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gNewClassWindow), "Add Class");
  gtk_signal_connect (GTK_OBJECT (gNewClassWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoNewClass), NULL);
  gtk_window_set_policy (GTK_WINDOW (gNewClassWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gNewClassWindow), 0);
  gtk_widget_realize (gNewClassWindow);

  gNewClassBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gNewClassBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gNewClassWindow), gNewClassBox);
#else
  gtk_container_add (GTK_CONTAINER (gNewClassWindow), gNewClassBox);
#endif
  gtk_widget_show (gNewClassBox);

  gNewClassTable = gtk_table_new (2, 9, FALSE);
  gtk_widget_show (gNewClassTable);
  gtk_box_pack_start (GTK_BOX (gNewClassBox), gNewClassTable, TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gNewClassWindow), menu2);
#else
  GetMenu (gNewClassWindow, &GMMenuBar2, 2);
  gtk_table_attach (GTK_TABLE (gNewClassTable), GMMenuBar2, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar2);
#endif

  gNewClassLabel = gtk_label_new ("Enter data for the new class below:");
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassLabel, 0, 2, 1, 2,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassLabel);

  gNewClassSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassSep, 0, 2,
    2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewClassSep);

  gNewClassFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassFrame, 0, 2, 3, 4,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassFrame);

  gNewClassTable2 = gtk_table_new (2, 4, FALSE);
  gtk_widget_show (gNewClassTable2);
  gtk_container_add (GTK_CONTAINER (gNewClassFrame), gNewClassTable2);

  gNewClassLabel2 = gtk_label_new ("Class:");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassLabel2, 0, 1, 0, 1,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassLabel2);

  gNewClassEntry = gtk_entry_new_with_max_length (MAX_CLASS);
  gtk_signal_connect (GTK_OBJECT (gNewClassEntry), "activate",
    GTK_SIGNAL_FUNC (ToActivity), NULL);
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassEntry, 1, 2, 0, 1,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gNewClassEntry);
  gtk_widget_show (gNewClassEntry);

  gNewClassLabel3 = gtk_label_new ("Activity:");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassLabel3, 0, 1, 1, 2,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassLabel3);

  gNewClassEntry2 = gtk_entry_new_with_max_length (MAX_ACTIVITY);
  gtk_signal_connect (GTK_OBJECT (gNewClassEntry2), "activate",
    GTK_SIGNAL_FUNC (ToTeacher), NULL);
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassEntry2, 1, 2, 1, 2,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassEntry2);

  gNewClassLabel4 = gtk_label_new ("Teacher:");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassLabel4, 0, 1, 2, 3,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassLabel4);

  gNewClassEntry3 = gtk_entry_new_with_max_length (MAX_TEACHER);
  gtk_signal_connect (GTK_OBJECT (gNewClassEntry3), "activate",
    GTK_SIGNAL_FUNC (ToMentor), NULL);
  gtk_entry_set_text (GTK_ENTRY (gNewClassEntry3), "Optional.");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassEntry3, 1, 2, 2, 3,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassEntry3);

  gNewClassLabel5 = gtk_label_new ("Mentor:");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassLabel5, 0, 1, 3, 4,
    GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassLabel5);

  gNewClassEntry4 = gtk_entry_new_with_max_length (MAX_MENTOR);
  gtk_signal_connect (GTK_OBJECT (gNewClassEntry4), "activate",
    GTK_SIGNAL_FUNC (AddClass), NULL);
  gtk_entry_set_text (GTK_ENTRY (gNewClassEntry4), "Optional.");
  gtk_table_attach (GTK_TABLE (gNewClassTable2), gNewClassEntry4, 1, 2, 3, 4,
    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gNewClassEntry4);

  gNewClassSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassSep2, 0, 2, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewClassSep2);

  gNewClassButton = gtk_button_new_with_label ("Add");
  gtk_widget_set_usize (GTK_WIDGET (gNewClassButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gNewClassButton), "clicked",
    GTK_SIGNAL_FUNC (AddClass), NULL);
  GTK_WIDGET_SET_FLAGS (gNewClassButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gNewClassWindow), gNewClassButton);
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassButton, 0, 1, 5, 6,
    GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gNewClassButton);

  gNewClassButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gNewClassButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gNewClassButton2), "clicked",
    GTK_SIGNAL_FUNC (NoNewClass), NULL);
  GTK_WIDGET_SET_FLAGS (gNewClassButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassButton2, 1, 2, 5, 6,
    GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gNewClassButton2);

  gNewClassSep3 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassSep3, 0, 2, 6, 7,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewClassSep3);

  gNewClassButton3 = gtk_button_new_with_label ("Default Preferences");
  gtk_widget_set_usize (GTK_WIDGET (gNewClassButton3), 300, 0);
  gtk_signal_connect (GTK_OBJECT (gNewClassButton3), "clicked",
    GTK_SIGNAL_FUNC (Preferences), (gpointer) 1);
  GTK_WIDGET_SET_FLAGS (gNewClassButton3, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassButton3, 0, 2, 7, 8,
    GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND, 0, 0);
  gtk_widget_show (gNewClassButton3);

  gNewClassStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gNewClassTable), gNewClassStatBar, 0, 2,
    8, 9, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gNewClassStatBar);
  gtk_statusbar_push (GTK_STATUSBAR (gNewClassStatBar), 1, " Add Class");

  gtk_widget_show (gNewClassWindow);
}

void NoNewClass (void)
{
  if (gNewClassWindow != NULL)
  {
    gtk_widget_destroy (gNewClassWindow);
    gNewClassWindow = NULL;
  }
}

void AddClass (void)
{
  int iFd;
  int iTemp;
  char sNewTeacher[MAX_TEACHER];
  char sNewMentor[MAX_MENTOR];
  char sNewFileName[MAX_FILE];
  char sFileUpper[MAX_FILE];

  snprintf (sClass, MAX_CLASS + 1, "%s",
    gtk_entry_get_text (GTK_ENTRY (gNewClassEntry)));
  snprintf (sActivity, MAX_ACTIVITY + 1, "%s",
    gtk_entry_get_text (GTK_ENTRY (gNewClassEntry2)));
  snprintf (sNewTeacher, MAX_TEACHER, "%s", gtk_entry_get_text (GTK_ENTRY
    (gNewClassEntry3)));
  snprintf (sNewMentor, MAX_MENTOR, "%s",
    gtk_entry_get_text (GTK_ENTRY (gNewClassEntry4)));
  if ((strcmp (sClass, "") == 0) || (strcmp (sActivity, "") == 0))
  {
    Message ("You must enter a class and an activity!");
  }
  else
  {
    snprintf (sNewFileName, MAX_FILE, "%s_%s", sClass, sActivity);
    snprintf (sFileUpper, MAX_FILE, "%s", "");
    for (iTemp = 0; iTemp < strlen (sNewFileName); iTemp++)
    {
      snprintf (sTempC, MAX_STRING, "%s", sFileUpper);
      snprintf (sFileUpper, MAX_FILE, "%s%c", sTempC, toupper
        (sNewFileName[iTemp]));
    }
    snprintf (sNewFileName, MAX_FILE, "%s%s.cls", sClassPath, sFileUpper);
    if ((iFd = open (sNewFileName, O_WRONLY | O_CREAT | O_TRUNC, 0600)) == -1)
    {
      Message ("Could not add class!");
    }
    else
    {
      snprintf (sWriteText, MAX_STRING, "grades: %i\n", iDefaultGrades);
      write (iFd, sWriteText, strlen (sWriteText));
      if (iDefaultGrades == 1)
      {
        snprintf (sWriteText, MAX_STRING, "letters: %i\n", iDefaultLetters);
        write (iFd, sWriteText, strlen (sWriteText));
        for (iTemp = 0; iTemp < ALPH; iTemp++)
        {
          if (strcmp (sDefaultGrades[iTemp], "-200") != 0)
          {
            snprintf (sWriteText, MAX_STRING, "%c: %s\n", iTemp + 65,
              sDefaultGrades[iTemp]);
            write (iFd, sWriteText, strlen (sWriteText));
          }
        }
      }
      snprintf (sWriteText, MAX_STRING, "lowest pos: %.2f\n",
        dDefaultLowestPos);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "highest pos: %.2f\n",
        dDefaultHighestPos);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "highlight below: %.2f\n",
        dDefaultHighlightBelow);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "plus: %.2f\n", dDefaultPlus);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "minus: %.2f\n", dDefaultMinus);
      write (iFd, sWriteText, strlen (sWriteText));
      if (iDefaultGrades == 2)
      {
        snprintf (sWriteText, MAX_STRING, "final averages: %i\n",
          iDefaultFinalAvg);
        write (iFd, sWriteText, strlen (sWriteText));
      }
      snprintf (sWriteText, MAX_STRING, "drop lowest: %i\n", iDefaultDrop);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "teacher: %s\n", sNewTeacher);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "mentor: %s\n", sNewMentor);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "%s", "students: 0\n");
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "%s", "periods: 1\n");
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "%s", "period weight: 1\n");
      write (iFd, sWriteText, strlen (sWriteText));
      if (iDefaultGrades == 2)
      {
        snprintf (sWriteText, MAX_STRING, "%s", "period averages: 3\n");
        write (iFd, sWriteText, strlen (sWriteText));
      }
      snprintf (sWriteText, MAX_STRING, "%s", "period works: 0\n");
      write (iFd, sWriteText, strlen (sWriteText));
      close (iFd);
      if (gNewClassWindow != NULL)
      {
        gtk_widget_destroy (gNewClassWindow);
        gNewClassWindow = NULL;
      }
      if (gClassesWindow != NULL)
      {
        gtk_widget_destroy (gClassesWindow);
        gClassesWindow = NULL;
      }
      LoadClasses ();
    }
  }
}

void SelectClass (GtkWidget *scW, gpointer *scData)
{
  GtkWidget *gSelectClassBox;
  GtkWidget *gSelectClassTable;
  GtkWidget *gSelectClassLabel;
  GtkWidget *gSelectClassLabel2;
  GtkWidget *gSelectClassSep;
  GtkWidget *gSelectClassSep2;
  GtkWidget *gSelectClassButton;
  GtkWidget *gSelectClassButton2;
  GtkWidget *gSelectClassButton3;
  GtkWidget *gSelectClassButton4;
  int iFd;
  int iTemp;
  int iTemp2;
  int iTemp3;
  int iWrongGrades;
  char sRead[1];
  char sOption[MAX_OPTION];
  int iRead;
  int iTheGrade;
  char sThePerc[MAX_STRING];

  if (gEditClassWindow != NULL)
  {
    Message ("Already editing a class!");
  }
  else
  {
    iSelected = (int) scData;
    snprintf (sFileName, MAX_FILE, "%s%s", sClassPath, sClasses[iSelected]);
    iWrongGrades = 0;
    if ((iFd = open (sFileName, O_RDONLY, 0600)) == -1)
    {
      Message ("Could not open class!");
    }
    else
    {
      for (iTemp = 0; iTemp < ALPH; iTemp++)
      {
        snprintf (sGrades[iTemp], MAX_GRADE_L, "%s", "-200");
      }
      Read (iFd, 0, "grades: ");
      iGrades = atoi (sReturns);
      if (iGrades == 1)
      {
        Read (iFd, 0, "letters: ");
        iLetters = atoi (sReturns);
        for (iTemp = 0; iTemp < iLetters; iTemp++)
        {
          iRead = read (iFd, sRead, 1);
          iTheGrade = sRead[0] - 65;
          iRead = read (iFd, sRead, 1);
          iRead = read (iFd, sRead, 1);
          snprintf (sThePerc, MAX_STRING, "%s", "");
          do
          {
            iRead = read (iFd, sRead, 1);
            if (sRead[0] != '\n')
            {
              snprintf (sTempC, MAX_STRING, "%s", sThePerc);
              snprintf (sThePerc, MAX_STRING, "%s%c", sTempC, sRead[0]);
            }
          } while (sRead[0] != '\n');
          snprintf (sGrades[iTheGrade], MAX_GRADE_L, "%s", sThePerc);
        }
      }
      Read (iFd, 0, "lowest pos: ");
      dLowestPos = atof (sReturns);
      Read (iFd, 0, "highest pos: ");
      dHighestPos = atof (sReturns);
      Read (iFd, 0, "highlight below: ");
      dHighlightBelow = atof (sReturns);
      Read (iFd, 0, "plus: ");
      dPlus = atof (sReturns);
      Read (iFd, 0, "minus: ");
      dMinus = atof (sReturns);
      if (iGrades == 2)
      {
        Read (iFd, 0, "final averages: ");
        iFinalAvg = atoi (sReturns);
      }
      else
      {
        iFinalAvg = 1;
      }
      Read (iFd, 0, "drop lowest: ");
      iDrop = atoi (sReturns);
      Read (iFd, 0, "teacher: ");
      snprintf (sTeacher, MAX_TEACHER, "%s", sReturns);
      Read (iFd, 0, "mentor: ");
      snprintf (sMentor, MAX_MENTOR, "%s", sReturns);
      Read (iFd, 0, "students: ");
      iStudents = atoi (sReturns);
      Read (iFd, 0, "periods: ");
      iPeriods = atoi (sReturns);
      for (iTemp = 1; iTemp <= iPeriods; iTemp++)
      {
        Read (iFd, 0, "period weight: ");
        iPeriodWeights[iTemp] = atoi (sReturns);
        if (iGrades == 2)
        {
          Read (iFd, 0, "period averages: ");
          iPeriodAvg[iTemp] = atoi (sReturns);
        }
        Read (iFd, 0, "period works: ");
        iWorks[iTemp] = atoi (sReturns);
        for (iTemp2 = 1; iTemp2 <= iWorks[iTemp]; iTemp2++)
        {
          Read (iFd, 0, "work name: ");
          snprintf (sWorkNames[iTemp][iTemp2], MAX_NAME, "%s", sReturns);
          Read (iFd, 0, "work weight: ");
          iWorkWeights[iTemp][iTemp2] = atoi (sReturns);
          Read (iFd, 1, "work extra:\n");
          snprintf (sWorkExtra[iTemp][iTemp2], MAX_TEXT, "%s", sReturns);
          snprintf (sOption, MAX_OPTION, "%s", "");
          do
          {
            read (iFd, sRead, 1);
            snprintf (sTempC, MAX_STRING, "%s", sOption);
            snprintf (sOption, MAX_OPTION, "%s%c", sTempC, sRead[0]);
          } while (strcmp (sOption, "work grades:\n") != 0);
          for (iTemp3 = 1; iTemp3 <= iStudents; iTemp3++)
          {
            Read (iFd, 2, "");
            snprintf (sWorkGrades[iTemp][iTemp2][iTemp3], MAX_GRADE_L, "%s",
              sReturns);
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
      for (iTemp = 1; iTemp <= iStudents; iTemp++)
      {
        Read (iFd, 0, "student name: ");
        snprintf (sStudentNames[iTemp], MAX_NAME, "%s", sReturns);
        Read (iFd, 0, "student code: ");
        snprintf (sStudentCodes[iTemp], MAX_CODE, "%s", sReturns);
        Read (iFd, 0, "student sex: ");
        snprintf (sStudentSex[iTemp], MAX_SEX, "%s", sReturns);
        Read (iFd, 1, "student extra:\n");
        snprintf (sStudentExtra[iTemp], MAX_TEXT, "%s", sReturns);
      }
      close (iFd);
    }

    if (gSelectClassWindow != NULL)
    {
      gtk_widget_destroy (gSelectClassWindow);
      gSelectClassWindow = NULL;
    }

#ifdef USE_GNOME
    gSelectClassWindow = gnome_app_new ("Question", "Question");
#else
    gSelectClassWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gSelectClassWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gSelectClassWindow), GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (gSelectClassWindow), "Question");
    gtk_signal_connect (GTK_OBJECT (gSelectClassWindow), "delete_event",
      GTK_SIGNAL_FUNC (NoSelectClass), NULL);
    gtk_window_set_policy (GTK_WINDOW (gSelectClassWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gSelectClassWindow), 0);
    gtk_widget_realize (gSelectClassWindow);

    gSelectClassBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gSelectClassBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gSelectClassWindow), gSelectClassBox);
#else
    gtk_container_add (GTK_CONTAINER (gSelectClassWindow), gSelectClassBox);
#endif
    gtk_widget_show (gSelectClassBox);

    gSelectClassTable = gtk_table_new (4, 5, FALSE);
    gtk_widget_show (gSelectClassTable);
    gtk_box_pack_start (GTK_BOX (gSelectClassBox), gSelectClassTable,
      TRUE, TRUE, 0);

    gSelectClassLabel = gtk_label_new
      ("Select an action for the class below:");
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassLabel, 0, 4,
      0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gSelectClassLabel);

    gSelectClassSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassSep, 0, 4,
      1, 2, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassSep);

    gSelectClassLabel2 = gtk_label_new (sButtonLabel[iSelected]);
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassLabel2, 0, 4,
      2, 3, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gSelectClassLabel2);

    gSelectClassSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassSep2, 0, 4,
      3, 4, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassSep2);

    gSelectClassButton = gtk_button_new_with_label ("Edit");
    gtk_widget_set_usize (GTK_WIDGET (gSelectClassButton), 75, 0);
    gtk_signal_connect (GTK_OBJECT (gSelectClassButton), "clicked",
      GTK_SIGNAL_FUNC (EditClass), NULL);
    GTK_WIDGET_SET_FLAGS (gSelectClassButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gSelectClassWindow),
      gSelectClassButton);
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassButton, 0, 1,
      4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassButton);

    gSelectClassButton2 = gtk_button_new_with_label ("Rename");
    gtk_widget_set_usize (GTK_WIDGET (gSelectClassButton2), 75, 0);
    gtk_signal_connect (GTK_OBJECT (gSelectClassButton2), "clicked",
      GTK_SIGNAL_FUNC (RenameClass), NULL);
    GTK_WIDGET_SET_FLAGS (gSelectClassButton2, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassButton2, 1, 2,
      4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassButton2);

    gSelectClassButton3 = gtk_button_new_with_label ("Delete");
    gtk_widget_set_usize (GTK_WIDGET (gSelectClassButton3), 75, 0);
    gtk_signal_connect (GTK_OBJECT (gSelectClassButton3), "clicked",
      GTK_SIGNAL_FUNC (DeleteClass), NULL);
    GTK_WIDGET_SET_FLAGS (gSelectClassButton3, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassButton3, 2, 3,
      4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassButton3);

    gSelectClassButton4 = gtk_button_new_with_label ("Cancel");
    gtk_widget_set_usize (GTK_WIDGET (gSelectClassButton4), 75, 0);
    gtk_signal_connect (GTK_OBJECT (gSelectClassButton4), "clicked",
      GTK_SIGNAL_FUNC (NoSelectClass), NULL);
    GTK_WIDGET_SET_FLAGS (gSelectClassButton4, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gSelectClassTable), gSelectClassButton4, 3, 4,
      4, 5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gSelectClassButton4);

    gtk_widget_show (gSelectClassWindow);

    if (iWrongGrades == 1)
    {
      Message ("Ignored some incorrect grades!");
    }
  }
}

void NoSelectClass (void)
{
  if (gSelectClassWindow != NULL)
  {
    gtk_widget_destroy (gSelectClassWindow);
    gSelectClassWindow = NULL;
  }
}

void EditClass (void)
{
  GtkWidget *gEditClassBox;
  GtkWidget *gEditClassTable;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar3;
#endif
  GtkWidget *gEditClassButton;
  GtkWidget *gEditClassButton2;
  GtkWidget *gEditClassButton3;
  GtkWidget *gEditClassButton4;
  GtkWidget *gEditClassButton5;
  GtkWidget *gEditClassButton6;
  GtkWidget *gEditClassSep;
  GtkWidget *gEditClassSep2;
  GtkWidget *gEditClassScrolled;
  GtkWidget *gEditClassStatBar;
  int         iTemp;

  gchar *EditList[BEFORE_GR + MAX_WORKS] = {
    "#",
    "Student",
    "Final Avg",
    "Period Avg",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
    "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
    "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
    "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
  };

  if (gSelectClassWindow != NULL)
  {
    gtk_widget_destroy (gSelectClassWindow);
    gSelectClassWindow = NULL;
  }
  if (gEditClassWindow != NULL)
  {
    gtk_widget_destroy (gEditClassWindow);
    gEditClassWindow = NULL;
  }
  iPeriod = iPeriods;

#ifdef USE_GNOME
  gEditClassWindow = gnome_app_new ("Edit Class", "Edit Class");
#else
  gEditClassWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gEditClassWindow), 750, 0);
  gtk_window_position (GTK_WINDOW (gEditClassWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gEditClassWindow), "Edit Class");
  gtk_signal_connect (GTK_OBJECT (gEditClassWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoEditClass), NULL);
  gtk_window_set_policy (GTK_WINDOW (gEditClassWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gEditClassWindow), 0);
  gtk_widget_realize (gEditClassWindow);

  gEditClassBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gEditClassBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gEditClassWindow), gEditClassBox);
#else
  gtk_container_add (GTK_CONTAINER (gEditClassWindow), gEditClassBox);
#endif
  gtk_widget_show (gEditClassBox);

  gEditClassTable = gtk_table_new (6, 7, FALSE);
  gtk_widget_show (gEditClassTable);
  gtk_box_pack_start (GTK_BOX (gEditClassBox), gEditClassTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gEditClassWindow), menu3);
  gtk_menu_item_right_justify (GTK_MENU_ITEM (menu3[1].widget));
#else
  GetMenu (gEditClassWindow, &GMMenuBar3, 3);
  gtk_table_attach (GTK_TABLE (gEditClassTable), GMMenuBar3, 0, 6, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar3);
#endif

  snprintf (sLabelText, MAX_LABEL, "%s - Period: %i/%i",
    sButtonLabel[iSelected], iPeriod, iPeriods);
  gEditClassLabel = gtk_label_new (sLabelText);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassLabel, 0, 6, 1,
    2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gEditClassLabel);

  gEditClassSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassSep, 0, 6, 2, 3,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassSep);

  gEditClassScrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_set_usize (GTK_WIDGET (gEditClassScrolled), 0, 400);
  gtk_container_set_border_width (GTK_CONTAINER (gEditClassScrolled), 10);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (gEditClassScrolled),
    GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassScrolled, 0, 6,
    3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassScrolled);

  gEditClassList = gtk_clist_new_with_titles (BEFORE_GR + MAX_WORKS, EditList);
  gtk_clist_set_shadow_type (GTK_CLIST (gEditClassList), GTK_SHADOW_IN);
  gtk_clist_set_use_drag_icons (GTK_CLIST (gEditClassList), FALSE);
  gtk_clist_set_column_resizeable (GTK_CLIST (gEditClassList), 0, FALSE);
  for (iTemp = 0; iTemp < BEFORE_GR + MAX_WORKS; iTemp++)
  {
    gtk_clist_set_column_auto_resize (GTK_CLIST (gEditClassList), iTemp, TRUE);
    if (iTemp > (BEFORE_GR - 1))
    {
      gtk_clist_set_column_visibility (GTK_CLIST (gEditClassList), iTemp,
        FALSE);
    }
  }
  gtk_clist_column_titles_passive (GTK_CLIST (gEditClassList));
  gtk_container_add (GTK_CONTAINER (gEditClassScrolled), gEditClassList);
  gtk_widget_show (gEditClassList);

  gEditClassSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassSep2, 0, 6, 4, 5,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassSep2);

  gEditClassButton = gtk_button_new_with_label ("Save");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton), "clicked",
    GTK_SIGNAL_FUNC (SaveEditClass), NULL);
  GTK_WIDGET_SET_FLAGS (gEditClassButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gEditClassWindow), gEditClassButton);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton, 0, 1, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton);

  gEditClassButton2 = gtk_button_new_with_label ("Students");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton2), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton2), "clicked",
    GTK_SIGNAL_FUNC (Students), NULL);
  GTK_WIDGET_SET_FLAGS (gEditClassButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton2, 1, 2, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton2);

  gEditClassButton3 = gtk_button_new_with_label ("Grades & Periods");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton3), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton3), "clicked",
    GTK_SIGNAL_FUNC (Grades), NULL);
  GTK_WIDGET_SET_FLAGS (gEditClassButton3, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton3, 2, 3, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton3);

  gEditClassButton4 = gtk_button_new_with_label ("Print");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton4), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton4), "clicked",
    GTK_SIGNAL_FUNC (Print), NULL);
  GTK_WIDGET_SET_FLAGS (gEditClassButton4, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton4, 3, 4, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton4);

  gEditClassButton5 = gtk_button_new_with_label ("Preferences");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton5), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton5), "clicked",
    GTK_SIGNAL_FUNC (Preferences), (gpointer) 0);
  GTK_WIDGET_SET_FLAGS (gEditClassButton5, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton5, 4, 5, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton5);

  gEditClassButton6 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gEditClassButton6), 125, 0);
  gtk_signal_connect (GTK_OBJECT (gEditClassButton6), "clicked",
    GTK_SIGNAL_FUNC (NoEditClass), NULL);
  GTK_WIDGET_SET_FLAGS (gEditClassButton6, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassButton6, 5, 6, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassButton6);

  gEditClassStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gEditClassTable), gEditClassStatBar, 0, 6,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gEditClassStatBar);
  gtk_statusbar_push (GTK_STATUSBAR (gEditClassStatBar), 1, " Edit Class");

  gtk_widget_show (gEditClassWindow);
  UpdateList ();
}

void RenameClass (void)
{
  GtkWidget *gRenameClassBox;
  GtkWidget *gRenameClassTable;
  GtkWidget *gRenameClassTable2;
  GtkWidget *gRenameClassLabel;
  GtkWidget *gRenameClassLabel2;
  GtkWidget *gRenameClassLabel3;
  GtkWidget *gRenameClassSep;
  GtkWidget *gRenameClassSep2;
  GtkWidget *gRenameClassFrame;
  GtkWidget *gRenameClassButton;
  GtkWidget *gRenameClassButton2;
  GtkWidget *gRenameClassStatBar;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar4;
#endif

  ClassActivity ();
  if (gSelectClassWindow != NULL)
  {
    gtk_widget_destroy (gSelectClassWindow);
    gSelectClassWindow = NULL;
  }
  if (gRenameClassWindow != NULL)
  {
    gtk_widget_destroy (gRenameClassWindow);
    gRenameClassWindow = NULL;
  }

#ifdef USE_GNOME
  gRenameClassWindow = gnome_app_new ("Rename Class", "Rename Class");
#else
  gRenameClassWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gRenameClassWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gRenameClassWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gRenameClassWindow), "Rename Class");
  gtk_signal_connect (GTK_OBJECT (gRenameClassWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoRenameClass), NULL);
  gtk_window_set_policy (GTK_WINDOW (gRenameClassWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gRenameClassWindow), 0);
  gtk_widget_realize (gRenameClassWindow);

  gRenameClassBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gRenameClassBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gRenameClassWindow), gRenameClassBox);
#else
  gtk_container_add (GTK_CONTAINER (gRenameClassWindow), gRenameClassBox);
#endif
  gtk_widget_show (gRenameClassBox);

  gRenameClassTable = gtk_table_new (2, 7, FALSE);
  gtk_widget_show (gRenameClassTable);
  gtk_box_pack_start (GTK_BOX (gRenameClassBox), gRenameClassTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gRenameClassWindow), menu4);
#else
  GetMenu (gRenameClassWindow, &GMMenuBar4, 4);
  gtk_table_attach (GTK_TABLE (gRenameClassTable), GMMenuBar4, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar4);
#endif

  snprintf (sLabelText, MAX_LABEL, "Enter the new classname and activityname\n"
    "for %s below:", sButtonLabel[iSelected]);
  gRenameClassLabel = gtk_label_new (sLabelText);
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassLabel, 0, 2, 1,
    2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gRenameClassLabel);

  gRenameClassSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassSep, 0, 2, 2, 3,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gRenameClassSep);

  gRenameClassFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassFrame, 0, 2,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gRenameClassFrame);

  gRenameClassTable2 = gtk_table_new (2, 2, FALSE);
  gtk_widget_show (gRenameClassTable2);
  gtk_container_add (GTK_CONTAINER (gRenameClassFrame), gRenameClassTable2);

  gRenameClassLabel2 = gtk_label_new ("Class:");
  gtk_table_attach (GTK_TABLE (gRenameClassTable2), gRenameClassLabel2, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gRenameClassLabel2);

  gRenameClassEntry = gtk_entry_new_with_max_length (MAX_CLASS);
  gtk_signal_connect (GTK_OBJECT (gRenameClassEntry), "activate",
    GTK_SIGNAL_FUNC (ToActivity2), NULL);
  gtk_entry_set_text (GTK_ENTRY (gRenameClassEntry), sClass);
  gtk_entry_select_region (GTK_ENTRY (gRenameClassEntry), 0, GTK_ENTRY
    (gRenameClassEntry)->text_length);
  gtk_table_attach (GTK_TABLE (gRenameClassTable2), gRenameClassEntry, 1, 2,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gRenameClassEntry);
  gtk_widget_show (gRenameClassEntry);

  gRenameClassLabel3 = gtk_label_new ("Activity:");
  gtk_table_attach (GTK_TABLE (gRenameClassTable2), gRenameClassLabel3, 0, 1,
    1, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gRenameClassLabel3);

  gRenameClassEntry2 = gtk_entry_new_with_max_length (MAX_ACTIVITY);
  gtk_signal_connect (GTK_OBJECT (gRenameClassEntry2), "activate",
    GTK_SIGNAL_FUNC (YesRenameClass), NULL);
  gtk_entry_set_text (GTK_ENTRY (gRenameClassEntry2), sActivity);
  gtk_table_attach (GTK_TABLE (gRenameClassTable2), gRenameClassEntry2, 1, 2,
    1, 2, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 10, 10);
  gtk_widget_show (gRenameClassEntry2);

  gRenameClassSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassSep2, 0, 2, 4,
    5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gRenameClassSep2);

  gRenameClassButton = gtk_button_new_with_label ("Rename");
  gtk_widget_set_usize (GTK_WIDGET (gRenameClassButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gRenameClassButton), "clicked",
    GTK_SIGNAL_FUNC (YesRenameClass), NULL);
  GTK_WIDGET_SET_FLAGS (gRenameClassButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gRenameClassWindow), gRenameClassButton);
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassButton, 0, 1, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gRenameClassButton);

  gRenameClassButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gRenameClassButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gRenameClassButton2), "clicked",
    GTK_SIGNAL_FUNC (NoRenameClass), NULL);
  GTK_WIDGET_SET_FLAGS (gRenameClassButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassButton2, 1, 2,
    5, 6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gRenameClassButton2);

  gRenameClassStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gRenameClassTable), gRenameClassStatBar, 0, 2,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gRenameClassStatBar);
  gtk_statusbar_push (GTK_STATUSBAR (gRenameClassStatBar), 1, " Rename Class");

  gtk_widget_show (gRenameClassWindow);
}

void NoRenameClass (void)
{
  if (gRenameClassWindow != NULL)
  {
    gtk_widget_destroy (gRenameClassWindow);
    gRenameClassWindow = NULL;
  }
}

void YesRenameClass (void)
{
  char sNewFileName[MAX_FILE];
  char sFileUpper[MAX_FILE];
  int iTemp;

  snprintf (sClass, MAX_CLASS + 1, "%s",
    gtk_entry_get_text (GTK_ENTRY (gRenameClassEntry)));
  snprintf (sActivity, MAX_ACTIVITY + 1, "%s", gtk_entry_get_text (GTK_ENTRY
    (gRenameClassEntry2)));
  if ((strcmp (sClass, "") == 0) || (strcmp (sActivity, "") == 0))
  {
    Message ("You must enter a class and an activity!");
  }
  else
  {
    snprintf (sNewFileName, MAX_FILE, "%s_%s", sClass, sActivity);
    snprintf (sFileUpper, MAX_FILE, "%s", "");
    for (iTemp = 0; iTemp < strlen (sNewFileName); iTemp++)
    {
      snprintf (sTempC, MAX_STRING, "%s", sFileUpper);
      snprintf (sFileUpper, MAX_FILE, "%s%c", sTempC, toupper
        (sNewFileName[iTemp]));
    }
    snprintf (sNewFileName, MAX_FILE, "%s%s.cls", sClassPath, sFileUpper);

    if ((rename (sFileName, sNewFileName)) == -1)
    {
      Message ("Could not rename class!");
    }
    else
    {
      if (gRenameClassWindow != NULL)
      {
        gtk_widget_destroy (gRenameClassWindow);
        gRenameClassWindow = NULL;
      }
      if (gClassesWindow != NULL)
      {
        gtk_widget_destroy (gClassesWindow);
        gClassesWindow = NULL;
      }
      LoadClasses ();
    }
  }
}

void DeleteClass (void)
{
  GtkWidget *gDeleteClassBox;
  GtkWidget *gDeleteClassTable;
  GtkWidget *gDeleteClassLabel;
  GtkWidget *gDeleteClassLabel2;
  GtkWidget *gDeleteClassSep;
  GtkWidget *gDeleteClassSep2;
  GtkWidget *gDeleteClassButton;
  GtkWidget *gDeleteClassButton2;

  if (gSelectClassWindow != NULL)
  {
    gtk_widget_destroy (gSelectClassWindow);
    gSelectClassWindow = NULL;
  }
  if (gDeleteClassWindow != NULL)
  {
    gtk_widget_destroy (gDeleteClassWindow);
    gDeleteClassWindow = NULL;
  }

#ifdef USE_GNOME
  gDeleteClassWindow = gnome_app_new ("Delete Class", "Delete Class");
#else
  gDeleteClassWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gDeleteClassWindow), 300, 0);
  gtk_window_position (GTK_WINDOW (gDeleteClassWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gDeleteClassWindow), "Delete Class");
  gtk_signal_connect (GTK_OBJECT (gDeleteClassWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoDeleteClass), NULL);
  gtk_window_set_policy (GTK_WINDOW (gDeleteClassWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gDeleteClassWindow), 0);
  gtk_widget_realize (gDeleteClassWindow);

  gDeleteClassBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gDeleteClassBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gDeleteClassWindow), gDeleteClassBox);
#else
  gtk_container_add (GTK_CONTAINER (gDeleteClassWindow), gDeleteClassBox);
#endif
  gtk_widget_show (gDeleteClassBox);

  gDeleteClassTable = gtk_table_new (2, 5, FALSE);
  gtk_widget_show (gDeleteClassTable);
  gtk_box_pack_start (GTK_BOX (gDeleteClassBox), gDeleteClassTable,
    TRUE, TRUE, 0);

  gDeleteClassLabel = gtk_label_new ("Are you sure you want to delete the "
    "class below:");
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassLabel, 0, 2, 0,
    1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gDeleteClassLabel);

  gDeleteClassSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassSep, 0, 2, 1, 2,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteClassSep);

  gDeleteClassLabel2 = gtk_label_new (sButtonLabel[iSelected]);
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassLabel2, 0, 2, 2,
    3, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gDeleteClassLabel2);

  gDeleteClassSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassSep2, 0, 2, 3,
    4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteClassSep2);

  gDeleteClassButton = gtk_button_new_with_label ("Delete");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteClassButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteClassButton), "clicked",
    GTK_SIGNAL_FUNC (YesDeleteClass), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteClassButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gDeleteClassWindow), gDeleteClassButton);
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassButton, 0, 1, 4,
    5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteClassButton);

  gDeleteClassButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gDeleteClassButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gDeleteClassButton2), "clicked",
    GTK_SIGNAL_FUNC (NoDeleteClass), NULL);
  GTK_WIDGET_SET_FLAGS (gDeleteClassButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gDeleteClassTable), gDeleteClassButton2, 1, 2,
    4, 5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gDeleteClassButton2);

  gtk_widget_show (gDeleteClassWindow);
}

void NoDeleteClass (void)
{
  if (gDeleteClassWindow != NULL)
  {
    gtk_widget_destroy (gDeleteClassWindow);
    gDeleteClassWindow = NULL;
  }
}

void YesDeleteClass (void)
{
  if ((remove (sFileName)) == -1)
  {
    Message ("Could not delete class!");
  }
  if (gDeleteClassWindow != NULL)
  {
    gtk_widget_destroy (gDeleteClassWindow);
    gDeleteClassWindow = NULL;
  }
  if (gClassesWindow != NULL)
  {
    gtk_widget_destroy (gClassesWindow);
    gClassesWindow = NULL;
  }
  LoadClasses ();
}

void NoEditClass (void)
{
  if (gEditClassWindow != NULL)
  {
    gtk_widget_destroy (gEditClassWindow);
    gEditClassWindow = NULL;
  }
}

void SaveEditClass (void)
{
  int iFd;
  int iTemp;
  int iTemp2;
  int iTemp3;
  int iLength;

  if ((iFd = open (sFileName, O_WRONLY | O_CREAT | O_TRUNC, 0600)) == -1)
  {
    Message ("Could not save class!");
  }
  else
  {
    snprintf (sWriteText, MAX_STRING, "grades: %i\n", iGrades);
    write (iFd, sWriteText, strlen (sWriteText));
    if (iGrades == 1)
    {
      snprintf (sWriteText, MAX_STRING, "letters: %i\n", iLetters);
      write (iFd, sWriteText, strlen (sWriteText));
      for (iTemp = 0; iTemp < ALPH; iTemp++)
      {
        if (strcmp (sGrades[iTemp], "-200") != 0)
        {
          snprintf (sWriteText, MAX_STRING, "%c: %s\n", iTemp + 65,
            sGrades[iTemp]);
          write (iFd, sWriteText, strlen (sWriteText));
        }
      }
    }
    snprintf (sWriteText, MAX_STRING, "lowest pos: %.2f\n",
      dLowestPos);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "highest pos: %.2f\n",
      dHighestPos);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "highlight below: %.2f\n",
      dHighlightBelow);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "plus: %.2f\n", dPlus);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "minus: %.2f\n", dMinus);
    write (iFd, sWriteText, strlen (sWriteText));
    if (iGrades == 2)
    {
      snprintf (sWriteText, MAX_STRING, "final averages: %i\n", iFinalAvg);
      write (iFd, sWriteText, strlen (sWriteText));
    }
    snprintf (sWriteText, MAX_STRING, "drop lowest: %i\n", iDrop);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "teacher: %s\n", sTeacher);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "mentor: %s\n", sMentor);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "students: %i\n", iStudents);
    write (iFd, sWriteText, strlen (sWriteText));
    snprintf (sWriteText, MAX_STRING, "periods: %i\n", iPeriods);
    write (iFd, sWriteText, strlen (sWriteText));
    for (iTemp = 1; iTemp <= iPeriods; iTemp++)
    {
      snprintf (sWriteText, MAX_STRING, "period weight: %i\n",
        iPeriodWeights[iTemp]);
      write (iFd, sWriteText, strlen (sWriteText));
      if (iGrades == 2)
      {
        snprintf (sWriteText, MAX_STRING, "period averages: %i\n",
          iPeriodAvg[iTemp]);
        write (iFd, sWriteText, strlen (sWriteText));
      }
      snprintf (sWriteText, MAX_STRING, "period works: %i\n", iWorks[iTemp]);
      write (iFd, sWriteText, strlen (sWriteText));
      for (iTemp2 = 1; iTemp2 <= iWorks[iTemp]; iTemp2++)
      {
        snprintf (sWriteText, MAX_STRING, "work name: %s\n",
          sWorkNames[iTemp][iTemp2]);
        write (iFd, sWriteText, strlen (sWriteText));
        snprintf (sWriteText, MAX_STRING, "work weight: %i\n",
          iWorkWeights[iTemp][iTemp2]);
        write (iFd, sWriteText, strlen (sWriteText));
        write (iFd, "work extra:\n", strlen ("work extra:\n"));
        iLength = strlen (sWorkExtra[iTemp][iTemp2]);
        write (iFd, sWorkExtra[iTemp][iTemp2], iLength);
        if (iLength != MAX_TEXT)
        {
          for (iTemp3 = iLength; iTemp3 < MAX_TEXT; iTemp3++)
          {
            write (iFd, "#", 1);
          }
        }
        write (iFd, "\n", strlen ("\n"));
        write (iFd, "work grades:\n", strlen ("work grades:\n"));
        for (iTemp3 = 1; iTemp3 <= iStudents; iTemp3++)
        {
          snprintf (sWriteText, MAX_STRING, "%s\n",
            sWorkGrades[iTemp][iTemp2][iTemp3]);
          write (iFd, sWriteText, strlen (sWriteText));
        }
      }
    }
    for (iTemp = 1; iTemp <= iStudents; iTemp++)
    {
      snprintf (sWriteText, MAX_STRING, "student name: %s\n",
        sStudentNames[iTemp]);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "student code: %s\n",
        sStudentCodes[iTemp]);
      write (iFd, sWriteText, strlen (sWriteText));
      snprintf (sWriteText, MAX_STRING, "student sex: %s\n",
        sStudentSex[iTemp]);
      write (iFd, sWriteText, strlen (sWriteText));
      write (iFd, "student extra:\n", strlen ("student extra:\n"));
      iLength = strlen (sStudentExtra[iTemp]);
      write (iFd, sStudentExtra[iTemp], iLength);
      if (iLength != MAX_TEXT)
      {
        for (iTemp2 = iLength; iTemp2 < MAX_TEXT; iTemp2++)
        {
          write (iFd, "#", 1);
        }
      }
      write (iFd, "\n", strlen ("\n"));
    }
    close (iFd);
    if (gEditClassWindow != NULL)
    {
      gtk_widget_destroy (gEditClassWindow);
      gEditClassWindow = NULL;
    }
  }
}

void UpdateClassLabel (void)
{
  snprintf (sLabelText, MAX_LABEL, "%s - Period: %i/%i",
    sButtonLabel[iSelected], iPeriod, iPeriods);
  gtk_label_set_text (GTK_LABEL (gEditClassLabel), sLabelText);
}

void ClassActivity (void)
{
  int iTemp;
  int iTemp2;

  snprintf (sClass, MAX_CLASS + 1, "%s", "");
  snprintf (sActivity, MAX_ACTIVITY + 1, "%s", "");
  iTemp2 = 0;
  for (iTemp = 0; iTemp < strlen (sButtonLabel[iSelected]); iTemp++)
  {
    if (sButtonLabel[iSelected][iTemp] == ' ')
    {
      iTemp2 = 1;
    }
    else
    {
      if (iTemp2 == 0)
      {
        snprintf (sTempC, MAX_STRING, "%s", sClass);
        snprintf (sClass, MAX_CLASS + 1, "%s%c", sTempC,
          sButtonLabel[iSelected][iTemp]);
      }
      else if ((sButtonLabel[iSelected][iTemp] != '(') &&
        (sButtonLabel[iSelected][iTemp] != ')'))
      {
        snprintf (sTempC, MAX_STRING, "%s", sActivity);
        snprintf (sActivity, MAX_ACTIVITY + 1, "%s%c", sTempC,
          sButtonLabel[iSelected][iTemp]);
      }
    }
  }
}
