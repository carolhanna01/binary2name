/* Ggradebook 0.91 (print.c)
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
#include "print.h"
#include "gg.h"
#include "to.h"
#include "message.h"
#include "class.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu7[] = {
  {GNOME_APP_UI_ITEM, N_("_Print"), 0, YesPrint, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PRINT, 'P', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_SEPARATOR,
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoPrint, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu7[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu7),
  GNOMEUIINFO_END
};
#endif

void Print (void)
{
#ifndef USE_GNOME
  GtkWidget *GMMenuBar7;
#endif
  GtkWidget *gPrintBox;
  GtkWidget *gPrintTable;
  GtkWidget *gPrintTable2;
  GtkWidget *gPrintTable3;
  GtkWidget *gPrintTable4;
  GtkWidget *gPrintLabel;
  GtkWidget *gPrintLabel2;
  GtkWidget *gPrintLabel3;
  GtkWidget *gPrintSep;
  GtkWidget *gPrintSep2;
  GtkWidget *gPrintSep3;
  GtkWidget *gPrintFrame;
  GtkWidget *gPrintButton;
  GtkWidget *gPrintButton2;
  GtkWidget *gPrintStatBar;
  GtkWidget *gPrintRButton;
  GtkWidget *gPrintRButton2;
  GtkWidget *gPrintRButton3;
  GtkWidget *gPrintRButton4;
  GtkWidget *gPrintRButton5;
  GtkWidget *gPrintRButton6;
  GtkWidget *gPrintRButton7;
  GtkWidget *gPrintRButton8;
  GSList    *gPrintRGroup;
  GSList    *gPrintRGroup2;
  GSList    *gPrintRGroup3;
  GtkWidget *gPrintBook;
  GtkWidget *gPrintBLabel;
  GdkPixmap *gPrintPixmap;
  GdkPixmap *gPrintPixmap2;
  GdkBitmap *gPrintMask;
  GdkBitmap *gPrintMask2;
  GtkStyle  *gPrintStyle;
  GtkStyle  *gPrintStyle2;
  GtkWidget *gPrintPixmapWid;
  GtkWidget *gPrintPixmapWid2;

  if (gPrintWindow != NULL)
  {
    gtk_widget_destroy (gPrintWindow);
    gPrintWindow = NULL;
  }

#ifdef USE_GNOME
  gPrintWindow = gnome_app_new ("Print", "Print");
#else
  gPrintWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
  gtk_widget_set_usize (GTK_WIDGET (gPrintWindow), 450, 0);
  gtk_window_position (GTK_WINDOW (gPrintWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_title (GTK_WINDOW (gPrintWindow), "Print");
  gtk_signal_connect (GTK_OBJECT (gPrintWindow), "delete_event",
    GTK_SIGNAL_FUNC (NoPrint), NULL);
  gtk_window_set_policy (GTK_WINDOW (gPrintWindow), 0, 0, 1);
  gtk_container_border_width (GTK_CONTAINER (gPrintWindow), 0);
  gtk_widget_realize (gPrintWindow);

  gPrintBox = gtk_vbox_new (FALSE, 0);
  gtk_container_border_width (GTK_CONTAINER (gPrintBox), 0);
#ifdef USE_GNOME
  gnome_app_set_contents (GNOME_APP (gPrintWindow), gPrintBox);
#else
  gtk_container_add (GTK_CONTAINER (gPrintWindow), gPrintBox);
#endif
  gtk_widget_show (gPrintBox);

  gPrintTable = gtk_table_new (2, 7, FALSE);
  gtk_widget_show (gPrintTable);
  gtk_box_pack_start (GTK_BOX (gPrintBox), gPrintTable,
    TRUE, TRUE, 0);

#ifdef USE_GNOME
  gnome_app_create_menus (GNOME_APP (gPrintWindow), menu7);
#else
  GetMenu (gPrintWindow, &GMMenuBar7, 7);
  gtk_table_attach (GTK_TABLE (gPrintTable), GMMenuBar7, 0, 2, 0, 1,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (GMMenuBar7);
#endif

  gPrintLabel = gtk_label_new ("Select a format and a printer or file below:");
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintLabel, 0, 2, 1,
    2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintLabel);

  gPrintSep = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintSep, 0, 2, 2, 3,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintSep);

  gPrintFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintFrame, 0, 2,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintFrame);

  gPrintTable2 = gtk_table_new (1, 1, FALSE);
  gtk_widget_show (gPrintTable2);
  gtk_container_add (GTK_CONTAINER (gPrintFrame), gPrintTable2);

  gPrintBook = gtk_notebook_new ();
  gtk_notebook_set_tab_pos (GTK_NOTEBOOK (gPrintBook), GTK_POS_TOP);
  gtk_table_attach (GTK_TABLE (gPrintTable2), gPrintBook, 0, 1,
    0, 1, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintBook);

  gPrintTable3 = gtk_table_new (3, 5, FALSE);
  gtk_widget_show (gPrintTable3);
  gtk_container_add (GTK_CONTAINER (gPrintBook), gPrintTable3);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (gPrintBook),
    gPrintTable3, "General");

  gPrintBLabel = gtk_label_new ("Specific");
  gtk_widget_show (gPrintBLabel);
  gPrintTable4 = gtk_table_new (2, 4, FALSE);
  gtk_widget_show (gPrintTable4);
  gtk_notebook_append_page (GTK_NOTEBOOK (gPrintBook),
    gPrintTable4, gPrintBLabel);
  gtk_notebook_set_page (GTK_NOTEBOOK (gPrintBook), 0);

  gPrintLabel2 = gtk_label_new ("Format:");
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintLabel2, 0, 1,
    0, 2, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintLabel2);

  gPrintRButton = gtk_radio_button_new_with_label (NULL, "ASCII");
  gtk_widget_set_usize (GTK_WIDGET (gPrintRButton), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton), "clicked",
    GTK_SIGNAL_FUNC (Format2), NULL);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPrintRButton), TRUE);
  iFormat = 1;
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintRButton, 1, 2,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPrintRButton);

  gPrintStyle2 = gtk_widget_get_style (gPrintWindow);
  gPrintPixmap2 = gdk_pixmap_create_from_xpm
    (gPrintWindow->window, &gPrintMask2,
    &gPrintStyle2->bg[GTK_STATE_NORMAL], PKGDATADIR"pix/ascii.xpm");
  if (gPrintPixmap2 != NULL)
  {
    gPrintPixmapWid2 = gtk_pixmap_new (gPrintPixmap2, gPrintMask2);
    gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintPixmapWid2, 2, 3,
      0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPrintPixmapWid2);
  }

  gPrintRGroup = gtk_radio_button_group (GTK_RADIO_BUTTON (gPrintRButton));
  gPrintRButton2 = gtk_radio_button_new_with_label (gPrintRGroup, "Latex");
  gtk_widget_set_usize (GTK_WIDGET (gPrintRButton2), 150, 0);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton2), "clicked",
    GTK_SIGNAL_FUNC (Format1), NULL);
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintRButton2, 1, 2,
    1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 10);
  gtk_widget_show (gPrintRButton2);
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintRButton2), 0);

  gPrintStyle = gtk_widget_get_style (gPrintWindow);
  gPrintPixmap = gdk_pixmap_create_from_xpm
    (gPrintWindow->window, &gPrintMask,
    &gPrintStyle->bg[GTK_STATE_NORMAL], PKGDATADIR"pix/tex.xpm");
  if (gPrintPixmap != NULL)
  {
    gPrintPixmapWid = gtk_pixmap_new (gPrintPixmap, gPrintMask);
    gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintPixmapWid, 2, 3,
      1, 2, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gPrintPixmapWid);
    gtk_widget_set_sensitive (GTK_WIDGET (gPrintPixmapWid), 0);
  }

  gPrintSep3 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintSep3, 0, 3, 2, 3,
    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintSep3);

  gPrintRButton3 = gtk_radio_button_new_with_label (NULL, "Printer:");
  gtk_signal_connect (GTK_OBJECT (gPrintRButton3), "clicked",
    GTK_SIGNAL_FUNC (Print2), NULL);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPrintRButton3), TRUE);
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintRButton3, 0, 1,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintRButton3);

  gPrintEntry = gtk_entry_new_with_max_length (MAX_PRINTER);
  gtk_signal_connect (GTK_OBJECT (gPrintEntry), "activate",
    GTK_SIGNAL_FUNC (YesPrint), NULL);
  gtk_entry_set_text (GTK_ENTRY (gPrintEntry), "lpr");
  gtk_entry_select_region (GTK_ENTRY (gPrintEntry), 0, GTK_ENTRY
    (gPrintEntry)->text_length);
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintEntry, 1, 3,
    3, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_grab_focus (gPrintEntry);
  iToWhat = 1;
  gtk_widget_show (gPrintEntry);

  gPrintRGroup2 = gtk_radio_button_group (GTK_RADIO_BUTTON (gPrintRButton3));
  gPrintRButton4 = gtk_radio_button_new_with_label (gPrintRGroup2, "File:");
  gtk_signal_connect (GTK_OBJECT (gPrintRButton4), "clicked",
    GTK_SIGNAL_FUNC (Print1), NULL);
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintRButton4, 0, 1,
    4, 5, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintRButton4);

  gPrintEntry2 = gtk_entry_new_with_max_length (MAX_FILE);
  gtk_signal_connect (GTK_OBJECT (gPrintEntry2), "activate",
    GTK_SIGNAL_FUNC (YesPrint), NULL);
  snprintf (sWriteText, MAX_STRING, "%s/output.asc", getenv ("HOME"));
  gtk_entry_set_text (GTK_ENTRY (gPrintEntry2), sWriteText);
  gtk_table_attach (GTK_TABLE (gPrintTable3), gPrintEntry2, 1, 3,
    4, 5, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintEntry2), 0);
  gtk_widget_show (gPrintEntry2);

  gPrintLabel3 = gtk_label_new ("Print:");
  gtk_table_attach (GTK_TABLE (gPrintTable4), gPrintLabel3, 0, 1,
    0, 4, GTK_FILL, GTK_FILL, 10, 10);
  gtk_widget_show (gPrintLabel3);

  gPrintRButton5 = gtk_radio_button_new_with_label (NULL, "Everything.");
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gPrintRButton5), TRUE);
  iToPrint = 1;
  gtk_table_attach (GTK_TABLE (gPrintTable4), gPrintRButton5, 1, 2,
    0, 1, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton5), "clicked",
    GTK_SIGNAL_FUNC (ToPrint), (gpointer) 1);
  gtk_widget_show (gPrintRButton5);

  gPrintRGroup3 = gtk_radio_button_group (GTK_RADIO_BUTTON (gPrintRButton5));
  gPrintRButton6 = gtk_radio_button_new_with_label (gPrintRGroup3, "Current "
    "period only.");
  gtk_table_attach (GTK_TABLE (gPrintTable4), gPrintRButton6, 1, 2,
    1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton6), "clicked",
    GTK_SIGNAL_FUNC (ToPrint), (gpointer) 2);
  gtk_widget_show (gPrintRButton6);

  gPrintRGroup3 = gtk_radio_button_group (GTK_RADIO_BUTTON (gPrintRButton6));
  gPrintRButton7 = gtk_radio_button_new_with_label (gPrintRGroup3, "Final "
    "averages only.");
  gtk_table_attach (GTK_TABLE (gPrintTable4), gPrintRButton7, 1, 2,
    2, 3, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton7), "clicked",
    GTK_SIGNAL_FUNC (ToPrint), (gpointer) 3);
  gtk_widget_show (gPrintRButton7);

  gPrintRGroup3 = gtk_radio_button_group (GTK_RADIO_BUTTON (gPrintRButton7));
  gPrintRButton8 = gtk_radio_button_new_with_label (gPrintRGroup3, "The "
    "students only.");
  gtk_table_attach (GTK_TABLE (gPrintTable4), gPrintRButton8, 1, 2,
    3, 4, GTK_EXPAND | GTK_FILL, GTK_FILL, 10, 10);
  gtk_signal_connect (GTK_OBJECT (gPrintRButton8), "clicked",
    GTK_SIGNAL_FUNC (ToPrint), (gpointer) 4);
  gtk_widget_show (gPrintRButton8);

  gPrintSep2 = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintSep2, 0, 2, 4,
    5, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintSep2);

  gPrintButton = gtk_button_new_with_label ("Print");
  gtk_widget_set_usize (GTK_WIDGET (gPrintButton), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gPrintButton), "clicked",
    GTK_SIGNAL_FUNC (YesPrint), NULL);
  GTK_WIDGET_SET_FLAGS (gPrintButton, GTK_CAN_DEFAULT);
  gtk_window_set_default (GTK_WINDOW (gPrintWindow), gPrintButton);
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintButton, 0, 1, 5,
    6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintButton);

  gPrintButton2 = gtk_button_new_with_label ("Cancel");
  gtk_widget_set_usize (GTK_WIDGET (gPrintButton2), 225, 0);
  gtk_signal_connect (GTK_OBJECT (gPrintButton2), "clicked",
    GTK_SIGNAL_FUNC (NoPrint), NULL);
  GTK_WIDGET_SET_FLAGS (gPrintButton2, GTK_CAN_DEFAULT);
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintButton2, 1, 2,
    5, 6, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintButton2);

  gPrintStatBar = gtk_statusbar_new ();
  gtk_table_attach (GTK_TABLE (gPrintTable), gPrintStatBar, 0, 2,
    6, 7, GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (gPrintStatBar);
  gtk_statusbar_push (GTK_STATUSBAR (gPrintStatBar), 1, " Print");

  gtk_widget_show (gPrintWindow);
}

void NoPrint (void)
{
  if (gPrintWindow != NULL)
  {
    gtk_widget_destroy (gPrintWindow);
    gPrintWindow = NULL;
  }
}

void YesPrint (void)
{
  char sEntry1[MAX_PRINTER + 1];
  char sEntry2[MAX_FILE + 1];
  int iWarning;
  int iTemp;
  int iTemp2;
  int iTemp3;

  iWarning = 0;
  snprintf (sEntry1, MAX_PRINTER + 1, "%s",
    gtk_entry_get_text (GTK_ENTRY (gPrintEntry)));
  snprintf (sEntry2, MAX_FILE + 1, "%s",
    gtk_entry_get_text (GTK_ENTRY (gPrintEntry2)));
  if (gPrintWindow != NULL)
  {
    gtk_widget_destroy (gPrintWindow);
    gPrintWindow = NULL;
  }

  if (iToWhat == 1)
  {
    snprintf (sWriteText, MAX_STRING, "%s", sEntry1);
    fPrinter = popen (sWriteText, "w");
    if (fPrinter == NULL)
    {
      Message ("Your printer is on fire!");
      iWarning = 1;
    }
  }
  else
  {
    if ((iPrintFd = open (sEntry2, O_WRONLY | O_CREAT | O_TRUNC, 0600)) == -1)
    {
      Message ("Could not open file!");
      iWarning = 1;
    }
  }
  if (iWarning == 0)
  {
    if (iFormat == 1)
    {
      snprintf (sWriteText, MAX_STRING, "\nGenerated by Ggradebook %s\n",
        VERSION);
      PrintIt (sWriteText);
      switch (iToPrint)
      {
        case 1: PrintIt ("(Everything)\n\n"); break;
        case 2: PrintIt ("(Current period only)\n\n"); break;
        case 3: PrintIt ("(Final averages only)\n\n"); break;
        case 4: PrintIt ("(The students only)\n\n"); break;
      }
      ClassActivity ();
      snprintf (sWriteText, MAX_STRING, "Class:    %s\n", sClass);
      PrintIt (sWriteText);
      snprintf (sWriteText, MAX_STRING, "Activity: %s\n", sActivity);
      PrintIt (sWriteText);
      snprintf (sWriteText, MAX_STRING, "Teacher:  %s\n", sTeacher);
      PrintIt (sWriteText);
      snprintf (sWriteText, MAX_STRING, "Mentor:   %s\n", sMentor);
      PrintIt (sWriteText);
      snprintf (sWriteText, MAX_STRING, "Total students: %i\n", iStudents);
      PrintIt (sWriteText);
      snprintf (sWriteText, MAX_STRING, "Total periods:  %i\n\n", iPeriods);
      PrintIt (sWriteText);
      for (iTemp = 0; iTemp < ALPH; iTemp++)
      {
        if (strcmp (sGrades[iTemp], "-200") != 0)
        {
          snprintf (sWriteText, MAX_STRING, "(%c: %s)\n", iTemp + 65,
            sGrades[iTemp]);
          PrintIt (sWriteText);
        }
      }
      snprintf (sWriteText, MAX_STRING, "(Plus (+): %.2f and minus "
        "(-): %.2f)\n", dPlus, dMinus);
      PrintIt (sWriteText);
      if (iGrades == 2)
      {
        switch (iFinalAvg)
        {
          case 1: snprintf (sWriteText, MAX_STRING, "%s",
            "(Final averages: Wholes)\n"); break;
          case 2: snprintf (sWriteText, MAX_STRING, "%s",
            "(Final averages: Halves)\n"); break;
          case 3: snprintf (sWriteText, MAX_STRING, "%s",
            "(Final averages: Decimals)\n"); break;
        }
        PrintIt (sWriteText);
      }
      if (iDrop == 0)
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "(Drop lowest grades: OFF)\n\n");
      }
      else
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "(Drop lowest grades: ON)\n\n");
      }
      PrintIt (sWriteText);
      if ((iToPrint != 2) && (iToPrint != 3))
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "----------------------------------------"
          "---------------------------------------\n");
        PrintIt (sWriteText);
        snprintf (sWriteText, MAX_STRING, "%s", "Students\n");
        PrintIt (sWriteText);
        for (iTemp = 1; iTemp <= iStudents; iTemp++)
        {
          snprintf (sWriteText, MAX_STRING, "%s",
            "--------------------------------------"
            "-----------------------------------------\n");
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Student: %i/%i\n", iTemp,
            iStudents);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Name:    %s\n",
            sStudentNames[iTemp]);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Code:    %s\n",
            sStudentCodes[iTemp]);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Sex:     %s\n",
            sStudentSex[iTemp]);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Extra:\n%s\n",
            sStudentExtra[iTemp]);
          PrintIt (sWriteText);
          if (iToPrint != 4)
          {
            for (iTemp2 = 1; iTemp2 <= iPeriods; iTemp2++)
            {
              snprintf (sWriteText, MAX_STRING, "Period %i average: %s\n",
                iTemp2, sPeriodAvgData[iTemp2][iTemp]);
              PrintIt (sWriteText);
            }
            snprintf (sWriteText, MAX_STRING, "Final average: %s\n",
              sFinalAvgData[iTemp]);
            PrintIt (sWriteText);
          }
        }
      }
      if (iToPrint == 1)
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "\n--------------------------------------"
          "-----------------------------------------\n");
        PrintIt (sWriteText);
        snprintf (sWriteText, MAX_STRING, "%s", "Periods\n");
        PrintIt (sWriteText);
      }
      for (iTemp = 1; iTemp <= iPeriods; iTemp++)
      {
        if (((iToPrint != 2) && (iToPrint != 3) && (iToPrint != 4)) ||
          ((iToPrint == 2) && (iTemp == iPeriod)))
        {
          snprintf (sWriteText, MAX_STRING, "%s",
            "--------------------------------------"
            "-----------------------------------------\n");
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Period:      %i/%i\n", iTemp,
            iPeriods);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Weight:      %i\n",
            iPeriodWeights[iTemp]);
          PrintIt (sWriteText);
          snprintf (sWriteText, MAX_STRING, "Total works: %i\n\n",
            iWorks[iTemp]);
          PrintIt (sWriteText);
          switch (iPeriodAvg[iTemp])
          {
            case 1: snprintf (sWriteText, MAX_STRING, "%s",
              "(Period averages: Wholes)\n"); break;
            case 2: snprintf (sWriteText, MAX_STRING, "%s",
              "(Period averages: Halves)\n"); break;
            case 3: snprintf (sWriteText, MAX_STRING, "%s",
              "(Period averages: Decimals)\n"); break;
          }
          PrintIt (sWriteText);
          for (iTemp2 = 1; iTemp2 <= iWorks[iTemp]; iTemp2++)
          {
            snprintf (sWriteText, MAX_STRING,
              "\n----------\nWork %i\n----------\n", iTemp2);
            PrintIt (sWriteText);
            snprintf (sWriteText, MAX_STRING, "Work name:   %s\n",
              sWorkNames[iTemp][iTemp2]);
            PrintIt (sWriteText);
            snprintf (sWriteText, MAX_STRING, "Work weight: %i\n",
              iWorkWeights[iTemp][iTemp2]);
            PrintIt (sWriteText);
            snprintf (sWriteText, MAX_STRING, "Work extra:\n%s\n",
              sWorkExtra[iTemp][iTemp2]);
            PrintIt (sWriteText);
            for (iTemp3 = 1; iTemp3 <= iStudents; iTemp3++)
            {
              snprintf (sWriteText, MAX_STRING, "%s: %s\n",
                sStudentNames[iTemp3], sWorkGrades[iTemp][iTemp2][iTemp3]);
              PrintIt (sWriteText);
            }
          }
        }
      }
      if (iToPrint == 2)
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "\n----------\nPeriod averages\n----------\n");
        PrintIt (sWriteText);
        for (iTemp = 1; iTemp <= iStudents; iTemp++)
        {
          snprintf (sWriteText, MAX_STRING, "%s: %s\n",
            sStudentNames[iTemp], sPeriodAvgData[iPeriod][iTemp]);
          PrintIt (sWriteText);
        }
      }
      if (iToPrint == 3)
      {
        snprintf (sWriteText, MAX_STRING, "%s",
          "--------------------------------------"
          "-----------------------------------------\n");
        PrintIt (sWriteText);
        snprintf (sWriteText, MAX_STRING, "%s", "Final averages\n");
        PrintIt (sWriteText);
        snprintf (sWriteText, MAX_STRING, "%s",
          "--------------------------------------"
          "-----------------------------------------\n");
        PrintIt (sWriteText);
        for (iTemp = 1; iTemp <= iStudents; iTemp++)
        {
          snprintf (sWriteText, MAX_STRING, "%s: %s\n",
            sStudentNames[iTemp], sFinalAvgData[iTemp]);
          PrintIt (sWriteText);
        }
      }

      if (iToWhat == 1)
      {
        if ((pclose (fPrinter)) == -1)
        {
          Message ("Printing may have failed!");
        }
      }
      else
      {
        close (iPrintFd);
      }
    }
    else
    {
      if (iToWhat == 1)
      {
        if ((pclose (fPrinter)) == -1)
        {
          Message ("Printing may have failed!");
        }
      }
      else
      {
        close (iPrintFd);
      }
    }
  }
}

void Print1 (void)
{
  iToWhat = 2;
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintEntry), 0);
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintEntry2), 1);
}

void Print2 (void)
{
  iToWhat = 1;
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintEntry), 1);
  gtk_widget_set_sensitive (GTK_WIDGET (gPrintEntry2), 0);
}

void PrintIt (char *sPrint)
{
  if (iToWhat == 1)
  {
    fprintf (fPrinter, sPrint);
  }
  else
  {
    write (iPrintFd, sPrint, strlen (sPrint));
  }
}

void Format1 (void)
{
  iFormat = 2;
  snprintf (sWriteText, MAX_STRING, "%s/output.tex", getenv ("HOME"));
  gtk_entry_set_text (GTK_ENTRY (gPrintEntry2), sWriteText);
}

void Format2 (void)
{
  iFormat = 1;
  snprintf (sWriteText, MAX_STRING, "%s/output.asc", getenv ("HOME"));
  gtk_entry_set_text (GTK_ENTRY (gPrintEntry2), sWriteText);
}
