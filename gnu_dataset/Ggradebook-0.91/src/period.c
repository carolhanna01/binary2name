/* Ggradebook 0.91 (period.c)
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
#include "period.h"
#include "message.h"
#include "class.h"
#include "gg.h"

#ifdef USE_GNOME
GnomeUIInfo filemenu11[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoNextPeriod, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu11[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu11),
  GNOMEUIINFO_END
};

GnomeUIInfo filemenu12[] = {
  {GNOME_APP_UI_ITEM, N_("_Cancel"), 0, NoNextPeriod, 0, 0,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CLOSE, 'C', GDK_CONTROL_MASK, 0},
  GNOMEUIINFO_END
};

GnomeUIInfo menu12[] = {
  GNOMEUIINFO_SUBTREE (N_("_File"), filemenu12),
  GNOMEUIINFO_END
};
#endif

void PrevPeriod (void)
{
  if (iPeriod == 1)
  {
    Message ("You already are in the first period!");
  }
  else
  {
    if (gGradesWindow != NULL)
    {
      if (gGradesWindow != NULL)
      {
        gtk_widget_destroy (gGradesWindow);
        gGradesWindow = NULL;
      }
      gGradesWindow = NULL;
    }
    iPeriod--;
    UpdateClassLabel ();
    UpdateList ();
  }
}

void NextPeriod (GtkWidget *npW, gpointer *npData)
{
  GtkWidget *gNextPeriodBox;
  GtkWidget *gNextPeriodTable;
  GtkWidget *gNextPeriodTable2;
  GtkWidget *gNextPeriodLabel;
  GtkWidget *gNextPeriodLabel2;
  GtkWidget *gNextPeriodLabel3;
  GtkWidget *gNextPeriodSep;
  GtkWidget *gNextPeriodSep2;
  GtkWidget *gNextPeriodFrame;
  GtkWidget *gNextPeriodButton;
  GtkWidget *gNextPeriodButton2;
  GtkWidget *gNextPeriodStatBar;
#ifndef USE_GNOME
  GtkWidget *GMMenuBar11;
#endif
  GtkObject *gNextPeriodSpinButtonAdj;
  GtkWidget *gNextPeriodRButton;
  GtkWidget *gNextPeriodRButton2;
  GtkWidget *gNextPeriodRButton3;
  GSList    *gNextPeriodRGroup;
  int iData;

  if (gGradesWindow != NULL)
  {
    gtk_widget_destroy (gGradesWindow);
    gGradesWindow = NULL;
  }
  iData = (int)npData;
  iPeriodAverageTemp = 3;

  if ((iPeriod == iPeriods) || (iData == 1))
  {
    if (gNextPeriodWindow != NULL)
    {
      gtk_widget_destroy (gNextPeriodWindow);
      gNextPeriodWindow = NULL;
    }

#ifdef USE_GNOME
    if (iData == 0)
    {
      gNextPeriodWindow = gnome_app_new ("Next Period", "Next Period");
    }
    else
    {
      gNextPeriodWindow = gnome_app_new ("Edit Current Period",
        "Edit Current Period");
    }
#else
    gNextPeriodWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
    gtk_widget_set_usize (GTK_WIDGET (gNextPeriodWindow), 300, 0);
    gtk_window_position (GTK_WINDOW (gNextPeriodWindow), GTK_WIN_POS_CENTER);
    if (iData == 0)
    {
      gtk_window_set_title (GTK_WINDOW (gNextPeriodWindow), "Next Period");
    }
    else
    {
      gtk_window_set_title (GTK_WINDOW (gNextPeriodWindow), "Edit Current "
        "Period");
    }
    gtk_signal_connect (GTK_OBJECT (gNextPeriodWindow), "delete_event",
      GTK_SIGNAL_FUNC (NoNextPeriod), NULL);
    gtk_window_set_policy (GTK_WINDOW (gNextPeriodWindow), 0, 0, 1);
    gtk_container_border_width (GTK_CONTAINER (gNextPeriodWindow), 0);
    gtk_widget_realize (gNextPeriodWindow);

    gNextPeriodBox = gtk_vbox_new (FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (gNextPeriodBox), 0);
#ifdef USE_GNOME
    gnome_app_set_contents (GNOME_APP (gNextPeriodWindow), gNextPeriodBox);
#else
    gtk_container_add (GTK_CONTAINER (gNextPeriodWindow), gNextPeriodBox);
#endif
    gtk_widget_show (gNextPeriodBox);

    gNextPeriodTable = gtk_table_new (2, 7, FALSE);
    gtk_widget_show (gNextPeriodTable);
    gtk_box_pack_start (GTK_BOX (gNextPeriodBox), gNextPeriodTable,
      TRUE, TRUE, 0);

#ifdef USE_GNOME
    if (iData == 0)
    {
      gnome_app_create_menus (GNOME_APP (gNextPeriodWindow), menu11);
    }
    else
    {
      gnome_app_create_menus (GNOME_APP (gNextPeriodWindow), menu12);
    }
#else
    if (iData == 0)
    {
      GetMenu (gNextPeriodWindow, &GMMenuBar11, 11);
    }
    else
    {
      GetMenu (gNextPeriodWindow, &GMMenuBar11, 12);
    }
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), GMMenuBar11, 0, 2, 0, 1,
      GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (GMMenuBar11);
#endif

    if (iData == 0)
    {
      snprintf (sLabelText, MAX_LABEL, "Enter the data for the new period %i "
        "below:", iPeriod + 1);
    }
    else
    {
      snprintf (sLabelText, MAX_LABEL, "Change the data for period %i below:",
        iPeriod);
    }
    gNextPeriodLabel = gtk_label_new (sLabelText);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodLabel, 0, 2, 1,
      2, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodLabel);

    gNextPeriodSep = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodSep, 0, 2, 2, 3,
      GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gNextPeriodSep);

    gNextPeriodFrame = gtk_aspect_frame_new ("data", 0.5, 0.5, 1, TRUE);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodFrame, 0, 2,
      3, 4, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodFrame);

    gNextPeriodTable2 = gtk_table_new (2, 4, FALSE);
    gtk_widget_show (gNextPeriodTable2);
    gtk_container_add (GTK_CONTAINER (gNextPeriodFrame), gNextPeriodTable2);

    gNextPeriodLabel2 = gtk_label_new ("Weight:");
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodLabel2, 0, 1,
      0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodLabel2);

    if (iData == 0)
    {
      gNextPeriodSpinButtonAdj = gtk_adjustment_new (1, 1, 10, 1, 5, 0);
    }
    else
    {
      gNextPeriodSpinButtonAdj = gtk_adjustment_new (iPeriodWeights[iPeriod],
        1, 10, 1, 5, 0);
    }
    gNextPeriodSpinButton = gtk_spin_button_new (GTK_ADJUSTMENT
      (gNextPeriodSpinButtonAdj), 1, 0);
    gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (gNextPeriodSpinButton),
      TRUE);
    gtk_spin_button_set_update_policy (GTK_SPIN_BUTTON (gNextPeriodSpinButton),
      GTK_UPDATE_IF_VALID);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodSpinButton,
      1, 2, 0, 1, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodSpinButton);

    gNextPeriodLabel3 = gtk_label_new ("Period Averages in:");
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodLabel3, 0, 1,
      1, 4, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodLabel3);

    gNextPeriodRButton = gtk_radio_button_new_with_label (NULL, "Wholes");
    gtk_signal_connect (GTK_OBJECT (gNextPeriodRButton), "clicked",
      GTK_SIGNAL_FUNC (SetAvg), (gpointer) 4);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodRButton, 1, 2,
      1, 2, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodRButton);

    gNextPeriodRGroup = gtk_radio_button_group (GTK_RADIO_BUTTON
      (gNextPeriodRButton));
    gNextPeriodRButton2 = gtk_radio_button_new_with_label (gNextPeriodRGroup,
      "Halves");
    gtk_signal_connect (GTK_OBJECT (gNextPeriodRButton2), "clicked",
      GTK_SIGNAL_FUNC (SetAvg), (gpointer) 5);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodRButton2, 1, 2,
      2, 3, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodRButton2);

    gNextPeriodRGroup = gtk_radio_button_group (GTK_RADIO_BUTTON
      (gNextPeriodRButton2));
    gNextPeriodRButton3 = gtk_radio_button_new_with_label (gNextPeriodRGroup,
      "Decimals");
    gtk_signal_connect (GTK_OBJECT (gNextPeriodRButton3), "clicked",
      GTK_SIGNAL_FUNC (SetAvg), (gpointer) 6);
    if (iData == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gNextPeriodRButton3),
        TRUE);
    }
    else
    {
      switch (iPeriodAvg[iPeriod])
      {
        case 1:
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
            (gNextPeriodRButton), TRUE);
          break;
        case 2:
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
            (gNextPeriodRButton2), TRUE);
          break;
        case 3:
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON
            (gNextPeriodRButton3), TRUE);
      }
    }
    gtk_table_attach (GTK_TABLE (gNextPeriodTable2), gNextPeriodRButton3, 1, 2,
      3, 4, GTK_FILL, GTK_FILL, 10, 10);
    gtk_widget_show (gNextPeriodRButton3);

    gNextPeriodSep2 = gtk_hseparator_new ();
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodSep2, 0, 2, 4,
      5, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gNextPeriodSep2);

    if (iData == 0)
    {
      gNextPeriodButton = gtk_button_new_with_label ("Add");
    }
    else
    {
      gNextPeriodButton = gtk_button_new_with_label ("Change");
    }
    gtk_widget_set_usize (GTK_WIDGET (gNextPeriodButton), 150, 0);
    if (iData == 0)
    {
      gtk_signal_connect (GTK_OBJECT (gNextPeriodButton), "clicked",
        GTK_SIGNAL_FUNC (YesNextPeriod), (gpointer) 0);
    }
    else
    {
      gtk_signal_connect (GTK_OBJECT (gNextPeriodButton), "clicked",
        GTK_SIGNAL_FUNC (YesNextPeriod), (gpointer) 1);
    }
    GTK_WIDGET_SET_FLAGS (gNextPeriodButton, GTK_CAN_DEFAULT);
    gtk_window_set_default (GTK_WINDOW (gNextPeriodWindow), gNextPeriodButton);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodButton, 0, 1, 5,
      6, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gNextPeriodButton);

    gNextPeriodButton2 = gtk_button_new_with_label ("Cancel");
    gtk_widget_set_usize (GTK_WIDGET (gNextPeriodButton2), 150, 0);
    gtk_signal_connect (GTK_OBJECT (gNextPeriodButton2), "clicked",
      GTK_SIGNAL_FUNC (NoNextPeriod), NULL);
    GTK_WIDGET_SET_FLAGS (gNextPeriodButton2, GTK_CAN_DEFAULT);
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodButton2, 1, 2,
      5, 6, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gNextPeriodButton2);

    gNextPeriodStatBar = gtk_statusbar_new ();
    gtk_table_attach (GTK_TABLE (gNextPeriodTable), gNextPeriodStatBar, 0, 2,
      6, 7, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (gNextPeriodStatBar);
    if (iData == 0)
    {
      gtk_statusbar_push (GTK_STATUSBAR (gNextPeriodStatBar), 1,
        " Next Period");
    }
    else
    {
      gtk_statusbar_push (GTK_STATUSBAR (gNextPeriodStatBar), 1,
        " Edit Current Period");
    }

    if (iGrades == 1)
    {
      gtk_widget_set_sensitive (GTK_WIDGET (gNextPeriodRButton), 0);
      gtk_widget_set_sensitive (GTK_WIDGET (gNextPeriodRButton2), 0);
      gtk_widget_set_sensitive (GTK_WIDGET (gNextPeriodRButton3), 0);
    }
    gtk_widget_show (gNextPeriodWindow);
  }
  else
  {
    iPeriod++;
    UpdateClassLabel ();
    UpdateList ();
  }
}

void NoNextPeriod (void)
{
  if (gNextPeriodWindow != NULL)
  {
    gtk_widget_destroy (gNextPeriodWindow);
    gNextPeriodWindow = NULL;
  }
}

void YesNextPeriod (GtkWidget *ynpW, gpointer *ynpData)
{
  int iData;

  iData = (int)ynpData;

  if (iData == 0)
  {
    iPeriods++;
    iPeriod++;
  }
  iPeriodWeights[iPeriod] = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON
    (gNextPeriodSpinButton));
  if (iGrades == 2)
  {
    iPeriodAvg[iPeriod] = iPeriodAverageTemp;
  }
  if (gNextPeriodWindow != NULL)
  {
    gtk_widget_destroy (gNextPeriodWindow);
    gNextPeriodWindow = NULL;
  }
  UpdateClassLabel ();
  UpdateList ();
}
