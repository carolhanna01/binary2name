/* Ggradebook 0.91 (to.c)
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
#include "to.h"

void ToActivity (void)
{
  gtk_widget_grab_focus (gNewClassEntry2);
}

void ToTeacher (void)
{
  gtk_widget_grab_focus (gNewClassEntry3);
}

void ToMentor (void)
{
  gtk_widget_grab_focus (gNewClassEntry4);
}

void ToMentorP (void)
{
  gtk_widget_grab_focus (gPreferencesEntry2);
}

void ToActivity2 (void)
{
  gtk_widget_grab_focus (gRenameClassEntry2);
}

void ToCode (void)
{
  gtk_widget_grab_focus (gNewStudentEntry2);
}

void ToExtra (void)
{
  gtk_widget_grab_focus (gNewStudentText);
}

void ToPerc (GtkWidget *tpW, gpointer *tpData)
{
  int iData;

  iData = (int)tpData;

  switch (iData)
  {
    case 4: gtk_widget_grab_focus (gPreferencesEntry4); break;
    case 5: gtk_widget_grab_focus (gPreferencesEntry5); break;
  }
}

void ToMinusP (void)
{
  gtk_widget_grab_focus (gPreferencesEntry9);
}

void ToBody (void)
{
  gtk_widget_grab_focus (gFeedbackText);
}

void ToText (void)
{
  gtk_widget_grab_focus (gAddGradesText);
}

void ToNextAddEntry (GtkWidget *tnaeW, gpointer *tnaeData)
{
  int iData;

  iData = (int)tnaeData;

  gtk_widget_grab_focus (gAddGradesEntryS[iData]);
}

void ToPrint (GtkWidget *tpW, gpointer *tpData)
{
  int iData;

  iData = (int)tpData;
  iToPrint = iData;
}

void ToHighest (void)
{
  gtk_widget_grab_focus (gPreferencesEntry4);
}
