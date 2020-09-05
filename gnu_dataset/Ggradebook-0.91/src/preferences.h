/* Ggradebook 0.91 (preferences.h)
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

#ifndef PREFERENCES_H
#define PREFERENCES_H

void Preferences (GtkWidget *pW, gpointer pData);
void NoPreferences (void);
void YesPreferences (GtkWidget *ypW, gpointer ypData);
void SelectedRow (GtkWidget *gList, int iRow, int iColumn, GdkEventButton
  *gEvent, gpointer gData);
void RemoveLetter (void);
void AddLetter (void);
void ChangeLetter (GtkWidget *clW, gpointer pData);
void InsertLetters (void);
void SaveDefaults (void);
void InsertLettersList (void);

#endif
