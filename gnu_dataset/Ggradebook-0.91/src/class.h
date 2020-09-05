/* Ggradebook 0.91 (class.h)
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

#ifndef CLASS_H
#define CLASS_H

void AskClassPath (void);
void PathQuit (void);
void LoadClasses (void);
void NewClass (void);
void NoNewClass (void);
void AddClass (void);
void SelectClass (GtkWidget *scW, gpointer *scData);
void NoSelectClass (void);
void EditClass (void);
void RenameClass (void);
void NoRenameClass (void);
void YesRenameClass (void);
void DeleteClass (void);
void NoDeleteClass (void);
void YesDeleteClass (void);
void NoEditClass (void);
void SaveEditClass (void);
void UpdateClassLabel (void);
void ClassActivity (void);

#endif
