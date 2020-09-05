/* Ggradebook 0.91 (grades.h)
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

#ifndef GRADES_H
#define GRADES_H

void Grades (void);
void NoGrades (void);
void Grades1 (void);
void Grades2 (void);
void AddGrades (GtkWidget *agW, gpointer *agData);
void NoAddGrades (void);
void YesAddGrades (GtkWidget *yagW, gpointer *yagData);
void EditGrades (void);
void NoEditGrades (void);
void YesEditGrades (void);
void DeleteGrades (void);
void NoDeleteGrades (void);
void YesDeleteGrades (void);
void DontDeleteGrades (void);
void DoDeleteGrades (GtkWidget *ddgW, gpointer *ddgData);
void Lowest (double dGrade, int iWeight);
void Averages (void);
int IsCorrectGrade (int iGradeType, char *sGrade);
void ToggleDrop (void);
double Difference (float fOne, float fTwo);

#endif
