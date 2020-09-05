/* Ggradebook 0.91 (student.h)
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

#ifndef STUDENT_H
#define STUDENT_H

void Students (void);
void AddStudent (GtkWidget *asW, gpointer *asData);
void SexMale (void);
void SexFemale (void);
void EditStudent (void);
void NoEditStudent (void);
void YesEditStudent (void);
void DeleteStudent (void);
void NoDeleteStudent (void);
void YesDeleteStudent (void);
void DontDeleteStudent (void);
void DoDeleteStudent (GtkWidget *ddsW, gpointer *ddsData);
void NoNewStudent (void);
void YesNewStudent (GtkWidget *ynsW, gpointer *ynsData);
void NoStudents (void);
void QuitAdd (void);
void AddFromAnother (void);
void YesAddFromAnother (GtkWidget *yafaW, gpointer *yafaData);

#endif
