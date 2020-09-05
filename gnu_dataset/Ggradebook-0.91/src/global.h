/* Ggradebook 0.91 (global.h)
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

#ifndef GLOBAL_H
#define GLOBAL_H

#include <gtk/gtk.h>
#ifdef USE_GNOME
#include <gnome.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <ctype.h>
#include <math.h>

#define VERSION      "0.91"               /* Ggradebook version             */
#define E_MAIL       "hack@altavista.net" /* Feedback e-mail address        */
#define MAX_OPTION   100                  /* Used when reading from files   */
#define EXIT_NORMAL  0                    /* Normal exit                    */
#define EXIT_ERROR   1                    /* Exit with error                */
#define MAX_FILE     250                  /* Max. file length               */
#define MAX_PATH     100                  /* Max. path length               */
#define MAX_CLASSES  100                  /* Max. number of classes         */
#define MAX_CLASS    10                   /* Max. name length of a class    */
#define MAX_ACTIVITY 20                   /* Max. activity name length      */
#define MAX_TEACHER  30                   /* Largest teacher name           */
#define MAX_MENTOR   30                   /* Max. mentor name length        */
#define MAX_LABEL    250                  /* Max. text length on a label    */
#define MAX_STUDENTS 250                  /* Largest number of students     */
#define MAX_WORKS    50                   /* This is per period!            */
#define MAX_PERIODS  30                   /* Largest number of periods      */
#define MAX_NAME     30                   /* Largest student name           */
#define MAX_SEX      10                   /* Max. student sex length        */
#define MAX_CODE     10                   /* Max. student code length       */
#define MAX_STRING   2000                 /* Largest text to write          */
#define MAX_TEXT     1000                 /* Max. text length in widget     */
#define MAX_PRINTER  20                   /* Max. printer entry length      */
#define MAX_PERC     10                   /* Max. perc. entry length        */
#define MAX_SNUMBER  3                    /* Max. student # entry length    */
#define MAX_GRADE_L  8                    /* Max. grade entry length        */
#define BEFORE_GR    4                    /* Columns before grades in clist */
#define MAX_SUBJECT  30                   /* Max. subject entry length      */
#define ALPH         26                   /* Letters in alphabet.           */

extern GtkWidget *gMessageWindow;
extern GtkWidget *gPathEntry;
extern GtkWidget *gPathWindow;
extern GtkWidget *gStudentsWindow;
extern GtkWidget *gEditClassList;
extern GtkWidget *gGradesWindow;
extern GtkWidget *gDeleteStudentWindow;
extern GtkWidget *gDeleteStudentEntry;
extern GtkWidget *gEditStudentWindow;
extern GtkWidget *gEditStudentEntry;
extern GtkWidget *gYesDeleteStudentWindow;
extern GtkWidget *gAddGradesWindow;
extern GtkWidget *gAddGradesEntry;
extern GtkWidget *gAddGradesSpinButton;
extern GtkWidget *gAddGradesText;
extern GtkWidget *gAddGradesEntryS[MAX_STUDENTS + 1];
extern GtkWidget *gPreferencesWindow;
extern GtkWidget *gPreferencesEntry;
extern GtkWidget *gPreferencesEntry2;
extern GtkWidget *gPreferencesEntry3;
extern GtkWidget *gPreferencesEntry4;
extern GtkWidget *gPreferencesEntry5;
extern GtkWidget *gPreferencesEntry8;
extern GtkWidget *gPreferencesEntry9;
extern GtkWidget *gPreferencesRButton5;
extern GtkWidget *gPreferencesRButton6;
extern GtkWidget *gPreferencesRButton7;
extern GtkWidget *gPreferencesButton4;
extern GtkWidget *gPreferencesButton5;
extern GtkWidget *gPreferencesScrolled;
extern GtkWidget *gPreferencesOptionMenu;
extern GtkWidget *gPreferencesSpinButton;
extern GtkWidget *gPreferencesSpinButton2;
extern GtkWidget *gPreferencesSpinButton3;
extern GtkWidget *gPreferencesSpinButton4;
extern GtkWidget *gPreferencesSpinButton5;
extern GtkWidget *gPreferencesSpinButton6;
extern GtkWidget *gPreferencesList;
extern GtkWidget *gPreferencesOptionMenu1;
extern GtkWidget *gPrintWindow;
extern GtkWidget *gPrintEntry;
extern GtkWidget *gPrintEntry2;
extern GtkWidget *gNewClassWindow;
extern GtkWidget *gNewClassEntry;
extern GtkWidget *gNewClassEntry2;
extern GtkWidget *gNewClassEntry3;
extern GtkWidget *gNewClassEntry4;
extern GtkWidget *gClassesWindow;
extern GtkWidget *gSelectClassWindow;
extern GtkWidget *gEditClassWindow;
extern GtkWidget *gEditClassLabel;
extern GtkWidget *gDeleteClassWindow;
extern GtkWidget *gRenameClassWindow;
extern GtkWidget *gRenameClassEntry;
extern GtkWidget *gRenameClassEntry2;
extern GtkWidget *gNewStudentWindow;
extern GtkWidget *gNewStudentEntry;
extern GtkWidget *gNewStudentEntry2;
extern GtkWidget *gNewStudentText;
extern GtkWidget *gFeedbackWindow;
extern GtkWidget *gFeedbackCButton;
extern GtkWidget *gFeedbackEntry;
extern GtkWidget *gFeedbackText;
extern GtkWidget *gNextPeriodWindow;
extern GtkWidget *gNextPeriodSpinButton;
extern GtkWidget *gEditGradesWindow;
extern GtkWidget *gEditGradesEntry;
extern GtkWidget *gDeleteGradesWindow;
extern GtkWidget *gDeleteGradesEntry;
extern GtkWidget *gYesDeleteGradesWindow;
extern GtkWidget *gHelpWindow;
extern GtkWidget *gAboutWindow;
extern GtkWidget *gAddWindow;

extern int iConfigFd;
extern int iSelected;
extern int iStudents;
extern int iPeriod;
extern int iPeriods;
extern int iSex;
extern int iFeelFree;
extern int iGradesTemp;
extern int iFinalAverageTemp;
extern int iPeriodAverageTemp;
extern int iColor;
extern int iDropTemp;
extern double dLowestGrade;
extern int iLowestWeight;
extern int iToPrint;
extern int iFormat;
extern int iToWhat;
extern int iClasses;
extern int iPrintFd;

extern int iDefaultGrades;
extern int iGrades;
extern int iDefaultLetters;
extern int iLetters;
extern double dDefaultLowestPos;
extern double dLowestPos;
extern double dDefaultHighestPos;
extern double dHighestPos;
extern double dDefaultHighlightBelow;
extern double dHighlightBelow;
extern double dDefaultPlus;
extern double dPlus;
extern double dDefaultMinus;
extern double dMinus;
extern int iDefaultFinalAvg;
extern int iFinalAvg;
extern int iDefaultDrop;
extern int iDrop;

extern int iSelectedRow;
extern int iNewLetter;
extern int iLettersTemp;

extern char sGgConfig[MAX_FILE];
extern char sClassPath[MAX_PATH];
extern char sClasses[MAX_CLASSES][MAX_FILE];
extern char sButtonLabel[MAX_CLASSES][MAX_LABEL];
extern char sFileName[MAX_FILE];
extern char sLabelText[MAX_LABEL];
extern char sWriteText[MAX_STRING];
extern char sReturns[MAX_TEXT];
extern char sClass[MAX_CLASS + 1];
extern char sActivity[MAX_ACTIVITY + 1];
extern char sTempC[MAX_STRING];
extern char sClassesTemp[MAX_CLASSES][MAX_FILE];
extern char sDefaultGrades[ALPH][MAX_GRADE_L];
extern char sGrades[ALPH][MAX_GRADE_L];
extern char sGradesTemp[ALPH][MAX_GRADE_L];

extern char sStudentNames[MAX_STUDENTS + 1][MAX_NAME];
extern char sStudentCodes[MAX_STUDENTS + 1][MAX_CODE];
extern char sStudentSex[MAX_STUDENTS + 1][MAX_SEX];
extern char sStudentExtra[MAX_STUDENTS + 1][MAX_TEXT];

extern int iPeriodWeights[MAX_PERIODS + 1];
extern int iPeriodAvg[MAX_PERIODS + 1];
extern int iWorks[MAX_PERIODS + 1];

extern char sPeriodAvgData[MAX_PERIODS + 1][MAX_STUDENTS + 1][MAX_GRADE_L];
extern char sFinalAvgData[MAX_STUDENTS + 1][MAX_GRADE_L];

extern char sWorkNames[MAX_PERIODS + 1][MAX_WORKS + 1][MAX_NAME];
extern int iWorkWeights[MAX_PERIODS + 1][MAX_WORKS + 1];
extern char sWorkExtra[MAX_PERIODS + 1][MAX_WORKS + 1][MAX_TEXT];
extern char sWorkGrades[MAX_PERIODS + 1]
  [MAX_WORKS + 1][MAX_STUDENTS + 1][MAX_GRADE_L];

extern char sTeacher[MAX_TEACHER];
extern char sMentor[MAX_MENTOR];
extern GdkColormap *cmap;
extern GdkColor color;
extern GdkColor color2;
extern GdkFont *gHelpFont;
extern FILE *fPrinter;

#endif
