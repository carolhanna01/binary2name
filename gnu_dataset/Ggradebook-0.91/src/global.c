/* Ggradebook 0.91 (global.c)
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

GtkWidget *gMessageWindow = NULL;
GtkWidget *gPathEntry;
GtkWidget *gPathWindow = NULL;
GtkWidget *gStudentsWindow = NULL;
GtkWidget *gEditClassList;
GtkWidget *gGradesWindow = NULL;
GtkWidget *gDeleteStudentWindow = NULL;
GtkWidget *gDeleteStudentEntry;
GtkWidget *gEditStudentWindow = NULL;
GtkWidget *gEditStudentEntry;
GtkWidget *gYesDeleteStudentWindow = NULL;
GtkWidget *gAddGradesWindow = NULL;
GtkWidget *gAddGradesEntry;
GtkWidget *gAddGradesSpinButton;
GtkWidget *gAddGradesText;
GtkWidget *gAddGradesEntryS[MAX_STUDENTS + 1];
GtkWidget *gPreferencesWindow = NULL;
GtkWidget *gPreferencesEntry;
GtkWidget *gPreferencesEntry2;
GtkWidget *gPreferencesEntry3;
GtkWidget *gPreferencesEntry4;
GtkWidget *gPreferencesEntry5;
GtkWidget *gPreferencesEntry8;
GtkWidget *gPreferencesEntry9;
GtkWidget *gPreferencesRButton5;
GtkWidget *gPreferencesRButton6;
GtkWidget *gPreferencesRButton7;
GtkWidget *gPreferencesButton4;
GtkWidget *gPreferencesButton5;
GtkWidget *gPreferencesScrolled;
GtkWidget *gPreferencesOptionMenu;
GtkWidget *gPreferencesSpinButton;
GtkWidget *gPreferencesSpinButton2;
GtkWidget *gPreferencesSpinButton3;
GtkWidget *gPreferencesSpinButton4;
GtkWidget *gPreferencesSpinButton5;
GtkWidget *gPreferencesSpinButton6;
GtkWidget *gPreferencesList;
GtkWidget *gPreferencesOptionMenu1;
GtkWidget *gPrintWindow = NULL;
GtkWidget *gPrintEntry;
GtkWidget *gPrintEntry2;
GtkWidget *gNewClassWindow = NULL;
GtkWidget *gNewClassEntry;
GtkWidget *gNewClassEntry2;
GtkWidget *gNewClassEntry3;
GtkWidget *gNewClassEntry4;
GtkWidget *gClassesWindow = NULL;
GtkWidget *gSelectClassWindow = NULL;
GtkWidget *gEditClassWindow = NULL;
GtkWidget *gEditClassLabel;
GtkWidget *gDeleteClassWindow = NULL;
GtkWidget *gRenameClassWindow = NULL;
GtkWidget *gRenameClassEntry;
GtkWidget *gRenameClassEntry2;
GtkWidget *gNewStudentWindow = NULL;
GtkWidget *gNewStudentEntry;
GtkWidget *gNewStudentEntry2;
GtkWidget *gNewStudentText;
GtkWidget *gFeedbackWindow = NULL;
GtkWidget *gFeedbackCButton;
GtkWidget *gFeedbackEntry;
GtkWidget *gFeedbackText;
GtkWidget *gNextPeriodWindow = NULL;
GtkWidget *gNextPeriodSpinButton;
GtkWidget *gEditGradesWindow = NULL;
GtkWidget *gEditGradesEntry;
GtkWidget *gDeleteGradesWindow = NULL;
GtkWidget *gDeleteGradesEntry;
GtkWidget *gYesDeleteGradesWindow = NULL;
GtkWidget *gHelpWindow = NULL;
GtkWidget *gAboutWindow = NULL;
GtkWidget *gAddWindow = NULL;

int iConfigFd;
int iSelected;
int iStudents;
int iPeriod;
int iPeriods;
int iSex;
int iFeelFree;
int iGradesTemp;
int iFinalAverageTemp;
int iPeriodAverageTemp;
int iColor;
int iDropTemp;
double dLowestGrade;
int iLowestWeight;
int iToPrint;
int iFormat;
int iToWhat;
int iClasses;
int iPrintFd;

int iDefaultGrades;
int iGrades;
int iDefaultLetters;
int iLetters;
double dDefaultLowestPos;
double dLowestPos;
double dDefaultHighestPos;
double dHighestPos;
double dDefaultHighlightBelow;
double dHighlightBelow;
double dDefaultPlus;
double dPlus;
double dDefaultMinus;
double dMinus;
int iDefaultFinalAvg;
int iFinalAvg;
int iDefaultDrop;
int iDrop;

int iSelectedRow;
int iNewLetter;
int iLettersTemp;

char sGgConfig[MAX_FILE];
char sClassPath[MAX_PATH];
char sClasses[MAX_CLASSES][MAX_FILE];
char sButtonLabel[MAX_CLASSES][MAX_LABEL];
char sFileName[MAX_FILE];
char sLabelText[MAX_LABEL];
char sWriteText[MAX_STRING];
char sReturns[MAX_TEXT];
char sClass[MAX_CLASS + 1];
char sActivity[MAX_ACTIVITY + 1];
char sTempC[MAX_STRING];
char sClassesTemp[MAX_CLASSES][MAX_FILE];
char sDefaultGrades[ALPH][MAX_GRADE_L];
char sGrades[ALPH][MAX_GRADE_L];
char sGradesTemp[ALPH][MAX_GRADE_L];

char sStudentNames[MAX_STUDENTS + 1][MAX_NAME];
char sStudentCodes[MAX_STUDENTS + 1][MAX_CODE];
char sStudentSex[MAX_STUDENTS + 1][MAX_SEX];
char sStudentExtra[MAX_STUDENTS + 1][MAX_TEXT];

int iPeriodWeights[MAX_PERIODS + 1];
int iPeriodAvg[MAX_PERIODS + 1];
int iWorks[MAX_PERIODS + 1];

char sPeriodAvgData[MAX_PERIODS + 1][MAX_STUDENTS + 1][MAX_GRADE_L];
char sFinalAvgData[MAX_STUDENTS + 1][MAX_GRADE_L];

char sWorkNames[MAX_PERIODS + 1][MAX_WORKS + 1][MAX_NAME];
int iWorkWeights[MAX_PERIODS + 1][MAX_WORKS + 1];
char sWorkExtra[MAX_PERIODS + 1][MAX_WORKS + 1][MAX_TEXT];
char sWorkGrades[MAX_PERIODS + 1]
  [MAX_WORKS + 1][MAX_STUDENTS + 1][MAX_GRADE_L];

char sTeacher[MAX_TEACHER];
char sMentor[MAX_MENTOR];
GdkColormap *cmap;
GdkColor color;
GdkColor color2;
GdkFont *gHelpFont;
FILE *fPrinter;
