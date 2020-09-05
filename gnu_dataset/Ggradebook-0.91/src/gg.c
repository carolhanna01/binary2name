/* Ggradebook 0.91 (gg.c)
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
#include "gg.h"
#include "message.h"
#include "class.h"
#include "grades.h"
#include "feedback.h"
#include "helpabout.h"
#include "student.h"
#include "print.h"
#include "preferences.h"
#include "period.h"

#ifndef USE_GNOME
static GtkItemFactoryEntry menu[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Add Class...",      "<control>A", NewClass,      0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
  {"/File/_Quit",              "<control>Q", Quit,          0, NULL},
  {"/_Help",                   NULL,         NULL,          0, "<LastBranch>"},
  {"/Help/_Feedback...",       "<control>F", Feedback,      0, NULL},
  {"/Help/_Help...",           "<control>H", Help,          0, NULL},
  {"/Help/sep2",               NULL,         NULL,          0, "<Separator>"},
  {"/Help/A_bout...",          "<control>B", About,         0, NULL},
};

static GtkItemFactoryEntry menu2[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Add",               "<control>A", AddClass,      0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
  {"/File/_Cancel",            "<control>C", NoNewClass,    0, NULL},
};

static GtkItemFactoryEntry menu3[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Save",              "<control>S", SaveEditClass, 0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
  {"/File/S_tudents...",       "<control>T", Students,      0, NULL},
  {"/File/_Grades & Periods...", "<control>G", Grades,        0, NULL},
  {"/File/sep2",               NULL,         NULL,          0, "<Separator>"},
  {"/File/_Print...",          "<control>P", Print,         0, NULL},
  {"/File/sep3",               NULL,         NULL,          0, "<Separator>"},
  {"/File/_Cancel",            "<control>C", NoEditClass,   0, NULL},
  {"/_Help",                   NULL,         NULL,          0, "<LastBranch>"},
  {"/Help/_Feedback...",       "<control>F", Feedback,      0, NULL},
  {"/Help/_Help...",           "<control>H", Help,          0, NULL},
  {"/Help/sep2",               NULL,         NULL,          0, "<Separator>"},
  {"/Help/A_bout...",          "<control>B", About,         0, NULL},
};

static GtkItemFactoryEntry menu4[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Rename",            "<control>R", YesRenameClass,0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
  {"/File/_Cancel",            "<control>C", NoRenameClass, 0, NULL},
};

static GtkItemFactoryEntry menu5[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/_Add",               "<control>A", YesNewStudent, 0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoNewStudent,  0, NULL},
};

static GtkItemFactoryEntry menu6[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/_Apply",             "<control>A", YesPreferences,0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoPreferences, 0, NULL},
};

static GtkItemFactoryEntry menu7[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Print",             "<control>P", YesPrint,      0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
  {"/File/_Cancel",            "<control>C", NoPrint,       0, NULL},
};

static GtkItemFactoryEntry menu8[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/_Add",               "<control>A", YesAddGrades,  0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoAddGrades,   0, NULL},
};

static GtkItemFactoryEntry menu9[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/C_hange",            "<control>H", YesNewStudent, 0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoNewStudent,  0, NULL},
};

static GtkItemFactoryEntry menu10[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
  {"/File/_Close",             "<control>C", QuitFeedback,  0, NULL},
};

static GtkItemFactoryEntry menu11[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/_Add",               "<control>A", YesNextPeriod, 0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoNextPeriod,  0, NULL},
};

static GtkItemFactoryEntry menu12[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/C_hange",            "<control>H", YesNextPeriod, 0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoNextPeriod,  0, NULL},
};

static GtkItemFactoryEntry menu13[] = {
  {"/_File",                   NULL,         NULL,          0, "<Branch>"},
/*
  {"/File/C_hange",            "<control>H", YesAddGrades,  0, NULL},
  {"/File/sep",                NULL,         NULL,          0, "<Separator>"},
*/
  {"/File/_Cancel",            "<control>C", NoAddGrades,   0, NULL},
};
#endif

int main (int argc, char *argv[])
{
  int iTemp;
  int iRead;
  int iTheGrade;
  char sRead[1];
  char sThePerc[MAX_STRING];

  if (argc == 2)
  {
    if (strcmp (argv[1], "--version") == 0)
    {
      printf ("%s\n", VERSION);
      exit (EXIT_NORMAL);
    }
  }

  if ((getenv ("DISPLAY")) == 0)
  {
    printf ("[Start the X Window System first]\n");
    exit (EXIT_ERROR);
  }

#ifdef USE_GNOME
  gnome_init ("Ggradebook", VERSION, argc, argv);
#else
  gtk_init (&argc, &argv);
#endif

  iColor = 1;
  cmap = gdk_colormap_get_system ();
  /* lavender */
  color.red = 0xe600;
  color.green = 0xe600;
  color.blue = 0xfa00;
  if (!gdk_color_alloc (cmap, &color))
  {
    iColor = 0;
  }
  /* RoyalBlue4 */
  color2.red = 0x2700;
  color2.green = 0x4000;
  color2.blue = 0x8b00;
  if (!gdk_color_alloc (cmap, &color2))
  {
    iColor = 0;
  }
  gHelpFont = gdk_font_load ("-adobe-utopia-bold-r-normal-*-*-140-*-*-p-*-"
    "iso8859-9");

  for (iTemp = 0; iTemp < ALPH; iTemp++)
  {
    snprintf (sDefaultGrades[iTemp], MAX_GRADE_L, "%s", "-200");
  }
  snprintf (sGgConfig, MAX_FILE, "%s/.gg", getenv ("HOME"));
  if ((iConfigFd = open (sGgConfig, O_RDONLY, 0600)) == -1)
  {
    AskClassPath ();
  }
  else
  {
    Read (iConfigFd, 0, "classpath: ");
    snprintf (sClassPath, MAX_PATH, "%s", sReturns);
    Read (iConfigFd, 0, "default grades: ");
    iDefaultGrades = atoi (sReturns);
    if (iDefaultGrades == 1)
    {
      Read (iConfigFd, 0, "default letters: ");
      iDefaultLetters = atoi (sReturns);
      for (iTemp = 0; iTemp < iDefaultLetters; iTemp++)
      {
        iRead = read (iConfigFd, sRead, 1);
        iTheGrade = sRead[0] - 65;
        iRead = read (iConfigFd, sRead, 1);
        iRead = read (iConfigFd, sRead, 1);
        snprintf (sThePerc, MAX_STRING, "%s", "");
        do
        {
          iRead = read (iConfigFd, sRead, 1);
          if (sRead[0] != '\n')
          {
            snprintf (sTempC, MAX_STRING, "%s", sThePerc);
            snprintf (sThePerc, MAX_STRING, "%s%c", sTempC, sRead[0]);
          }
        } while (sRead[0] != '\n');
        snprintf (sDefaultGrades[iTheGrade], MAX_GRADE_L, "%s", sThePerc);
      }
    }
    Read (iConfigFd, 0, "lowest pos: ");
    dDefaultLowestPos = atof (sReturns);
    Read (iConfigFd, 0, "highest pos: ");
    dDefaultHighestPos = atof (sReturns);
    Read (iConfigFd, 0, "highlight below: ");
    dDefaultHighlightBelow = atof (sReturns);
    Read (iConfigFd, 0, "plus: ");
    dDefaultPlus = atof (sReturns);
    Read (iConfigFd, 0, "minus: ");
    dDefaultMinus = atof (sReturns);
    if (iDefaultGrades == 2)
    {
      Read (iConfigFd, 0, "final averages: ");
      iDefaultFinalAvg = atoi (sReturns);
    }
    else
    {
      iDefaultFinalAvg = 1;
    }
    Read (iConfigFd, 0, "drop lowest: ");
    iDefaultDrop = atoi (sReturns);
    close (iConfigFd);
    LoadClasses ();
  }

  gtk_main ();
  return EXIT_NORMAL;
}

void Quit (void)
{
  gtk_exit (EXIT_NORMAL);
}

#ifndef USE_GNOME
void GetMenu (GtkWidget *GMWindow, GtkWidget **GMMenuBar, int iMenu)
{
  GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group;
  int nmenu_items;

  nmenu_items = 0;
  switch (iMenu)
  {
    case 1: nmenu_items = sizeof (menu) / sizeof (menu[0]); break;
    case 2: nmenu_items = sizeof (menu2) / sizeof (menu2[0]); break;
    case 3: nmenu_items = sizeof (menu3) / sizeof (menu3[0]); break;
    case 4: nmenu_items = sizeof (menu4) / sizeof (menu4[0]); break;
    case 5: nmenu_items = sizeof (menu5) / sizeof (menu5[0]); break;
    case 6: nmenu_items = sizeof (menu6) / sizeof (menu6[0]); break;
    case 7: nmenu_items = sizeof (menu7) / sizeof (menu7[0]); break;
    case 8: nmenu_items = sizeof (menu8) / sizeof (menu8[0]); break;
    case 9: nmenu_items = sizeof (menu9) / sizeof (menu9[0]); break;
    case 10: nmenu_items = sizeof (menu10) / sizeof (menu10[0]); break;
    case 11: nmenu_items = sizeof (menu11) / sizeof (menu11[0]); break;
    case 12: nmenu_items = sizeof (menu12) / sizeof (menu12[0]); break;
    case 13: nmenu_items = sizeof (menu13) / sizeof (menu13[0]); break;
  }

  accel_group = gtk_accel_group_new ();
  item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>",
    accel_group);
  switch (iMenu)
  {
    case 1:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu, NULL);
      break;
    case 2:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu2, NULL);
      break;
    case 3:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu3, NULL);
      break;
    case 4:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu4, NULL);
      break;
    case 5:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu5, NULL);
      break;
    case 6:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu6, NULL);
      break;
    case 7:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu7, NULL);
      break;
    case 8:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu8, NULL);
      break;
    case 9:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu9, NULL);
      break;
    case 10:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu10, NULL);
      break;
    case 11:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu11, NULL);
      break;
    case 12:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu12, NULL);
      break;
    case 13:
      gtk_item_factory_create_items (item_factory, nmenu_items, menu13, NULL);
      break;
  }
  gtk_accel_group_attach (accel_group, GTK_OBJECT (GMWindow));
  if (GMMenuBar)
  {
    *GMMenuBar = gtk_item_factory_get_widget (item_factory, "<main>");
  }
}
#endif

void SetAvg (GtkWidget *saW, gpointer *saData)
{
  int iData;

  iData = (int)saData;
  switch (iData)
  {
    case 1: iFinalAverageTemp = 1; break;
    case 2: iFinalAverageTemp = 2; break;
    case 3: iFinalAverageTemp = 3; break;
    case 4: iPeriodAverageTemp = 1; break;
    case 5: iPeriodAverageTemp = 2; break;
    case 6: iPeriodAverageTemp = 3; break;
    default:
      Message ("Wrong SetAvg case!");
  }
}

void UpdateList (void)
{
  int iTemp;
  int iTemp2;
  gchar *sInsert[BEFORE_GR + MAX_WORKS];

  Averages ();
  gtk_clist_freeze (GTK_CLIST (gEditClassList));
  gtk_clist_clear (GTK_CLIST (gEditClassList));
  for (iTemp = BEFORE_GR; iTemp <= BEFORE_GR + MAX_WORKS; iTemp++)
  {
    gtk_clist_set_column_visibility (GTK_CLIST (gEditClassList), iTemp, FALSE);
  }
  for (iTemp = 1; iTemp <= iStudents; iTemp++)
  {
    snprintf (sWriteText, MAX_STRING, "%i", iTemp);
    for (iTemp2 = 0; iTemp2 < BEFORE_GR + MAX_WORKS; iTemp2++)
    {
      sInsert[iTemp2] = "";
    }
    gtk_clist_insert (GTK_CLIST (gEditClassList), iTemp - 1,
      sInsert);
    if (iColor == 1)
    {
      gtk_clist_set_background (GTK_CLIST (gEditClassList), iTemp - 1,
        &color);
      gtk_clist_set_foreground (GTK_CLIST (gEditClassList), iTemp - 1,
        &color2);
    }

    snprintf (sWriteText, MAX_STRING, "%i", iTemp);
    gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
      0, sWriteText);
    snprintf (sWriteText, MAX_STRING, "%s", sStudentNames[iTemp]);
    gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
      1, sWriteText);
    snprintf (sWriteText, MAX_STRING, "%s", sFinalAvgData[iTemp]);
    gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
      2, sWriteText);
    snprintf (sWriteText, MAX_STRING, "%s", sPeriodAvgData[iPeriod][iTemp]);
    gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
      3, sWriteText);

    for (iTemp2 = 1; iTemp2 <= iWorks[iPeriod]; iTemp2++)
    {
      snprintf (sWriteText, MAX_STRING, "%s",
        sWorkGrades[iPeriod][iTemp2][iTemp]);
      gtk_clist_set_text (GTK_CLIST (gEditClassList), iTemp - 1,
        BEFORE_GR + iTemp2 - 1, sWriteText);
      gtk_clist_set_column_visibility (GTK_CLIST (gEditClassList),
        BEFORE_GR + iTemp2 - 1, TRUE);
    }
    gtk_clist_set_selectable (GTK_CLIST (gEditClassList), iTemp - 1, FALSE);
  }
  gtk_clist_thaw (GTK_CLIST (gEditClassList));
}

void Read (int iFd, int iExtra, char *sFind)
{
  char sOption[MAX_OPTION];
  char sRead[1];
  int iTemp;
  int iRead;

  if (iExtra != 2)
  {
    sRead[0] = '#';
    snprintf (sOption, MAX_OPTION, "%s", "");
    do
    {
      if (sRead[0] == '\n')
      {
        snprintf (sOption, MAX_OPTION, "%s", "");
      }
      iRead = read (iFd, sRead, 1);
      snprintf (sTempC, MAX_STRING, "%s", sOption);
      snprintf (sOption, MAX_OPTION, "%s%c", sTempC, sRead[0]);
    } while ((strcmp (sOption, sFind) != 0) && (iRead != 0));
    if (iRead == 0)
    {
      g_print ("[FAILED] Could not find string '%s'!\n", sFind);
      gtk_exit (EXIT_ERROR);
    }
  }
  snprintf (sReturns, MAX_TEXT, "%s", "");
  if (iExtra == 1)
  {
    for (iTemp = 0; iTemp <= MAX_TEXT; iTemp++)
    {
      read (iFd, sRead, 1);
      if ((sRead[0] != '#') && (iTemp != MAX_TEXT))
      {
        snprintf (sTempC, MAX_STRING, "%s", sReturns);
        snprintf (sReturns, MAX_TEXT, "%s%c", sTempC, sRead[0]);
      }
    }
  }
  else
  {
    do
    {
      read (iFd, sRead, 1);
      if (sRead[0] != '\n')
      {
        snprintf (sTempC, MAX_STRING, "%s", sReturns);
        snprintf (sReturns, MAX_TEXT, "%s%c", sTempC, sRead[0]);
      }
    } while (sRead[0] != '\n');
  }
}
