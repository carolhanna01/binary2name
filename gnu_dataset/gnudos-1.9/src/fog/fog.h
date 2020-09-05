/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: fog.h
 *    This file is part of fog.
 *
 *    fog is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    fog is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with fog.  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __FOG_H
#define __FOG_H

#include "../corelib/dialogs.h"
#include "form.h"

#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif
void showAboutDialogBox();
void showQuickReference();
void showReadMe();
void drawMenuBar(int x, int y, int w);
void drawStatusBar();
#define totalMainMenus 4
char *menu[totalMainMenus];

void showMenu(int menu);
#define fTotal	5
#define eTotal	5
#define oTotal	3
#define hTotal	3
char *fileMenu[fTotal];
char *editMenu[eTotal];
char *optionsMenu[oTotal];
char *helpMenu[hTotal];

/*//values used as index into the color array
#define COLOR_WINDOW		0
#define COLOR_HIGHLIGHT_TEXT	1
#define COLOR_MENU_BAR		2
#define COLOR_STATUS_BAR	3
#define COLOR_BUTTONS		4
#define COLOR_HBUTTONS		5
//array to save foreground and background colors
int FG_COLOR[6];
int BG_COLOR[6];*/
char *COLOR_STR[8];
int FG_COLOR_ARRAY[8];
int BG_COLOR_ARRAY[8];
char *findColorName(int c);
void refreshChangeColorsDialog();
void write_config_file();
void saveOldColors();
void resetColors();
int showColorChooserDialog();
int old_window_color;

#define total_tools 5
char *tool[total_tools];
void drawToolBox();
void getInputFromToolBoxWin();
void getInputFromFormWin();
int selected_tool;
bool adding_tool;
char *BULLET;
void makeProject();
void deleteFormTool();
void editFormToolText();
void changeFormWidth();
void changeFormHeight();
void changeFormTitle();

int activeWindow;
#define FORM_WIN	0
#define TOOLBOX_WIN	1
int endme;

typedef enum { NEW, OPENED, SAVED, MODIFIED } fstate;
fstate FILE_STATE;

#endif