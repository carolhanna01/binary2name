/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: options.h
 *    This file is part of mino (Mino).
 *
 *    mino (Mino) is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    mino (Mino) is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with mino (Mino).  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __OPTIONS_H
#define __OPTIONS_H

#include "defs.h"

void optionsMenu_Change_Colors();
void optionsMenu_Tab_Spaces();
void optionsMenu_Reset_Config();

#define changeColorsDialogOptionsN	6

//values used as index into the color array
#define COLOR_WINDOW		0
#define COLOR_HIGHLIGHT_TEXT	1
#define COLOR_MENU_BAR		2
#define COLOR_STATUS_BAR	3
#define COLOR_BUTTONS		4
#define COLOR_HBUTTONS		5
//array to save foreground and background colors
int FG_COLOR[changeColorsDialogOptionsN];
int BG_COLOR[changeColorsDialogOptionsN];
char *COLOR_STR[8];
int FG_COLOR_ARRAY[8];
int BG_COLOR_ARRAY[8];
char *findColorName(int c);
void refreshChangeColorsDialog();
void write_config_file();
void saveOldColors();
void resetColors();
int showColorChooserDialog();

//determine if automatic highlighting is on/off
bool AUTO_HIGHLIGHTING;
//different highlight modes according to opened file
typedef enum 
{
  C_MODE,
  CPP_MODE,
  PERL_MODE,
  SHELL_MODE,
  TEXI_MODE,
  ASM_MODE,
  PYTHON_MODE,
  HTML_MODE,
  JAVASCRIPT_MODE,
  BASIC_MODE,
  PASCAL_MODE,
  F77_MODE,
  NO_MODE,
} hmode;
hmode HIGHLIGHT_MODE;
//special colors for syntax highlighting
#define COLOR_HCOMMENT		37
#define COLOR_HKEYWORD		33
#define COLOR_HSTRING		31
#define COLOR_HDIRECTIVE	35
#define COLOR_HPARAMETERS	36
#define COLOR_HBRACES		32
#define COLOR_HWINDOW		40
int old_window_color;
char *keyword[420];
int total_keywords;
void loadKeywords(hmode mode);

//switch auto-indentation on/off
bool AUTO_INDENT;
//the string holding the spaces/tabes to indent new lines
char AUTO_INDENT_STR[100];

#endif