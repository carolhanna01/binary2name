/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: options.h
 *    This file is part of the GnuDOS project.
 *
 *    GnuDOS is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    GnuDOS is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with GnuDOS.  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __OPTIONS_H
#define __OPTIONS_H

void changeColors();

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
void saveOldColors();
void resetColors();
int showColorChooserDialog();

int old_window_color;

#endif