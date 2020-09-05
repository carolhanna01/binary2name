/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: options.h
 *    This file is part of Prime.
 *
 *    Prime is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    Prime is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with Prime.  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __OPTIONS_H
#define __OPTIONS_H

void optionsMenu_Change_Colors();
void optionsMenu_Reset_Config();

#define changeColorsDialogOptionsN	6

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
#endif