/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: form.h
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
#ifndef __FORM_H
#define __FORM_H

typedef struct 
{
  int x;
  int y;
} tpos;

int total_form_tools;
#define MAX_FORM_TOOLS	30
char *form_tool[MAX_FORM_TOOLS];
char *form_tool_text[MAX_FORM_TOOLS];
tpos tool_pos[MAX_FORM_TOOLS];
#define total_tools 5
int tool_count[total_tools];
int selected_form_tool;

int FORM_WIDTH;
int FORM_HEIGHT;
char *FORM_TITLE;
#define MAX_FORM_TITLE_LEN	100
char *form_file_name;
#define MAX_FORM_FILE_NAME_LEN	255
int NEW_FILE;
void drawForm();
int saveForm();
void openForm();
void newForm();

#endif