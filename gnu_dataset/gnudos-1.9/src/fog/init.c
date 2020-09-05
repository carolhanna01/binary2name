/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: init.c
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
#include "fog.h"

void init();

void init() 
{
  int i;
  menu[0] = "&File";
  menu[1] = "&Edit";
  menu[2] = "&Options";
  menu[3] = "&Help";
  
  fileMenu[0] = "New form      ^N";
  fileMenu[1] = "Open form     ^O";
  fileMenu[2] = "Save form     ^S";
  fileMenu[3] = "Write project ^W";
  fileMenu[4] = "Exit          ^Q";
  editMenu[0] = "Delete tool    DEL";
  editMenu[1] = "Edit tool text ENT";
  editMenu[2] = "Form Width        ";
  editMenu[3] = "Form Height       ";
  editMenu[4] = "Form Title        ";
  optionsMenu[0] = "View toolbox     T";
  optionsMenu[1] = "View form        F";
  optionsMenu[2] = "Edit colors       ";
  helpMenu[0] = "View README     ";
  helpMenu[1] = "Quick reference ";
  helpMenu[2] = "About           ";

  //initialize color arrays
  FG_COLOR_ARRAY[0] = 30;
  FG_COLOR_ARRAY[1] = 31;
  FG_COLOR_ARRAY[2] = 32;
  FG_COLOR_ARRAY[3] = 33;
  FG_COLOR_ARRAY[4] = 34;
  FG_COLOR_ARRAY[5] = 35;
  FG_COLOR_ARRAY[6] = 36;
  FG_COLOR_ARRAY[7] = 37;
  BG_COLOR_ARRAY[0] = 40;
  BG_COLOR_ARRAY[1] = 41;
  BG_COLOR_ARRAY[2] = 42;
  BG_COLOR_ARRAY[3] = 43;
  BG_COLOR_ARRAY[4] = 44;
  BG_COLOR_ARRAY[5] = 45;
  BG_COLOR_ARRAY[6] = 46;
  BG_COLOR_ARRAY[7] = 47;
  FG_COLOR[COLOR_WINDOW] = 37;
  FG_COLOR[COLOR_HIGHLIGHT_TEXT] = 34;
  FG_COLOR[COLOR_MENU_BAR] = 34;
  FG_COLOR[COLOR_STATUS_BAR] = 34;
  FG_COLOR[COLOR_BUTTONS] = 37;
  FG_COLOR[COLOR_HBUTTONS] = 32;
  BG_COLOR[COLOR_WINDOW] = 44;
  BG_COLOR[COLOR_HIGHLIGHT_TEXT] = 47;
  BG_COLOR[COLOR_MENU_BAR] = 47;
  BG_COLOR[COLOR_STATUS_BAR] = 47;
  BG_COLOR[COLOR_BUTTONS] = 41;
  BG_COLOR[COLOR_HBUTTONS] = 41;
  old_window_color = BG_COLOR[COLOR_WINDOW];
  
  tool[0] = "Input field";
  tool[1] = "Message field";
  tool[2] = "Bullet item";
  tool[3] = "Option item";
  tool[4] = "Button";
  selected_tool = 0;
  adding_tool = 0;
  selected_form_tool = 0;
  //total_tools = 5;
  activeWindow = TOOLBOX_WIN;
  
  FORM_WIDTH = SCREEN_W-17;
  FORM_HEIGHT = SCREEN_H-5;
  FORM_TITLE = (char *) malloc(SCREEN_W-2);
  strcpy(FORM_TITLE, "Sample Form");
  form_file_name = (char *) malloc(MAX_FORM_FILE_NAME_LEN);
  strcpy(form_file_name, "SampleForm.fog");
  NEW_FILE = 1;
  total_form_tools = 0;
  for(i = 0; i < MAX_FORM_TOOLS; i++) 
  {
    tool_pos[i].x = 0; tool_pos[i].y = 0;
    form_tool[i] = (char *) malloc(FORM_WIDTH);
    strcpy(form_tool[i], "");
    form_tool_text[i] = (char *) malloc(FORM_WIDTH);
    strcpy(form_tool_text[i], "");
  }
  for(i = 0; i < total_tools; i++)
    tool_count[i] = 0;

  BULLET = (char *) malloc(5);
  strcpy(BULLET, "* \0");

  endme = 0;
  FILE_STATE = NEW;
}