/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: edit.h
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

#ifndef __EDIT_H
#define __EDIT_H

#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif

/* functions of Edit menu */
void editMenu_Cut();
void editMenu_Copy();
void editMenu_Paste();
void editMenu_Undo();
void editMenu_Redo();
void editMenu_SelectAll();
void editMenu_DeleteLine();
void editMenu_Find();
void editMenu_Replace();
void editMenu_ToggleSelectMode();
void remove_selected_text(bool RECORD);
void clear_selected_range();
void swap_lines();
void swap_chars();

/* 
 * boolean telling if we are in selecting mode 
 * (i.e. using SHIFT+Arrow to select) 
 */
bool SELECTING;
bool SELECTED;
#define MAX_CLIPBOARD_SIZE	1024
char clipboard[MAX_CLIPBOARD_SIZE];
bool CLIPBOARD_IS_EMPTY;
int total_lines_in_clipboard;

typedef struct { //structure defining a point by its position in
  int nline;	 //line number (zero-based), and
  int nchar;	 //char number (zero-based)
} point;
//char selected_text[MAX_CLIPBOARD_SIZE];
point sel_range_start;//start point of selected text
point sel_range_end;  //end point of selected text

typedef enum { 
  UNDO_ACTION_INSERT, 
  UNDO_ACTION_DELETE,
  UNDO_ACTION_REPLACE
} undoActionType;
#define MAX_UNDO_ACTIONS	100
undoActionType undo_action[MAX_UNDO_ACTIONS];//type of undo: insert or delete?
char *undo_text[MAX_UNDO_ACTIONS];//the text to undo insert/delete
char *undo_text_replace[MAX_UNDO_ACTIONS];
point undo_action_start[MAX_UNDO_ACTIONS];//the start position of action
point undo_action_end[MAX_UNDO_ACTIONS];//the end position of action
int last_undo_action;
int total_undo_actions;
bool RECORDING_UNDO_ACTION;
void begin_undo_action(undoActionType utype);
void add_to_undo_action(char *ch);
void finish_undo_action();

//definitions for the find/replace functions
point find_result_pos[100];
int total_find_results;
void _replace(int pos, char **f, char **r);
void _do_replace(int pos, char **f, char **r);
void _find(char **f);

#define MAX_TABS	1000	//max tabs per document
point tab_pos[MAX_TABS];	//positions of tabs in document
int tab_spaces[MAX_TABS];	//size of each tab in document
int total_tabs;
void checkTabsInLine(int pos);
void checkAllTabs();
void removeBadTabs();
void shiftAllTabs(int start_line, int shift);
void shiftTabsInLine(int line, int start_char, int shift);
void calcTotalCharsInLine(int pos);
void fixLineLength(int pos);

#endif