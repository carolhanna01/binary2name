/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: file.c
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

#include "defs.h"
#include "file.h"
#include "edit.h"
#include "options.h"

/************************************
 * Function called when user selects
 * New from File Menu or presses
 * CTRL+N.
 * **********************************/
void fileMenu_New() 
{
    if(FILE_STATE == NEW) { refreshView(); return; }
    if(open_file) 
    { /*fflush(open_file);*/ fclose(open_file); open_file = NULL; }

    if(FILE_STATE == MODIFIED) 
    {//if #1
      int i = msgBox("File has been changed. Save changes?", YES|NO, INFO);
      if(i == YES)
      {//if #2
	if(NEW_FILE)
	{//if #3
	  if(!openSaveFile(SAVE, YES))
	  {
	    msgBox("Failed to save file.", OK, ERROR);
	  } else { FILE_STATE = NEW; }
	} 
	else
	{
	  if(!openSaveFile(SAVE, NO))
	  {
	    msgBox("Failed to save file.", OK, ERROR);
	  } else { NEW_FILE = 1; FILE_STATE = NEW; }
	}//end if #3
      } else { NEW_FILE = 1; FILE_STATE = NEW; }
    } 
    else
    { 
      NEW_FILE = 1; FILE_STATE = NEW;
    }//end if #1
    if(FILE_STATE == NEW)
    {
      int i;
	for(i = 0; i < MAX_LINES; i++) 
	{
	  lines[i][0] = '\0';
	  totalCharsInLine[i] = 0;
	  LINE_IS_LINKED[i] = 0;
	}
	totalLines = 1;
	firstVisLine = 0;
	selectedLine = 0;
	selectedChar = 0;
	last_undo_action = -1;
	strcpy(documentTitle, DEFAULT_TITLE);
	//remove backup file, if any
	//char *backup_file_name = (char *) malloc(strlen(open_file_name)+2);
	//strcpy(backup_file_name, open_file_name);
	//strcat(backup_file_name, "~\0");
	//remove(backup_file_name);
	//free(backup_file_name);
    }
  BG_COLOR[COLOR_WINDOW] = old_window_color;
  refreshView();
}


/************************************
 * Function called when user selects
 * Open from File Menu or presses
 * CTRL+O.
 * **********************************/
void fileMenu_Open()
{
    if(FILE_STATE == MODIFIED)
    {
      int i = msgBox("File has been changed. Save changes?", YES|NO, INFO);
      if(i == YES)
      {//if #2
	if(NEW_FILE)
	{//if #3
	  if(!openSaveFile(SAVE, YES))
	  {
	    msgBox("Failed to save file.", OK, ERROR); 
	    refreshView(); return;
	  } else { FILE_STATE = SAVED; }
	} 
	else
	{
	  if(!openSaveFile(SAVE, NO))
	  {
	    msgBox("Failed to save file.", OK, ERROR); 
	    refreshView(); return;
	  } else { FILE_STATE = SAVED; }
	}//end if #3
      } else { ; } // FILE_STATE = IDLE; }
    }
    
    if(open_file) 
    { /*fflush(open_file);*/ fclose(open_file); open_file = NULL; }
    
    if(!openSaveFile(OPEN, YES))
    {
      msgBox("Failed to open file.", OK, ERROR);
      //FILE_STATE = IDLE;
    } else { FILE_STATE = OPENED; NEW_FILE = 0; last_undo_action = -1; }
    refreshView();

    //remove backup file, if any
    //char *backup_file_name = (char *) malloc(strlen(open_file_name)+2);
    //strcpy(backup_file_name, open_file_name);
    //strcat(backup_file_name, "~\0");
    //remove(backup_file_name);
    //free(backup_file_name);
}

/************************************
 * Function called when user selects
 * Save from File Menu or presses
 * CTRL+S.
 * **********************************/
void fileMenu_Save()
{
    if(NEW_FILE)
    {
	if(open_file) { fclose(open_file); open_file = NULL; }
	//if new file is created on startup, save file with this name
	if(strcmp(documentTitle, DEFAULT_TITLE) == 0) 
	{
	 if(!openSaveFile(SAVE, YES)) 
	 {//show Save as.. dialog box
	  msgBox("Failed to save file.", OK, ERROR);
	 } else { FILE_STATE = SAVED; }
	 refreshView();
	}
	else
	{
	 if(!openSaveFile(SAVE, NO)) 
	 {//do not show dialog box
	  msgBox("Failed to save file.", OK, ERROR);
	 } else { FILE_STATE = SAVED; }
	 refreshView();
	}
    } 
    else
    { //do not show dialog box
	if(!openSaveFile(SAVE, NO))
	{
	  msgBox("Failed to save file.", OK, ERROR);
	} else { FILE_STATE = SAVED; }
	refreshView();
    }
}

/************************************
 * Function called when user selects
 * Save As.. from File Menu.
 * **********************************/
void fileMenu_SaveAs() 
{
    //if(open_file) { fflush(open_file); fclose(open_file); }
    if(!openSaveFile(SAVE, YES))
    {
      msgBox("Failed to save file.", OK, ERROR);
    } else { FILE_STATE = SAVED; }
    refreshView();
}

void fileMenu_Print()
{
  msgBox("Oops! This function is not currently implemented!.", OK, INFO);
  refreshView();
}

void fileMenu_Exit()
{
  exit_gracefully(0);
}
