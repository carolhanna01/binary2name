/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: edit.c
 *    This file is part of mino.
 *
 *    mino is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    mino is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with mino.  If not, see <http://www.gnu.org/licenses/>.
 */    

#include "defs.h"
#include "edit.h"
#include "kbd.h"
#include "options.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>

/* main.c */
extern char *tmp;
extern char *tmp2;


void swap_lines() 
{
  int tmp;
  tmp = sel_range_end.nline;
  sel_range_end.nline = sel_range_start.nline;
  sel_range_start.nline = tmp;
  tmp = sel_range_end.nchar;
  sel_range_end.nchar = sel_range_start.nchar;
  sel_range_start.nchar = tmp;
}

void swap_chars() 
{
  int tmp;
  tmp = sel_range_end.nchar;
  sel_range_end.nchar = sel_range_start.nchar;
  sel_range_start.nchar = tmp;
}

/*****************************************
 * Clears the selected range. This is
 * helpful when exiting from the select
 * mode (like using arrow keys after
 * releasing SHIFT).
 * ***************************************/
void clear_selected_range() 
{
  
}

void editMenu_DeleteLine() 
{
  deleteLine();
}

/*****************************************
 * Toggles select mode ON/OFF. Select mode
 * is useful when running under X, to
 * emulate SHIFT-selection.
 * ***************************************/
void editMenu_ToggleSelectMode() 
{
  if(X_IS_RUNNING) 
  {
    SELECTING = !SELECTING;
    refreshBottomView();
    if(SELECTING) 
    {
      sel_range_start.nline = firstVisLine+selectedLine;
      sel_range_start.nchar = selectedChar;
      sel_range_end.nline = firstVisLine+selectedLine;
      sel_range_end.nchar = selectedChar;
    }
  } 
  else 
  {
    msgBox("The select mode is only available under X.\nUse SHIFT to select text.", OK, INFO);
    refreshView();
  }
}

/*****************************************
 * Copies selection to clipboard.
 * ***************************************/
void editMenu_Copy() 
{
 if(SELECTING || SELECTED) 
 {
  int i, j, k, l, pos = 0;
  strcpy(clipboard, "");
  //swap the select range boundaries if needed
  int swap = 0;
  if(sel_range_start.nline > sel_range_end.nline) { swap = 1; swap_lines(); }
  else if(sel_range_start.nline == sel_range_end.nline &&
	  sel_range_start.nchar > sel_range_end.nchar) 
  { swap = 2; swap_chars(); }

  total_lines_in_clipboard = 0;
  for(j = sel_range_start.nline; j <= sel_range_end.nline; j++) 
  {
    if(j == sel_range_start.nline) 
    { 
      k = sel_range_start.nchar;
      l = totalCharsInLine[j]-1;
    } 
    else 
    {
      k = 0;
      if(j == sel_range_end.nline) l = sel_range_end.nchar-1;
      else l = totalCharsInLine[j]-1;
    }
    for(i = k; i <= l; i++) 
    {
      clipboard[pos++] = lines[j][i*4];
      clipboard[pos++] = lines[j][i*4+1];
      clipboard[pos++] = lines[j][i*4+2];
      clipboard[pos++] = lines[j][i*4+3];
    }
    if(!LINE_IS_LINKED[j]) 
    { 
      clipboard[pos++] = '\n';
      clipboard[pos++] = ' ';
      clipboard[pos++] = ' ';
      clipboard[pos++] = ' ';
      total_lines_in_clipboard++;
    }
  }//end for
  clipboard[pos] = '\0';
  CLIPBOARD_IS_EMPTY = 0;
  
  if(swap == 1) swap_lines();//return them back to normal
  if(swap == 2) swap_chars();//return them back to normal
 }//end if
}//end function

/*********************************************
 * Copies selection to clipboard then cuts it.
 * *******************************************/
void editMenu_Cut() 
{
  if(SELECTING || SELECTED) 
  {
    editMenu_Copy();
    remove_selected_text(1);
  }
}//end minoMenu_Cut()

/*****************************************
 * Removes the selected range of text.
 * ***************************************/
void remove_selected_text(bool RECORD) 
{
  int i, j, posLine, posLine2, posChar, posChar2;
  int swap = 0;
  if(sel_range_start.nline > sel_range_end.nline) { swap = 1; swap_lines(); }
  else if(sel_range_start.nline == sel_range_end.nline &&
	  sel_range_start.nchar > sel_range_end.nchar) { swap = 2; swap_chars(); }

  if(RECORD) 
  {
      if(RECORDING_UNDO_ACTION) finish_undo_action();
      int tmp1, tmp2, tmp3;
      tmp1 = firstVisLine; tmp2 = selectedLine; tmp3 = selectedChar;
      firstVisLine = 0; selectedLine = sel_range_start.nline;
      selectedChar = sel_range_start.nchar;
      begin_undo_action(UNDO_ACTION_DELETE);
      firstVisLine = tmp1; selectedLine = tmp2; selectedChar = tmp3;
  }//end RECORD
  
  posLine = sel_range_start.nline;
  posLine2 = sel_range_end.nline;
  posChar = sel_range_start.nchar;
  posChar2 = sel_range_end.nchar;
  
  if(posLine == posLine2) 
  {//if #1
    //if deleting in one line
    if(RECORD)
    {
      int t; t = selectedChar;
      selectedChar = sel_range_start.nchar;
      for(i = posChar; i < posChar2; i++)
      { 
	add_to_undo_action(lines[posLine]+(i*4));
	selectedChar++; 
      }
      selectedChar = t;
    } /* finish RECORD */
    strcpy(tmp, lines[posLine]+(posChar2*4));
    strcpy(lines[posLine]+(posChar*4), tmp);
    checkTabsInLine(posLine);
    int old_len = strlen(lines[posLine]);
    fixLineLength(posLine);
    selectedChar = posChar;
    calcCharCarry(posLine);
    if(old_len != strlen(lines[posLine])) refreshView();
    else refreshSelectedLine();
  }
  else
  {
    //if deleting on multiple lines
    if(RECORD)
    {
	int t = selectedChar; int t2 = firstVisLine; int t3 = selectedLine;
	firstVisLine = 0; selectedLine = posLine; selectedChar = posChar;
	i = posLine; j = posChar*4;
	while(1)
	{
	  selectedChar++;
	  add_to_undo_action(lines[i]+j); j += 4;
	  if(j >= strlen(lines[i]))
	  {
	    i++; j = 0; selectedChar = 0; selectedLine++;
	    //add_to_undo_action("\n   ");
	  }
	  if(i >= totalLines) break;
	  if(i >= posLine2 && j >= posChar2*4) break;
	}
	selectedChar = t; firstVisLine = t2; selectedLine = t3;
	if(undo_text[last_undo_action][strlen(undo_text[last_undo_action])-4] == '\n')
	  undo_action_end[last_undo_action].nchar = 0;
	undo_action_end[last_undo_action].nline = posLine2;
    } /* finish RECORD */
    strcpy(tmp, lines[posLine2]+(posChar2*4));
    lines[posLine][posChar*4] = '\0';
    calcTotalCharsInLine(posLine);	/* make sure we have the correct number */
    int howmany = MAX_CHARS_PER_LINE-totalCharsInLine[posLine];
    strncat(lines[posLine], tmp, howmany*4);
    int len = strlen(tmp)/4;
    LINE_IS_LINKED[posLine] = LINE_IS_LINKED[posLine2];
    if(howmany < len)
    {
      strcpy(tmp2, tmp+(howmany*4));
      strcpy(lines[posLine+1], tmp2);
      /* make it zero for the calculation below it */
      LINE_IS_LINKED[posLine] = 0;
      checkTabsInLine(posLine);
      LINE_IS_LINKED[posLine] = 1;
      LINE_IS_LINKED[posLine+1] = LINE_IS_LINKED[posLine2];
      posLine++;
    }
    checkTabsInLine(posLine);
    int old_posLine = posLine;
    /* 
     * Now, the grand shift!
     * Shift all lines up the difference between
     * posLine2 and posLine.
     */
    posLine++;
    posLine2++;
    while(posLine2 < totalLines)
    {
      strcpy(lines[posLine], lines[posLine2]);
      LINE_IS_LINKED[posLine] = LINE_IS_LINKED[posLine2];
      totalCharsInLine[posLine] = totalCharsInLine[posLine2];
      posLine++;
      posLine2++;
    }
    int diff = posLine2-posLine;
    totalLines -= diff;
    fixLineLength(old_posLine);
    posLine = old_posLine;
  }
  

    SELECTING = 0; SELECTED = 0;
    FILE_STATE = MODIFIED;
    //posChar = j; posLine = i;
    selectedChar = posChar;
    //adjust the view
    selectedLine = posLine-firstVisLine;
    if(selectedLine < 0) 
    {
      firstVisLine = posLine; selectedLine = 0;
    }
    else if(selectedLine >= totalVisLines) 
    {
      firstVisLine = posLine-totalVisLines-1;
      selectedLine = totalVisLines-1;
    }
    if(RECORD) finish_undo_action();
    refreshView();
    
  if(swap == 1) swap_lines();//return them back to normal
  if(swap == 2) swap_chars();//return them back to normal
}

/*****************************************
 * Pastes whatever in the clipboard into
 * the current position.
 * ***************************************/
void editMenu_Paste() 
{
  if(CLIPBOARD_IS_EMPTY) return;
  int i = 0;
  int j = firstVisLine+selectedLine;
  int k = selectedChar;
  int l;
  if((total_lines_in_clipboard+totalLines) > MAX_LINES) l = MAX_LINES - totalLines;
  else l = total_lines_in_clipboard;
  l--;
  
  if(RECORDING_UNDO_ACTION) finish_undo_action();
  begin_undo_action(UNDO_ACTION_INSERT);
  
  //if pasting in the middle of a line, save the rest of the line
  char *tmp = (char *) malloc(MAX_CHARS_PER_LINE*4); 
  strcpy(tmp, "");
  int n;
  if(k < strlen(lines[j])/4) 
  { 
    strcpy(tmp, lines[j]+(k*4));
  }
  n = l+1;
  
  for(i = totalLines+l; i > j+l; i--) 
  {
    strcpy(lines[i], lines[i-l]);
    totalCharsInLine[i] = strlen(lines[i])/4;
    LINE_IS_LINKED[i] = LINE_IS_LINKED[i-l];
  }
  i = 0;
  l += j+1;
  
  while(j <= l) 
  {
    if(k >= MAX_CHARS_PER_LINE) 
    { 
      int m;
      for(m = totalLines; m > j+1; m--) 
      {
	strcpy(lines[m], lines[m-1]);
	totalCharsInLine[m] = strlen(lines[m])/4;
	LINE_IS_LINKED[m] = LINE_IS_LINKED[m-1];
      } totalLines++; n++;
      LINE_IS_LINKED[j] = 1;
      lines[j][k*4] = '\0';
      totalCharsInLine[j] = strlen(lines[j])/4;
      j++; k = 0; 
    }
    add_to_undo_action(clipboard+i);
    if(clipboard[i] == '\0') { lines[j][k*4] = '\0'; break; }
    if(clipboard[i] == '\n') 
    { 
      LINE_IS_LINKED[j] = 0;
      lines[j][k*4] = '\0'; 
      totalCharsInLine[j] = strlen(lines[j])/4;
      j++; k = 0; i+=4; selectedLine++;
    } else { 
      lines[j][k*4] = clipboard[i];
      lines[j][k*4+1] = clipboard[i+1];
      lines[j][k*4+2] = clipboard[i+2];
      lines[j][k*4+3] = clipboard[i+3];
      k++; i+=4; 
    }
  }//end while
  totalLines += n;
  selectedLine += n;
  
  //if there is text in tmp, append it to the last pasted line
  if(strlen(tmp)) 
  {
    strcat(lines[j], tmp);
      
    if(strlen(lines[j])/4 > MAX_CHARS_PER_LINE) 
    {
      for(i = totalLines; i > j+1; i--) 
      {
	strcpy(lines[i], lines[i-1]);
	totalCharsInLine[i] = strlen(lines[i])/4;
	LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
      }
      strcpy(lines[j+1], lines[j]+(MAX_CHARS_PER_LINE*4));
      lines[j][MAX_CHARS_PER_LINE*4] = '\0';
      LINE_IS_LINKED[j] = 1;
      totalCharsInLine[j+1] = strlen(lines[j+1])/4;
      totalLines++;
    }
    free(tmp);
  }
  
  totalCharsInLine[j] = strlen(lines[j])/4;
  checkTabsInLine(j);
  //adjust the view
  if(selectedLine > totalVisLines-1) 
  {
    firstVisLine += (selectedLine-totalVisLines+1);
    selectedLine -= (selectedLine-totalVisLines+1);
  }
  if(totalLines <= totalVisLines) 
  {
    firstVisLine = 0; selectedLine = totalLines-1;
  } 
  else if((totalLines-j) < totalVisLines) 
  {
    firstVisLine = totalLines-totalVisLines;
    selectedLine = totalVisLines-(totalLines-j)-1;
  }
  selectedChar = k;
  finish_undo_action();
  SELECTED = 0; SELECTING = 0;
  FILE_STATE = MODIFIED;
  refreshView();
}

/*****************************************
 * 
 * ***************************************/
void editMenu_SelectAll() 
{
  SELECTING = 1;
  sel_range_start.nline = 0;
  sel_range_end.nline = totalLines-1;
  sel_range_start.nchar = 0;
  sel_range_end.nchar = strlen(lines[totalLines-1]);
  if(sel_range_end.nchar < 0) sel_range_end.nchar = 0;
  if(totalLines <= totalVisLines) 
  {
    selectedLine = totalLines-1;
    selectedChar = totalCharsInLine[selectedLine];
  } 
  else 
  {
    firstVisLine = totalLines-totalVisLines;
    selectedLine = totalVisLines-1;
    selectedChar = totalCharsInLine[firstVisLine+selectedLine];
  }
  refreshView();
  SELECTING = 0;
  SELECTED = 1;
  refreshBottomView();
}

/************************************************
 * Undo the last action done. Last action can be:
 * UNDO_ACTION_REPLACE: The user replaced a
 * 			character with INSERT on
 * UNDO_ACTION_INSERT: The user typed regularly
 * UNDO_ACTION_DELETE: The user deleted using
 * 			DEL or BACKSPACE
 * **********************************************/
void editMenu_Undo() 
{
  if(last_undo_action < 0) return;
  if(RECORDING_UNDO_ACTION) finish_undo_action();
  if(undo_action[last_undo_action] == UNDO_ACTION_REPLACE) 
  {
    int i = strlen(undo_text[last_undo_action])/4;
    int j = strlen(undo_text_replace[last_undo_action])/4;
    int k, l = 0;
    if(i == j) 
    {//if #2
      for(k = undo_action_start[last_undo_action].nchar;
	  k <= undo_action_end[last_undo_action].nchar; k++)
	  {
	  lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text[last_undo_action][l*4];
	  lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text[last_undo_action][l*4+1];
	  lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text[last_undo_action][l*4+2];
	  lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text[last_undo_action][l*4+3];
		l++;
	  }
	  if(k == strlen(lines[undo_action_start[last_undo_action].nline])/4)
	    lines[undo_action_start[last_undo_action].nline][k*4] = '\0';
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    } else if(i < j) {
      //THE REPLACEMENT TEXT IS LONGER THAN THE ORIGINAL TEXT
      for(k = undo_action_start[last_undo_action].nchar;
	  k < undo_action_start[last_undo_action].nchar+i; k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text[last_undo_action][l*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text[last_undo_action][l*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text[last_undo_action][l*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text[last_undo_action][l*4+3];
	    l++;
	  }
      l = k;
      for(k = l; k < totalCharsInLine[undo_action_start[last_undo_action].nline]-(j-i); k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text[last_undo_action][(k+j-i)*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text[last_undo_action][(k+j-i)*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text[last_undo_action][(k+j-i)*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text[last_undo_action][(k+j-i)*4+3];
	  }
      lines[undo_action_start[last_undo_action].nline][k*4] = '\0';
      totalCharsInLine[undo_action_start[last_undo_action].nline] =
	  strlen(lines[undo_action_start[last_undo_action].nline])/4;
      //check to see if the line is linked..
      l = undo_action_start[last_undo_action].nline;
      while(LINE_IS_LINKED[l]) 
      {
	j = MAX_CHARS_PER_LINE-(strlen(lines[l])/4);
	strncat(lines[l], lines[l+1], j*4);
	totalCharsInLine[l] = strlen(lines[l])/4;
	//if line is short remove it
	if(strlen(lines[l+1])/4 < j) {
	  for(i = l+1; i < totalLines-1; i++) 
	  {
	    strcpy(lines[i], lines[i+1]);
	    totalCharsInLine[i] = strlen(lines[i])/4;
	    LINE_IS_LINKED[i] = LINE_IS_LINKED[i+1];
	  } totalLines--;
	  LINE_IS_LINKED[l] = 0; break;
	}
	strcpy(lines[l+1], lines[l+1]+(j*4));
	totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	l++; if(l >= totalLines) break;
      }
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    //////////////////////////////////////////////////////
    } else {
      //THE REPLACEMENT TEXT IS SHORTER THAN THE ORIGINAL TEXT
      l = undo_action_start[last_undo_action].nchar+i-j;
      int m = (strlen(lines[undo_action_start[last_undo_action].nline])/4)+i-j;
      for(k = m; k > l; k--)
      {
	  lines[undo_action_start[last_undo_action].nline][k*4] =
	    lines[undo_action_start[last_undo_action].nline][(k-(i-j))*4];
	  lines[undo_action_start[last_undo_action].nline][k*4+1] =
	    lines[undo_action_start[last_undo_action].nline][(k-(i-j))*4+1];
	  lines[undo_action_start[last_undo_action].nline][k*4+2] =
	    lines[undo_action_start[last_undo_action].nline][(k-(i-j))*4+2];
	  lines[undo_action_start[last_undo_action].nline][k*4+3] =
	    lines[undo_action_start[last_undo_action].nline][(k-(i-j))*4+3];
      }
      lines[undo_action_start[last_undo_action].nline][(m+1)*4] = '\0';
      l = 0;
      for(k = undo_action_start[last_undo_action].nchar;
	  k <= undo_action_start[last_undo_action].nchar+i-j; k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text[last_undo_action][l*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text[last_undo_action][l*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text[last_undo_action][l*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text[last_undo_action][l*4+3];
	    l++;
	  }
      //if the line becomes too long, check to see if the line is 
      //linked. If it is, grab characters from the next line and
      //append them here.. continue till we hit a nonlinked line.
      l = undo_action_start[last_undo_action].nline;
      if(totalCharsInLine[l] > MAX_CHARS_PER_LINE) 
      {//if1
	if(LINE_IS_LINKED[l]) 
	{//if2
	  while(LINE_IS_LINKED[l]) 
	  {
	    strcpy(tmp, lines[l+1]);
	    strcpy(lines[l+1], lines[l]+(MAX_CHARS_PER_LINE*4));
	    strcat(lines[l+1], tmp);
	    lines[l][MAX_CHARS_PER_LINE*4] = '\0';
	    l++; if(l >= totalLines) break;
	  }//end while
	  //check the last line is not too long
	  if(totalCharsInLine[--l]/4 > MAX_CHARS_PER_LINE) 
	  {
	    //make some room
	    for(i = totalLines; i > l+1; i++) 
	    {
	      strcpy(lines[i], lines[i-1]);
	      totalCharsInLine[i] = strlen(lines[i])/4;
	      LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
	    }
	    strcpy(lines[l+1], lines[l]+(MAX_CHARS_PER_LINE*4));
	    lines[l][MAX_CHARS_PER_LINE*4] = '\0';
	    LINE_IS_LINKED[l] = 1;
	    totalCharsInLine[l] = strlen(lines[l])/4;
	    totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	  }
	} 
	else 
	{
	}//end if2
      }//end if1
    ///////////////////////////////////////////////
    ///////////////////////////////////////////////
    ///////////////////////////////////////////////
    }//end if #2
    
    l = undo_action_start[last_undo_action].nline;
    selectedChar = undo_action_start[last_undo_action].nchar;
    //adjust the view
    selectedLine = l-firstVisLine;
    if(selectedLine < 0) 
    {
      firstVisLine = l; selectedLine = 0;
    } 
    else if(selectedLine >= totalVisLines) 
    {
      firstVisLine = l-totalVisLines;
      selectedLine = totalVisLines-1;
    }
    last_undo_action--;
    refreshView();
  } else if(undo_action[last_undo_action] == UNDO_ACTION_INSERT) {
    sel_range_start.nline = undo_action_start[last_undo_action].nline;
    sel_range_end.nline = undo_action_end[last_undo_action].nline;
    sel_range_start.nchar = undo_action_start[last_undo_action].nchar;
    sel_range_end.nchar = undo_action_end[last_undo_action].nchar;
    remove_selected_text(0);
    last_undo_action--;
    refreshView();
  } else if(undo_action[last_undo_action] == UNDO_ACTION_DELETE) {
    int linecount = undo_action_end[last_undo_action].nline -
	    undo_action_start[last_undo_action].nline;
	    
    /* Deletion was in one line */
    if(linecount == 0)
    {
      //int c1 = undo_action_end[last_undo_action].nchar;
      int c2 = undo_action_start[last_undo_action].nchar;
      int l1 = undo_action_end[last_undo_action].nline;
      if(c2*4 >= strlen(lines[l1])) tmp[0] = '\0';
      else strcpy(tmp, lines[l1]+(c2*4));
      lines[l1][c2*4] = '\0';
      strcat(lines[l1], undo_text[last_undo_action]);
      checkTabsInLine(l1);
      int howmany = MAX_CHARS_PER_LINE-totalCharsInLine[l1];
      int len = strlen(tmp);
      strncat(lines[l1], tmp, howmany);
      if(howmany < len)
      {
	strcpy(tmp2, tmp+(howmany*4));
	move_lines_down(totalLines, l1+1);
	strcpy(lines[l1+1], tmp2);
	LINE_IS_LINKED[l1+1] = LINE_IS_LINKED[l1];
	LINE_IS_LINKED[l1] = 1;
	checkTabsInLine(l1);
	checkTabsInLine(l1+1);
	fixLineLength(l1);
	refreshView();
      }
      else
      {
	checkTabsInLine(l1);
	fixLineLength(l1);
	refreshSelectedLine();
      }
      last_undo_action--;
      return;
    }
    /* Deletion was on multiple lines */
    else
    {
      int c1 = undo_action_start[last_undo_action].nchar;
      int c2 = undo_action_end[last_undo_action].nchar;
      int l1 = undo_action_start[last_undo_action].nline;
      int l2 = undo_action_end[last_undo_action].nline;
      /* Add empty lines */
      int i, j, k;
      if(c1 == 0)
      {
	j = l1+linecount ;
	tmp[0] = '\0';
      }
      else
      {
	j = l1+linecount+1;
	strcpy(tmp, lines[l1]+(c1*4));
      }
      k = totalLines+linecount -1;
      for(i = k; i >= j; i--)
      {
	strcpy(lines[i], lines[i-linecount]);
	LINE_IS_LINKED[i] = LINE_IS_LINKED[i-linecount];
	totalCharsInLine[i] = totalCharsInLine[i-linecount];
      }
      totalLines += linecount;
      /* now put the chars back! */
      i = 0;
      c1 <<= 2;
      c2 <<= 2;
      while(l1 <= l2)
      {
	lines[l1][c1] = undo_text[last_undo_action][i];
	lines[l1][c1+1] = undo_text[last_undo_action][i+1];
	lines[l1][c1+2] = undo_text[last_undo_action][i+2];
	lines[l1][c1+3] = undo_text[last_undo_action][i+3];
	if(undo_text[last_undo_action][i] == '\n')
	{
	  lines[l1][c1+4] = '\0';
	  c1 = 0; l1++;
	}
	else { c1 += 4; }
	i += 4;
	if(l1 == l2 && c1 == c2) break;
      }
      strcat(lines[l1], tmp);
      checkAllTabs();
      refreshView();
      last_undo_action--;
      return;
    }
    refreshView();
  }//end if
}

/************************************************
 * Redo the last action done. Last action can be:
 * UNDO_ACTION_REPLACE: The user replaced a
 * 			character with INSERT on
 * UNDO_ACTION_INSERT: The user typed regularly
 * UNDO_ACTION_DELETE: The user deleted using
 * 			DEL or BACKSPACE
 * **********************************************/
void editMenu_Redo() 
{
  if(last_undo_action < -1) return;
  if(RECORDING_UNDO_ACTION) finish_undo_action();
  last_undo_action++;
  if(last_undo_action >= total_undo_actions) return;
  if(undo_action[last_undo_action] == UNDO_ACTION_REPLACE) 
  {
    int i = strlen(undo_text[last_undo_action]);
    int j = strlen(undo_text_replace[last_undo_action]);
    int k, l = 0;
    if(i == j) 
    {//if #2
      for(k = undo_action_start[last_undo_action].nchar;
	  k <= undo_action_end[last_undo_action].nchar; k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text_replace[last_undo_action][l*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text_replace[last_undo_action][l*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text_replace[last_undo_action][l*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text_replace[last_undo_action][l*4+3];
	    l++;
	  }
	  if(k == strlen(lines[undo_action_start[last_undo_action].nline])/4)
	    lines[undo_action_start[last_undo_action].nline][k*4] = '\0';
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    } else if(j < i) {
      for(k = undo_action_start[last_undo_action].nchar;
	  k < undo_action_start[last_undo_action].nchar+j; k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text_replace[last_undo_action][l*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text_replace[last_undo_action][l*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text_replace[last_undo_action][l*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text_replace[last_undo_action][l*4+3];
	    l++;
	  }
      l = k;
      for(k = l; k < (strlen(lines[undo_action_start[last_undo_action].nline])/4)-(i-j); k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text_replace[last_undo_action][(k+i-j)*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text_replace[last_undo_action][(k+i-j)*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text_replace[last_undo_action][(k+i-j)*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text_replace[last_undo_action][(k+i-j)*4+3];
	  }
      lines[undo_action_start[last_undo_action].nline][k*4] = '\0';
      totalCharsInLine[undo_action_start[last_undo_action].nline] =
	  strlen(lines[undo_action_start[last_undo_action].nline]);
      //check to see if the line is linked..
      l = undo_action_start[last_undo_action].nline;
      while(LINE_IS_LINKED[l]) 
      {
	j = MAX_CHARS_PER_LINE-(strlen(lines[l])/4);
	strncat(lines[l], lines[l+1], j*4);
	strcpy(lines[l+1], lines[l+1]+(j*4));
	totalCharsInLine[l] = strlen(lines[l])/4;
	totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	l++; if(l >= totalLines) break;
      }
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    } else {
      l = undo_action_end[last_undo_action].nchar;
      int m = (strlen(lines[undo_action_start[last_undo_action].nline])/4)+j-i;
      for(k = m; k > l; k--)
      {
	lines[undo_action_start[last_undo_action].nline][k*4] =
	  lines[undo_action_start[last_undo_action].nline][(k-(j-i))*4];
	lines[undo_action_start[last_undo_action].nline][k*4+1] =
	  lines[undo_action_start[last_undo_action].nline][(k-(j-i))*4+1];
	lines[undo_action_start[last_undo_action].nline][k*4+2] =
	  lines[undo_action_start[last_undo_action].nline][(k-(j-i))*4+2];
	lines[undo_action_start[last_undo_action].nline][k*4+3] =
	  lines[undo_action_start[last_undo_action].nline][(k-(j-i))*4+3];
      }
      lines[undo_action_start[last_undo_action].nline][(m+1)*4] = '\0';
      totalCharsInLine[undo_action_start[last_undo_action].nline] =
	  strlen(lines[undo_action_start[last_undo_action].nline])/4;
      l = 0;
      for(k = undo_action_start[last_undo_action].nchar;
	  k < undo_action_start[last_undo_action].nchar+j; k++)
	  {
	    lines[undo_action_start[last_undo_action].nline][k*4] =
		undo_text_replace[last_undo_action][l*4];
	    lines[undo_action_start[last_undo_action].nline][k*4+1] =
		undo_text_replace[last_undo_action][l*4+1];
	    lines[undo_action_start[last_undo_action].nline][k*4+2] =
		undo_text_replace[last_undo_action][l*4+2];
	    lines[undo_action_start[last_undo_action].nline][k*4+3] =
		undo_text_replace[last_undo_action][l*4+3];
	    l++;
	  }
      //if the line becomes too long, check to see if the line is 
      //linked. If it is, grab characters from the next line and
      //append them here.. continue till we hit a nonlinked line.
      l = undo_action_start[last_undo_action].nline;
      if(totalCharsInLine[l] > MAX_CHARS_PER_LINE) 
      {//if1
	if(LINE_IS_LINKED[l]) 
	{//if2
	  char *tmp = (char *) malloc(MAX_CHARS_PER_LINE*4);
	  while(LINE_IS_LINKED[l]) 
	  {
	    strcpy(tmp, lines[l+1]);
	    strcpy(lines[l+1], lines[l]+(MAX_CHARS_PER_LINE*4));
	    lines[l][MAX_CHARS_PER_LINE*4] = '\0';
	    totalCharsInLine[l] = strlen(lines[l])/4;
	    strcat(lines[l+1], tmp);
	    totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	    l++; if(l >= totalLines) break;
	  }//end while
	  //check the last line is not too long
	  if(totalCharsInLine[l]/4 > MAX_CHARS_PER_LINE) 
	  {
	    //make some room
	    for(i = totalLines; i > l+1; i++) 
	    {
	      strcpy(lines[i], lines[i-1]);
	      totalCharsInLine[i] = strlen(lines[i])/4;
	      LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
	    }
	    strcpy(lines[l+1], lines[l]+(MAX_CHARS_PER_LINE*4));
	    lines[l][MAX_CHARS_PER_LINE*4] = '\0';
	    LINE_IS_LINKED[l] = 1;
	    totalCharsInLine[l] = strlen(lines[l])/4;
	    totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	  }
	  free(tmp);
	} else {
	  for(i = totalLines; i > l+1; i--) 
	  {
	    strcpy(lines[i], lines[i-1]);
	    totalCharsInLine[i] = strlen(lines[i])/4;
	    LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
	  } totalLines++;
	  strcpy(lines[l+1], lines[l]+(MAX_CHARS_PER_LINE*4));
	  lines[l][MAX_CHARS_PER_LINE*4] = '\0';
	  totalCharsInLine[l] = strlen(lines[l])/4;
	  totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	  LINE_IS_LINKED[l] = 1;
	}//end if2
      }//end if1
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    /////////////////////////////////////////////////
    }//end if #2
    l = undo_action_start[last_undo_action].nline;
    selectedChar = undo_action_start[last_undo_action].nchar;
    selectedLine = l-firstVisLine;
    if(selectedLine < 0) 
    {
      firstVisLine = l; selectedLine = 0;
    } else if(selectedLine >= totalVisLines) {
      firstVisLine = l-totalVisLines;
      selectedLine = totalVisLines-1;
    }
    refreshView();
  } else if(undo_action[last_undo_action] == UNDO_ACTION_DELETE) {
    sel_range_start.nline = undo_action_start[last_undo_action].nline;
    sel_range_end.nline = undo_action_end[last_undo_action].nline;
    sel_range_start.nchar = undo_action_start[last_undo_action].nchar;
    sel_range_end.nchar = undo_action_end[last_undo_action].nchar;
    if((sel_range_start.nline != sel_range_end.nline) && sel_range_end.nchar) sel_range_end.nchar++;
    remove_selected_text(0);
    refreshView();
  } else if(undo_action[last_undo_action] ==  UNDO_ACTION_INSERT) {
    int l, i = undo_action_end[last_undo_action].nline -
	    undo_action_start[last_undo_action].nline;
    if(i)
      for(l = totalLines+i; l > undo_action_end[last_undo_action].nline; l--) {
	strcpy(lines[l], lines[l-i]);
	totalCharsInLine[l] = strlen(lines[l])/4;
	LINE_IS_LINKED[l] = LINE_IS_LINKED[l-i];
      }
    int j = undo_action_start[last_undo_action].nchar;
    int k = undo_action_end[last_undo_action].nchar;
    
    totalLines += i;
    l = undo_action_start[last_undo_action].nline;
    //save the rest of the line to reattach it later
    char *tmp = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
    strcpy(tmp, "");
    strcpy(tmp, lines[l]+(j*4));
    int ltmp = LINE_IS_LINKED[l];
    i = 0;
    while(l <= undo_action_end[last_undo_action].nline) 
    {
      if(undo_text[last_undo_action][i] == '\n') 
      { LINE_IS_LINKED[l] = 0; lines[l][j*4] = '\0'; l++; j = 0; i+=4; 
	checkTabsInLine(l-1); }
      if(j > MAX_CHARS_PER_LINE) { LINE_IS_LINKED[l] = 1; l++; j = 0; }
      if(l >= undo_action_end[last_undo_action].nline && j >= k) break;
      lines[l][j*4] = undo_text[last_undo_action][i];
      lines[l][j*4+1] = undo_text[last_undo_action][i+1];
      lines[l][j*4+2] = undo_text[last_undo_action][i+2];
      lines[l][j*4+3] = undo_text[last_undo_action][i+3];
      i+=4; j++;
    }//end while
    lines[l][j*4] = '\0';
    int ptmp = l;
    //reattach the rest of line we saved earlier
    if(strlen(tmp) != 0) 
    {
      k = MAX_CHARS_PER_LINE-(strlen(lines[l])/4);
      strncat(lines[l], tmp, k*4);
      if(strlen(tmp)/4 > k) {
	for(i = totalLines; i > l+1; i--) 
	{
	  strcpy(lines[i], lines[i-1]);
	  totalCharsInLine[i] = strlen(lines[i])/4;
	  LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
	} totalLines++;
	for(i = 0; i < (strlen(tmp)/4)-k; i++)
	{
	  tmp[i*4] = tmp[(i+k)*4];
	  tmp[i*4+1] = tmp[(i+k)*4+1];
	  tmp[i*4+2] = tmp[(i+k)*4+2];
	  tmp[i*4+3] = tmp[(i+k)*4+3];
	}
	tmp[i*4] = '\0';
	strcpy(lines[l+1], tmp);
	totalCharsInLine[l+1] = strlen(lines[l+1])/4;
	LINE_IS_LINKED[l] = 1;
	ptmp++;
      }
    }
    totalCharsInLine[l] = strlen(lines[l])/4;
    //if the first line was linked, relink
    while(ltmp) 
    {
      k = MAX_CHARS_PER_LINE-(strlen(lines[ptmp])/4);
      strncat(lines[ptmp], lines[ptmp+1], k*4);
      //if line is short, remove it
      if(strlen(lines[ptmp+1])/4 < k) 
      {
	for(i = ptmp+1; i < totalLines-1; i++) 
	{
	  strcpy(lines[i], lines[i+1]);
	  totalCharsInLine[i] = strlen(lines[i])/4;
	  LINE_IS_LINKED[i] = LINE_IS_LINKED[i+1];
	} totalLines--;
	LINE_IS_LINKED[ptmp] = 0; break;
      }
      ltmp = LINE_IS_LINKED[++ptmp];
      for(i = k; i < strlen(lines[ptmp])/4; i++)
      {
	lines[ptmp][i*4] = lines[ptmp][(i+1)*4];
	lines[ptmp][i*4+1] = lines[ptmp][(i+1)*4+1];
	lines[ptmp][i*4+2] = lines[ptmp][(i+1)*4+2];
	lines[ptmp][i*4+3] = lines[ptmp][(i+1)*4+3];
      }
      totalCharsInLine[ptmp] = strlen(lines[ptmp])/4;
    }
    free(tmp);
    selectedChar = j;
    //adjust the view
    ptmp = l;
    selectedLine = ptmp-firstVisLine;
    if(selectedLine < 0) 
    {
      firstVisLine = ptmp; selectedLine = 0;
    } else if(selectedLine >= totalVisLines) {
      firstVisLine = ptmp-totalVisLines;
      selectedLine = totalVisLines-1;
    }
    refreshView();
  }//end if
}

void begin_undo_action(undoActionType utype) 
{
  if(last_undo_action >= MAX_UNDO_ACTIONS-1) 
  {
    //the the undo queue is full, move all members down one
    //place to make room for a new undo action.
    int i;
    free(undo_text[0]);
    free(undo_text_replace[0]);
    for(i = 0; i < MAX_UNDO_ACTIONS-1; i++) 
    {
      undo_action[i] = undo_action[i+1];
      strcpy(undo_text[i], undo_text[i+1]);
      strcpy(undo_text_replace[i], undo_text_replace[i+1]);
      undo_action_start[i] = undo_action_start[i+1];
      undo_action_end[i] = undo_action_end[i+1];
    } last_undo_action = MAX_UNDO_ACTIONS-2;
  }
  last_undo_action++;
  total_undo_actions = last_undo_action+1;
  undo_action_start[last_undo_action].nline = firstVisLine+selectedLine;
  undo_action_start[last_undo_action].nchar = selectedChar;
  
  undo_text[last_undo_action] = (char *) malloc(MAX_CHARS_PER_LINE*4);
  /*
   * FIXME: We should be tolerant to this error and not to exit abruptly like this.
   */
  if(!undo_text[last_undo_action]) { msgBox("Insufficient memory", OK, ERROR); exit(1); }
  undo_text_replace[last_undo_action] = (char *) malloc(MAX_CHARS_PER_LINE*4);
  /*
   * FIXME: We should be tolerant to this error and not to exit abruptly like this.
   */
  if(!undo_text_replace[last_undo_action]) { msgBox("Insufficient memory", OK, ERROR); exit(1); }
  
  strcpy(undo_text[last_undo_action], "\0");
  strcpy(undo_text_replace[last_undo_action], "\0");
  undo_action[last_undo_action] = utype;
  RECORDING_UNDO_ACTION = 1;
}

void finish_undo_action()
{
  int tmp;
  if(undo_action_end[last_undo_action].nline < undo_action_start[last_undo_action].nline) 
  {
    tmp = undo_action_end[last_undo_action].nline;
    undo_action_end[last_undo_action].nline = undo_action_start[last_undo_action].nline;
    undo_action_start[last_undo_action].nline = tmp;
  } 
  else if(undo_action_end[last_undo_action].nline == undo_action_start[last_undo_action].nline) 
  {
    if(undo_action_end[last_undo_action].nchar < undo_action_start[last_undo_action].nchar) 
    {
      tmp = undo_action_end[last_undo_action].nchar;
      undo_action_end[last_undo_action].nchar = undo_action_start[last_undo_action].nchar;
      undo_action_start[last_undo_action].nchar = tmp;
    }
  }
  RECORDING_UNDO_ACTION = 0;
}

void add_to_undo_action(char *ch) 
{
  int i = strlen(undo_text[last_undo_action]);
  undo_text[last_undo_action][i] = ch[0];
  undo_text[last_undo_action][i+1] = ch[1];
  undo_text[last_undo_action][i+2] = ch[2];
  undo_text[last_undo_action][i+3] = ch[3];
  undo_text[last_undo_action][i+4] = '\0';
  undo_action_end[last_undo_action].nline = firstVisLine+selectedLine;
  undo_action_end[last_undo_action].nchar = selectedChar;
}


/*****************************************
 * Finds a string text in the file.
 * ***************************************/
void editMenu_Find() 
{
  inputBox("Enter text to find:  ", " Find ");
  if(!strlen(input))
  {
    refreshView();
    return;
  }//end if
  char *f = (char *)malloc(strlen(input));
  if(!f) { msgBox("Insufficient memory!", OK, ERROR); return; }
  strcpy(f, input);
  
  int i;
  _find(&f);
  
  if(!total_find_results) 
  {
    msgBox("No matches were found.", OK, INFO);
    refreshView();
    return;
  }
  
  //infinite loop to get user input
  i = 0;
  char *c = (char *)malloc(5);
  if(!c) { msgBox("Insufficient memory!", OK, ERROR); return; }
  while(1) 
  {
    int x = find_result_pos[i].nline-firstVisLine;
    if(x < 0)
    {
      firstVisLine += x;
      selectedLine = 0;
    }
    else
    {
      if(x >= totalVisLines)
      {
	x -= totalVisLines;
	firstVisLine += (x+1);
	selectedLine = totalVisLines-1;
      }
      else selectedLine = x;
    }
    selectedChar = find_result_pos[i].nchar;
    calcCharCarry(find_result_pos[i].nline);
    refreshView();
    setScreenColors(FG_COLOR[COLOR_STATUS_BAR], BG_COLOR[COLOR_STATUS_BAR]);
    if(GNU_DOS_LEVEL > 2)
      printf("\e[%d;1HFind(%d/%d): [C-p] Prev [C-n] Next [C-g] Cancel",
	   SCREEN_H, i+1, total_find_results);
    else if(GNU_DOS_LEVEL > 1)
      printf("\e[%d;1HFind(%d/%d): [C-p] Prev [C-n] Next [ESC] Cancel",
	   SCREEN_H, i+1, total_find_results);
    else
      printf("\e[%d;1HFind(%d/%d): [Up] Prev [Down] Next [ESC] Cancel",
	   SCREEN_H, i+1, total_find_results);
    fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
    fflush(stdout);
get_key:
    c = getKey();
    switch(c[0]) 
    {
      case('p'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_up;
      case(UP_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_up:
	if(i <= 0) i = total_find_results-1;
	else i--;
	break;
      case('n'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_down;
      case(DOWN_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_down:
      case(ENTER_KEY):
      case(SPACE_KEY):
	if(i >= total_find_results-1) i = 0;
	else i++;
	break;
      case('g'):
	if(GNU_DOS_LEVEL < 3) break;
	if(!CTRL) break;
	goto do_esc;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
	refreshView();
	return;
	break;
      default:
	goto get_key;
	break;
    }//end switch
  }//end while
}


/*****************************************
 * Replaces Find text with Replace text.
 * ***************************************/
void editMenu_Replace() 
{
  inputBox("Enter text to find:  ", " Find ");
  if(!strlen(input))
  {
    refreshView();
    return;
  }//end if
  char *f = (char *)malloc(strlen(input));
  if(!f) { msgBox("Insufficient memory!", OK, ERROR); return; }
  strcpy(f, input);
  inputBox("Enter replacement text: ", " Replace ");
  if(!strlen(input))
  {
    refreshView();
    return;
  }//end if
  char *r = (char *)malloc(strlen(input));
  if(!r) { msgBox("Insufficient memory!", OK, ERROR); return; }
  strcpy(r, input);
  
  int i;
  _find(&f);
  
  if(!total_find_results) 
  {
    msgBox("No matches were found.", OK, INFO);
    refreshView();
    return;
  }
  
  //infinite loop to get user input
  i = 0;
  char c2[5];
  char *c = c2;
  while(1) 
  {
    int x = find_result_pos[i].nline-firstVisLine;
    if(x < 0)
    {
      firstVisLine += x;
      selectedLine = 0;
    }
    else
    {
      if(x >= totalVisLines)
      {
	x -= totalVisLines;
	firstVisLine += (x+1);
	selectedLine = totalVisLines-1;
      }
      else selectedLine = x;
    }
    selectedChar = find_result_pos[i].nchar;
    calcCharCarry(find_result_pos[i].nline);
    refreshView();
    setScreenColors(FG_COLOR[COLOR_STATUS_BAR], BG_COLOR[COLOR_STATUS_BAR]);
    if(GNU_DOS_LEVEL > 2)
      printf("\e[%d;1HFind(%d/%d): [ENTER] Replace [A] Replace All [C-g] Cancel",
	   SCREEN_H, i+1, total_find_results);
    else
      printf("\e[%d;1HFind(%d/%d): [ENTER] Replace [A] Replace All [ESC] Cancel",
	   SCREEN_H, i+1, total_find_results);
    fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
    fflush(stdout);
get_key:
    c = getKey();
    switch(c[0]) 
    {
      case('p'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_up;
      case(UP_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_up:
	if(i <= 0) i = total_find_results-1;
	else i--;
	break;
      case('n'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_down;
      case(DOWN_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_down:
	if(i >= total_find_results-1) i = 0;
	else i++;
	break;
      case('g'):
	if(GNU_DOS_LEVEL < 3) break;
	if(!CTRL) break;
	goto do_esc;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
	refreshView();
	return;
	break;
      case(SPACE_KEY):
      case(ENTER_KEY):
	_replace(i, &f, &r);
	if(i >= total_find_results) i--;
	if(total_find_results <= 0) { refreshView(); return; }
	break;
      case('a'):
	_replace(-1, &f, &r);
	total_find_results = 0;
	refreshView();
	return;
	break;
      default:
	goto get_key;
	break;
    }//end switch
  }//end while
}

/*******************************************
 * Function is called by minoMenu_Replace()
 * to replace find result number 'pos' with
 * the replacement text.
 * *****************************************/
void _replace(int pos, char **f, char **r) 
{
  //pass position to replace in find_result_pos[] array,
  //or -1 to replace all find results.
  int i = strlen(*f);
  int j = strlen(*r);
  
  int old_firstVisLine = firstVisLine;
  int old_selectedLine = selectedLine;
  int old_selectedChar = selectedChar;
  int old_selectedCharCarry = selectedCharCarry;

  if(pos >= 0) 
  {//if #1
    _do_replace(pos, &(*f), &(*r));
    FILE_STATE = MODIFIED;
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////
  } else {	//-1 means replace all
    if(i == j)
    {
      for(pos = 0; pos < total_find_results; pos++)
      {
	_do_replace(pos, &(*f), &(*r));
      }
    }
    else
    {
      while(total_find_results)
      {
	_do_replace(0, &(*f), &(*r));
      }
    }
  }//end if #1
  firstVisLine = old_firstVisLine;
  selectedLine = old_selectedLine;
  selectedChar = old_selectedChar;
  selectedCharCarry = old_selectedCharCarry;
}

void _find(char **f)
{
  int i, k = 0;
  char *j, *l;
  total_find_results = 0;
  for(i = 0; i < totalLines; i++)
  {
    j = strstr(lines[i], *f);
    while(j) 
    { 
      find_result_pos[k].nline = i; 
      find_result_pos[k++].nchar = (j-lines[i])/4;
      total_find_results++;
      l = j+strlen(*f);
      j = strstr(l, *f);
    }//end while
  }//end for
}

void _do_replace(int pos, char **f, char **r)
{
    int i = strlen(*f);
    int j = strlen(*r);
    int k, l, n, m;
    k = find_result_pos[pos].nchar;
    l = find_result_pos[pos].nline;
    n = 0;
    if(RECORDING_UNDO_ACTION) finish_undo_action();
    begin_undo_action(UNDO_ACTION_REPLACE);
    firstVisLine = find_result_pos[pos].nline;
    selectedLine = 0;
    for(m = 0; m < i; m+=4)
      { add_to_undo_action(*f+m); selectedChar++; }
    strcpy(undo_text_replace[last_undo_action], *r);
    finish_undo_action();
    
    if(i == j)
    {//if #2
      /*
       * find & replace of same length
       */
      for(m = k*4; m < k*4+i; m++)
	lines[l][m] = *r[n++];
      /* shift the results up */
      for(m = pos; m < total_find_results-1; m++)
      {
	find_result_pos[m].nline = find_result_pos[m+1].nline;
	find_result_pos[m].nchar = find_result_pos[m+1].nchar;
      }//end for
      total_find_results--;
    }
    else if(i > j)
    {
      /*
       * find is longer than replace
       */
      /* copy whatever is after the 'find' text */
      strcpy(tmp, lines[l]+(k*4)+i);
      lines[l][k*4] = '\0';
      strcat(lines[l], *r);
      strcat(lines[l], tmp);
      checkTabsInLine(l);
      /* we will need to update search results */
      _find(&(*f));
    }
    else
    {
      /*
       * replace is longer than find
       */
      n = j-i;
      /* copy whatever is after the 'find' text */
      strcpy(tmp, lines[l]+(k*4)+i);
      lines[l][k*4] = '\0';
      if((strlen(lines[l])+j)/4 <= MAX_CHARS_PER_LINE)
      {
	strcat(lines[l], *r);
	/* bring the rest of the old line */
	if((strlen(lines[l])+strlen(tmp))/4 <= MAX_CHARS_PER_LINE)
	{
	  strcat(lines[l], tmp);
	  checkTabsInLine(l);
	}
	else
	{
	  fixLineLength(l);
	  int howmany = MAX_CHARS_PER_LINE-totalCharsInLine[l];
	  strncat(lines[l], tmp, howmany);
	  if(strlen(tmp) > howmany)
	  {
	    move_lines_down(totalLines, l+1);
	    strcpy(lines[l+1], tmp+(howmany*4));
	    LINE_IS_LINKED[l+1] = LINE_IS_LINKED[l];
	    LINE_IS_LINKED[l] = 1;
	  }
	  checkTabsInLine(l);
	}
      }
      else
      {
	fixLineLength(l);
	int howmany = MAX_CHARS_PER_LINE-totalCharsInLine[l];
	strncat(lines[l], *r, howmany);
	move_lines_down(totalLines, l+1);
	strcpy(lines[l+1], *r+(howmany*4));
	strcat(lines[l+1], tmp);
	LINE_IS_LINKED[l+1] = LINE_IS_LINKED[l];
	LINE_IS_LINKED[l] = 1;
	checkTabsInLine(l);
      }
      /* we will need to update search results */
      _find(&(*f));
    }//end if #2
    FILE_STATE = MODIFIED;
}

void checkAllTabs() 
{
  int i;
  for(i = 0; i < totalLines; i++)
    checkTabsInLine(i);
}

void checkTabsInLine(int pos) 
{
  int i, j, k, l;
  int carry = 0;
  int pos2 = pos;
  //here comes the real stuff
  for(l = 0; l < strlen(lines[pos]); l+=4)
    if(lines[pos][l] == '\t') 
    {
      j = TAB_CHARS-(((l/4)+carry)%TAB_CHARS);
      j = 0 ? TAB_CHARS : j;
      carry += (j-1);
      
      k = (strlen(lines[pos])/4)+carry;
      char *t = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
      
      if(k > MAX_CHARS_PER_LINE) 
      {
	if(LINE_IS_LINKED[pos]) 
	{
	  int j2;
	  j2 = MAX_CHARS_PER_LINE-(carry);
	  strcpy(t, lines[pos]+(j2*4));
	  lines[pos][j2*4] = '\0';
	  lines[pos][j2*4+1] = '\0';
	  lines[pos][j2*4+2] = '\0';
	  lines[pos][j2*4+3] = '\0';
	  int old_pos = pos;
	  while(LINE_IS_LINKED[pos]) 
	  {
	    strcpy(tmp, lines[pos+1]);
	    strcpy(lines[pos+1], t);
	    int howmany = strlen(tmp)/4;
	    if(howmany+carry > MAX_CHARS_PER_LINE)
	    {
	      howmany -= carry;
	      strcpy(t, tmp+(howmany*4));
	    }
	    strncat(lines[pos+1], tmp, howmany*4);
	    pos++; if(pos >= totalLines) break;
	  }//end while
	  pos2 = pos;
	  pos = old_pos;
	} 
	else 
	{//line is not linked, create a new line
	  move_lines_down(totalLines, pos+1);
	  int j2 = MAX_CHARS_PER_LINE-(carry);
	  strcpy(lines[pos+1], lines[pos]+(j2*4));
	  LINE_IS_LINKED[pos] = 1;
	  LINE_IS_LINKED[pos+1] = 0;
	  lines[pos][j2*4] = '\0';
	  lines[pos][j2*4+1] = '\0';
	  lines[pos][j2*4+2] = '\0';
	  lines[pos][j2*4+3] = '\0';
	}
      }
    }
    
    /*
     * FIXME: Do we need to calculate ALL lines' length?
     *        Isn't it costing CPU cycles?
     */
    for(i = pos; i <= pos2; i++) 
    //for(i = 0; i < totalLines; i++) 
    {
      totalCharsInLine[i] = 0;
      int carry = 0;
      for(j = 0; j < strlen(lines[i]); j+=4)
	if(lines[i][j] == '\n') break;
	else if(lines[i][j] == '\t') 
	{
	  k = TAB_CHARS-(((j/4)+carry)%TAB_CHARS); k=0?TAB_CHARS:k;
	  totalCharsInLine[i] += k; carry += (k-1);
	} else totalCharsInLine[i]++;
    }
  //free(tmp);
  return;
}//end checkTabsInLine()

void calcTotalCharsInLine(int pos)
{
  totalCharsInLine[pos] = 0;
  int carry = 0;
  int j, k;
  for(j = 0; j < strlen(lines[pos]); j+=4)
    if(lines[pos][j] == '\n') break;
    else if(lines[pos][j] == '\t')
    {
      k = TAB_CHARS-(((j/4)+carry)%TAB_CHARS); k=0?TAB_CHARS:k;
      totalCharsInLine[pos] += k; carry += (k-1);
    } else totalCharsInLine[pos]++;
}

void fixLineLength(int pos)
{
  totalCharsInLine[pos] = 0;
  int carry = 0;
  int j, k;
  for(j = 0; j < strlen(lines[pos]); j+=4)
    if(lines[pos][j] == '\n') break;
    else if(lines[pos][j] == '\t')
    {
      k = TAB_CHARS-(((j/4)+carry)%TAB_CHARS); k=0?TAB_CHARS:k;
      totalCharsInLine[pos] += k; carry += (k-1);
    } else totalCharsInLine[pos]++;
  
  /* fix short lines */
  if(totalCharsInLine[pos] < MAX_CHARS_PER_LINE && LINE_IS_LINKED[pos])
  {
    while(LINE_IS_LINKED[pos])
    {
      j = MAX_CHARS_PER_LINE - totalCharsInLine[pos];
      strncat(lines[pos], lines[pos+1], j*4);
      if(j >= totalCharsInLine[pos+1])
      {
	move_lines_up(pos+1, totalLines-1);
	LINE_IS_LINKED[pos] = 0;
	break;
      }
      strcpy(tmp, lines[pos+1]+(j*4));
      strcpy(lines[pos+1], tmp);
      calcTotalCharsInLine(pos+1);
      pos++;
    }
  }
  
  /* fix long lines */
  if(totalCharsInLine[pos] > MAX_CHARS_PER_LINE)
  {
    if(LINE_IS_LINKED[pos])
    {
      int j2;
      //int len = strlen(lines[pos])/4;
      j2 = MAX_CHARS_PER_LINE-(carry);
      strcpy(tmp2, lines[pos]+(j2*4));
      lines[pos][j2*4] = '\0';
      lines[pos][j2*4+1] = '\0';
      lines[pos][j2*4+2] = '\0';
      lines[pos][j2*4+3] = '\0';
      int old_pos = pos;
      while(LINE_IS_LINKED[pos])
      {
	strcpy(tmp, lines[pos+1]);
	strcpy(lines[pos+1], tmp2);
	int howmany = strlen(tmp)/4;
	if(howmany+carry > MAX_CHARS_PER_LINE)
	{
	  howmany -= carry;
	  strcpy(tmp2, tmp+(howmany*4));
	}
	strncat(lines[pos+1], tmp, howmany*4);
	pos++; if(pos >= totalLines) break;
      }//end while
      pos = old_pos;
    }
    else
    {//line is not linked, create a new line
      move_lines_down(totalLines, pos+1);
      int j2 = MAX_CHARS_PER_LINE-(carry);
      strcpy(lines[pos+1], lines[pos]+(j2*4));
      LINE_IS_LINKED[pos] = 1;
      LINE_IS_LINKED[pos+1] = 0;
      lines[pos][j2*4] = '\0';
      lines[pos][j2*4+1] = '\0';
      lines[pos][j2*4+2] = '\0';
      lines[pos][j2*4+3] = '\0';
    }
  }
}
