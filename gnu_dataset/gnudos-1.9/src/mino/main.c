/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015, 2016 (c)
 * 
 *    file: main.c
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
#include "file.h"
#include "kbd.h"
#include "options.h"
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <poll.h>
#include <signal.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <stdlib.h>

extern int fcloseall (void);    /* stdio.h */
extern char * strcasestr (const char *HAYSTACK, const char *NEEDLE);    /* string.h */

#define EXIT()		\
{			\
 restoreTerminal();	\
 fcloseall();		\
 exit(0);		\
}

char *tmp;
char *tmp2;

extern char *STARTUP_FILE_NAME;
extern void checkFileExtension();

void drawScrollBar();
int isKeyword(int pos, int start);

static sig_atomic_t end = 0;
FILE *NULL_DEV;

void sighandler(int signo)
{
    //fprintf(stdout, "SIGNAL %d received\n", signo);
    if(signo == 2) 
    {	//CTRL-C pressed
      editMenu_Copy();
    } 
    else 
    {
      end = 1;
    }
}
struct termios oldtio, curtio;
struct sigaction sa;

int main(int argc, char **argv) 
{
 NULL_DEV = fopen("/dev/null", "r");
 if(NULL_DEV == NULL)
   fprintf(stderr, "Failed to open NULL device\n");
 
 parseLineArgs(argc, argv);  
 init();			//some initialization code

 tmp = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
 tmp2 = (char *) malloc((MAX_CHARS_PER_LINE+TAB_CHARS)*4);
 if(!tmp || !tmp2) { fprintf(stderr, "Insufficient memory\n"); EXIT(); }
 
 clearScreen();			//clear the screen
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 drawMenuBar(1, 1, SCREEN_W);		//draw main menu bar
 //draw main window
 drawBox(2, 1, SCREEN_H-1, SCREEN_W, documentTitle, YES);
 refreshBottomView();

 if(open_file_at_startup) 
 {
      int i;
      for(i = 0; i < MAX_LINES; i++) 
      {
	lines[i] = (char *) malloc(MAX_CHARS_PER_LINE*4);
	if(!lines[i]) { fprintf(stderr, "Insufficient memory\n"); EXIT(); }
	strcpy(lines[i], "\0");
	totalCharsInLine[i] = 0;
	total_tabs = 0;
      } totalLines = 1;

      if(strrchr(STARTUP_FILE_NAME, '/'))
	strcpy(open_file_name, STARTUP_FILE_NAME);
      else 
      {
	strcpy(open_file_name, getcwd(NULL, 0));
	strcat(open_file_name, "/");
	strcat(open_file_name, STARTUP_FILE_NAME);
      }
      if(!openSaveFile(OPEN, NO)) 
      {//failed to open file
	NEW_FILE = 0; FILE_STATE = NEW;
	checkFileExtension();
      }
      else 
      { 
	FILE_STATE = OPENED; NEW_FILE = 0; 
      }
      if(strrchr(open_file_name, '/'))
	strcpy(documentTitle, strrchr(open_file_name, '/')+1);
      else strcpy(documentTitle, open_file_name);
      //checkFileExtension();
      getScreenSize();
      refreshView();
 }
  
 if(SHOW_README) showREADMEOnStartup();
 
 fprintf(stdout, "\e[3;2H");
 fflush(stdout);
 
 int char_inserted = 0;
 char *ch;
 ch = (char *) malloc(5);
 if(!ch) { fprintf(stderr, "Insufficient memory\n"); EXIT(); }
 while(!end) 
 {	//infinite program loop//
  //selectedCharCarry = 0;
  ch = getKey();
  switch(ch[0]) 
  {
   case(ESC_KEY):
      if(GNU_DOS_LEVEL > 2) break;
do_esc:
      SELECTED = 0; SELECTING = 0;
      refreshView();
      break;
   case(SHIFT_DOWN):
     if(GNU_DOS_LEVEL > 3) break;
do_shift_down:
     SELECTING = 1; SELECTED = 0;
     sel_range_start.nline = firstVisLine+selectedLine;
     sel_range_start.nchar = selectedChar;
     sel_range_end.nline = firstVisLine+selectedLine;
     sel_range_end.nchar = selectedChar;
     refreshBottomView();
     break;
   case(SHIFT_UP):
     if(GNU_DOS_LEVEL > 3) break;
//do_shift_up:
     SELECTING = 0; SELECTED = 1;
     if(char_inserted) { SELECTED = 0; char_inserted = 0; }
     refreshBottomView();
     break;
   case(INS_KEY):
     INSERT = !INSERT;
     char_inserted = 0;
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     refreshBottomView();
     break;
   case(CAPS_KEY):
     CAPS = !CAPS;
     char_inserted = 0;
     refreshBottomView();
     break;
   case(PGUP_KEY):
     if(GNU_DOS_LEVEL > 2) break;
do_pg_up:
     char_inserted = 0;
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     if(firstVisLine == 0) 
       selectedLine = 0;
     else if(firstVisLine-totalVisLines < 0) 
     {
       selectedLine = firstVisLine;
       firstVisLine = 0;
     } else 
       firstVisLine -= totalVisLines; 
     
     if(selectedChar > totalCharsInLine[firstVisLine+selectedLine])
       selectedChar = totalCharsInLine[firstVisLine+selectedLine];
     if(SELECTING) 
     {
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
     }
     if(SELECTED) SELECTED = 0;
     refreshView();
     fflush(stdout);
     break;
   case(PGDOWN_KEY):
     if(GNU_DOS_LEVEL > 2) break;
do_pg_down:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
     firstVisLine += totalVisLines;
     if(firstVisLine+totalVisLines >= totalLines) 
     {
       firstVisLine = totalLines-totalVisLines;
       selectedLine = totalVisLines-1;
     }
     if(selectedChar > totalCharsInLine[firstVisLine+selectedLine])
       selectedChar = totalCharsInLine[firstVisLine+selectedLine];
     if(SELECTING) 
     {
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
     }
     if(SELECTED) SELECTED = 0;
     refreshView();
     fflush(stdout);
     break;
   case(HOME_KEY):
     if(GNU_DOS_LEVEL > 2) break;
do_home:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
     selectedCharCarry = 0;
      if((CTRL && GNU_DOS_LEVEL < 3)
	|| (CTRL && GNU_DOS_LEVEL >= 3 && ch[0] == '>')) 
      {
	selectedLine = 0;
	firstVisLine = 0;
	refreshView();
      } else if(selectedChar == 0) break;
      selectedChar = 0;
      if(SELECTING) 
      { 
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
      }
      fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
      refreshSelectedLine();
      //refreshBottomView();
      if(SELECTED) { SELECTED = 0; refreshView(); }
      fflush(stdout);
      break;
   case(END_KEY):
     if(GNU_DOS_LEVEL > 2) break;
do_end:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
      if((CTRL && GNU_DOS_LEVEL < 3)
	|| (CTRL && GNU_DOS_LEVEL >= 3 && ch[0] == '<')) 
      {
	if(totalLines <= totalVisLines) 
	{
	  selectedLine = totalLines-1; firstVisLine = 0;
	} 
	else 
	{
	  firstVisLine = totalLines-totalVisLines;
	  selectedLine = totalVisLines-1;
	  refreshView();
	}
      } else if(selectedChar == strlen(lines[firstVisLine+selectedLine])/4) break;
      selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
      if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
      
      if(selectedChar >= MAX_CHARS_PER_LINE) selectedChar = MAX_CHARS_PER_LINE-1;
      //*******claculate char offset
      selectedCharCarry = 0; int i;
      for(i = 0; i < selectedChar*4; i+=4)
	if(lines[firstVisLine+selectedLine][i] == '\t') 
	{ int j = TAB_CHARS-(((i/4)+selectedCharCarry)%TAB_CHARS); j=0?TAB_CHARS:j; selectedCharCarry+=(j-1); }
      fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
      if(SELECTING) 
      { 
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
      }
      refreshSelectedLine();
      if(SELECTED) { SELECTED = 0; refreshView(); }
      fflush(stdout);
      break;
   case(RIGHT_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_right:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
     if(WRAP_LINES)
     {//if #1
       int i, oldChar = selectedChar;
       int pos = firstVisLine+selectedLine;
       if(selectedChar+selectedCharCarry >= totalCharsInLine[pos] ||
	  selectedChar+selectedCharCarry >= MAX_CHARS_PER_LINE-1) 
       { //if #2
	 if((selectedLine+firstVisLine) >= (totalLines-1)) break;
	 selectedCharCarry = 0;
	 if(selectedLine == totalVisLines-1) 
	 { //if #3
	   firstVisLine++;
	   if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	   {//if using CTRL, goto next word
	     for(i = 0; i < strlen(lines[pos+1]); i++) 
	     {
		if(is_whitespace(lines[pos+1][i*4]) && 
		  !is_whitespace(lines[pos+1][i*4+4])) { selectedChar = i+1; break; }
		else if(is_whitespace(lines[pos+1][i*4]) && 
		  !is_whitespace(lines[pos+1][i*4-4])) { selectedChar = i; break; }
	     } if(selectedChar == oldChar) selectedChar = i;//means we didn't find a space till EOL
	   } else selectedChar = 0;
	   //Set selection range
	   if(SELECTING) 
	   { 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	   }
	   if(selectedChar >= MAX_CHARS_PER_LINE) selectedChar = MAX_CHARS_PER_LINE-1;
	   if(selectedChar >= strlen(lines[firstVisLine+selectedLine])/4)
	   {
	     selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
	    if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
	   }
	   //*******claculate char offset
	   calcCharCarry(firstVisLine+selectedLine);
	   //////////////////////////////
	   refreshView();
	 } 
	 else 
	 {
	   selectedLine++;
	   if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	   {//if using CTRL, goto next word
	     for(i = 0; i < strlen(lines[pos+1]); i++) 
	     {
		if(is_whitespace(lines[pos+1][i*4]) && 
		  !is_whitespace(lines[pos+1][i*4+4])) { selectedChar = i+1; break; }
		else if(is_whitespace(lines[pos+1][i*4]) && 
		  !is_whitespace(lines[pos+1][i*4-4])) { selectedChar = i; break; }
	     } if(selectedChar == oldChar) selectedChar = i;//means we didn't find a space till EOL
	   } else selectedChar = 0;
	   fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
	   if(SELECTING) 
	   { 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	   }
	   if(selectedChar >= MAX_CHARS_PER_LINE) selectedChar = MAX_CHARS_PER_LINE-1;
	   if(selectedChar >= strlen(lines[firstVisLine+selectedLine])/4)
	   {
	     selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
	    if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
	   }
	   //*******claculate char offset
	   calcCharCarry(firstVisLine+selectedLine);
	   //////////////////////////////
	   refreshSelectedLine();
	   refreshBottomView();
	 }//if #3
       } 
       else 
       {
	  if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	  {//if using CTRL, goto next word
	     for(i = (selectedChar*4)+4; i < strlen(lines[pos]); i+=4) 
	     {
		if(is_whitespace(lines[pos][i]) && 
		  !is_whitespace(lines[pos][i+4])) { selectedChar = (i/4)+1; break; }
		else if(is_whitespace(lines[pos][i]) && 
		  !is_whitespace(lines[pos][i-4])) { selectedChar = i/4; break; }
	     }
	     if(selectedChar == oldChar)
	     {
	       selectedChar = i/4;//means we didn't find a space till EOL
	       if(lines[pos][selectedChar*4-4] == '\n') selectedChar--;
	     }
	  } else selectedChar++;
	
	  if(selectedChar >= MAX_CHARS_PER_LINE) selectedChar = MAX_CHARS_PER_LINE-1;
	  if(selectedChar >= strlen(lines[firstVisLine+selectedLine])/4)
	  {
	    selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
	    if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
	  }
	 //************calculate character offset
	 calcCharCarry(pos);
	 ////////////////////////////////////////////////////////////
	      
	 if(SELECTING) 
	 { 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	 }
	 refreshSelectedLine();
	 refreshBottomView();
       }//if #2
     }//if #1
     if(SELECTED) { SELECTED = 0; refreshView(); }
     break;
   case(LEFT_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_left:
     char_inserted = 0;
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     //int i; 
     int pos = firstVisLine+selectedLine;
     if(selectedChar == 0) 
     { //if #1
       if(selectedLine == 0) 
       { //if #2
	 if(firstVisLine == 0) break;
	 firstVisLine--;
	 if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	 {//if using CTRL, goto previous word
	   for(i = strlen(lines[pos-1])-4; i >= 0; i-=4) 
	   {
	      if(is_whitespace(lines[pos-1][i]) &&
		 !is_whitespace(lines[pos-1][i+4])) { selectedChar = (i/4)+1; break; }
	      else if(is_whitespace(lines[pos-1][i]) &&
		 !is_whitespace(lines[pos-1][i-4])) { selectedChar = i/4; break; }
	   } if(i < 0) selectedChar = 0;//means we didn't find a space till EOL
	 } else
	 {
	   selectedChar = strlen(lines[pos-1])/4;
	   if(lines[pos-1][selectedChar*4-4] == '\n') selectedChar--;
	 }
	 if(SELECTING) 
	 { 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	 }
	 //calculate character offset
	 calcCharCarry(pos-1);
	 ////////////////////////////////////////////////////////////
	 refreshView();
       } 
       else 
       {
	 selectedLine--;
	 if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	 {//if using CTRL, goto previous word
	   for(i = strlen(lines[pos-1])-4; i >= 0; i-=4) 
	   {
	      if(is_whitespace(lines[pos-1][i]) &&
		 !is_whitespace(lines[pos-1][i+4])) { selectedChar = (i/4)+1; break; }
	      else if(is_whitespace(lines[pos-1][i]) &&
		 !is_whitespace(lines[pos-1][i-4])) { selectedChar = i/4; break; }
	   } if(i < 0) selectedChar = 0;//means we didn't find a space till EOL
	 } 
	 else 
	 {
	   selectedChar = strlen(lines[pos-1])/4;
	   if(lines[pos-1][selectedChar*4-4] == '\n') selectedChar--;
	 }
	 //calculate character offset
	 calcCharCarry(pos-1);
	 ////////////////////////////////////////////////////////////
	 if(selectedChar+selectedCharCarry >= MAX_CHARS_PER_LINE) selectedChar--;
	 if(totalCharsInLine[pos-1] >= MAX_CHARS_PER_LINE)
	    fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+1+selectedCharCarry);
	 else fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
	 if(SELECTING) 
	 { 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	     refreshView();
	 } 
	 else 
	 {
	    refreshSelectedLine();
	    refreshBottomView();
	 }
       } //end if#2
     } 
     else 
     {
	if((CTRL && GNU_DOS_LEVEL == 1) || (ALT && GNU_DOS_LEVEL > 1)) 
	{//if using CTRL, goto previous word
	  for(i = selectedChar-2; i >= 0; i--) 
	  {
	      if(is_whitespace(lines[pos][i*4]) &&
		 !is_whitespace(lines[pos][i*4+4])) { selectedChar = i+1; break; }
	      else if(is_whitespace(lines[pos][i*4]) &&
		 !is_whitespace(lines[pos][i*4-4])) { selectedChar = i; break; }
	  } if(i < 0) selectedChar = 0;//means we didn't find a space till EOL
	} else selectedChar--;
	if(SELECTING)
	{ 
	     sel_range_end.nline = firstVisLine+selectedLine;
	     sel_range_end.nchar = selectedChar;
	}
       refreshSelectedLine();
       refreshBottomView();
     } //end if#1
     if(SELECTED) { SELECTED = 0; refreshView(); }
     break;
   case(UP_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_up:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
     if(selectedLine > 0) 
     {
       selectedLine--;
     } 
     else 
     {
       if(firstVisLine > 0) firstVisLine--;
       else 
       {
	 break;
       }
       if(!SELECTING) refreshView();
     } //end outer if
     if(selectedChar >= strlen(lines[firstVisLine+selectedLine])/4)
     {
       selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
       if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
     }
     //calculate character offset
     calcCharCarry(firstVisLine+selectedLine);
     //////////////////////////////////////////
     fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
     if(SELECTING) 
     { 
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
	refreshView();
     }
     if(SELECTED) { SELECTED = 0; refreshView(); }
     refreshView();
     fflush(stdout);
     break;
   case(DOWN_KEY):
     if(GNU_DOS_LEVEL > 1) break;
do_down:
     if(RECORDING_UNDO_ACTION) finish_undo_action();
     char_inserted = 0;
     if((firstVisLine+selectedLine) >= totalLines-1) break;
     if(selectedLine < totalVisLines-1) 
     {
       selectedLine++;
     } 
     else 
     { 
       if(firstVisLine < totalLines-totalVisLines) firstVisLine++;
       else break;
       if(!SELECTING) refreshView();
     } //end outer if
     if(selectedChar >= strlen(lines[firstVisLine+selectedLine])/4)
     {
       selectedChar = strlen(lines[firstVisLine+selectedLine])/4;
       if(lines[firstVisLine+selectedLine][selectedChar*4-4] == '\n') selectedChar--;
     }
     //calculate character offset
     calcCharCarry(firstVisLine+selectedLine);
     //////////////////////////////////////////
     fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
     if(SELECTING) 
     { 
	sel_range_end.nline = firstVisLine+selectedLine;
	sel_range_end.nchar = selectedChar;
	refreshView();
     }
     refreshView();
     if(SELECTED) { SELECTED = 0; refreshView(); }
     fflush(stdout);
     break;
   case(BACKSPACE_KEY):
     //if(GNU_DOS_LEVEL > 3) break;
//do_backspace:
     FILE_STATE = MODIFIED;
     char_inserted = 0;
     if(SELECTING || SELECTED) remove_selected_text(1);
     else 
     { 
       if((CTRL && GNU_DOS_LEVEL < 4) || ALT) deletePrevWord();
       else deletePrevChar(); 
     }
     fflush(stdout);
     break;
   case(DEL_KEY):
     if(GNU_DOS_LEVEL > 3) break;
do_del:
     FILE_STATE = MODIFIED;
     char_inserted = 0;
     if(SELECTING || SELECTED) remove_selected_text(1);
     else 
     { 
       if((CTRL && GNU_DOS_LEVEL < 4) || ALT) deleteNextWord();
       else deleteNextChar();
     }
     fflush(stdout);
     break;
   case(ENTER_KEY):
     FILE_STATE = MODIFIED;
     char_inserted = 0;
     if(SELECTING || SELECTED) remove_selected_text(1);
     insertEnter();
     fflush(stdout);
     break;
   case(TAB_KEY):
     FILE_STATE = MODIFIED;
     if(SELECTING || SELECTED) remove_selected_text(1);
     char_inserted = 1;
     insertTab();
     fflush(stdout);
     break;
   case(SPACE_KEY):
   default:
     if(ch[0] == 'f' && ALT) 
     {//if #1
        if(GNU_DOS_LEVEL > 1) goto do_right;//GNU key binding
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	showFileMenu(YES);
     } 
     else if(ch[0] == 'e' && ALT) 
     {
        if(GNU_DOS_LEVEL > 1) break;
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	showEditMenu(YES);
     } 
     else if(ch[0] == 'h' && ALT) 
     {
        if(GNU_DOS_LEVEL > 1) break;
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	showHelpMenu(YES);
     } 
     else if(ch[0] == 'o' && ALT) 
     {
        if(GNU_DOS_LEVEL > 1) break;
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	showOptionsMenu(YES);
     } 
     else if(ch[0] == 'b' && ALT) 
     { 
       if(GNU_DOS_LEVEL > 1) goto do_left;//GNU key binding
     }
     else if(ch[0] == 'w' && CTRL) 
     { 
	if(GNU_DOS_LEVEL < 4) break;
	FILE_STATE = MODIFIED;
	char_inserted = 0;
	if(SELECTING || SELECTED) remove_selected_text(1);
     }
     else if(ch[0] == ' ' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 3) goto do_shift_down;//GNU key binding
     }
     else if(ch[0] == 'g' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 2) goto do_esc;//GNU key binding
     }
     else if(ch[0] == 'v' && ALT)
     { 
       if(GNU_DOS_LEVEL > 2) goto do_pg_up;//GNU key binding
     }
     else if(ch[0] == 'v' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 2) goto do_pg_down;//GNU key binding
       editMenu_Paste();
     }
     else if(ch[0] == 'c' && CTRL)
     { 
       editMenu_Copy();
     }
     else if(ch[0] == 'x' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 4)
       {//GNU key binding
	 setScreenColors(FG_COLOR[COLOR_STATUS_BAR], BG_COLOR[COLOR_STATUS_BAR]);
	 fprintf(stdout, "\e[%d;%dH", SCREEN_H, 0);
	 fprintf(stdout, "[C-f] [C-e] [C-o] [C-h] [C-c] [C-s] [C-u] [C-g]");
	 while(1)
	 {
	    ch = getKey();
	    if(ch[0] == 'f' && CTRL)      { showFileMenu(YES); break; }
	    else if(ch[0] == 'e' && CTRL) { showEditMenu(YES); break; }
	    else if(ch[0] == 'o' && CTRL) { showOptionsMenu(YES); break; }
	    else if(ch[0] == 'h' && CTRL) { showHelpMenu(YES); break; }
	    else if(ch[0] == 'c' && CTRL) { fileMenu_Exit(); break; }
	    else if(ch[0] == 's' && CTRL) { fileMenu_Save(); break; }
	    else if(ch[0] == 'u')         { editMenu_Undo(); break; }
	    else if(ch[0] == 'g' && CTRL) { break; }
	 }//end while
	 refreshBottomView();
	 break;
       }//end inner if
       else editMenu_Cut();
     }
     else if(ch[0] == '/' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 4) editMenu_Undo();//GNU key binding
     }
     else if(ch[0] == '_' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 4) editMenu_Undo();//GNU key binding
     }
     else if(ch[0] == 'a' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 2) goto do_home;//GNU key binding
       editMenu_SelectAll();
     }
     else if(ch[0] == 'z' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 4) break;
       editMenu_Undo();
     }
     else if(ch[0] == 'y' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 4) //GNU key binding
	  editMenu_Paste();
       else editMenu_Redo();
     }
     else if(ch[0] == 'r' && CTRL) 
     { 
       editMenu_Replace();
     }
     else if(ch[0] == 'f' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 1) goto do_right;
       editMenu_Find();
     }
     else if(ch[0] == 'e' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 2) goto do_end;
       editMenu_ToggleSelectMode();
     }
     else if(ch[0] == 'o' && CTRL) 
     { 
       fileMenu_Open(); 
     }
     else if(ch[0] == 's' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 4) editMenu_Find();
       else fileMenu_Save(); 
     }
     else if(ch[0] == 'n' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 1) goto do_down;
       fileMenu_New(); 
     }
     else if(ch[0] == 'p' && CTRL)
     { 
       if(GNU_DOS_LEVEL > 1) goto do_up;
       fileMenu_Print(); 
     }
     else if(ch[0] == 'q' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 4) break;
       fileMenu_Exit(); 
     }
     else if(ch[0] == 'd' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 3) goto do_del;
       deleteLine(); 
     }
     else if(ch[0] == 'k' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 3) deleteLine(); 
     }
     else if(ch[0] == 'b' && CTRL) 
     { 
       if(GNU_DOS_LEVEL > 1) goto do_left;
     }
     else 
     {
      //if(ch >= 32 && ch <= 126)
      //{//if #2
       if(ch[1] == '\0') ch[1] = ' ';
       if(ch[2] == '\0') ch[2] = ' ';
       if(ch[3] == '\0') ch[3] = ' ';
	FILE_STATE = MODIFIED;
	//if(SELECTING || SELECTED) remove_selected_text(1);
	if(SELECTING || SELECTED)
	{
	  if(sel_range_start.nchar == sel_range_end.nchar)
	    if(sel_range_start.nline != sel_range_end.nline)
	      remove_selected_text(1);
	  SELECTED = SELECTING = 0;
	}
	if(CAPS)
	{
	  if(ch[0] >= 'a' && ch[0] <= 'z') ch[0]-=32; //insert CAPITAL letter
	  else if(ch[0] >= 'A' && ch[0] <= 'Z') ch[0]+=32; //insert small letter
	  else insertChar(ch);
	} else insertChar(ch);
	char_inserted = 1;
	fflush(stdout);
	break;
      //}//end if #2
     }//end if #1
     break;
  }//end switch
  //fflush(stdout);
 }//end while
 
 restoreTerminal();
 fcloseall();
 exit(0);
}//end main

void deleteLine()
{
  int pos = selectedLine+firstVisLine;
  FILE_STATE = MODIFIED;
  int chr = selectedChar; selectedChar = 0;

  sel_range_start.nline = pos;
  sel_range_end.nline = pos+1;
  sel_range_start.nchar = 0;
  sel_range_end.nchar = 0;
  remove_selected_text(1);
  undo_action_end[last_undo_action].nline++;
  selectedChar = chr;
  if(selectedChar > strlen(lines[pos])/4) selectedChar = strlen(lines[pos])/4;
  refreshView();
}

bool is_whitespace(char c)
{
  if(c == ' ' || c == '\t' ||
     c == '\v' || c == '\n')
    return 1;
  return 0;
}

void deleteNextWord() 
{
  int i, j, pos = firstVisLine+selectedLine;
  int chr = selectedChar*4;
  int oF = firstVisLine; int oS = selectedLine; int oC = selectedChar;
  if(selectedChar >= strlen(lines[pos])/4) 
  {
    if(pos >= totalLines-1) return;
    LINE_IS_LINKED[pos] = 1; pos++; chr = 0;
    selectedChar = 0; firstVisLine++;
  }//end if
  FILE_STATE = MODIFIED;
  if(RECORDING_UNDO_ACTION) finish_undo_action();
  begin_undo_action(UNDO_ACTION_DELETE);
  
  for(i = chr+4; i < strlen(lines[pos]); i+=4)
    if(is_whitespace(lines[pos][i])) break;
    else { add_to_undo_action(lines[pos]+i); selectedChar++; }
    
  if(i >= strlen(lines[pos])) lines[pos][chr] = '\0';
  else
  {
    strcpy(tmp, lines[pos]+i);
    strcpy(lines[pos]+(chr), tmp);
  }
  
  firstVisLine = oF; selectedLine = oS; selectedChar = oC;
  calcTotalCharsInLine(pos);
  pos = firstVisLine+selectedLine;
  
  while(LINE_IS_LINKED[pos]) 
  {
    j = MAX_CHARS_PER_LINE*4-strlen(lines[pos]);
    strncat(lines[pos], lines[pos+1], j);
    strcpy(tmp, lines[pos+1]+j);
    strcpy(lines[pos+1], tmp);
    totalCharsInLine[pos] = strlen(lines[pos])/4;
    pos++; if(pos >= totalLines) break;
  }//end while
  if(strlen(lines[pos]) == 0) 
  {
    LINE_IS_LINKED[pos-1] = 0;
    for(i = pos; i < totalLines-1; i++) 
    {
      strcpy(lines[i], lines[i+1]);
      totalCharsInLine[i] = strlen(lines[i])/4;
      LINE_IS_LINKED[i] = LINE_IS_LINKED[i+1];
    } totalLines--;
  }
  checkAllTabs();
  refreshView();
}

void deletePrevWord() 
{
  int i, j, pos = firstVisLine+selectedLine;
  int chr = selectedChar;
  int oF = firstVisLine; int oS = selectedLine; int oC = selectedChar;
  if(selectedChar <= 0) 
  {
    if(pos <= 0) return;
    LINE_IS_LINKED[pos-1] = 1; pos--; chr = strlen(lines[pos-1])/4;
    selectedChar = chr; firstVisLine--;
  }//end if
  FILE_STATE = MODIFIED;
  if(RECORDING_UNDO_ACTION) finish_undo_action();
  begin_undo_action(UNDO_ACTION_DELETE);
  
  for(i = chr; i >= 0; i--)
    if(is_whitespace(lines[pos][i*4])) break;
    else { add_to_undo_action(lines[pos]+i); selectedChar--; }
    
  if(i < 0) i = 0;
  strcpy(tmp, lines[pos]+(chr*4));
  strcpy(lines[pos]+(i*4), tmp);
  
  firstVisLine = oF; selectedLine = oS; selectedChar = oC;
  
  while(LINE_IS_LINKED[pos]) 
  {
    j = MAX_CHARS_PER_LINE*4-strlen(lines[pos]);
    strncat(lines[pos], lines[pos+1], j);
    strcpy(tmp, lines[pos+1]+j);
    strcpy(lines[pos+1], tmp);
    totalCharsInLine[pos] = strlen(lines[pos])/4;
    pos++; if(pos >= totalLines) break;
  }//end while
  //free(tmp);
  if(strlen(lines[pos]) == 0) 
  {
    LINE_IS_LINKED[pos-1] = 0;
    for(i = pos; i < totalLines-1; i++) 
    {
      strcpy(lines[i], lines[i+1]);
      totalCharsInLine[i] = strlen(lines[i]);
      LINE_IS_LINKED[i] = LINE_IS_LINKED[i+1];
    } totalLines--;
  } else totalCharsInLine[pos] = strlen(lines[pos])/4;
  checkAllTabs();
  refreshView();
}

void deleteNextChar() 
{
  int i, pos = firstVisLine+selectedLine;
  if(selectedChar < strlen(lines[pos])/4)
  {//if #1
    if(lines[pos][selectedChar*4] == '\n') goto delete_at_newline;
    if(RECORDING_UNDO_ACTION) finish_undo_action();
    begin_undo_action(UNDO_ACTION_DELETE);
    add_to_undo_action(lines[pos]+(selectedChar*4));
    finish_undo_action();
    
    i = selectedChar*4;
    strcpy(tmp, lines[pos]+(i+4));
    strcpy(lines[pos]+i, tmp);
    calcTotalCharsInLine(pos);
    //if the line is linked to the next line, grab a char
    //from the next line and append it here.. go on until we
    //hit a line that is not linked to its next one.
    if(LINE_IS_LINKED[pos]) 
    {
      while(LINE_IS_LINKED[pos]) 
      {
	i = strlen(lines[pos]);
	lines[pos][i] = lines[pos+1][0];
	lines[pos][i+1] = lines[pos+1][1];
	lines[pos][i+2] = lines[pos+1][2];
	lines[pos][i+3] = lines[pos+1][3];
	lines[pos][i+4] = '\0';
	totalCharsInLine[pos] = strlen(lines[pos])/4;
	i = 0;
	if(strlen(lines[pos+1]))
	{
	  strcpy(tmp, lines[pos+1]+4);
	  strcpy(lines[pos+1], tmp);
	  lines[pos+1][strlen(lines[pos+1])] = '\0';
	}
	totalCharsInLine[pos+1] = strlen(lines[pos+1])/4;
	//if the next line becomes empty, delete it
	if(strlen(lines[pos+1]) == 0) 
	{
	  move_lines_up(pos+1, totalLines-1);
	  LINE_IS_LINKED[pos] = 0;
	  break;
	}
	pos++; if(pos >= totalLines) break;
      }//end while
      refreshView();
    } 
    else 
    {
      refreshSelectedLine();
    }
  } 
  else 
  {
delete_at_newline:
    //deleting at the end of line
    if(pos == totalLines-1) return;
    if(RECORDING_UNDO_ACTION) finish_undo_action();
    begin_undo_action(UNDO_ACTION_DELETE);
    add_to_undo_action("\n   ");
    finish_undo_action();
    
    //is the next line an empty line?
    if(strlen(lines[pos+1]) == 0) 
    {
      move_lines_up(pos+1, totalLines-1);
      refreshView(); return;
    }

    //is this line an empty line?
    if(strlen(lines[pos]) == 0) 
    {
      move_lines_up(pos, totalLines-1);
      refreshView(); return;
    }

    int j;
    
    LINE_IS_LINKED[pos] = 1;

    while(LINE_IS_LINKED[pos]) 
    {
      int len1;
      len1 = strlen(lines[pos]);
      if(lines[pos][len1-4] == '\n')
	lines[pos][len1-4] = '\0';
      calcTotalCharsInLine(pos); /* make sure calculation is correct */
      len1 = totalCharsInLine[pos];
      j = (MAX_CHARS_PER_LINE*4)-(len1);
      strncat(lines[pos], lines[pos+1], j);
      //the next line is short, so remove it
      if(strlen(lines[pos+1]) < j) 
      {
	move_lines_up(pos+1, totalLines-1);
	LINE_IS_LINKED[pos] = 0;
	break;
      }
      strcpy(tmp, lines[pos+1]+j);
      strcpy(lines[pos+1], tmp);
      calcTotalCharsInLine(pos+1);
      pos++; if(pos >= totalLines) break;
    }//end while

    refreshView();
  }//end if #1
}//end deleteNextChar()

void deletePrevChar() 
{
  int i, pos = firstVisLine+selectedLine;
  if(RECORDING_UNDO_ACTION && 
    undo_action[last_undo_action] != UNDO_ACTION_DELETE)
  finish_undo_action();
  
  if(selectedChar == 0) 
  { //if #1
    if(selectedLine == 0) 
    { //if #2
      if(firstVisLine == 0) return;
      firstVisLine--;      
    } 
    else { selectedLine--; }//end if #2

    //if the previous line is an empty line
    if(strlen(lines[pos-1]) == 0) 
    {
      if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_DELETE);
      add_to_undo_action("\n   ");
      move_lines_up(pos-1, totalLines-1);
      if(totalLines > totalVisLines)
	if(totalLines-firstVisLine < totalVisLines) 
	  { firstVisLine=totalLines-totalVisLines; }
      refreshView(); return;
    }

    //if the current line is an empty line
    if(strlen(lines[pos]) == 0) 
    {
      move_lines_up(pos, totalLines-1);
      selectedChar = totalCharsInLine[pos-1];
      calcCharCarry(pos-1);
      if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_DELETE);
      add_to_undo_action("\n   ");
      refreshView(); return;
    }
    
    /* Append to previous line */
    selectedChar = strlen(lines[pos-1])/4;
    if(lines[pos-1][selectedChar*4-4] == '\n') selectedChar--;
    if(selectedChar < 0) selectedChar = 0;
    calcCharCarry(pos-1);
    int old_pos = pos;
    LINE_IS_LINKED[pos-1] = 1;
    while(LINE_IS_LINKED[pos-1])
    {
      int len1;
      len1 = strlen(lines[pos-1]);
      if(lines[pos-1][len1-4] == '\n')
	lines[pos-1][len1-4] = '\0';
      calcTotalCharsInLine(pos-1); /* make sure calculation is correct */
      len1 = totalCharsInLine[pos-1];
      int len2 = totalCharsInLine[pos];
      if(len1 < MAX_CHARS_PER_LINE)
      {
	int howmany = MAX_CHARS_PER_LINE - len1;
	strncat(lines[pos-1], lines[pos], howmany*4);
	if(howmany >= len2)
	{
	  move_lines_up(pos, totalLines-1);
	  LINE_IS_LINKED[pos-1] = 0;
	  break;
	}
	strcpy(tmp, lines[pos]+(howmany*4));
	strcpy(lines[pos], tmp);
	checkTabsInLine(pos-1);
	calcTotalCharsInLine(pos);
      }
      pos++; if(pos >= totalLines) break;
    } /* end while */
    pos = old_pos;
    
    if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_DELETE);
    add_to_undo_action("\n   ");
    refreshView(); return;
    
  } 
  else 
  {
    //deleting in the middle/end of line
    selectedChar--;
    if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_DELETE);
    add_to_undo_action(lines[pos]+selectedChar*4);

    //if the char deleted is a tab, we need some housekeeping work
    if(lines[pos][selectedChar*4] == '\t')
    {
      selectedCharCarry = 0;
      for(i = 0; i < selectedChar; i++)
	if(lines[pos][i*4] == '\t') 
	{ int j = TAB_CHARS-((i+selectedCharCarry)%TAB_CHARS); j=0?TAB_CHARS:j; selectedCharCarry+=(j-1); }
    }
    
    //do delete the char
    strcpy(tmp, lines[pos]+(selectedChar*4+4));
    strcpy(lines[pos]+(selectedChar*4), tmp);
    fixLineLength(pos);
    if(LINE_IS_LINKED[pos]) 
    {
      refreshView();
    } 
    else 
    {
      refreshSelectedLine();
    }
  }//end if #1
}//end deletePrevChar()

void insertEnter() 
{
  int i;
  int pos = firstVisLine+selectedLine;
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);

  if(AUTO_INDENT)
  {
    int m = 0;
    for(i = 0; i < strlen(lines[pos]); i+=4)
      if(lines[pos][i] == ' ' || lines[pos][i] == '\t')
      { 
	AUTO_INDENT_STR[m] = lines[pos][i]; 
	AUTO_INDENT_STR[m+1] = lines[pos][i+1]; 
	AUTO_INDENT_STR[m+2] = lines[pos][i+2]; 
	AUTO_INDENT_STR[m+3] = lines[pos][i+3]; 
	m+=4;
      }
      else break;
    AUTO_INDENT_STR[m] = '\0';
  }
  
  if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_INSERT);
  if(pos < totalLines-1) 
  { //if #1
     for(i = totalLines; i > pos; i--) 
     {
      strcpy(lines[i], lines[i-1]);
      totalCharsInLine[i] = totalCharsInLine[i-1];
      LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
     }//end for
  } 
  else if(pos == totalLines-1) 
  {
    if(AUTO_INDENT)
    {
      strcpy(lines[pos+1], AUTO_INDENT_STR);
      strcat(lines[pos+1], lines[pos]+(selectedChar*4));
    }
    else strcpy(lines[pos+1], lines[pos]+(selectedChar*4));
    
    lines[pos][selectedChar*4] = '\0';
    totalCharsInLine[pos] = strlen(lines[pos])/4;
    totalCharsInLine[pos+1] = strlen(lines[pos+1])/4;
    LINE_IS_LINKED[pos+1] = 0;
    LINE_IS_LINKED[pos] = 0;
    selectedCharCarry = 0;
    totalLines++; selectedChar = 0; selectedLine++;
    if(selectedLine >= totalVisLines-1) { selectedLine--; firstVisLine++; }
    add_to_undo_action("\n   ");
    refreshView();
    return;
  } 
  else 
  {//inserting ENTER before the last line
    strcpy(lines[pos+1], lines[pos]);
    strcpy(lines[pos], "");
    totalCharsInLine[pos+1] = totalCharsInLine[pos];
    totalCharsInLine[pos] = 0;
    LINE_IS_LINKED[pos+1] = 0;
    LINE_IS_LINKED[pos] = 0;
  }//end if #1
  
  if(selectedChar == 0) 
  {
    lines[pos][selectedChar*4] = '\0';
    totalCharsInLine[pos] = 0;
    LINE_IS_LINKED[pos] = 0;
    totalLines++;
  } 
  else if(selectedChar < totalCharsInLine[pos]) 
  {//if #2
    strcpy(lines[pos+1], "");
    strcpy(lines[pos+1], lines[pos]+(selectedChar*4));
    totalCharsInLine[pos+1] = strlen(lines[pos+1])/4;
    lines[pos][selectedChar*4] = '\0';
    totalCharsInLine[pos] = strlen(lines[pos])/4;
    totalLines++;
    
    if(LINE_IS_LINKED[pos]) 
    {      
      int i = pos+1;
      int j = 0;
do_linked_line:
      if(strlen(lines[i+1]) == 0) LINE_IS_LINKED[i] = 0;
      else
      {
	j = MAX_CHARS_PER_LINE-(strlen(lines[i])/4);
	strncat(lines[i], lines[i+1], j*4);
	if(j >= strlen(lines[i+1])/4)
	{
	  tmp[0] = '\0';
	  move_lines_up(i+1, totalLines-1);
	  LINE_IS_LINKED[i+1] = 0;
	}
	else
	{
	  strcpy(tmp, lines[i+1]+(j*4));
	  strcpy(lines[i+1], tmp);
	}
	totalCharsInLine[i] = strlen(lines[i])/4;
	i++;
	if(LINE_IS_LINKED[i]) goto do_linked_line;
      }
      LINE_IS_LINKED[pos] = 0;
    }
      
    if(AUTO_INDENT)
    {
      pos = firstVisLine+selectedLine;
      strcpy(tmp2, lines[pos+1]);
      strcpy(lines[pos+1], AUTO_INDENT_STR);
      strcat(lines[pos+1], tmp2);
    }
  } 
  else 
  {
      lines[pos][selectedChar*4] = '\0';
      totalCharsInLine[pos+1] = 0;
      lines[pos+1][0] = '\0';
      if(AUTO_INDENT)
      {
	strcpy(lines[pos+1], AUTO_INDENT_STR);
	totalCharsInLine[pos+1] = strlen(lines[pos+1])/4;
	checkTabsInLine(pos+1);
      }
      LINE_IS_LINKED[pos] = 0;
      totalLines++;
  }//end if #2
  selectedChar = 0;
  selectedCharCarry = 0;
  if(selectedLine == totalVisLines-1) firstVisLine++;
  else selectedLine++;
  add_to_undo_action("\n   ");
  refreshView();
}//end insertEnter()


void insertTab() 
{
  int j = TAB_CHARS-((selectedChar+selectedCharCarry)%TAB_CHARS);
  j = 0 ? TAB_CHARS:j;

  static char *t = "\t   ";
  insertChar(t);
  
  refreshBottomView();
  return;
}//end insertTab()

void insertChar(char *ch) 
{
  int i;
  int pos = firstVisLine+selectedLine;
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  if(WRAP_LINES) 
  {//if #1
    if(selectedChar < MAX_CHARS_PER_LINE-1)
    {//if #2
      if(INSERT) 
      {//replace current character
	if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_REPLACE);
	if(undo_action[last_undo_action] != UNDO_ACTION_REPLACE) 
	{
	  finish_undo_action(); begin_undo_action(UNDO_ACTION_REPLACE);
	}
	if(RECORDING_UNDO_ACTION) add_to_undo_action(lines[pos]+selectedChar*4);
	i = strlen(undo_text_replace[last_undo_action]);
	undo_text_replace[last_undo_action]
	      [strlen(undo_text_replace[last_undo_action])+4] = '\0';
	undo_text_replace[last_undo_action][i] = ch[0];
	undo_text_replace[last_undo_action][i+1] = ch[1];
	undo_text_replace[last_undo_action][i+2] = ch[2];
	undo_text_replace[last_undo_action][i+3] = ch[3];
	lines[pos][selectedChar*4] = ch[0];
	lines[pos][selectedChar*4+1] = ch[1];
	lines[pos][selectedChar*4+2] = ch[2];
	lines[pos][selectedChar*4+3] = ch[3];
	selectedChar++;
	checkTabsInLine(pos);
	int old_len = strlen(lines[pos]);
	fixLineLength(pos);
	if(old_len != strlen(lines[pos])) refreshView();
	else refreshSelectedLine();
	return;
      }//end if(INSERT)
      if(selectedChar < strlen(lines[pos])/4) 
      { //if #3
	//there are chars to the right of cursor
	if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_INSERT);
	strcpy(tmp, lines[pos+1]);
	char last_char[5];
	strncpy(last_char, lines[pos]+(strlen(lines[pos]))-4, 4);
	last_char[4] = '\0';
	
	if(strlen(lines[pos])/4 > MAX_CHARS_PER_LINE)
	  lines[pos][MAX_CHARS_PER_LINE*4] = '\0';
	else
	  lines[pos][strlen(lines[pos])+4] = '\0';
	
	for(i = strlen(lines[pos])+3; i >= selectedChar*4+4; i--)
	  lines[pos][i] = lines[pos][i-4];
	lines[pos][selectedChar*4] = ch[0];
	lines[pos][selectedChar*4+1] = ch[1];
	lines[pos][selectedChar*4+2] = ch[2];
	lines[pos][selectedChar*4+3] = ch[3];
	strcpy(lines[pos+1], tmp);

	selectedChar++;
	if(RECORDING_UNDO_ACTION) add_to_undo_action(ch);
	//calculate character offset
	calcCharCarry(firstVisLine+selectedLine);
	//////////////////////////////////////////
	fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
	calcTotalCharsInLine(pos);
	int old_pos = pos;
	if(totalCharsInLine[pos] <= MAX_CHARS_PER_LINE)
	  goto finish_insert;
	//THE LINE IS SPILLING OVER.. ADD LAST CHAR TO NEXT LINE
	lines[pos][MAX_CHARS_PER_LINE*4] = '\0';
	if(strlen(lines[pos])/4 > MAX_CHARS_PER_LINE && !LINE_IS_LINKED[pos])
	{
	  /* make new empty line */
	  move_lines_down(totalLines, pos+1);
	  LINE_IS_LINKED[pos] = 1;
	}
	
	int max;
	old_pos = pos;
	while(LINE_IS_LINKED[pos])
	{
	  strcpy(tmp, lines[pos+1]);
	  max = strlen(lines[pos+1]);
	  strcpy(lines[pos+1], last_char);
	  strncpy(last_char, tmp+max-4, 4);
	  strcat(lines[pos+1], tmp);
	  lines[pos+1][MAX_CHARS_PER_LINE*4] = '\0';
	  pos++;
	}
finish_insert:
	checkTabsInLine(old_pos);
	refreshView();
      } 
      else 
      {
	//the cursor is at the last char
	if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_INSERT);
	lines[pos][selectedChar*4] = ch[0];
	lines[pos][selectedChar*4+1] = ch[1];
	lines[pos][selectedChar*4+2] = ch[2];
	lines[pos][selectedChar*4+3] = ch[3];
	lines[pos][selectedChar*4+4] = '\0';
	selectedChar++;
	checkTabsInLine(pos);
	refreshSelectedLine();
	if(RECORDING_UNDO_ACTION) add_to_undo_action(ch);
      }//end if #3
    }
    else 
    {
//insert_at_newline:
      if(INSERT) 
      {//replace current character
	if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_REPLACE);
	if(RECORDING_UNDO_ACTION) add_to_undo_action(ch);
	i = strlen(undo_text_replace[last_undo_action]);
	undo_text_replace[last_undo_action][i] = lines[pos+1][selectedChar*4-4];
	undo_text_replace[last_undo_action][i+1] = lines[pos+1][selectedChar*4-3];
	undo_text_replace[last_undo_action][i+2] = lines[pos+1][selectedChar*4-2];
	undo_text_replace[last_undo_action][i+3] = lines[pos+1][selectedChar*4-1];
	undo_text_replace[last_undo_action][i+4] = '\0';
      } 
      else 
      {
	if(!RECORDING_UNDO_ACTION) begin_undo_action(UNDO_ACTION_INSERT);
	if(RECORDING_UNDO_ACTION) add_to_undo_action(ch);
      }
      
      //we are at the last char in line.. see if we need to insert
      //a new line or just continue to the next line if linked
      if(LINE_IS_LINKED[pos]) 
      {
	if(INSERT) 
	{
	  lines[pos][MAX_CHARS_PER_LINE*4-4] = ch[0];
	  lines[pos][MAX_CHARS_PER_LINE*4-3] = ch[1];
	  lines[pos][MAX_CHARS_PER_LINE*4-2] = ch[2];
	  lines[pos][MAX_CHARS_PER_LINE*4-1] = ch[3];
	  lines[pos][MAX_CHARS_PER_LINE*4] = '\0';
	  selectedChar = 0;
	  checkTabsInLine(pos);
	  if(selectedLine == totalVisLines-1) firstVisLine++;
	  else selectedLine++;
	  refreshView();
	  return;
	}
	int i = pos+1;
	char c[5];
	c[0] = lines[pos][MAX_CHARS_PER_LINE*4-4];
	c[1] = lines[pos][MAX_CHARS_PER_LINE*4-3];
	c[2] = lines[pos][MAX_CHARS_PER_LINE*4-2];
	c[3] = lines[pos][MAX_CHARS_PER_LINE*4-1];
	c[4] = '\0';
	lines[pos][MAX_CHARS_PER_LINE*4-4] = ch[0];
	lines[pos][MAX_CHARS_PER_LINE*4-3] = ch[1];
	lines[pos][MAX_CHARS_PER_LINE*4-2] = ch[2];
	lines[pos][MAX_CHARS_PER_LINE*4-1] = ch[3];
	lines[pos][MAX_CHARS_PER_LINE*4] = '\0';
	totalCharsInLine[pos] = strlen(lines[pos])/4;

	while(LINE_IS_LINKED[i-1]) 
	{
	  strcpy(tmp, lines[i]);
	  lines[i][0] = c[0];
	  lines[i][1] = c[1];
	  lines[i][2] = c[2];
	  lines[i][3] = c[3];
	  lines[i][4] = '\0';
	  strcat(lines[i], tmp);
	  if(strlen(tmp) < MAX_CHARS_PER_LINE*4) break;
	  c[0] = lines[pos][MAX_CHARS_PER_LINE*4-4];
	  c[1] = lines[pos][MAX_CHARS_PER_LINE*4-3];
	  c[2] = lines[pos][MAX_CHARS_PER_LINE*4-2];
	  c[3] = lines[pos][MAX_CHARS_PER_LINE*4-1];
	  lines[i][MAX_CHARS_PER_LINE*4-4] = '\0';
	  totalCharsInLine[i] = strlen(lines[i])/4;
	  i++;
	}//end while
	//if last line while long but not linked, make a new
	//line and dump the extra char into it
	if(strlen(tmp) == MAX_CHARS_PER_LINE*4) 
	{
	  int j;
	  for(j = totalLines; j > i+1; j--) 
	  {
	    strcpy(lines[j], lines[j-1]);
	    LINE_IS_LINKED[j] = LINE_IS_LINKED[j-1];
	    totalCharsInLine[j] = strlen(lines[j])/4;
	  } totalLines++;
	  lines[i][0] = c[0];
	  lines[i][1] = c[1];
	  lines[i][2] = c[2];
	  lines[i][3] = c[3];
	  lines[i][4] = '\0';
	  LINE_IS_LINKED[i] = 0;
	  totalCharsInLine[i] = strlen(lines[i])/4;
	}
	selectedChar = 0;
	if(selectedLine == totalVisLines-1) firstVisLine++;
	else selectedLine++;
	checkAllTabs();
	refreshView();
	return;
      } 
      else 
      {
      //we are at the last char in line. Insert a new line,
      //move current word to new line, and shift all lines below
      //current line one step down.
      move_lines_down(totalLines, pos+1);
      selectedChar = 0;
      if(selectedLine < totalVisLines-1) { selectedLine++; } 
      else { firstVisLine++; }
      lines[pos][MAX_CHARS_PER_LINE*4-4] = ch[0];
      lines[pos][MAX_CHARS_PER_LINE*4-3] = ch[1];
      lines[pos][MAX_CHARS_PER_LINE*4-2] = ch[2];
      lines[pos][MAX_CHARS_PER_LINE*4-1] = ch[3];
      lines[pos][MAX_CHARS_PER_LINE*4] = '\0';
      calcTotalCharsInLine(pos);
      LINE_IS_LINKED[pos] = 1;
      LINE_IS_LINKED[pos+1] = 0;
      lines[pos+1][0] = '\0';
      calcTotalCharsInLine(pos+1);
      checkTabsInLine(pos);
      refreshView();
      }
    }//end if #2
  }//end if #1
}//end insertChar()

void refreshSelectedLine() 
{
  int i, swap = 0;
  int pos = firstVisLine+selectedLine;
  checkTabsInLine(pos);
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\e[%d;%dH", selectedLine+3, 2);
  if(WRAP_LINES) 
  {//if #1
    if(SELECTING) 
    {//if #2
      //swap the select range boundaries if needed
      if(sel_range_start.nline > sel_range_end.nline) { swap = 1; swap_lines(); }
      else if(sel_range_start.nline == sel_range_end.nline &&
	  sel_range_start.nchar > sel_range_end.nchar) { swap = 2; swap_chars(); }
      
      if((firstVisLine+selectedLine) >= sel_range_start.nline &&
	 (firstVisLine+selectedLine) <= sel_range_end.nline) 
      {
	if(sel_range_start.nline == sel_range_end.nline) 
	{
	  int carry = 0;
	  for(i = 0; i < strlen(lines[sel_range_start.nline])/4; i++) 
	  {
	    if(i >= sel_range_start.nchar && i <= sel_range_end.nchar)
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    else setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    int j;
	    if(lines[pos][i*4] == '\t')
	      { j = TAB_CHARS-((i+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	    else putuchar(pos, i*4);
	  }//end for
	} 
	else if(pos == sel_range_start.nline) 
	{
	  int carry = 0;
	  for(i = 0; i < strlen(lines[pos])/4; i++) 
	  {
	    if(i >= sel_range_start.nchar)
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    else setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    int j;
	    if(lines[pos][i*4] == '\t')
	      { j = TAB_CHARS-((i+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	    else putuchar(pos, i*4);
	  }//end for
	} 
	else if(pos == sel_range_end.nline) 
	{
	  int carry = 0;
	  for(i = 0; i < strlen(lines[pos])/4; i++) 
	  {
	    if(i <= sel_range_end.nchar)
	      setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    else setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    int j;
	    if(lines[pos][i*4] == '\t')
	      { j = TAB_CHARS-((i+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1);  printf("%*s", j, " "); }
	    else putuchar(pos, i*4);
	  }//end for
	} 
	else if(pos > sel_range_start.nline && pos < sel_range_end.nline) 
	{
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;%dH", selectedLine+3, 2);
	  int j, carry = 0;
	  for(i = 0; i < strlen(lines[pos]); i+=4)
	    if(lines[pos][i] == '\t')
	      { j = TAB_CHARS-(((i/4)+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	    else putuchar(pos, i);
	}
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	if(totalCharsInLine[pos] < MAX_CHARS_PER_LINE)
	  printf("%*s", MAX_CHARS_PER_LINE-totalCharsInLine[pos], " ");
	if(swap == 1) swap_lines();//return them back to normal
	if(swap == 2) swap_chars();//return them back to normal
      } 
      else 
      {
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	  { j = TAB_CHARS-(((i/4)+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	if(totalCharsInLine[pos] < MAX_CHARS_PER_LINE)
	  printf("%*s", MAX_CHARS_PER_LINE-totalCharsInLine[pos], " ");
	fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
      }
    } 
    else 
    {
      int printed_chars = 0;
      if(AUTO_HIGHLIGHTING) 
      { 
	refreshSelectedLineInColor(pos);
	if(totalCharsInLine[pos] < MAX_CHARS_PER_LINE)
	  printf("%*s", MAX_CHARS_PER_LINE-totalCharsInLine[pos], " ");
      } 
      else 
      {
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	fprintf(stdout, "\e[%d;%dH", selectedLine+3, 2);
	for(i = 0; i < strlen(lines[pos]); i+=4)
	{
	  if(lines[pos][i] == '\n') break;
	  else if(lines[pos][i] == '\t')
	  { j = TAB_CHARS-(((i/4)+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); printed_chars += j; }
	  else { putuchar(pos, i); printed_chars++; }
	}
	if(printed_chars < MAX_CHARS_PER_LINE)
	  printf("%*s", MAX_CHARS_PER_LINE-printed_chars, " ");
      }
      fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
    }//end if #2
  }//end if #1
  refreshBottomView();
  fflush(stdout);
}//end refreshSelectedLine()

void putuchar(int pos, int index)
{
  static char c[5];
  memset(c, 0, 5);
  c[0] = lines[pos][index];
  /*
   * TODO: Is it really good to inhibit newline output
   *       globally like this? All functions calling this
   *       function will result in newline chars not being
   *       output. Is it healthy?
   */
  if(c[0] == '\n') return;
  if ((c[0] & mask[0]) == mask[0]) c[1] = lines[pos][index+1];
  if ((c[0] & mask[1]) == mask[1]) c[2] = lines[pos][index+2];
  if ((c[0] & mask[2]) == mask[2]) c[3] = lines[pos][index+3];
  c[4] = '0';
  printf("%s", c);
}

/********************************************************
 * This function colorizes the text according to the
 * predefined highlight colors. It searches for keywords,
 * braces, comments and strings in each line.
 * ******************************************************/
void refreshSelectedLineInColor(int pos) 
{
  int i;
  i = pos;

  //check for multi-line HTML comments
  if(HIGHLIGHT_MODE == HTML_MODE)
  {
   for(i = pos-((pos>0)?1:0); i >= 0; i--)
    if(strstr(lines[i], "-   -   >")) break;
    else if(strstr(lines[i], "<   !   -   -"))
    {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	    { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	return;
    }
  }

  //check for multi-line PASCAL comments
  if(HIGHLIGHT_MODE == PASCAL_MODE)
  {
   for(i = pos-((pos>0)?1:0); i >= 0; i--)
    if(strstr(lines[i], "*   }")) break;
    else if(strstr(lines[i], "{   *"))
    {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	    { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	  
	return;
    }
  }
  
  //check for FORTRAN 77 comments
  if(HIGHLIGHT_MODE == F77_MODE)
  {
    if(lines[i][0] == 'c' || lines[i][0] == 'C' || lines[i][0] == 'd'
       || lines[i][0] == 'D' || lines[i][0] == '*' || lines[i][0] == '!')
    {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	    { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	return;
    }
  }

  //check for python multi-line comments
  if(HIGHLIGHT_MODE == PYTHON_MODE)
  {
   for(i = pos; i >= 0; i--)
    if(strstr(lines[i], "\"   \""))
    {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	    { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	  
	return;
    }
  }
  
  //check for texi comments
  if(HIGHLIGHT_MODE == TEXI_MODE &&
     (strstr(lines[i], "@   c   o   m   m   e   n   t")
     || (strstr(lines[i], "@   c    ") == lines[i])))
  {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = 0; k < strlen(lines[i]); k+=4)
	  if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); }
	  else putuchar(i, k);
	return;
  }
  //check first to see if this is part of a multi-line comment.
  //if so, it will save us the pain of parsing the line.
   i = pos;
   if(HIGHLIGHT_MODE == C_MODE || HIGHLIGHT_MODE == CPP_MODE
      || HIGHLIGHT_MODE == JAVASCRIPT_MODE) 
   {
   while(i && LINE_IS_LINKED[i-1]) i--;
   if(LINE_IS_LINKED[i] && i != pos) 
   {//see if it is a commented line
    int j; 
    for(j = strlen(lines[i])-1; j > 0; j-=4)
      if(lines[i][j-4] == '/' && (lines[i][j] == '*' || lines[i][j] == '/')) 
      {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = 0; k < strlen(lines[pos]); k+=4)
	  if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); }
	  else putuchar(pos, k);
	return;
      }
   }
   //check to see if we are in a multi-line comment
   for(i = pos-1; i >= 0; i--)
    if(strstr(lines[i], "*   /")) break;
    else if(strstr(lines[i], "/   *")) 
    {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int j, carry = 0;
	for(i = 0; i < strlen(lines[pos]); i+=4)
	  if(lines[pos][i] == '\t')
	    { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); }
	  else putuchar(pos, i);
	  
	return;
    }
  }//finished checking for C/C++ comments

  char STRING_STARTED = 0;//bool to indicate if we are inside a string
  int printed_chars = 0;
  //so the line is not part of a comment.. parse it.
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  for(i = 0; i < strlen(lines[pos]); i+=4)
    if(lines[pos][i] == ';') 
    { 
      if(STRING_STARTED)
      { 
	putuchar(pos, i); printed_chars++;
	continue; 
      }
      //Check for assembly comments, as they start with a semi-colon
      if(HIGHLIGHT_MODE == ASM_MODE)
      {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = i; k < strlen(lines[pos]); k+=4)
	  if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	  else { putuchar(pos, k); printed_chars++; }
	return;
      }
      //if not in ASM_MODE, just output the semi-colon
      putchar(lines[pos][i]);  printed_chars++; continue; 
    }
    else if(lines[pos][i] == '{' || lines[pos][i] == '}'
	    || lines[pos][i] == '(' || lines[pos][i] == ')'
	    || lines[pos][i] == '<' || lines[pos][i] == '>'
	    || lines[pos][i] == ':' || lines[pos][i] == '[' 
	    || lines[pos][i] == ']') 
    { 
      if(STRING_STARTED)
      { 
	putuchar(pos, i); printed_chars++;
	continue; 
      }
      //Check for PASCAL comments, as they start with a curly brace
      if(HIGHLIGHT_MODE == PASCAL_MODE && lines[pos][i] == '{')
      {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = i; k < strlen(lines[pos]); k+=4)
	  if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	  else { putuchar(pos, k); printed_chars++; }
	return;
      }
      //if not in PASCAL_MODE, just output curly brace
      setScreenColors(COLOR_HBRACES, BG_COLOR[COLOR_WINDOW]);
      putuchar(pos, i);
      setScreenColors(COLOR_HPARAMETERS, BG_COLOR[COLOR_WINDOW]); continue; 
    }
    else if(lines[pos][i] == '"' || lines[pos][i] == '\'')
    { 
      //Check for basic comments, as they start with a single quote
      if(HIGHLIGHT_MODE == BASIC_MODE && lines[pos][i] == '\'')
      {
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = i; k < strlen(lines[pos]); k+=4)
	  if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	  else { putuchar(pos, k); printed_chars++; }
	//printf("%s", lines[pos]);
	return;
      }
      //if not in BASIC_MODE, just output the single quote
      if(HIGHLIGHT_MODE != TEXI_MODE)
	setScreenColors(COLOR_HSTRING, BG_COLOR[COLOR_WINDOW]);
      putuchar(pos, i);
      if(STRING_STARTED)
      {//string is finished, switch color
	STRING_STARTED = 0;
	setScreenColors(COLOR_HPARAMETERS, BG_COLOR[COLOR_WINDOW]); continue; 
	printf("\e[1m"); //set bold font
      }
      else 
      {
	STRING_STARTED = 1;
	printf("\e[22m"); //remove bold font
      }
    }
    else if(lines[pos][i] != ' ' && lines[pos][i] != '\t') 
    {
      if(STRING_STARTED)
      { 
	putuchar(pos, i); printed_chars++;
	continue; 
      }
      if(lines[pos][i] == '/' && (lines[pos][i+4] == '/' || lines[pos][i+4] == '*')) 
      {
	if(HIGHLIGHT_MODE != C_MODE && HIGHLIGHT_MODE != CPP_MODE
	   && HIGHLIGHT_MODE != JAVASCRIPT_MODE)
	  { 
	    putuchar(pos, i); printed_chars++;
	    continue; 
	  }
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	int k, l, carry = 0;
	for(k = i; k < strlen(lines[pos]); k+=4)
	if(lines[pos][k] == '\t')
	{ l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	else { putuchar(pos, k); printed_chars++; }
	return;
      } 
      else if(lines[pos][i] == '#') 
      {
	if(HIGHLIGHT_MODE == C_MODE || HIGHLIGHT_MODE == CPP_MODE) 
	{
	  setScreenColors(COLOR_HDIRECTIVE, BG_COLOR[COLOR_WINDOW]);
	} 
	else if(HIGHLIGHT_MODE == PERL_MODE || HIGHLIGHT_MODE == SHELL_MODE
		|| HIGHLIGHT_MODE == PYTHON_MODE)
	{
	  setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	}
	else 
	{
	  putuchar(pos, i); printed_chars++; continue;
	}
	//We will print out a C/C++ preprocessor directive.
	//But sometimes programmers write comments to the right
	//side of these, so check if there is a comment in the line.
	int m;
	char *comment = strstr(lines[pos], "/   /   ");
	if(comment == NULL)
	  comment = strstr(lines[pos], "/   *   ");
	if(comment == NULL)
	  m = strlen(lines[pos]);
	else m = comment-lines[pos];

	//print the preprocessor directive first
	int k, l, carry = 0;
	for(k = i; k < m; k+=4)
	    if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	    else { putuchar(pos, k); printed_chars++; }
	//then print the comment, if any
	setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	i = k;
	for(k = i; k < strlen(lines[pos]); k+=4)
	    if(lines[pos][k] == '\t')
	    { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	    else { putuchar(pos, k); printed_chars++; }
	return;
      } 
      else 
      {
	if(STRING_STARTED)
	{ 
	    putuchar(pos, i); printed_chars++;
	    continue; 
	}
	int j;
	if((j = isKeyword(pos, i))) 
	{
	  //Check for basic comments, as they start with REM
	  if(HIGHLIGHT_MODE == BASIC_MODE 
	     && (strcasestr(lines[pos], "R   E   M")))// != (char *)NULL))
	  {
	    setScreenColors(COLOR_HCOMMENT, BG_COLOR[COLOR_WINDOW]);
	    int k, l, carry = 0;
	    for(k = i; k < strlen(lines[pos]); k+=4)
	      if(lines[pos][k] == '\t')
	      { l = TAB_CHARS-((k/4+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); printed_chars += l; }
	      else { putuchar(pos, k); printed_chars++; }
	    return;
	  }
	  //if not in BASIC_MODE, just output the keyword
	  setScreenColors(COLOR_HKEYWORD, BG_COLOR[COLOR_WINDOW]);
	  int k;
	  for(k = 0; k < j; k++)
	  {
	    putuchar(pos, i); i += 4; printed_chars++;
	  }
	  i -= 4;
	} 
	else 
	{
	  if(lines[pos][i] == '\n') continue;
	  setScreenColors(COLOR_HPARAMETERS, BG_COLOR[COLOR_WINDOW]);
	  putuchar(pos, i); printed_chars++;
	}
      }
    }
    else  
    {
      int j, carry = 0;
      if(lines[pos][i] == '\t')
      { j = TAB_CHARS-((i/4+carry)%TAB_CHARS); j=0?TAB_CHARS:j; carry+=(j-1); printf("%*s", j, " "); printed_chars += j; }
      else if(lines[pos][i] == '\n') continue;
      else { putuchar(pos, i); printed_chars++; }
    }
    fflush(stdout);
 /////////////////////////////////////////////////////
 /////////////////////////////////////////////////////
 /////////////////////////////////////////////////////
}

//This function tells refreshSelectedLineInColor() whether
//we are standing on a keyword (to give it keyword color)
//or not. If yes, it returns the length of the keyword.
int isKeyword(int pos, int start) 
{
  int i;
  int result = 0;
  for(i = 0; i < total_keywords; i++)
  {
    if(HIGHLIGHT_MODE == ASM_MODE || HIGHLIGHT_MODE == BASIC_MODE)
    {
      int j;
      //parse the keyword letter-by-letter. In this case, the
      //keywords are saved as CAPITALS, so check for both capital
      //and small versions of the keyword.
      for(j = 0; j < strlen(keyword[i]); j++)
	if(lines[pos][start+j*4] == keyword[i][j]
	   || lines[pos][start+j*4] == (keyword[i][j])+32) continue;
	else { result = -1; break; }
      if(result == -1) result = 0;
      else //make sure this is not part of another word
	if(((lines[pos][start+j*4] > 'z' 
	   || lines[pos][start+j*4] < 'a' && lines[pos][start+j*4] > 'Z'
	   || lines[pos][start+j*4] < 'A') && lines[pos][start+j*4] != '_')
	  &&
	((lines[pos][start-4] > 'z' 
	   || lines[pos][start-4] < 'a' && lines[pos][start-4] > 'Z'
	   || lines[pos][start-4] < 'A') && lines[pos][start-4] != '_'))
	return strlen(keyword[i]);
    }
    else if(HIGHLIGHT_MODE == F77_MODE || HIGHLIGHT_MODE == HTML_MODE
	    || HIGHLIGHT_MODE == PASCAL_MODE)
    {
      int j;
      //parse the keyword letter-by-letter. In this case, the
      //keywords are saved as SMALL LETTERS, so check for both capital
      //and small versions of the keyword.
      for(j = 0; j < strlen(keyword[i]); j++)
	if(lines[pos][start+j*4] == keyword[i][j]
	   || lines[pos][start+j*4] == (keyword[i][j])-32) continue;
	else { result = -1; break; }
      if(result == -1) result = 0;
      else //make sure this is not part of another word
	if(((lines[pos][start+j*4] > 'z' 
	   || lines[pos][start+j*4] < 'a' && lines[pos][start+j*4] > 'Z'
	   || lines[pos][start+j*4] < 'A') && lines[pos][start+j*4] != '_')
	  &&
	((lines[pos][start-4] > 'z' 
	   || lines[pos][start-4] < 'a' && lines[pos][start-4] > 'Z'
	   || lines[pos][start-4] < 'A') && lines[pos][start-4] != '_'))
	return strlen(keyword[i]);
    }
    else
    {
      int j;
      //parse the keyword letter-by-letter. In this case, the
      //keywords are saved as SMALL LETTERS, and they should be
      //interpreted strictly in this form. kewords in C/C++,
      //JavaScript, Perl, and Python should all be small case.
      for(j = 0; j < strlen(keyword[i]); j++)
	if(lines[pos][start+j*4] == keyword[i][j]) continue;
	else { result = -1; break; }
      if(result == -1) result = 0;
      else //make sure this is not part of another word
	if(((lines[pos][start+j*4] > 'z' 
	   || lines[pos][start+j*4] < 'a' && lines[pos][start+j*4] > 'Z'
	   || lines[pos][start+j*4] < 'A') && lines[pos][start+j*4] != '_')
	  &&
	((lines[pos][start-4] > 'z' 
	   || lines[pos][start-4] < 'a' && lines[pos][start-4] > 'Z'
	   || lines[pos][start-4] < 'A') && lines[pos][start-4] != '_'))
	return strlen(keyword[i]);
    }
  }
  return 0;
}

void refreshView() 
{
  //turn the cursor off
  printf("\e[?25l");
  int i;
  if(totalLines-firstVisLine < totalVisLines &&
     totalLines > totalVisLines) 
  {
    i = firstVisLine;
    firstVisLine = totalLines-totalVisLines;
    selectedLine += i-firstVisLine;
  }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  if(WRAP_LINES)
  {//if #1
    int swap = 0;
    if(totalLines < totalVisLines) 
    {//if #2
      if(SELECTING || SELECTED) 
      {//if #3
	//////////////////////////////////////////
	//If in Selecting Mode
	//////////////////////////////////////////
	if(sel_range_start.nline > sel_range_end.nline) { swap = 1; swap_lines(); }
	else if(sel_range_start.nline == sel_range_end.nline &&
	  sel_range_start.nchar > sel_range_end.nchar) { swap = 2; swap_chars(); }
	for(i = 0; i < totalLines; i++) 
	{
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	    int j, k, l;
	    if(i == sel_range_start.nline) 
	    { 
	      k = sel_range_start.nchar; l = totalCharsInLine[i]-1;
	    } 
	    else if(i == sel_range_end.nline) 
	    { 
	      k = 0; l = sel_range_end.nchar;
	    } 
	    else if(i > sel_range_start.nline && i < sel_range_end.nline) 
	    { 
	      k = 0; l = totalCharsInLine[i]-1;
	    } 
	    else 
	    { 
	      k = -1; l = -1;
	    }
	    int carry = 0;
	    for(j = 0; j < strlen(lines[i])/4; j++) 
	    {
	      if(j >= k && j <= l) setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      else setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	      int k;
	      if(lines[i][j*4] == '\t')
	      { k = TAB_CHARS-((j+carry)%TAB_CHARS); k=0?TAB_CHARS:k; carry+=(k-1); printf("%*s", k, " "); }
	      else putuchar(i, j*4);
	    }
	    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    if(totalCharsInLine[i] < MAX_CHARS_PER_LINE)
	      printf("%*s", MAX_CHARS_PER_LINE-j, " ");
	}//end for1
	for(i = totalLines; i < totalVisLines; i++)
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	  printf("%*s", MAX_CHARS_PER_LINE, " ");
	}//end for2
	if(swap == 1) swap_lines();//return them back to normal
	if(swap == 2) swap_chars();//return them back to normal
      } 
      else 
      {
	//////////////////////////////////////////
	//If in Regular Mode
	//////////////////////////////////////////
	int j, k;
	for(i = 0; i < totalLines; i++) 
	{
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	  int carry = 0;
	  if(AUTO_HIGHLIGHTING)
	    refreshSelectedLineInColor(firstVisLine+i);
	  else
	    for(j = 0; j < strlen(lines[i]); j+=4)
	      if(lines[i][j] == '\t')
	      { k = TAB_CHARS-((j/4+carry)%TAB_CHARS); k=0?TAB_CHARS:k; carry+=(k-1); printf("%*s", k, " "); }
	      else putuchar(i, j);
	  if(totalCharsInLine[firstVisLine+i] < MAX_CHARS_PER_LINE)
	    printf("%*s", MAX_CHARS_PER_LINE-totalCharsInLine[firstVisLine+i], " ");
	}//end for1
	for(i = totalLines; i < totalVisLines; i++) 
	{
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	  printf("%*s", MAX_CHARS_PER_LINE, " ");
	}//end for2
      }//end if #3
    } 
    else 
    {
      if(SELECTING || SELECTED) 
      {//if #4
	//////////////////////////////////////////
	//If in Selecting Mode
	//////////////////////////////////////////
	if(sel_range_start.nline > sel_range_end.nline) { swap = 1; swap_lines(); }
	else if(sel_range_start.nline == sel_range_end.nline &&
	  sel_range_start.nchar > sel_range_end.nchar) { swap = 2; swap_chars(); }
	for(i = 0; i < totalVisLines; i++) 
	{
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	    int j, k, l;
	    if(i == sel_range_start.nline-firstVisLine) 
	    { 
	      k = sel_range_start.nchar; l = totalCharsInLine[firstVisLine+i]-1;
	    } 
	    else if(i == sel_range_end.nline-firstVisLine) 
	    { 
	      k = 0; l = sel_range_end.nchar;
	    } 
	    else 
	    { 
	      k = 0; 
	      if(i > sel_range_start.nline-firstVisLine &&
		 i < sel_range_end.nline-firstVisLine)
		    l = totalCharsInLine[firstVisLine+i]-1;
	      else l = -1;
	    }
	    int carry = 0;
	    for(j = 0; j < strlen(lines[firstVisLine+i])/4; j++) 
	    {
	      if(j >= k && j <= l) setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	      else setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	      int l;
	      if(lines[i][j*4] == '\t')
	      { l = TAB_CHARS-((j+carry)%TAB_CHARS); l=0?TAB_CHARS:l; carry+=(l-1); printf("%*s", l, " "); }
	      else putuchar(firstVisLine+i, j*4);
	    }
	    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    if(totalCharsInLine[firstVisLine+i] < MAX_CHARS_PER_LINE)
	      printf("%*s", MAX_CHARS_PER_LINE-j, " ");
	}//end for1
	if(swap == 1) swap_lines();//return them back to normal
	if(swap == 2) swap_chars();//return them back to normal
      } 
      else 
      {
	//////////////////////////////////////////
	//If in Regular Mode
	//////////////////////////////////////////
	int j, k;
	for(i = 0; i < totalVisLines; i++) 
	{
	  fprintf(stdout, "\e[%d;%dH", i+3, 2);
	  int carry = 0;
	  int printed_chars = 0;
	  if(AUTO_HIGHLIGHTING)
	    refreshSelectedLineInColor(firstVisLine+i);
	  else 
	    for(j = 0; j < strlen(lines[firstVisLine+i]); j+=4)
	      if(lines[firstVisLine+i][j] == '\n') break;
	      else if(lines[firstVisLine+i][j] == '\t')
	      { k = TAB_CHARS-((j/4+carry)%TAB_CHARS); k=0?TAB_CHARS:k; carry+=(k-1); printf("%*s", k, " "); printed_chars += k; }
	      else { putuchar(firstVisLine+i, j); printed_chars++; }
	  if(printed_chars < MAX_CHARS_PER_LINE)
	    printf("%*s", MAX_CHARS_PER_LINE-printed_chars, " ");
	}//end for
      }//end if #4
    }//end if #2
    fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
  }//end if #1
  drawMenuBar(1, 1, SCREEN_W);		//draw main menu bar
  drawBox(2, 1, SCREEN_H-1, SCREEN_W, documentTitle, NO);	//draw main window
  drawScrollBar();
  refreshBottomView();
  //turn the cursor on
  printf("\e[?25h");
  fflush(stdout);
}//end refreshView()

void drawScrollBar() 
{
  int h = SCREEN_H-5;
  int i;
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  for(i = 0; i <= h; i++)
    printf("\e[%d;%dH ", i+3, SCREEN_W);
  double h2;
  h2 = firstVisLine+selectedLine+1;
  h2 /= totalLines;
  h2 *= h;
  if(h2 < 0) h2 = 0;
  if(h2 > (SCREEN_H-5)) h2 = SCREEN_H-5;
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  printf("\e[%d;%dH%c", (int)(h2)+3, SCREEN_W, 177);
  fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
}

void refreshBottomView() 
{
  setScreenColors(FG_COLOR[COLOR_STATUS_BAR], BG_COLOR[COLOR_STATUS_BAR]);
  fprintf(stdout, "\e[%d;%dH", SCREEN_H, 0);
  printf("%*s", SCREEN_W, " ");
  fprintf(stdout, "\e[%d;%dH", SCREEN_H, SCREEN_W-19);
  printf("| LINE:%-3d COL:%-3d", firstVisLine+selectedLine+1, selectedChar+1);  
  if(CAPS) fprintf(stdout, "\e[%d;%dHCAPS", SCREEN_H, SCREEN_W-24);
  if(INSERT) fprintf(stdout, "\e[%d;%dHINS", SCREEN_H, SCREEN_W-28);
  if(SELECTING) fprintf(stdout, "\e[%d;%dHSEL", SCREEN_H, SCREEN_W-32);
  fprintf(stdout, "\e[%d;%dH", SCREEN_H, 2);
  switch(FILE_STATE) 
  {
    case(MODIFIED): printf("Modified"); break;
    case(NEW):      printf("New");      break;
    case(SAVED):    printf("Saved");    break;
    case(OPENED):   printf("Opened");   break;
    case(IDLE):     printf("Idle");     break;
  }
  if(selectedChar+selectedCharCarry > MAX_CHARS_PER_LINE-1)
    fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+1+selectedCharCarry);
  else fprintf(stdout, "\e[%d;%dH", selectedLine+3, selectedChar+2+selectedCharCarry);
  fflush(stdout);
}

/***************************************
 * drawMenuBar(): 
 * Procedure to draw the main menu bar.
 * **************************************/
void drawMenuBar(int x, int y, int w) 
{
  setScreenColors(FG_COLOR[COLOR_MENU_BAR], BG_COLOR[COLOR_MENU_BAR]);
  fprintf(stdout, "\x1b[%d;%dH", x, y);		//reposition the cursor
  int i,j, lastChar=y;
  for(i = 0; i < w; i++) fputc(' ', stdout);	//Draw empty menu bar
  fprintf(stdout, "\x1b[%d;%dH", x, y);		//reposition the cursor

  for(i = 0; i < totalMainMenus; i++) 
  {
    j=0; lastChar++;
    fprintf(stdout, " ");
    while(menu[i][j] != '\0') 
    {
      if(menu[i][j] == '&') 
      {	//turn on underline feature to print the shortcut key
	fprintf(stdout, "\x1b[4m%c", menu[i][j+1]);
	fprintf(stdout, "\x1b[24m"); //then turn it off
      }
      else
	fprintf(stdout, "%c", menu[i][j+1]);	//print normal chars (other than the
      lastChar++; j++;					//shortcut key)
    }
    fprintf(stdout, " ");
  }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\x1b[24m");
  fflush(stdout);
}

void catchSignals() 
{
    if(signal(SIGINT, sighandler) == SIG_ERR) 
    {
      printf("Error interrupting SIGINT.\n");
      exit(1);
    }
    if(signal(SIGQUIT, sighandler) == SIG_ERR) 
    {
      printf("Error interrupting SIGQUIT.\n");
      exit(1);
    }
    if(signal(SIGABRT, sighandler) == SIG_ERR) 
    {
      printf("Error interrupting SIGABRT.\n");
      exit(1);
    }
    if(signal(SIGTERM, sighandler) == SIG_ERR) 
    {
      printf("Error interrupting SIGTERM.\n");
      exit(1);
    }
    if(signal(SIGTSTP, sighandler) == SIG_ERR) 
    {
      //exit(1);
    }
    if(signal(SIGKILL, sighandler) == SIG_ERR) 
    {
      //exit(1);
    }
    if(signal(SIGSTOP, sighandler) == SIG_ERR) 
    {
      //exit(1);
    }
}//end catchSignals()
      
/***************************************
 * drawBox(): 
 * Procedure to draw a box with the given
 * coordinates, title, and a flag
 * indicating whether to clear the window
 * area or not (passed as YES or NO).
 * **************************************/
void drawBox(int x1, int y1, int x2, int y2, char *title, int clearArea) 
{
  char spaces[y2-y1];
  int i;
  for(i = 0; i < y2-y1-1; i++) spaces[i] = ' ';
  spaces[i] = '\0';
  //Draw the box first//
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\x1b[%d;%dH", x1, y1);	//control sequence to move cursor
  /* NOTE: this is a TERRIBLE hack! but it does the following:
   *       "\e)0" will define G1 charset to be "VT100 Graphics Mapping"
   *       "\x0e" a.k.a. ^N, activates G1 charset.
   */
  fprintf(stdout, "\e)0\x0e");
  fflush(stdout);

  putchar(ULC);					//print the upper-left corner
  for(i = 0; i < (y2-y1)-1; i++) 
  {
    putchar(HB);				//print the horizontal upper bar
  }
  putchar(URC);  				//print the upper-right corner
  putchar('\n');			//finished window top, make a new line
  
  for(i = 0; i < (x2-x1)-1; i++) 
  {
    fprintf(stdout, "\x1b[%d;%dH", x1+i+1, y1);	//move cursor to left window edge
    if(clearArea == YES) 
    {
      fprintf(stdout, "%c%s\x1b[%d;%dH", VB, spaces, x1+i+1, y2);
      fprintf(stdout, "\e(0%c", VB);	//print left VB, spaces and right vertical bar
    } 
    else 
    {//print left VB, no spaces and right vertical bar
      fprintf(stdout, "%c\x1b[%d;%dH%c", VB, x1+i+1, y2, VB);
    }
  }

  fprintf(stdout, "\x1b[%d;%dH", x2, y1);	//control sequence to move cursor
  putchar(LLC);				//print the lower-left corner
  for(i = 0; i < (y2-y1)-1; i++) 
  {
    putchar(HB);			//print the horizontal lower bar
  }
  putchar(LRC);  
  /* NOTE: this is a TERRIBLE hack! but it does the following:
   *       "\x0f" a.k.a. ^O, activates G0 charset.
   */
  fprintf(stdout, "\x0f");
  fflush(stdout);  
  
  //Then put on the box title, if any//
  if(title != NULL) 
  {
    int tmp1=(y2-y1)/2;
    int tmp2=strlen(title)/2;
    fprintf(stdout, "\x1b[%d;%dH%s",	//move the cursor
		    x1,			//to the top
		    y1+tmp1-tmp2,	//and center of the box
		    title);		//to print this title
  }
  fflush(stdout);
}

/**************************************
 * makes a regular string from mino's
 * special 4-byte length string.
 * ************************************/
int makestr(char *newstr, char *str)
{
  //static char *c = (char *)malloc(MAX_CHARS_PER_LINE);
  int i = 0;
  int j = 0;
  while(i < strlen(str))
  {
    if(str[i] == '\0') break;
    newstr[j++] = str[i];
    if((str[i] & mask[0]) == mask[0]) newstr[j++] = str[i+1];
    if((str[i] & mask[1]) == mask[1]) newstr[j++] = str[i+2];
    if((str[i] & mask[2]) == mask[2]) newstr[j++] = str[i+3];
    i+=4;
  }
  newstr[j] = '\0';
  return j;
}


void move_lines_up(int first, int last)
{
  int i;
  for(i = first; i < last; i++)
  {
    strcpy(lines[i], lines[i+1]);
    LINE_IS_LINKED[i] = LINE_IS_LINKED[i+1];
    totalCharsInLine[i] = totalCharsInLine[i+1];
  }
  lines[last][0] = '\0';
  LINE_IS_LINKED[last] = 0;
  totalCharsInLine[last] = 0;
  totalLines--;
}

void move_lines_down(int first, int last)
{
  int i;
  for(i = first; i > last; i--)
  {
    strcpy(lines[i], lines[i-1]);
    LINE_IS_LINKED[i] = LINE_IS_LINKED[i-1];
    totalCharsInLine[i] = totalCharsInLine[i-1];
  }
  lines[last][0] = '\0';
  LINE_IS_LINKED[last] = 0;
  totalCharsInLine[last] = 0;
  totalLines++;
}

void calcCharCarry(int pos)
{
  selectedCharCarry = 0;
  int i;
  for(i = 0; i < selectedChar; i++)
    if(lines[pos][i*4] == '\t')
    {
      int j = TAB_CHARS -((i+selectedCharCarry)%TAB_CHARS);
      j = 0 ? TAB_CHARS : j;
      selectedCharCarry+=(j-1);
    }
}
