/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015, 2016 (c)
 * 
 *    file: menu.c
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

#include <stdio.h>
#include <stdlib.h>
#include "defs.h"
#include "options.h"
//#include "print.h"
#include "edit.h"
#include "sys/stat.h"
#include "find.h"
#include <pwd.h>

#define EOL	10

extern char *tmp, *tmp2;
extern time_t tm;	//used to get system time to log it on shutdown

/********************************************
 * This procedure shows the FILE menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'HELP' and the right 'EDIT' respectively.
 * ******************************************/
void showFileMenu(int visible) 
{
  int i, ch, fSelect = 0;
  int endme = 0;
  if(visible == YES) 
  {	//show the file menu
    //turn the cursor off
    printf("\e[?25l");
    //activeWindow = FILE_MENU;
    drawBox(3, 2, 9, 19, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[4;3HNew directory ^N");
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    fprintf(stdout, "\e[5;3HOpen Location ^O");
    fprintf(stdout, "\e[6;3HExport Tree   ^E");
    fprintf(stdout, "\e[7;3HPrint         ^P");
    fprintf(stdout, "\e[8;3HExit          ^Q");
    fprintf(stdout, "\e[4;18H");
    fflush(stdout);
   while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  refreshWindows();
	  endme = 1;
	  break;
	case('p'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_up;
	case(UP_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_up:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;3H%s", fSelect+4, fileMenu[fSelect]);
	  fSelect--;
	  if(fSelect < 0) fSelect = fTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;3H%s", fSelect+4, fileMenu[fSelect]);
	  fprintf(stdout, "\e[%d;18H", fSelect+4);
          fflush(stdout);
	  break;
	case('n'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_down;
	case(DOWN_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_down:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;3H%s", fSelect+4, fileMenu[fSelect]);
	  fSelect++;
	  if(fSelect >= fTotal) fSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;3H%s", fSelect+4, fileMenu[fSelect]);
	  fprintf(stdout, "\e[%d;18H", fSelect+4);
          fflush(stdout);
	  break;
	//case(ALT_F_KEY)://hide file menu and return control to main program
	case('f'):
	 if(ALT) 
	 {
	  activeWindow = DIR_WIN;
	  refreshWindows();
	  return;
	 }
	 else if(CTRL)
	   if(GNU_DOS_LEVEL > 1) goto do_right;
	 break;
	//case(ALT_E_KEY):	//navigate to Edit menu
	case('e'):
	  if(!ALT) break;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  //fprintf(log_file, "Opening edit menu..\n");
	  refreshWindows();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case('h'):
	  if(!ALT) break;
	  goto do_left;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  //fprintf(log_file, "Opening help menu..\n");
	  refreshWindows();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(fSelect == 0) 
	  {	//user selected 'New directory'
	    char *res = (char *) malloc(MAX_DIR_NAME_LEN);
	    if(!res) { msgBox("Insufficient memory", OK, ERROR); endme = 1; break; }
	    
	    inputBox("Enter directory name to create:", "New Directory");
	    strcpy(res, input);
	    if(res != NULL) 
	    {
	      struct stat st;
	      if(stat(res, &st) == -1) 
	      {
		mkdir(res, 0775);//check if it doesn't exist, create it
		fprintf(log_file, "Creating directory %s..\n", res);
	      } 
	      else 
	      {
		msgBox("Directory already exists!", OK, ERROR);
		fprintf(log_file, "Directory %s already exists..\n", res);
	      }
	      scanDir(cwd);
	      refreshWindows();
	    } free(res);
	    endme = 1;
	    break;
	  }
	  if(fSelect == 2) 
	  {	//user selected 'Export Tree'
	    exportTree(YES);
	    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    scanDir(cwd);
	    endme = 1;
	    break;
	  }
	  if(fSelect == 1) 
	  {	//user selected 'Open location'
	    char *res = file_open_location();
	    if(res != NULL) 
	    {
	      endme = 1;
	      break;
	    }
	  }
	  if(fSelect == 3) 
	  {	//user selected 'Print'
	    msgBox("Oops! This function is currently not implemented.", 
		   OK, INFO);
	    //fprintf(log_file, "Showing Print dialog box..\n");
	    //showPrintDialogBox();
	    refreshFileView();
	    refreshDirView();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 4) 
	  {	//user selected 'Exit'
	    i = msgBox("Are you sure you want to exit?", YES | NO, CONFIRM);
	    if(i == YES) 
	    { 	//exit gracefully
	      exit_gracefully();
	    }
	  }
	  refreshWindows();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the file menu
    activeWindow = DIR_WIN;
    refreshWindows();
    return;
  }
  //turn the cursor on
  printf("\e[?25h");
  //fprintf(log_file, "Returning to main program from File Menu\n");
}

/********************************************
 * This procedure shows the EDIT menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'FILE' and the right 'HELP' respectively.
 * ******************************************/
void showEditMenu(int visible) 
{
  int i, ch, eSelect = 0;
  int endme = 0;
  if(visible == YES) 
  {	//show the edit menu
    //turn the cursor off
    printf("\e[?25l");
    //activeWindow = EDIT_MENU;
    drawBox(3, 8, 11, 22, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[4;9H%s", editMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < eTotal; i++) 
    {
      fprintf(stdout, "\e[%d;9H%s", i+4, editMenu[i]);
    }
    //fprintf( log_file, "Now in line 109 in showEditMenu()..\n" );
    fprintf(stdout, "\e[4;22H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  refreshWindows();
	  endme = 1;
	  break;
	case('p'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_up;
	case(UP_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_up:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;9H%s", eSelect+4, editMenu[eSelect]);
	  eSelect--;
	  if(eSelect < 0) eSelect = eTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;9H%s", eSelect+4, editMenu[eSelect]);
	  fprintf(stdout, "\e[%d;22H", eSelect+4);
          fflush(stdout);
	  break;
	case('n'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_down;
	case(DOWN_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_down:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;9H%s", eSelect+4, editMenu[eSelect]);
	  eSelect++;
	  if(eSelect >= eTotal) eSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;9H%s", eSelect+4, editMenu[eSelect]);
	  fprintf(stdout, "\e[%d;22H", eSelect+4);
          fflush(stdout);
	  break;
	case('e'):
	 if(ALT) 
	 {
	  activeWindow = DIR_WIN;
	  refreshWindows();
	  return;
	 }
	  break;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case('f'):
	 if(CTRL)
	   if(GNU_DOS_LEVEL > 1) goto do_right;
	 if(!ALT) break;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  //fprintf(log_file, "Opening file menu..\n");
	  refreshWindows();
	  showFileMenu(YES);
	  endme = 1;
	  break;
	case('h'):
	  if(!ALT) break;
	  //fprintf(log_file, "Opening help menu..\n");
	  refreshWindows();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case('o'):
	 if(!ALT) break;
	 goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  //fprintf(log_file, "Opening options menu..\n");
	  refreshWindows();
	  showOptionsMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(eSelect == 0) cutMarked();
	  if(eSelect == 1) copyMarked();
	  if(eSelect == 2) pasteMarked();
	  if(eSelect == 3) markAll();
	  if(eSelect == 4) unMarkAll();
	  if(eSelect == 5) clearSelection();
	  if(eSelect == 6) 
	  { 
	    findFile();
	    hideCursor();
	    scanDir(cwd);
	  }
	  if(eSelect == 7) 
	  {
	    showPropertiesDialog();
	    scanDir(cwd); 
	  }
	  refreshAll();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the edit menu
    activeWindow = DIR_WIN;
    refreshWindows();
    return;
  }
  //turn the cursor on
  printf("\e[?25h");
  //fprintf(log_file, "Returning to main program from Edit Menu\n");
}


/********************************************
 * This procedure shows the OPTIONS menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'EDIT' and the right 'HELP' respectively.
 * ******************************************/
void showOptionsMenu(int visible)
{
  int i, ch, oSelect = 0;
  int endme = 0;
  if(visible == YES) 
  {	//show the options menu
    //turn the cursor off
    printf("\e[?25l");
    //activeWindow = HELP_MENU;
    drawBox(3, 14, 7, 30, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[4;15H%s", optionsMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < oTotal; i++) 
      fprintf(stdout, "\e[%d;15H%s", i+4, optionsMenu[i]);
    fprintf(stdout, "\e[4;30H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  refreshWindows();
	  endme = 1;
	  break;
	case('p'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_up;
	case(UP_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_up:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+4, optionsMenu[oSelect]);	//clear last selection
	  oSelect --;
	  if(oSelect < 0) oSelect = oTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+4, optionsMenu[oSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;30H", oSelect+4);
	  fflush(stdout);
	  break;
	case('n'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_down;
	case(DOWN_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_down:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+4, optionsMenu[oSelect]);	//clear last selection
	  oSelect++;
	  if(oSelect >= oTotal) oSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+4, optionsMenu[oSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;30H", oSelect+4);
	  fflush(stdout);
	  break;
	case('o'):	//hide the file menu and return control to main program
	  if(!ALT) break;
	  refreshWindows();
	  return;
	  break;
	case('h'):	//navigate to Help menu
	  if(!ALT) break;
	  goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  refreshWindows();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case('f'):	//navigate to File menu
	  if(CTRL && GNU_DOS_LEVEL > 1) goto do_right;
	  if(!ALT) break;
	  refreshWindows();
	  showFileMenu(YES);
	  endme = 1;
	  break;
	case('e'):	//navigate to Edit menu
	  if(!ALT) break;
	  goto do_left;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  refreshWindows();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(oSelect == 0) 
	  {			//selected 'Properties'
	    showPropertiesDialog();
	    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    scanDir(cwd);
	  }
	  else if(oSelect == 1) 
	  {			//selected 'Change colors'
	    optionsMenu_Change_Colors();
	  }
	  else if(oSelect == 2) 
	  {			//selected 'Reset config'
	    optionsMenu_Reset_Config();
	  }
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshWindows();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the options menu
    refreshWindows();
    return;
  }
  //turn the cursor on
  printf("\e[?25h");
  //fprintf(log_file, "Returning to main program from Options Menu\n");
}


/********************************************
 * This procedure shows the HELP menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'OPTIONS' and the right 'FILE' respectively.
 * ******************************************/
void showHelpMenu(int visible) 
{
  int i, ch, hSelect = 0;
  int endme = 0;
  if(visible == YES) 
  {	//show the help menu
    //turn the cursor off
    printf("\e[?25l");
    //activeWindow = HELP_MENU;
    drawBox(3, 23, 8, 39, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[4;24H%s", helpMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < hTotal; i++) 
      fprintf(stdout, "\e[%d;24H%s", i+4, helpMenu[i]);
    fprintf(stdout, "\e[4;39H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  refreshWindows();
	  endme = 1;
	  break;
	case('p'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_up;
	case(UP_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_up:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;24H%s", hSelect+4, helpMenu[hSelect]);
	  hSelect--;
	  if(hSelect < 0) hSelect = hTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;24H%s", hSelect+4, helpMenu[hSelect]);
	  fprintf(stdout, "\e[%d;39H", hSelect+4);
          fflush(stdout);
	  break;
	case('n'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_down;
	case(DOWN_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_down:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //clear last selection
	  fprintf(stdout, "\e[%d;24H%s", hSelect+4, helpMenu[hSelect]);
	  hSelect++;
	  if(hSelect >= hTotal) hSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  //highlight new selection
	  fprintf(stdout, "\e[%d;24H%s", hSelect+4, helpMenu[hSelect]);
	  fprintf(stdout, "\e[%d;39H", hSelect+4);
          fflush(stdout);
	  break;
	case('h'):
	 if(ALT) 
	 {
	  activeWindow = DIR_WIN;
	  refreshWindows();
	  return;
	 }
	 break;
	case('f'):
	 if(CTRL && GNU_DOS_LEVEL > 1) goto do_right;
	 if(!ALT) break;
	 goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  refreshWindows();
	  showFileMenu(YES);
	  endme = 1;
	  break;
	case('e'):
	  if(!ALT) break;
	  refreshWindows();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case('o'):
	 if(!ALT) break;
	 goto do_left;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  refreshWindows();
	  showOptionsMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(hSelect == 0) 
	  {			//show README
	    //fprintf(log_file, "Opening README..\n");
	    showReadMe();
	  } 
	  else if(hSelect == 1) 
	  {			//show KEYBINDINGS file
	    showKeybindings();
	  } 
	  else if(hSelect == 3) 
	  {		//show About msgbox
	    char *x = " Prime for GNU/Linux. \n Developed by "
		      "Mohammed Isam, \n 2013, 2014, 2015, 2016.";
	    show_about(x);
	  } 
	  else if(hSelect == 2) 
	  {	//show Quick reference
	    fprintf(log_file, "Opening Quick reference..\n");
	    drawBox(3, 5, 18, SCREEN_W-5, "Quick Reference", YES);
	    fprintf(stdout, "\e[4;7HBasic functions:");
	    fprintf(stdout, "\e[5;9HSPACEBAR: Toggle select/unselect");
	    fprintf(stdout, "\e[6;9HENTER: Navigate to directory");
	    fprintf(stdout, "\e[7;9HTAB: Navigate between dir/file "
			    "view windows");
	    fprintf(stdout, "\e[8;9HArrow keys: Navigate up/down");
	    fprintf(stdout, "\e[10;7HEditing:");
	    fprintf(stdout, "\e[11;9HDEL: Delete marked directories/files");
	    fprintf(stdout, "\e[12;9HCTRL+X: Cut marked directories/files");
	    fprintf(stdout, "\e[13;9HCTRL+C: Copy marked directories/files");
	    fprintf(stdout, "\e[14;9HCTRL+V: Paste marked directories/files");
	    fprintf(stdout, "\e[15;9HCTRL+F: Find files");
	    fprintf(stdout, "\e[16;9HCTRL+Q: Quit the program");
	    fprintf(stdout, "\e[17;7HPress any key to continue..");
            fflush(stdout);
	    do {;} while(!getKey());
	  }
	  refreshAll();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the help menu
    activeWindow = DIR_WIN;
    refreshWindows();
    return;
  }
  //turn the cursor on
  printf("\e[?25h");
  //fprintf(log_file, "Returning to main program from Help Menu\n");
}


////////////////////////////////////////////////////////
//shows a window with KEYBINDINGS file as its contents//
////////////////////////////////////////////////////////
void showKeybindings() 
{
  int res = show_readme("/usr/share/doc/gnudos/prime/keybindings", " KEYBINDINGS ", GNU_DOS_LEVEL);
  if(res != 0)
  {
    res = show_readme("/usr/local/share/doc/gnudos/prime/keybindings", " KEYBINDINGS ", GNU_DOS_LEVEL);
  }
  drawMenuBar(2, 2, SCREEN_W-2);	//draw main menu bar
  return;
}


/******************************************
 * this function shows a window with 
 * README file as its contents.
 * ****************************************/
void showReadMe() 
{
  int res = show_readme("/usr/share/doc/gnudos/prime/README", " README ", GNU_DOS_LEVEL);
  if(res != 0)
  {
    res = show_readme("/usr/local/share/doc/gnudos/prime/README", " README ", GNU_DOS_LEVEL);
  }
  drawMenuBar(2, 2, SCREEN_W-2);	//draw main menu bar
  return;
}

char *file_open_location()
{
  char *res = inputBox("Enter directory path to open:", "Open Location");
  if(res != NULL)
  {
    //************** check for '~'
    strcpy(tmp, res);
    if(strchr(tmp, '~'))
    {
      strcpy(tmp2, res+((strchr(res, '~')+1)-res));
      struct passwd *pass;//will be used to find the home dir
      if((pass = getpwuid(geteuid())))
	return NULL;
      strcpy(tmp, pass->pw_dir);
      strcat(tmp, "/");
      strcat(tmp, tmp2);
      strcat(tmp, "\0");
    }//***************
    scanDir(tmp);
    refreshWindows();
    return tmp;
  }
  return NULL;
}
