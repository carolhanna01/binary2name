/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2016 (c)
 * 
 *    file: menu.c
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

#include <stdio.h>
#include <stdlib.h>
#include "defs.h"
#include "kbd.h"
#include "file.h"
#include "edit.h"
#include "options.h"

#define EOL	10

extern void cutMarked();
extern void cutOne();
extern void markAll();
extern void unMarkAll();
extern void setScreenColors(int FG, int BG);

/********************************************
 * This procedure shows the FILE menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'HELP' and the right 'Mino' respectively.
 * ******************************************/
void showFileMenu(int visible) 
{
  int i, fSelect = 0;
  char *ch;
  int endme = 0;
  if(visible == YES) 
  {	//show the file menu
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    drawBox(2, 2, 9, 19, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[3;3H%s", fileMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < fTotal; i++) 
    {
      fprintf(stdout, "\e[%d;3H%s", i+3, fileMenu[i]);
    }
    fprintf(stdout, "\e[3;18H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch[0]) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  fflush(stdout);
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
	  fprintf(stdout, "\e[%d;3H%s", fSelect+3, fileMenu[fSelect]);	//clear last selection
	  fSelect--;
	  if(fSelect < 0) fSelect = fTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;3H%s", fSelect+3, fileMenu[fSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;18H", fSelect+3);
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
	  fprintf(stdout, "\e[%d;3H%s", fSelect+3, fileMenu[fSelect]);	//clear last selection
	  fSelect++;
	  if(fSelect >= fTotal) fSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;3H%s", fSelect+3, fileMenu[fSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;18H", fSelect+3);
	  fflush(stdout);
	  break;
	case('f'):	//hide the file menu and return control to main program
	  if(ALT)
	  {
	    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	    refreshView();
	    return;
	  }
	  else if(CTRL)
	   if(GNU_DOS_LEVEL > 1) goto do_right;
	  break;
	case('e'):	//navigate to Mino menu
	  if(!ALT) break;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case('h'):	//navigate to Help menu
	  if(!ALT) break;
	  goto do_left;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(fSelect == 0) 
	  {	//user selected 'New'
	    fileMenu_New();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 1) 
	  {	//user selected 'Open File'
	    fileMenu_Open();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 2) 
	  {	//user selected 'Save File'
	    fileMenu_Save();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 3) 
	  {	//user selected 'Save as..'
	    fileMenu_SaveAs();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 4) 
	  {	//user selected 'Print'
	    fileMenu_Print();
	    endme = 1;
	    break;
	  }
	  if(fSelect == 5) 
	  {	//user selected 'Exit'
	    fileMenu_Exit();
	  }
	  
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the file menu
    refreshView();
    return;
  }
}

/********************************************
 * This procedure shows the Mino menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'FILE' and the right 'OPTIONS' respectively.
 * ******************************************/
void showEditMenu(int visible) 
{
  int i, eSelect = 0;
  char *ch;
  int endme = 0;
  if(visible == YES) 
  {	//show the file menu
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    drawBox(2, 8, 13, 30, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[3;9H%s", editMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < eTotal; i++) 
    {
      fprintf(stdout, "\e[%d;9H%s", i+3, editMenu[i]);
    }
    fprintf(stdout, "\e[3;29H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch[0]) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
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
	  fprintf(stdout, "\e[%d;9H%s", eSelect+3, editMenu[eSelect]);	//clear last selection
	  eSelect--;
	  if(eSelect < 0) eSelect = eTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;9H%s", eSelect+3, editMenu[eSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;29H", eSelect+3);
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
	  fprintf(stdout, "\e[%d;9H%s", eSelect+3, editMenu[eSelect]);	//clear last selection
	  eSelect++;
	  if(eSelect >= eTotal) eSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;9H%s", eSelect+3, editMenu[eSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;29H", eSelect+3);
	  fflush(stdout);
	  break;
	case('e'):	//hide the file menu and return control to main program
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  return;
	  break;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case('f'):	//navigate to File menu
	 if(CTRL)
	   if(GNU_DOS_LEVEL > 1) goto do_right;
	 if(!ALT) break;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  //setScreenColors(WHITE, BGBLUE);
	  refreshView();
	  showFileMenu(YES);
	  endme = 1;
	  break;
	case('h'):	//navigate to Help menu
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case('o'):	//navigate to Options menu
	  if(!ALT) break;
	 goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showOptionsMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(eSelect == 0) editMenu_Cut();
	  if(eSelect == 1) editMenu_Copy();
	  if(eSelect == 2) editMenu_Paste();
	  if(eSelect == 3) editMenu_SelectAll();
	  if(eSelect == 4) editMenu_Undo();
	  if(eSelect == 5) editMenu_Redo();
	  if(eSelect == 6) editMenu_DeleteLine();
	  if(eSelect == 7) editMenu_Find();
	  if(eSelect == 8) editMenu_Replace();
	  if(eSelect == 9) editMenu_ToggleSelectMode();
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the edit menu
    refreshView();
    return;
  }
}



/********************************************
 * This procedure shows the OPTIONS menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'OPTIONS' and the right 'HELP' respectively.
 * ******************************************/
void showOptionsMenu(int visible)
{
  int i, oSelect = 0;
  char *ch;
  int endme = 0;
  if(visible == YES) 
  {	//show the file menu
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    drawBox(2, 14, 7, 31, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[3;15H%s", optionsMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < hTotal; i++) fprintf(stdout, "\e[%d;15H%s", i+3, optionsMenu[i]);
    fprintf(stdout, "\e[3;30H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch[0]) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
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
	  fprintf(stdout, "\e[%d;15H%s", oSelect+3, optionsMenu[oSelect]);	//clear last selection
	  oSelect --;
	  if(oSelect < 0) oSelect = oTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+3, optionsMenu[oSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;30H", oSelect+3);
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
	  fprintf(stdout, "\e[%d;15H%s", oSelect+3, optionsMenu[oSelect]);	//clear last selection
	  oSelect++;
	  if(oSelect >= oTotal) oSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;15H%s", oSelect+3, optionsMenu[oSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;30H", oSelect+3);
	  fflush(stdout);
	  break;
	case('o'):	//hide the file menu and return control to main program
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  return;
	  break;
	case('h'):	//navigate to Help menu
	  if(!ALT) break;
	  goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showHelpMenu(YES);
	  endme = 1;
	  break;
	case('f'):	//navigate to File menu
	  if(CTRL && GNU_DOS_LEVEL > 1) goto do_right;
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
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
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(oSelect == 0) 
	  {			//selected 'Change colors'
	    optionsMenu_Change_Colors();
	  }
	  else if(oSelect == 1) 
	  {			//selected 'Tab spaces'
	    optionsMenu_Tab_Spaces();
	  }
	  else if(oSelect == 2) 
	  {			//selected 'Autoindent'
	    if(AUTO_INDENT)
	    {
	      AUTO_INDENT = 0;
	      optionsMenu[2][14] = ' ';
	    }
	    else
	    {
	      AUTO_INDENT = 1;
	      optionsMenu[2][14] = '*';
	    }
	  }
	  else if(oSelect == 3) 
	  {			//selected 'Reset config'
	    optionsMenu_Reset_Config();
	  }
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the options menu
    refreshView();
    return;
  }
}


/********************************************
 * This procedure shows the HELP menu under
 * the main menu bar. It also takes control
 * of the user input to navigate the menu
 * with the arrow keys and to select menu
 * items with ENTER. Pressing right or left
 * arrows navigate to next menu on the left
 * 'Mino' and the right 'FILE' respectively.
 * ******************************************/
void showHelpMenu(int visible) 
{
  int i, hSelect = 0;
  char *ch;
  int endme = 0;
  if(visible == YES) 
  {	//show the file menu
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    drawBox(2, 23, 7, 40, NULL, YES);
    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
    fprintf(stdout, "\e[3;24H%s", helpMenu[0]);
    setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
    for(i = 1; i < hTotal; i++) fprintf(stdout, "\e[%d;24H%s", i+3, helpMenu[i]);
    fprintf(stdout, "\e[3;39H");
    fflush(stdout);
    while(!endme) 
    {				//wait for user input
      ch = getKey();
      switch(ch[0]) 
      {
	case('g'):
	  if(GNU_DOS_LEVEL < 3) break;
	  if(!CTRL) break;
	  goto do_esc;
	case(ESC_KEY):
	  if(GNU_DOS_LEVEL > 2) break;
do_esc:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
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
	  fprintf(stdout, "\e[%d;24H%s", hSelect+3, helpMenu[hSelect]);	//clear last selection
	  hSelect--;
	  if(hSelect < 0) hSelect = hTotal-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;24H%s", hSelect+3, helpMenu[hSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;39H", hSelect+3);
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
	  fprintf(stdout, "\e[%d;24H%s", hSelect+3, helpMenu[hSelect]);	//clear last selection
	  hSelect++;
	  if(hSelect >= hTotal) hSelect = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  fprintf(stdout, "\e[%d;24H%s", hSelect+3, helpMenu[hSelect]);	//highlight new selection
	  fprintf(stdout, "\e[%d;39H", hSelect+3);
	  fflush(stdout);
	  break;
	case('h'):	//hide the file menu and return control to main program
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  return;
	  break;
	case('f'):	//navigate to File menu
	 if(CTRL && GNU_DOS_LEVEL > 1) goto do_right;
	 if(!ALT) break;
	 goto do_right;
	case(RIGHT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_right:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showFileMenu(YES);
	  endme = 1;
	  break;
	case('e'):	//navigate to Mino menu
	  if(!ALT) break;
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showEditMenu(YES);
	  endme = 1;
	  break;
	case('o'):	//navigate to Options menu
	 if(!ALT) break;
	 goto do_left;
	case('b'):
	  if(GNU_DOS_LEVEL < 2) break;
	  if(!CTRL) break;
	  goto do_left;
	case(LEFT_KEY):
	  if(GNU_DOS_LEVEL > 1) break;
do_left:
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  refreshView();
	  showOptionsMenu(YES);
	  endme = 1;
	  break;
	case(ENTER_KEY):
	  if(hSelect == 0) 
	  {			//show README
	    showReadMe();
	  } 
	  else if(hSelect == 1) 
	  {			//show KEYBINDINGS file
	    showKeybindings();
	  } 
	  else if(hSelect == 3) 
	  {		//show About msgbox
	    char *x = "Mino for GNU/Linux.\nDeveloped by Mohammed Isam,\n2014, 2015.";
	    msgBox(x, OK, INFO);
	  } 
	  else if(hSelect == 2) 
	  {	//show Quick reference
	    drawBox(3, 5, 24, SCREEN_W-5, "Quick Reference", YES);
	    fprintf(stdout, "\e[4;7HBasic functions:");
	    fprintf(stdout, "\e[5;9HArrow keys: move around");
	    fprintf(stdout, "\e[6;9HALT+F: Open File menu");
	    fprintf(stdout, "\e[7;9HALT+E: Open Mino menu");
	    fprintf(stdout, "\e[8;9HALT+O: Open Options menu");
	    fprintf(stdout, "\e[9;9HALT+H: Open Help menu");
	    fprintf(stdout, "\e[11;7HShortcut keys:");
	    fprintf(stdout, "\e[12;9HCTRL+O: Open file dialog");
	    fprintf(stdout, "\e[13;9HCTRL+S: Save file");
	    fprintf(stdout, "\e[14;9HCTRL+Q: Exit mino");
	    fprintf(stdout, "\e[15;9HCTRL+X: Cut selection");
	    fprintf(stdout, "\e[16;9HCTRL+C: Copy selection");
	    fprintf(stdout, "\e[17;9HCTRL+V: Paste selection");
	    fprintf(stdout, "\e[18;9HCTRL+Z: Undo");
	    fprintf(stdout, "\e[19;9HCTRL+Y: Redo");
	    fprintf(stdout, "\e[20;9HCTRL+A: Select All");
	    fprintf(stdout, "\e[21;9HCTRL+F: Find");
	    fprintf(stdout, "\e[22;9HCTRL+R: Find & Replace");
	    fprintf(stdout, "\e[23;7HPress any key to continue..");
	    fflush(stdout);
	    do {;} while(!getKey());
	  }
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  drawMenuBar(1, 1, SCREEN_W);
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, documentTitle, YES);
	  refreshView();
	  endme = 1;
	  break;
      }
    }
  } 
  else 
  {		//hide the help menu
    refreshView();
    return;
  }
}



void _do_show(int readme_keybindings)
{
  int x = 2;
  int y = 2;
  int w = SCREEN_W-1;
  int h = SCREEN_H-1;
  FILE *README;
  char buf[4096*4];	//buffer to hold data
  int buf_len = 0;
  
  char *file_name[] =
  {
    "/usr/share/doc/gnudos/mino/README",
    "/usr/local/share/doc/gnudos/mino/README",
    "/usr/share/doc/gnudos/mino/keybindings",
    "/usr/local/share/doc/gnudos/mino/keybindings"
  };
  char *title[] = { " README ", " KEYBINDINGS " };
  char *err[] =
  {
    "Failed to read the README file!.",
    "Failed to read the KEYBINDINGS file!."
  };
  /* choose file to open according to readme_keybindings flag */
  char *file1 = readme_keybindings ? file_name[2] : file_name[0];
  char *file2 = readme_keybindings ? file_name[3] : file_name[1];
  
  if(!(README = fopen(file1, "r"))) 
  {
   if(!(README = fopen(file2, "r"))) 
   {
    msgBox(err[readme_keybindings], OK, ERROR);
    refreshView();
    return;
   }
  }
  
  if(!(buf_len = fread(buf, sizeof(char), sizeof(buf), README))) 
  {
    msgBox(err[readme_keybindings], OK, ERROR);
    refreshView();
    return;
  }
  
  drawBox(x, y, h, w, title[readme_keybindings], YES);
  
  y++; x++;
  int i = 0, j = x;
  int l = y;
  int lineStart[60];
  int firstVisLine = 0;
  int cnt = 0;
  char *ch = (char *)malloc(5);
  char moreLines = 1;	//used as boolean to indicate if still more lines
  lineStart[0] = 0;
  
  fprintf(stdout, "\e[%d;%dH", j, l);
  while(j < (h-x+2)) 
  {
    if(buf[i] == EOL) 
    { 
      lineStart[++cnt] = i+1;
      l=y;
      fprintf(stdout, "\e[%d;%dH", ++j, l); 
    } 
    else 
    { 
      if(l > (w-y+2)) 
      {
	lineStart[++cnt] = i;
	l=y;
	fprintf(stdout, "\e[%d;%dH", ++j, l);
      } putchar(buf[i]); l++;
    }
    if(++i >= buf_len) break;
  }
  
  fflush(stdout);
  //if the output chars are less than the buffer length,
  //it means there are more lines to the output.
  if(i <= buf_len) 
  {
    goto readMore;
  } 
  else 
  {
    do {;} while(!getKey());
    goto end;
    return;
  }
  
readMore:
  while(1) 
  {
    ch = getKey();
    switch(ch[0]) 
    {
      case('g'):
	if(GNU_DOS_LEVEL < 3) break;
	if(!CTRL) break;
	goto do_esc;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
      case(ENTER_KEY):
      case(SPACE_KEY):
	goto end;
	break;
      case('p'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_up;
      case(UP_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_up:
	if(firstVisLine == 0) break;	//can't go up--first line already
	firstVisLine--;
	break;
      case('n'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_down;
      case(DOWN_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_down:
	if(!moreLines) break;	//reached last line
	firstVisLine++;
	break;
    }	//end of switch
    //redraw the box with its contents
    drawBox(x-1, y-1, h, w, title[readme_keybindings], YES);
    j = x; l = y; cnt = firstVisLine; i = lineStart[firstVisLine];
    
    fprintf(stdout, "\e[%d;%dH", j, l);
    while(j < (h-x+2)) 
    {
     if(buf[i] == EOL) 
     { 
      lineStart[++cnt] = i+1;
      l=y;
      fprintf(stdout, "\e[%d;%dH", ++j, l); 
     } 
     else 
     { 
      if(l > (w-y+2)) 
      {
	lineStart[++cnt] = i;
	l=y;
	fprintf(stdout, "\e[%d;%dH", ++j, l);
      } putchar(buf[i]); l++;
     }
     if(++i >= buf_len) break;
    }	//end of inner while
    if(i <= buf_len) { moreLines = 1; }
    else { moreLines = 0; }
    fflush(stdout);

  }	//end of outer while
end:
  fclose(README);
  drawMenuBar(2, 2, SCREEN_W-2);							//draw main menu bar
  fflush(stdout);
  return;
}

////////////////////////////////////////////////////////
//shows a window with KEYBINDINGS file as its contents//
////////////////////////////////////////////////////////
void showKeybindings() 
{
  _do_show(1);
}

///////////////////////////////////////////////////
//shows a window with README file as its contents//
///////////////////////////////////////////////////
void showReadMe() 
{
  _do_show(0);
}
