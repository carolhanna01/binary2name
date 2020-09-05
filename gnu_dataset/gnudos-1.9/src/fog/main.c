/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: main.c
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
#include "../corelib/dialogs.h"
#include "../corelib/screen.h"
#include "../corelib/kbd.h"
#include "options.h"
//#include "mouse.h"
#include "fog.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

//int endme = 0;

extern void init();
extern void drawForm();
extern void drawToolBox();
extern void getInputFromToolBoxWin();
extern void getInputFromFormWin();

void sighandler(int signo) 
{  
}

/*void mouse_handler(int button, mouse pos) {
  if(button)
    printf("Button %d clicked at %d, %d\n", button,
	 pos.x, pos.y);
}*/

int main(int argc, char *argv[]) 
{
 //if(!init_kbd()) {
  if(!initTerminal()) 
  {
   fprintf(stderr, "Error initializing keyboard. Aborting.\n");
   exit(1);
 }
 
  if(argc > 1) 
  {
   int i;
   for(i = 1; i < argc; i++) 
   {
    if(strcmp(argv[i], "--version" ) == 0 || strcmp(argv[i], "-v")) 
    {
      //******Show version
      printf("\nFog version 1.0.\n");
    } 
    else if(strcmp(argv[i], "--help" ) == 0 || strcmp(argv[i], "-h")) 
    {
      //******Show help
      printf("\nFog (Console Form Designer) version 1.0.\n");
      printf("For GNU/Linux\n");
      printf("Developed by Mohammed Isam, 2014\n");
      printf("\n\nUse:\n");
      printf("  %s\n", argv[0]);
      printf("\nFor more information, try 'info fog' or 'man fog'\n");
      exit(0);
    }
   }//end for
  }
 
 //init_mouse();
 getScreenSize();
 init();
 
 //clear the screen
 clearScreen();
 //set screen colors
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 drawMenuBar(1, 1, SCREEN_W);
 drawBox(2, 1, SCREEN_H-1, SCREEN_W, " FOG: The console Form Designer ", YES);
 drawToolBox();
 drawForm();
 drawStatusBar();
 
 //Loop to get user input
 while(!endme) 
 {
   if(activeWindow == TOOLBOX_WIN)
     getInputFromToolBoxWin();
   else if(activeWindow == FORM_WIN)
     getInputFromFormWin();
   
   //if(getKey() == ENTER_KEY) break;
 }
 //very important to restore keyboard state to its
 //previous state before exiting
 //restore_kbd();
 setScreenColors(WHITE, BGBLACK);
 restoreTerminal();
 setScreenColors(WHITE, BGBLACK);
 clearScreen();
 fprintf(stdout, "\x1b[24m");
 fflush(stdout);
 exit(0);
}//end main

/***************************************
 * drawStatusBar(): 
 * Procedure to draw the status bar.
 * **************************************/
void drawStatusBar() 
{
  setScreenColors(FG_COLOR[COLOR_MENU_BAR], BG_COLOR[COLOR_MENU_BAR]);
  fprintf(stdout, "\x1b[%d;%dH", SCREEN_H, 1);	//reposition the cursor
  int i;
  for(i = 0; i < SCREEN_W; i++) fputc(' ', stdout);//Draw empty menu bar
  fprintf(stdout, "\x1b[%d;%dH", SCREEN_H, 1);//reposition the cursor
  printf("Form Size: Width %d, Height %d", FORM_WIDTH, FORM_HEIGHT);
  printf(", Active [%s]", (activeWindow == TOOLBOX_WIN) ?
			   "TOOLBOX" : 
			   (total_form_tools > 0) ?
			   form_tool_text[selected_tool] : "FORM");
  //reposition the cursor
  if(activeWindow == TOOLBOX_WIN) 
  {
    printf("\e[%d;%dH", selected_tool+4, (int)(SCREEN_W-14+strlen(tool[selected_tool])));
  } 
  else 
  {
    if(total_form_tools > 0)
      printf("\e[%d;%dH", tool_pos[selected_tool].x+3, tool_pos[selected_tool].y+1);
    else
      printf("\e[3;2H");
  }
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
	fprintf(stdout, "\x1b[4m%c\x1b[24m", menu[i][j+1]);//then turn it off
      }
      else
	fprintf(stdout, "%c", menu[i][j+1]);//print normal chars (other than
      lastChar++; j++;					//shortcut key)
    }
    fprintf(stdout, " ");
  }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\x1b[24m");
  fflush(stdout);
}


void showMenu(int menu) 
{
  int i, sel;
draw_menus:
  sel = 0;
  int total = 0;
  int y = 1;
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  switch(menu) 
  {
    case(0):		//***** File menu *****//
      drawBox(2, 2, fTotal+3, 20, "", YES);
      for(i = 0; i < fTotal; i++) 
      {
	locate(i+3, 3);
	printf("%s", fileMenu[i]);
      } locate(sel+3, 19);
      total = fTotal; y = 3;
      break;
    case(1):		//***** Edit menu *****//
      drawBox(2, 8, eTotal+3, 28, "", YES);
      for(i = 0; i < eTotal; i++) 
      {
	locate(i+3, 9);
	printf("%s", editMenu[i]);
      } locate(sel+3, 27);
      total = eTotal; y = 9;
      break;
    case(2):		//***** Options menu *****//
      drawBox(2, 14, oTotal+3, 34, "", YES);
      for(i = 0; i < oTotal; i++) 
      {
	locate(i+3, 15);
	printf("%s", optionsMenu[i]);
      } locate(sel+3, 33);
      total = oTotal; y = 15;
      break;
    case(3):		//***** Help menu *****//
      drawBox(2, 23, hTotal+3, 41, "", YES);
      for(i = 0; i < hTotal; i++) 
      {
	locate(i+3, 24);
	printf("%s", helpMenu[i]);
      } locate(sel+3, 40);
      total = hTotal; y = 24;
      break;
  }//end switch

  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
		  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  locate(sel+3, y);
  if(menu == 0) printf("%s", fileMenu[sel]);
  else if(menu == 1) printf("%s", editMenu[sel]);
  else if(menu == 2) printf("%s", optionsMenu[sel]);
  else if(menu == 3) printf("%s", helpMenu[sel]);
  fflush(stdout);
  
  //********* get user input
  while(1) 
  {
    int c = getKey();
    switch(c) 
    {
      case(UP_KEY):
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	locate(sel+3, y);
	if(menu == 0) printf("%s", fileMenu[sel]);
	else if(menu == 1) printf("%s", editMenu[sel]);
	else if(menu == 2) printf("%s", optionsMenu[sel]);
	else if(menu == 3) printf("%s", helpMenu[sel]);
	if(sel <= 0) sel = total-1;
	else sel--;
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	locate(sel+3, y);
	if(menu == 0) printf("%s", fileMenu[sel]);
	else if(menu == 1) printf("%s", editMenu[sel]);
	else if(menu == 2) printf("%s", optionsMenu[sel]);
	else if(menu == 3) printf("%s", helpMenu[sel]);
	fflush(stdout);
	break;
      case(DOWN_KEY):
	setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	locate(sel+3, y);
	if(menu == 0) printf("%s", fileMenu[sel]);
	else if(menu == 1) printf("%s", editMenu[sel]);
	else if(menu == 2) printf("%s", optionsMenu[sel]);
	else if(menu == 3) printf("%s", helpMenu[sel]);
	if(sel >= total-1) sel = 0;
	else sel++;
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	locate(sel+3, y);
	if(menu == 0) printf("%s", fileMenu[sel]);
	else if(menu == 1) printf("%s", editMenu[sel]);
	else if(menu == 2) printf("%s", optionsMenu[sel]);
	else if(menu == 3) printf("%s", helpMenu[sel]);
	fflush(stdout);
	break;
      case(RIGHT_KEY):
	if(menu >= 3) menu = 0;
	else menu++;
	drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		" FOG: The console Form Designer ", NO);
	drawForm(); drawToolBox();
	goto draw_menus;
	break;
      case(LEFT_KEY):
	if(menu <= 0) menu = 3;
	else menu--;
	drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		" FOG: The console Form Designer ", NO);
	drawForm(); drawToolBox();
	goto draw_menus;
	break;
      case(ESC_KEY):
	drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		" FOG: The console Form Designer ", NO);
	drawForm(); drawToolBox();
	return; break;
      case(ENTER_KEY):
	if(menu == 0)
	{//if1
	  //***file menu
	  if(sel == 0) newForm();
	  else if(sel == 1) openForm();
	  else if(sel == 2) saveForm();
	  else if(sel == 3) makeProject();
	  else if(sel == 4) { restoreTerminal(); clearScreen(); exit(0); }
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		  " FOG: The console Form Designer ", NO);
	  drawForm(); drawToolBox();
	  return;
	} 
	else if(menu == 1) 
	{
	  //***edit menu
	  if(sel == 0) deleteFormTool();
	  else if(sel == 1) editFormToolText();
	  else if(sel == 2) changeFormWidth();
	  else if(sel == 3) changeFormHeight();
	  else if(sel == 4) changeFormTitle();
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		  " FOG: The console Form Designer ", NO);
	  drawForm(); drawToolBox(); drawStatusBar();
	  return;
	} 
	else if(menu == 2) 
	{
	  //***options menu
	  if(sel == 0) activeWindow = TOOLBOX_WIN;
	  else if(sel == 1) activeWindow = FORM_WIN;
	  else if(sel == 2) 
	  {
	    //edit colors menu item
	    changeColors();
	  }
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		  " FOG: The console Form Designer ", NO);
	  drawForm(); drawToolBox(); drawStatusBar();
	  return;
	} 
	else if(menu == 3) 
	{
	  //***help menu
	  if(sel == 0) showReadMe();
	  else if(sel == 1) showQuickReference();
	  else if(sel == 2) showAboutDialogBox();
	  drawBox(2, 1, SCREEN_H-1, SCREEN_W, 
		  " FOG: The console Form Designer ", NO);
	  drawForm(); drawToolBox(); drawStatusBar();
	  return;
	}//end if1
	break;
    }//switch
  }//end while
}