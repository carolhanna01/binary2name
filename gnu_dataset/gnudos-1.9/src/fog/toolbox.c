/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: textbox.c
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

void drawToolBox() 
{
 setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
 drawBox(3, SCREEN_W-15, SCREEN_H-1, SCREEN_W, " Toolbox ", YES);
 int i;
 for(i = 0; i < total_tools; i++) 
 {
   printf("\e[%d;%dH", i+4, SCREEN_W-14);
   printf("%s", tool[i]);
 }
 
 if(activeWindow == TOOLBOX_WIN) 
 {
   setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
		   BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
   printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
   printf("%s", tool[selected_tool]);
 }
 fflush(stdout);
}

void getInputFromToolBoxWin() 
{
  int c;
  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14
		  +(int)strlen(tool[selected_tool]));
  while(1) 
  {
    c = getKey();
    switch(c) 
    {
      case(LEFT_KEY):
      case(UP_KEY):
	if(selected_tool <= 0) 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	  selected_tool = total_tools-1;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	} 
	else 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	  selected_tool--;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	}
	fflush(stdout);
	break;
      case(TAB_KEY):
      case(RIGHT_KEY):
      case(DOWN_KEY):
	if(selected_tool >= total_tools-1)
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	  selected_tool = 0;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	} 
	else 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	  selected_tool++;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			  BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH", selected_tool+4, SCREEN_W-14);
	  printf("%s", tool[selected_tool]);
	}
	fflush(stdout);
	break;
      case(ESC_KEY):
	restoreTerminal(); exit(0); break;
      case('f'):
	if(ALT) { showMenu(0); break; }
	adding_tool = 0;
	activeWindow = FORM_WIN;
	drawToolBox();
	drawStatusBar();
	return;
	break;
      case(SPACE_KEY):
      case(ENTER_KEY):
	adding_tool = 1;
	activeWindow = FORM_WIN;
	drawToolBox();
	return;
	break;
      case('s'):
	if(CTRL) saveForm();
	break;
      case('o'):
	if(CTRL) { openForm(); return; }
	if(ALT) showMenu(2);
	break;
      case('n'):
	if(CTRL) { newForm(); return; }
	break;
      case('q'):
	if(CTRL) { endme = 1; return; }
	break;
      case('w'):
	if(CTRL) makeProject();
	break;
      case('e'): if(ALT) showMenu(1); break;
      case('h'): if(ALT) showMenu(3); break;
    }//end switch
  }//end while
}