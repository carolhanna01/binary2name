/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: options.c
 *    This file is part of the GnuDOS project.
 *
 *    GnuDOS is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    GnuDOS is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with GnuDOS.  If not, see <http://www.gnu.org/licenses/>.
 */    
#include "fog.h"
#include "options.h"
#include "../corelib/kbd.h"
#include <pwd.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

struct passwd *pass;//will be used to find the home dir

int x, y, w, x, sel;
int oldFGColors[6];
int oldBGColors[6];

char *changeColorsDialogOptions[6] =
      { "Window          ",
	"Highlighted text",
	"Menu bar        ",
	"Status bar      ",
	"Buttons         ",
	"Selected Button "
      };

char* findColorName(int c) 
{
  switch(c) 
  {
    case(30):case(40): return "BLACK  "; break;
    case(31):case(41): return "RED    "; break;
    case(32):case(42): return "GREEN  "; break;
    case(33):case(43): return "BROWN  "; break;
    case(34):case(44): return "BLUE   "; break;
    case(35):case(45): return "MAGENTA"; break;
    case(36):case(46): return "CYAN   "; break;
    case(37):case(47): return "WHITE  "; break;
  }//end switch
  return NULL;
}

void saveOldColors() 
{
  int i;
  for(i = 0; i < 6; i++) 
  {
    oldFGColors[i] = FG_COLOR[i];
    oldBGColors[i] = BG_COLOR[i];
  }
}

void resetColors() 
{
  int i;
  for(i = 0; i < 6; i++) 
  {
    FG_COLOR[i] = oldFGColors[i];
    BG_COLOR[i] = oldBGColors[i];
  }
}

int showColorChooserDialog() 
{
  int i;
  int x1 = (SCREEN_H/2)-5;
  int y1 = (SCREEN_W/2)-10;
  drawBox(x1, y1, x1+10, y1+16, " Choose color ", YES);
  for(i = 30; i < 38; i++)
    printf("\e[%d;%dH%s", ++x1, y1+1, findColorName(i));
  printf("\e[%d;%dHCANCEL", ++x1, y1+1);
  x1 = (SCREEN_H/2)-4; i = 30;
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  printf("\e[%d;%dH%s", x1, y1+1, findColorName(i));
  fflush(stdout);
  int endme = 0;
  char c;
  
  while(!endme) 
  {
    c = getKey();
    //printf("         %c,%d", c, c);
    switch(c) 
    {
      case(UP_KEY):
	//msgBox("UP", OK, INFO);
	if(i <= 30) 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH%s", x1, y1+1, findColorName(i));
	  i = 38;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dHCANCEL", x1+i-30, y1+1);
	  //printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	} 
	else if(i == 38) 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dHCANCEL", x1+i-30, y1+1);
	  i = 37;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	} 
	else 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	  i--;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	}//end if
	fflush(stdout);
	endme = 0;
	break;
      case(DOWN_KEY):
	//msgBox("DOWN", OK, INFO);
	if(i >= 38) 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dHCANCEL", x1+i-30, y1+1);
	  //printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	  i = 30;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	} 
	else if(i == 37) 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	  //printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	  i = 38;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dHCANCEL", x1+i-30, y1+1);
	} 
	else 
	{
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	  i++;
	  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	  printf("\e[%d;%dH%s", x1+i-30, y1+1, findColorName(i));
	}//end if
	endme = 0;
	fflush(stdout);
	break;
      case(ENTER_KEY):
      case(SPACE_KEY):
	//msgBox("ENTER", OK, INFO);
	//c = getKey();
	if(i == 38) { i = 0; return 0; break; }
	endme = 1; //fflush(stdin);
	return i; break;
      case(0):
	continue; break;
      case(ESC_KEY):
      default:
	//msgBox("DEFAULT", OK, INFO);
	endme = 1; //i = 0; //fflush(stdin);
	return 0; break;
    }//end switch
  }//end while
  return i;
}

void refreshChangeColorsDialog() 
{
  int i;
  drawBox(x, y, h, w, "Change colors", YES);
  fprintf(stdout, "\e[%d;%dHForeground  Background", x+1, y+16);
  fprintf(stdout, "\e[%d;%dH  OK   RESET", h-1, y+12);
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  for(i = 0; i < changeColorsDialogOptionsN; i++) 
  {
    fprintf(stdout, "\e[%d;%dH", x+i+2, y+1);
    fprintf(stdout, "%s  %s  %s", changeColorsDialogOptions[i],
		    findColorName(FG_COLOR[i]),
		    findColorName(BG_COLOR[i]));
  }//end for
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  if(sel == 12)
    fprintf(stdout, "\e[%d;%dH  OK  ", h-1, y+12);
  else if(sel == 13)
    fprintf(stdout, "\e[%d;%dHRESET", h-1, y+19);
  else if(sel >= 0 && sel <= 5)
    fprintf(stdout, "\e[%d;%dH%s", x+sel+2, y+19, findColorName(FG_COLOR[sel]));
  else if(sel >= 6 && sel <= 11)
    fprintf(stdout, "\e[%d;%dH%s", x+(sel-6)+2, y+28, findColorName(BG_COLOR[sel-6]));
  fflush(stdout);
}


void changeColors() 
{
  x = (SCREEN_H/2)-5;
  y = (SCREEN_W/2)-18;
  w = y+38;
  h = x+10;
  //int i;
  sel = 0;//selected item: 0-5 FG_COLORS, 6-11 BG_COLORS,
	      //12 OK, 13 RESET
  saveOldColors();
  refreshChangeColorsDialog();
  int endme = 0;
  int enter_error = 0;
  //infinite loop to get user input
  while(!endme) 
  {
    char c = getKey();
    //printf("         ..%c,%d", c, c);
    switch(c) 
    {
      case(0):
	//continue; 
	break;
      case(UP_KEY):
	if(sel == 0 || sel == 6) sel = 12;
	else if(sel == 12) sel = 5;
	else if(sel == 13) sel = 11;
	else sel--;
	refreshChangeColorsDialog();
	enter_error = 0;
	break;
      case(DOWN_KEY):
	if(sel == 12) sel = 0;
	else if(sel == 13) sel = 6;
	else if(sel == 5 || sel == 6) sel = 12;
	else sel++;
	refreshChangeColorsDialog();
	enter_error = 0;
	break;
      case(LEFT_KEY):
      case(RIGHT_KEY):
	if(sel == 12) sel = 13;
	else if(sel == 13) sel = 12;
	else if(sel >= 0 && sel <= 5) sel += 6;
	else sel -= 6;
	refreshChangeColorsDialog();
	enter_error = 0;
	break;
      case(ESC_KEY):
	resetColors();
	//write_config_file();
	//refreshView();
	endme = 1;
	return; break;
      case(SPACE_KEY):
      case(ENTER_KEY):
	if(enter_error) continue;
	if(sel == 12) 
	{
	  //write_config_file();
	  //refreshView();
	  endme = 1;
	  return;
	} 
	else if(sel == 13) 
	{
	  resetColors();
	  refreshChangeColorsDialog();
	  //write_config_file();
	} 
	else 
	{
	  int tmp;
	  enter_error = !enter_error;
	  if(!enter_error) continue;
	  if(sel >= 0 && sel <= 5) 
	  {
	    tmp = showColorChooserDialog();
	    if(tmp) FG_COLOR[sel] = tmp;
	    //else { sprintf(lines[0], "%d", tmp); return; }
	    //c = DOWN_KEY;
	    //msgBox("1", OK, INFO);
	    refreshChangeColorsDialog();
	    //msgBox("2", OK, INFO);
	    //if(!X_IS_RUNNING) c = getKey();
	    break;
	  } 
	  else if(sel >= 6 && sel <= 11) 
	  {
	    tmp = showColorChooserDialog();
	    if(tmp) BG_COLOR[sel-6] = tmp+10;
	    //else { sprintf(lines[0], "%d", tmp); return; }
	    //c = DOWN_KEY;
	    refreshChangeColorsDialog();
	    //if(!X_IS_RUNNING) c = getKey();
	    break;
	  }
	}//end if
	break;
    }//end switch
    //printf("%d..%c", c, c);
    //msgBox("3", OK, INFO);
    //refreshChangeColorsDialog();
    //printf("         ..%c,%d", c, c);
  }//end while
  
}
