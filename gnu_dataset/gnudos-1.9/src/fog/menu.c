/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: menu.c
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
#define EOL	10

void showAboutDialogBox() 
{
	    char *x = " Fog: The console Form Designer for GNU/Linux.\
\n Developed by Mohammed Isam, 2014. ";
	    msgBox(x, OK, INFO);
}

void showQuickReference() 
{
	    drawBox(3, 5, 18, SCREEN_W-5, "Quick Reference", YES);
	    fprintf(stdout, "\e[4;7HBasic functions:");
	    fprintf(stdout, "\e[5;9HT    : Activate the toolbox");
	    fprintf(stdout, "\e[6;9HF    : Activate the form");
	    fprintf(stdout, "\e[7;9HENTER: Add a tool (if Toolbox active)");
	    fprintf(stdout, "\e[8;9H       Edit tool text (if Form active)");
	    fprintf(stdout, "\e[10;7DELETE: Remove tool");
	    fprintf(stdout, "\e[11;9HMenus:");
	    fprintf(stdout, "\e[12;9HALT+F: Open file menu");
	    fprintf(stdout, "\e[13;9HALT+E: Open edit menu");
	    fprintf(stdout, "\e[14;9HALT+O: Open options menu");
	    fprintf(stdout, "\e[15;9HALT+H: Open help menu");
	    fprintf(stdout, "\e[16;9HCTRL+Q: Quit the program");
	    fprintf(stdout, "\e[17;7HPress any key to continue..");
	    //char c = getKey();
	    do {;} while(getKey() < 0);
}

/******************************************
 * this function shows a window with 
 * README file as its contents.
 * ****************************************/
void showReadMe() 
{
  int x = 2;
  int y = 2;
  int w = SCREEN_W-1;
  int h = SCREEN_H-1;
  FILE *README;
  char buf[4096*4];	//buffer to hold data
  int buf_len = 0;
  
  if(!(README = fopen("/usr/share/doc/gnudos/fog/README", "r"))) 
  {
   if(!(README = fopen("/usr/local/share/doc/gnudos/fog/README", "r"))) 
   {
    msgBox("Failed to open README file!.", OK, ERROR);
    return;
   }
  }
  
  if(!(buf_len = fread(buf, sizeof(char), sizeof(buf), README))) 
  {
    msgBox("Failed to read the README file!.", OK, ERROR);
    return;
  }
  
  drawBox(x, y, h, w, " README ", YES);
  
  y++; x++;
  int i = 0, j = x;
  int l = y;
  int lineStart[60];
  int firstVisLine = 0;
  int cnt = 0;
  int ch;
  char moreLines = 1;	//used as boolean to indicate if still more lines
  lineStart[0] = 0;
  
  fprintf(stdout, "\e[%d;%dH", j, l);
  //for(i = 0; i < buf_len; i++) {
  while(j <= (h-x+2)) 
  {
    if(buf[i] == EOL) 
    { 
      lineStart[++cnt] = i+1;
      l=y;
      fprintf(stdout, "\e[%d;%dH", ++j, l); 
    }
    else 
    { 
      if(l >= (w-y+2)) 
      {
	lineStart[++cnt] = i;
	l=y;
	fprintf(stdout, "\e[%d;%dH", ++j, l);
      } putchar(buf[i]); l++;
    }
    if(++i >= buf_len) break;
  }
  drawBox(x-1, y-1, h, w, " README ", NO);
  
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
    switch(ch) 
    {
      case(ENTER_KEY):
      case(SPACE_KEY):
      case(ESC_KEY):
	goto end;
	break;
      case(UP_KEY):
	if(firstVisLine == 0) break;	//can't go up--first line already
	firstVisLine--;
	break;
      case(DOWN_KEY):
	if(!moreLines) break;	//reached last line
	firstVisLine++;
	break;
    }	//end of switch
    //redraw the box with its contents
    drawBox(x-1, y-1, h, w, " README ", YES);
    j = x; l = y; cnt = firstVisLine; i = lineStart[firstVisLine];
    
    fprintf(stdout, "\e[%d;%dH", j, l);
    while(j <= (h-x+2)) 
    {
     if(buf[i] == EOL) 
     { 
      lineStart[++cnt] = i+1;
      l=y;
      fprintf(stdout, "\e[%d;%dH", ++j, l); 
     } 
     else 
     { 
      if(l >= (w-y+2)) 
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
    drawBox(x-1, y-1, h, w, " README ", NO);

  }	//end of outer while
end:
  fclose(README);
  return;
}
