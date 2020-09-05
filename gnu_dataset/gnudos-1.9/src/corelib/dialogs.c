/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: dialogs.c
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

#include "dialogs.h"
#include "kbd.h"
#include "screen.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int inputLen = 0;	//how long is the input?
int highlightChar = 0;	//which char is under the cursor?
int firstVisChar = 0;	//which char is the first in input line? 
			//(used when scrolling a long input)

/*
 * Output a Unicode character to the output stream.
 */
void uputchar(char *ch)
{
  static char c[5];
  memset(c, 0, 5);
  c[0] = ch[0];
  if ((c[0] & mask[0]) == mask[0]) c[1] = ch[1];
  if ((c[0] & mask[1]) == mask[1]) c[2] = ch[2];
  if ((c[0] & mask[2]) == mask[2]) c[3] = ch[3];
  c[4] = '0';
  printf("%s", c);
}

/*
 * Common prologue used by all dialog functions.
 */
void __dialog_prologue(int *msgW, int *msgH, char *msg, char *title, int *x, int *y)
{
  int i, j;
  MAX_MSG_BOX_H = SCREEN_H-2;
  MAX_MSG_BOX_W = SCREEN_W-2;

  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  j = 0;
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { (*msgH)++; j = 0; }
    else 
    { 
      if(j > MAX_MSG_BOX_W) { j = 0; (*msgH)++; }
      j++; 
      if(j > (*msgW)) (*msgW) = j;
    }
  }
  *msgH += 4;
  *msgW += 3;	//adjust box size
  if(*msgW < 34) *msgW = 34;
  if(*msgH % 2) (*msgH)++;
  if(*msgH > MAX_MSG_BOX_H) *msgH = MAX_MSG_BOX_H;
  //draw the empty window first//
  *x = (SCREEN_H/2)-((*msgH)/2);
  *y = (SCREEN_W-(*msgW))/2;
  drawBox( *x,
	   *y,
	  (SCREEN_H/2)+((*msgH)/2),
	  (SCREEN_W/2)+((*msgW)/2), title, YES);
  
  (*y) += 2; j = (*x)+1; (*x)++;
  int l=(*y);
  fprintf(stdout, "\e[%d;%dH", j, l);
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { l=(*y); fprintf(stdout, "\e[%d;%dH", ++j, l); }
    else 
    { 
      if(l >= MAX_MSG_BOX_W)
      {
	l=(*y);
	fprintf(stdout, "\e[%d;%dH", j, l);
      } putchar(msg[i]);
    }
  }
}

void __draw_dialog_buttons(int *msgW, int *msgH, int buttons, int *x, int *y, int sel)
{
  int bx, by;
  if(buttons == 5) 
  {	// OK/CANCEL combination
    bx = *x + ((*msgH)-2);
    by = *y + (((*msgW)-16)/2) - 2;
    if(sel == 0)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   OK   ");
    by += 12;
    if(sel == 1)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, " CANCEL ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);//adjust cursor to point at "OK"
  } 
  else if(buttons == 10) 
  {	// YES/NO combination
    bx = *x + ((*msgH)-2);
    by = *y + (((*msgW)-16)/2) - 2;
    if(sel == 0)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   YES  ");
    by += 12;
    if(sel == 1)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   NO   ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);//adjust cursor to point at "YES"
  } 
  else if(buttons == 26) 
  {	// YES/ALL/NO combination
    bx = *x + ((*msgH)-2);
    by = *y + (((*msgW)-24)/2) - 2;
    if(sel == 0)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   YES  ");
    by += 10;
    if(sel == 1)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   ALL  ");
    by += 10;
    if(sel == 2)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   NO   ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by-17);//adjust cursor to point at "YES"
  }
  else 
  {			// OK only
    bx = *x + ((*msgH)-2);
    by = *y + (((*msgW)-8)/2) - 2;
    if(sel == 0)
      setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    else
      setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   OK   ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by+3);//adjust cursor to point at "OK"
  }
}

/**************************************************************************
 * inputBoxI(): 
 * Procedure to show an input box containing a message, specific buttons,
 * input field with inputValue as starting value, and a caption
 * (according to passed msgType). It returns the input string if
 * the user presses OK, or a NULL pointer if he pressed Cancel
 * or entered an empty string.
 * ************************************************************************/
char *inputBoxI(char *msg, char *inputValue, char *title) 
{
  int msgW = 0;
  int msgH = 0;
  int i = 0;
  int x, y;

  showCursor();
  __dialog_prologue(&msgW, &msgH, msg, title, &x, &y);

  memset((void *)input, 0, MAX_INPUT_MSG_LEN);
  //if passed an initial input value, load it into 'input'
  if(inputValue)
  {
    if(strlen(inputValue) > MAX_INPUT_MSG_LEN)
      strncpy(input, inputValue, MAX_INPUT_MSG_LEN);
    else strcpy(input, inputValue);
  }
  inputLen = strlen(input);
 
  //put an empty field for user entry
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
		 BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  fprintf(stdout, "\e[%d;%dH", x+(msgH-4), y);
  if(strlen(input) > msgW-3)
    for(i = 0; i < msgW-3; i++) putchar(input[i]);
  else
  {
    printf("%s", input);
    printf("%*s", (int)(msgW-3-strlen(input)), " ");
  }
 
  //then draw button(s)//
  int bx, by;
  int sel = 2;	//selection: 0=OK, 1=CANCEL, 2=INPUT FIELD
 
  __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, -1);
  bx = x + (msgH-2);
  by = y + ((msgW-16)/2) + 10;
  //adjust cursor to point at input field
  fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
  
  fflush(stdout);
  //wait for user response//
  int ch;
  while(1) 
  {	//infinite program loop//
    ch = getKey();
    switch(ch) 
    {
      case(ESC_KEY):
	firstVisChar = 0; highlightChar = 0; inputLen = 0;
	strcpy(input, "\0");
	return NULL;
	break;
      case(SPACE_KEY):
	if(sel == 2) 
	{	//if pressed space in input field, insert the space
	  goto enterInputChar;
	  break;
	}	//if pressed space on a button, fall through to ENTER below
      case(ENTER_KEY):
	if(sel == 0 || sel == 2)
	{	//pressed ENTER on OK button or on the input field
	  //if no input entered, return NULL
	  if(inputLen <= 0) { strcpy(input, "\0"); return NULL; }
	  firstVisChar = 0; highlightChar = 0; inputLen = 0;
	  return input;				//otherwise return the input
	}
	if(sel == 1) 
	{
	  firstVisChar = 0; highlightChar = 0; inputLen = 0;
	  strcpy(input, "\0");
	  return NULL;		//return NULL also if selected CANCEL
	}
	break;
      case(RIGHT_KEY):
	if(firstVisChar+highlightChar >= inputLen) break;	//already at last char
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	if(highlightChar >= msgW-4) 
	{	//need to scroll string
	   if(inputLen <= firstVisChar+msgW-4) break;//can't go further right
	   //adjust cursor to point at input field
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
	   for(i = ++firstVisChar; i <= (firstVisChar+msgW-4); i++)
	   {
	     if(input[i] == '\0') putchar(' ');
	     else putchar(input[i]);
	   }
	   //adjust cursor to point at input field
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	} 
	else 
	{		//no need to scroll string, just output the char
	      highlightChar++;
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	} fflush(stdout); break;
      case(LEFT_KEY):
      	if(firstVisChar == 0 && highlightChar == 0) 
	  break;	//already at first char
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	if(highlightChar == 0 && firstVisChar != 0)
	{	//need to scroll string
	   //adjust cursor to point at input field
	  fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
	   for(i = --firstVisChar; i <= (firstVisChar+msgW-4); i++) 
	     putchar(input[i]);
	   //adjust cursor to point at input field
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
	} 
	else 
	{		//no need to scroll string, just output the char
	      highlightChar--;
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	} fflush(stdout); break;
      case(TAB_KEY):
	  if(sel == 0)
	  {
	    sel = 1;
	    __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, sel);
	    //adjust cursor to point at "CANCEL"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);
	  } 
	  else if(sel == 1) 
	  {
	    sel = 2;
	    __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, sel);
	    //adjust cursor to point at input field
	    fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	  } 
	  else 
	  {
	    sel = 0;
	    __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, sel);
	    //adjust cursor to point at "OK"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);
	  }
	fflush(stdout); break;
      case(DEL_KEY):
	if((firstVisChar+highlightChar) == (inputLen))
	  break;	//can't delete.. at the last char
	for(i = firstVisChar+highlightChar; i < inputLen-1; i++) 
	  input[i] = input[i+1];
	input[--inputLen] = '\0';
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	//adjust cursor to point at input field
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	for(i = highlightChar; i < (msgW-3); i++) 
	{
	  if(input[firstVisChar+i] == '\0') putchar(' ');
	  else putchar(input[firstVisChar+i]);
	}
	//adjust cursor to point at input field
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	fflush(stdout); break;
      case(BACKSPACE_KEY):
	if(highlightChar == 0) 
	{
	  if(firstVisChar == 0) break;	//at first char
	  firstVisChar--;
	  for(i = firstVisChar; i < inputLen-1; i++) input[i] = input[i+1];
	  input[--inputLen] = '\0';
	} //end if
	else 
	{ 
	  highlightChar--;
	  //shift the chars one place to the left
	  for(i = firstVisChar+highlightChar; i < inputLen-1; i++) 
	    input[i] = input[i+1];
	  input[--inputLen] = '\0';
	}//end else
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	//adjust cursor to point at input field
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
	for(i = firstVisChar; i < (firstVisChar+msgW-3); i++) 
	{
	  if(input[i] == '\0') putchar(' ');
	  else putchar(input[i]);
	}
	//adjust cursor to point at input field
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	fflush(stdout); break;
      default:
	  if((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')  ||
	     (ch >= 32 && ch<= 64) || (ch >=123 && ch <= 126)) 
	  {	//if it is alphanumeric
enterInputChar:
	    if(strlen(input) >= MAX_INPUT_MSG_LEN) break;
	    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], 
			    BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    //inserting in the middle of a string means we need to shift all
	    //chars one position to the right before inserting the new char 
	    //at the highlighted position.
	    if(input[highlightChar] != '\0') 
	    {
	      for(i = inputLen; i > firstVisChar+highlightChar; i--) 
		input[i] = input[i-1];
	    }
	    input[firstVisChar+(highlightChar++)] = ch;
	    inputLen++;
	    if(highlightChar >= msgW-3)
	    {	//need to scroll string
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y);
	      highlightChar--;
	      for(i = ++firstVisChar; i <= (firstVisChar+msgW-4); i++) 
		putchar(input[i]);
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+msgW-4);
	    } 
	    else 
	    {		//no need to scroll string, just output the char
	      putchar(input[highlightChar-1]); //highlightChar++;
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	      if(inputLen > firstVisChar+highlightChar) 
	      {	//there are some chars to the right side
		for(i = highlightChar; i < (msgW-4); i++) 
		{
		  if(input[firstVisChar+i] == '\0') putchar(' ');
		  else putchar(input[firstVisChar+i]);
		}
	      }
	      //adjust cursor to point at input field
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);
	    }
	  }
	  fflush(stdout); 
    }
  }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fflush(stdout);  
  strcpy(input, "\0");
  return NULL;
}

/**************************************************************************
 * inputBox(): 
 * Procedure to show an input box containing a message, specific buttons,
 * empty field for user input, and a caption (according to passed 
 * msgType). It returns the input string if the user presses OK, or a NULL 
 * pointer if he pressed Cancel or entered an empty string.
 * ************************************************************************/
char *inputBox(char *msg, char *title) 
{
  return inputBoxI(msg, NULL, title);
}

int _msg_box(char *msg, char *title, int buttons)
{
  int msgW = 0;
  int msgH = 0;
  int x, y;

  __dialog_prologue(&msgW, &msgH, msg, title, &x, &y);

 //then draw button(s)//
 int bx, by;
 int sel = 0;
 __draw_dialog_buttons(&msgW, &msgH, buttons, &x, &y, sel);
 fflush(stdout);  

 //wait for user response//
 int ch;
 while(1) 
 {	//infinite program loop//
    ch = getKey();
    switch(ch) 
    {
      case(ESC_KEY):
	return ABORT;
	break;
      case(SPACE_KEY):
      case(ENTER_KEY): 
	//remember, buttons = 5 for OK/CANEL, and = 10 for YES/NO
	if(sel == 0 && buttons == 5)  return OK;
	if(sel == 0 && buttons == 10) return YES;
	if(sel == 1 && buttons == 5)  return CANCEL;
	if(sel == 1 && buttons == 10) return NO;
	if(sel == 0 && buttons == 1)  return OK;
	if(sel == 0 && buttons == 26) return YES;
	if(sel == 1 && buttons == 26) return ALL;
	if(sel == 2 && buttons == 26) return NO;
	break;
      case(RIGHT_KEY):
      case(LEFT_KEY):
      case(TAB_KEY):
	  bx = x + (msgH-2);
	  by = y + ((msgW-16)/2) - 2;
	  by += 12;
	  if(sel == 0 && buttons == 26) 
	  {
	    sel = 1;
	    __draw_dialog_buttons(&msgW, &msgH, 26 /* YES|ALL|NO */, &x, &y, sel);
	    //adjust cursor to point at "ALL"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-7);
	  } 
	  else if(sel == 1 && buttons == 26) 
	  {
	    sel = 2;
	    __draw_dialog_buttons(&msgW, &msgH, 26 /* YES|ALL|NO */, &x, &y, sel);
	    //adjust cursor to point at "NO"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+3);
	  } 
	  else if(sel == 2 && buttons == 26) 
	  {
	    sel = 0;
	    __draw_dialog_buttons(&msgW, &msgH, 26 /* YES|ALL|NO */, &x, &y, sel);
	    //adjust cursor to point at "YES"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-17);
	  } 
	  else if(sel == 0 && buttons == 5) 
	  {
	    sel = 1;
	    __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, sel);
	    //adjust cursor to point at "CANCEL"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);
	  } 
	  else if(sel == 0 && buttons == 10) 
	  {
	    sel = 1;
	    __draw_dialog_buttons(&msgW, &msgH, 10 /* YES|NO */, &x, &y, sel);
	    //adjust cursor to point at "NO"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);
	  } 
	  else if(sel == 1 && buttons == 5) 
	  {
	    sel = 0;
	    __draw_dialog_buttons(&msgW, &msgH, 5 /* OK|CANCEL */, &x, &y, sel);
	    //adjust cursor to point at "OK"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);
	  } 
	  else if(sel == 1 && buttons == 10)
	  {
	    sel = 0;
	    __draw_dialog_buttons(&msgW, &msgH, 10 /* YES|NO */, &x, &y, sel);
	    //adjust cursor to point at "YES"
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);
	  }
	  fflush(stdout);  
	break;
    }
 }
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fflush(stdout);
  return(0);
}

/***************************************
 * msgBox(): 
 * Procedure to show a message box 
 * containing a message, specific buttons,
 * and a caption (according to passed 
 * msgType).
 * **************************************/
int msgBox(char *msg, int buttons, msgtype tmsg)
{
  char *title;
  switch(tmsg) 
  {
    case(INFO): title = " INFORMATION "; break;
    case(ERROR): title = " ERROR "; break;
    case(CONFIRM): title = " CONFIRMATION "; break;
    default: title = " MESSAGE "; break;
  }
  return _msg_box(msg, title, buttons);
}

/***************************************
 * drawBox(): 
 * Procedure to draw a box with the given
 * coordinates, title, and a flag
 * indicating whether to clear the window
 * area or not (passed as YES or NO).
 * **************************************/
void drawBox(int x1, int y1, int x2, int y2, char *title, int clearArea) 
{
  if(y2 <= y1) return;
  if(x2 <= x1) return;
  char spaces[y2-y1];
  int i;
  for(i = 0; i < y2-y1-1; i++) spaces[i] = ' ';
  spaces[i] = '\0';
  printf("\x1b[0m");	/* reset settings to get rid of the evil underlining! */
  if(clearArea == YES)
  {
    for(i = 1; i < (x2-x1); i++)
    {
      fprintf(stdout, "\x1b[%d;%dH", x1+i, y1+1);	//move cursor
      fprintf(stdout, "%s", spaces);
    }
  }
  
  //Draw the box first//
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  fprintf(stdout, "\x1b[%d;%dH", x1, y1);//control sequence to move cursor
  /* NOTE: this is a TERRIBLE hack! but it does the following:
   *       "\e)0" will define G1 charset to be "VT100 Graphics Mapping"
   *       "\x0e" a.k.a. ^N, activates G1 charset.
   */
  fprintf(stdout, "\e)0\x0e");
  fflush(stdout);

  putchar(ULC);			//print the upper-left corner
  for(i = 0; i < (y2-y1)-1; i++)
  {
    putchar(HB);			//print the horizontal upper bar
  }
  putchar(URC);  			//print the upper-right corner
  putchar('\n');			//finished window top, make a new line
  
  for(i = 0; i < (x2-x1)-1; i++) 
  {
    fprintf(stdout, "\x1b[%d;%dH", x1+i+1, y1);	//move cursor to left window edge
    fprintf(stdout, "%c\x1b[%d;%dH%c", VB, x1+i+1, y2, VB);
  }

  fprintf(stdout, "\x1b[%d;%dH", x2, y1);//control sequence to move cursor
  putchar(LLC);				//print the lower-left corner
  for(i = 0; i < (y2-y1)-1; i++) 
  {
    putchar(HB);			//print the horizontal lower bar
  }
  putchar(LRC);  
  //fprintf(stdout, "\e(B");		//exit Alternative Char Set (ACS) Mode
  //fprintf(stdout, "\e[0m\e(B\e)0\017\e[?5l\e7\e[r\e8");

  /* NOTE: this is a TERRIBLE hack! but it does the following:
   *       "\x0f" a.k.a. ^O, activates G0 charset.
   */
  fprintf(stdout, "\x0f");
  fflush(stdout);  
  //printf("\x1b[0m");
  
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

/***************************************
 * drawBoxP():
 * same as drawBox(), except it accepts
 * coordinates as 'point' structures.
 * *************************************/
void drawBoxP(point p1, point p2, char* title, int clearArea) 
{
  drawBox(p1.row, p1.col, p2.row, p2.col, title, clearArea);
}

int catchAllSignals() 
{
  int res = 1;
    if(signal(SIGINT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGQUIT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGABRT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGTERM, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGTSTP, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGKILL, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGSTOP, sighandler) == SIG_ERR) { res = 0; }
    return res;
}

int catchSignals() 
{
  int res = 1;
    if(signal(SIGINT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGQUIT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGABRT, sighandler) == SIG_ERR) { res = 0; }
    if(signal(SIGTERM, sighandler) == SIG_ERR) { res = 0; }
    return res;
}


/******************************************
 * show_about(): 
 * Procedure to show an 'About' dialog box 
 * containing a message, OK button,
 * and a caption (About).
 * ****************************************/
int show_about(char *msg)
{
  char *title = " ABOUT ";
  return _msg_box(msg, title, OK);
}

/******************************************
 * show_readme(): 
 * Procedure to show a 'ReadMe' window
 * containing the contents of 'README' file,
 * passed as *readme. You can pass GNU_DOS_LEVEL
 * if u don't need it. Title is provided as a
 * convenience, for example GnuDOS programs
 * use this same function to display readme
 * and keybindings windows.
 * ****************************************/
int show_readme(char *readme, char *title, int GNU_DOS_LEVEL)
{
  char *readme_err[] =
  {
    "Failed to open README file!.",
    "Error reading README file!.",
    "Insufficient memory!.",
  };
  int err = -1, i;

  FILE *README;
  char *buf;		//buffer to hold data
  long buf_len = 0;
  
  if(!(README = fopen(readme, "r")))
  {
    msgBox(readme_err[0], OK, ERROR);
    return 1;
  }
  
  i = fseek(README, 0, SEEK_END);
  buf_len = ftell(README);
  rewind(README);
  if(i == -1 || buf_len == -1) { err = 1; goto return_err; }
  buf_len += 512;
  
  buf = (char *)malloc(buf_len);
  if(!buf) { err = 2; goto return_err; }

  
  int x = 3;
  int y = 3;
  int w = SCREEN_W-4;
  int h = SCREEN_H-4;
  
  i = 0;
  int firstVisLine = 0;
  int ch;
  char moreLines = 1;	//used as boolean to indicate if still more lines
  
  int index = 0;
  int inc;
  long total_lines = 0;
  long first_char = 0;
  while((inc = fgetc(README)) != EOF)
  {
    buf[index++] = inc;
    if(inc == '\n') { i = 0; total_lines++; }
    else i++;

    if(i == w)
    {
      buf[index++] = '\n';
      buf_len++;
      total_lines++;
      i = 0;
    }
    buf_len++;
  }
  if(!total_lines)
  {
    if(index) total_lines = 1;
    else
    {
      err = 1; goto return_err;
    }
  }
  if(total_lines < h) moreLines = 0;
  else moreLines = 1;
  firstVisLine = 0;
  first_char = 0;

  
  int lines = 0;
read:
  //redraw the box with its contents
  if(title) drawBox(x-1, y-1, h+x, w+y, title, YES);
  else      drawBox(x-1, y-1, h+x, w+y, " README ", YES);
  lines = 0;

  fprintf(stdout, "\e[%d;%dH", x, y);
  i = first_char;
  while(i < buf_len)
  {
    if(buf[i] == '\n')
    {
      lines++;
      fprintf(stdout, "\e[%d;%dH", x+lines, y);
      if(lines >= h) break;
    }
    else putchar(buf[i]);
    i++;
    fflush(stdout);
  }
  if(firstVisLine+lines < total_lines) moreLines = 1;
  else moreLines = 0;
  

  while(1) 
  {
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
	/* go up */
	if(firstVisLine == 0) break;
	i = first_char-1;
	while(i >= 0)
	{
	  i--;
	  if(buf[i] == '\n') break;
	}
	i++;
	first_char = i;
	firstVisLine--;
	goto read;
	break;
      case('n'):
	if(GNU_DOS_LEVEL < 2) break;
	if(!CTRL) break;
	goto do_down;
      case(DOWN_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_down:
	/* go down */
	if(!moreLines) break;
	i = first_char;
	while(i < buf_len)
	{
	  if(buf[i] == '\n' || buf[i] == '\0') break;
	  i++;
	}
	if(i < buf_len) i++;
	first_char = i;
	firstVisLine++;
	goto read;
	break;
    }	//end of switch
  }	//end of outer while
  
end:
  fclose(README);
  return 0;
return_err:
  fclose(README);
  msgBox(readme_err[err], OK, ERROR);
  return 1;
}
