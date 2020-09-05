/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: msgbox.c
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "defs.h"
#include "kbd.h"
#include "options.h"

/* how long is the input? */
int inputLen = 0;
/* which char is under the cursor? */
int highlightChar = 0;
/* which char is the first in input line? (used when scrolling a long input) */
int firstVisChar = 0;

void putunichar(char *ch)
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

/**************************************************************************
 * inputBox(): 
 * Procedure to show an input box containing a message, specific buttons,
 * empty field for user input, and a caption (according to passed 
 * msgType). It returns the input string if the user presses OK, or a NULL 
 * pointer if he pressed Cancel or entered an empty string.
 * ************************************************************************/
char* inputBox(char *msg, char *title) 
{
  int msgW = 0;
  int msgH = 0;
  int j, i = 0;

  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  j = 0;
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { msgH++; j = 0; }
    else 
    { 
      if(j > MAX_MSG_BOX_W) { j = 0; msgH++; }
      if ((msg[i] & mask[0]) == mask[0]) i++;
      if ((msg[i] & mask[1]) == mask[1]) i++;
      if ((msg[i] & mask[2]) == mask[2]) i++;
      j++; 
      if(j > msgW) msgW = j; 
    }
  }
  msgH += 6;
  msgW += 3;	//adjust box size
  //draw the empty window first//
  int x, y;
  x = (SCREEN_H/2)-(msgH/2);
  y = (SCREEN_W-msgW)/2;
  drawBox( x,
	   y,
	  (SCREEN_H/2)+(msgH/2),
	  (SCREEN_W/2)+(msgW/2), title, YES);
  
  y+=2; j=x+1; x++;
  int l=y;
  fprintf(stdout, "\e[%d;%dH", j, l);
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { l=y; fprintf(stdout, "\e[%d;%dH", ++j, l); }
    else 
    { 
      if(l >= MAX_MSG_BOX_W) 
      {
	l=y;
	fprintf(stdout, "\e[%d;%dH", j, l);
      } putunichar(msg+i);
      if ((msg[i] & mask[0]) == mask[0]) i++;
      if ((msg[i] & mask[1]) == mask[1]) i++;
      if ((msg[i] & mask[2]) == mask[2]) i++;
    }
  }

 /* put an empty field for user entry */
 setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
 fprintf(stdout, "\e[%d;%dH", x+(msgH-4), y);
 for(i = 0; i < msgW-3; i++) putchar(' ');
 
 /* then draw button(s) */
 int bx, by;
 int sel = 2;	//selection: 0=OK, 1=CANCEL, 2=INPUT FIELD

 memset(input, '\0', MAX_INPUT_MSG_LEN*4);
 
 bx = x + (msgH-2);
 by = y + ((msgW-16)/2) - 2;
 setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
 fprintf(stdout, "\x1b[%d;%dH", bx, by);
 fprintf(stdout, "   OK   ");
 by += 12;
 fprintf(stdout, "\x1b[%d;%dH", bx, by);
 fprintf(stdout, " CANCEL ");
 fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
  
 fflush(stdout);
 //wait for user response//
 char *ch;
 ch = (char *)malloc(5);
 
 while(1) 
 {	//infinite program loop//
    ch = getKey();
    switch(ch[0]) 
    {
      case('g'):
	if(GNU_DOS_LEVEL > 2 && CTRL) goto do_esc;
	goto enterInputChar;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
	firstVisChar = 0; highlightChar = 0; inputLen = 0;
	return NULL;
	break;
      case(SPACE_KEY):
	if(sel == 2) 
	{		//if pressed space in input field, insert the space
	  goto enterInputChar;
	  break;
	}			//if pressed space on a button, fall through to ENTER below
      case(ENTER_KEY):
	if(sel == 0 || sel == 2) 
	{	//pressed ENTER on OK button or on the input field
	  firstVisChar = 0; highlightChar = 0;
	  if(inputLen <= 0) return NULL;	//if no input entered, return NULL
	  inputLen = 0;
	  return input;				//otherwise return the input
	}
	if(sel == 1) 
	{
	  firstVisChar = 0; highlightChar = 0; inputLen = 0;
	  return NULL;		//return NULL also if selected CANCEL
	}
	break;
      case('f'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_right;
	goto enterInputChar;
      case(RIGHT_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_right:
	if(highlightChar >= inputLen) break;	//already at last char
	//if(inputLen > msgW-3) { 
	//}
	if(highlightChar >= msgW-4) 
	{	//need to scroll string
	   if(inputLen <= firstVisChar+msgW-4) break;//can't go further right
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
	   for(i = ++firstVisChar; i <= (firstVisChar+msgW-4); i++) putunichar(input+(i*4));
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	} 
	else 
	{	//no need to scroll string, just output the char
	      highlightChar++;
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	} fflush(stdout); break;
      case('b'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_left;
	goto enterInputChar;
      case(LEFT_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_left:
      	if(firstVisChar == 0 && highlightChar == 0) break;	//already at first char
	if(highlightChar == 0 && firstVisChar != 0) 
	{	//need to scroll string
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
	   for(i = --firstVisChar; i <= (firstVisChar+msgW-4); i++) putunichar(input+(i*4));
	   fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
	} 
	else 
	{				//no need to scroll string, just output the char
	      highlightChar--;
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	} fflush(stdout); break;
      case(TAB_KEY):
	  bx = x + (msgH-2);
	  by = y + ((msgW-16)/2) - 2;
	  if(sel == 0) 
	  {
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   OK   ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, " CANCEL ");
	    sel = 1;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);	//adjust cursor to point at "CANCEL"
	  } 
	  else if(sel == 1) 
	  {
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   OK   ");
	    by += 12;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, " CANCEL ");
	    sel = 2;
	    fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	  } 
	  else 
	  {
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   OK   ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, " CANCEL ");
	    sel = 0;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);	//adjust cursor to point at "OK"
	  }
	fflush(stdout); break;
      case('d'):
	if(GNU_DOS_LEVEL > 3 && CTRL) goto do_del;
	goto enterInputChar;
      case(DEL_KEY):
	if(GNU_DOS_LEVEL > 3) break;
do_del:
	if((firstVisChar+highlightChar) == (inputLen))
	  break;	//can't delete.. at the last char
	for(i = firstVisChar+highlightChar; i < inputLen-1; i++) 
	{
	  input[i*4] = input[i*4+4];
	  input[i*4+1] = input[i*4+5];
	  input[i*4+2] = input[i*4+6];
	  input[i*4+3] = input[i*4+7];
	}
	input[(--inputLen)*4] = '\0';
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	for(i = highlightChar; i < (msgW-3); i++) 
	{
	  if(input[(firstVisChar*4)+(i*4)] == '\0') putchar(' ');
	  else putunichar(input+(firstVisChar*4)+(i*4));
	}
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	fflush(stdout); break;
      case(BACKSPACE_KEY):
	if(highlightChar == 0) 
	{
	  if(firstVisChar == 0) break;	//at first char
	  firstVisChar--;
	  //for(i = firstVisChar; i < inputLen-1; i++) input[i] = input[i+1];
	  for(i = firstVisChar; i < inputLen-1; i++) 
	  {
	    input[i*4] = input[i*4+4];
	    input[i*4+1] = input[i*4+5];
	    input[i*4+2] = input[i*4+6];
	    input[i*4+3] = input[i*4+7];
	  }
	  input[(--inputLen)*4] = '\0';
	} 
	else 
	{ //end if
	  highlightChar--;
	  //shift the chars one place to the left
	  //for(i = firstVisChar+highlightChar; i < inputLen-1; i++) input[i] = input[i+1];
	  for(i = firstVisChar+highlightChar; i < inputLen; i++) 
	  {
	    input[i*4] = input[i*4+4];
	    input[i*4+1] = input[i*4+5];
	    input[i*4+2] = input[i*4+6];
	    input[i*4+3] = input[i*4+7];
	  }
	  input[(--inputLen)*4] = '\0';
	}//end else
	setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
	for(i = firstVisChar; i < (firstVisChar+msgW-3); i++) 
	{
	  if(input[i*4] == '\0') putchar(' ');
	  else putunichar(input+(i*4));
	}
	fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	fflush(stdout); break;
      default:
	  //if((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')  ||
	  //   (ch >= 32 && ch<= 64) || (ch >=123 && ch <= 126)) {//if it is alphanumeric
enterInputChar:
	    if(strlen(input)/4 >= MAX_INPUT_MSG_LEN) break;
	    setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
	    //inserting in the middle of a string means we need to shift all chars one position
	    //to the right before inserting the new char at the highlighted position.
	    if(input[highlightChar*4] != '\0') 
	    {
	      //for(i = inputLen; i > firstVisChar+highlightChar; i--) input[i] = input[i-1];
	      for(i = inputLen; i > firstVisChar+highlightChar; i--)
	      {
		input[i*4] = input[i*4-4];
		input[i*4+1] = input[i*4-3];
		input[i*4+2] = input[i*4-2];
		input[i*4+3] = input[i*4-1];
	      }
	    }
	    input[(firstVisChar+highlightChar)*4] = ch[0];
	    if((ch[0] & mask[0]) == mask[0])
	      input[(firstVisChar+highlightChar)*4+1] = ch[1];
	    else input[(firstVisChar+highlightChar)*4+1] = ' ';
	    if((ch[0] & mask[1]) == mask[1])
	      input[(firstVisChar+highlightChar)*4+2] = ch[2];
	    else input[(firstVisChar+highlightChar)*4+2] = ' ';
	    if((ch[0] & mask[2]) == mask[2])
	      input[(firstVisChar+highlightChar)*4+3] = ch[3];
	    else input[(firstVisChar+highlightChar)*4+3] = ' ';
	    highlightChar++;
	    inputLen++;
	    if(highlightChar >= msgW-3) 
	    {	//need to scroll string
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y);	//adjust cursor to point at input field
	      highlightChar--;
	      for(i = ++firstVisChar; i <= (firstVisChar+msgW-4); i++) 
		putunichar(input+(i*4));
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+msgW-4);	//adjust cursor to point at input field
	    } 
	    else 
	    {				//no need to scroll string, just output the char
	      putunichar(input+((highlightChar-1)*4)); 
	      //highlightChar++;
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	      if(inputLen > firstVisChar+highlightChar) 
	      {	//there are some chars to the right side
		//for(i = firstVisChar+highlightChar; i < inputLen; i++) putchar(input[i]);
		for(i = highlightChar; i < (msgW-4); i++) 
		{
		  if(input[(firstVisChar+i)*4] == '\0') putchar(' ');
		  else putunichar(input+((firstVisChar+i)*4));
		}
	      }
	      fprintf(stdout, "\x1b[%d;%dH", bx-2, y+highlightChar);	//adjust cursor to point at input field
	      //fprintf(stdout, "\x1b[%d;%dH", bx-2, y+2);	//adjust cursor to point at input field
	    }
	  //}
	  fflush(stdout); 
    }
 }
  //do {;} while (!getchar());

  //free(tmpC);
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  //setScreenColors(WHITE, BGBLUE);
  return NULL;
}


/***************************************
 * msgBox(): 
 * Procedure to show a message box 
 * containing a message, specific buttons,
 * and a caption (according to passed 
 * msgType).
 * **************************************/
int msgBox(char *msg, int Buttons, msgType tmsg) 
{
  int msgW = 0;
  int msgH = 0;
  int j, i = 0;
  //char *k[10];
  char *title;
  switch(tmsg) 
  {
    case(INFO): title = " INFORMATION "; break;
    case(ERROR): title = " ERROR "; break;
    case(CONFIRM): title = " CONFIRMATION "; break;
    default: title = " MESSAGE "; break;
  }

  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  //setScreenColors(WHITE, BGBLUE);
  j = 0;
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { msgH++; j = 0; }
    else 
    { 
      if(j > MAX_MSG_BOX_W) { j = 0; msgH++; }
      if ((msg[i] & mask[0]) == mask[0]) i++;
      if ((msg[i] & mask[1]) == mask[1]) i++;
      if ((msg[i] & mask[2]) == mask[2]) i++;
      j++; 
      if(j > msgW) msgW = j; 
    }
  }
  msgH += 5;
  msgW += 3;	//adjust box size
  //msgW = MAX_MSG_BOX_W;
  //draw the empty window first//
  int x, y;
  x = (SCREEN_H/2)-(msgH/2);
  y = (SCREEN_W-msgW)/2;
  drawBox( x,
	   y,
	  (SCREEN_H/2)+(msgH/2),
	  (SCREEN_W/2)+(msgW/2), title, YES);
  
  y+=2; j=x+1; x++;
  int l=y;
  fprintf(stdout, "\e[%d;%dH", j, l);
  for(i = 0; i < strlen(msg); i++) 
  {
    if(msg[i] == '\n') { l=y; fprintf(stdout, "\e[%d;%dH", ++j, l); }
    else 
    { 
      if(l >= MAX_MSG_BOX_W) 
      {
	l=y;
	fprintf(stdout, "\e[%d;%dH", j, l);
      } putunichar(msg+i);
      if ((msg[i] & mask[0]) == mask[0]) i++;
      if ((msg[i] & mask[1]) == mask[1]) i++;
      if ((msg[i] & mask[2]) == mask[2]) i++;
    }
  }

 //then draw button(s)//
 int bx, by;
 int sel = 0;
  if(Buttons == 5) 
  {	// OK/CANCEL combination
    bx = x + (msgH-3);
    by = y + ((msgW-16)/2) - 2;
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    //setScreenColors(GREEN, BGRED);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   OK   ");
    by += 12;
    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    //setScreenColors(WHITE, BGRED);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, " CANCEL ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);	//adjust cursor to point at "OK"
  } 
  else if(Buttons == 10) 
  {	// YES/NO combination
    bx = x + (msgH-3);
    by = y + ((msgW-16)/2) - 2;
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    //setScreenColors(GREEN, BGRED);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   YES  ");
    by += 12;
    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
    //setScreenColors(WHITE, BGRED);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   NO   ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);	//adjust cursor to point at "YES"
  } 
  else 
  {			// OK only
    bx = x + (msgH-3);
    by = y + ((msgW-8)/2) - 2;
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    //setScreenColors(GREEN, BGRED);
    fprintf(stdout, "\x1b[%d;%dH", bx, by);
    fprintf(stdout, "   OK   ");
    fprintf(stdout, "\x1b[%d;%dH", bx, by+3);	//adjust cursor to point at "OK"
  }
  
 fflush(stdout); 
 //wait for user response//
 char *ch;
 ch = (char *)malloc(5);
 
 while(1) 
 {	//infinite program loop//
    ch = getKey();
    switch(ch[0]) 
    {
      case('g'):
	if(GNU_DOS_LEVEL > 2 && CTRL) goto do_esc;
	break;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
	return CANCEL; break;
      case(SPACE_KEY):
      case(ENTER_KEY): 	//remember, Buttons = 5 for OK/CANEL, and = 10 for YES/NO
	if(sel == 0 && Buttons == 5) return OK;
	if(sel == 0 && Buttons == 10) return YES;
	if(sel == 1 && Buttons == 5) return CANCEL;
	if(sel == 1 && Buttons == 10) return NO;
	if(sel == 0 && Buttons == 1) return OK;
	break;
      case('b'):
      case('f'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_tab;
	break;
      case(RIGHT_KEY):
      case(LEFT_KEY):
	if(GNU_DOS_LEVEL > 1) break;
      case(TAB_KEY):
do_tab:
	  bx = x + (msgH-3);
	  by = y + ((msgW-16)/2) - 2;
	  if(sel == 0 && Buttons == 5) 
	  {
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   OK   ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, " CANCEL ");
	    sel = 1;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);	//adjust cursor to point at "CANCEL"
	  } 
	  else if(sel == 0 && Buttons == 10) 
	  {
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   YES  ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   NO   ");
	    sel = 1;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by+1);	//adjust cursor to point at "NO"
	  } 
	  else if(sel == 1 && Buttons == 5) 
	  {
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   OK   ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, " CANCEL ");
	    sel = 0;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);	//adjust cursor to point at "OK"
	  } 
	  else if(sel == 1 && Buttons == 10) 
	  {
	    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
	    //setScreenColors(GREEN, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   YES  ");
	    by += 12;
	    setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
	    //setScreenColors(WHITE, BGRED);
	    fprintf(stdout, "\x1b[%d;%dH", bx, by);
	    fprintf(stdout, "   NO   ");
	    sel = 0;
	    fprintf(stdout, "\x1b[%d;%dH", bx, by-9);	//adjust cursor to point at "YES"
	  }
	fflush(stdout); break;
    }
 }
  //do {;} while (!getchar());

  //free(tmpC);
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  //setScreenColors(WHITE, BGBLUE);
  return(0);
}
