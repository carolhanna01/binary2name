/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: kbd.c
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
#include "unistd.h"
#include "linux/kd.h"
#include "termios.h"
#include "fcntl.h"
#include "sys/ioctl.h"
#include <stdlib.h>
#include <stdio.h>
#include "kbd.h"
#include <stdlib.h>
#include <string.h>	//for memset()

unsigned short mask[] = {192, 224, 240};

static struct termios tty_attr_old;
static int old_keyboard_mode;

int initTerminal()
{
    struct termios tty_attr;

    /* save old keyboard mode */
    if (ioctl(0, KDGKBMODE, &old_keyboard_mode) < 0) 
    {
	X_IS_RUNNING = 1;
    } else X_IS_RUNNING = 0;

    tcgetattr(0, &tty_attr_old);

    /* turn off buffering, echo and key processing */
    tty_attr = tty_attr_old;
    tty_attr.c_lflag &= ~(ICANON | ECHO | ISIG);
    tty_attr.c_iflag &= ~(ISTRIP | INLCR | ICRNL | IGNCR | IXON | IXOFF);
    if((tcsetattr(0, TCSANOW, &tty_attr) == -1))
      return 0;

    if(!X_IS_RUNNING) ioctl(0, KDSKBMODE, K_RAW);
    //ioctl(0, KDSKBMODE, K_UNICODE);
    ALT = 0; 
    CTRL = 0; 
    SHIFT = 0;
    return 1;
}

void restoreTerminal()
{
    if(X_IS_RUNNING) 
    {
	    tcsetattr(0, TCSANOW, &tty_attr_old);
    }
    else
    {
	    tcsetattr(0, TCSAFLUSH, &tty_attr_old);
	    ioctl(0, KDSKBMODE, old_keyboard_mode);
    }
}//end restoreTerminal()

char *getKeyUnderConsole()
{
    char buf[5];
    int res;
    int bytes = 0;
    memset(uc, 0, 5);//set the unicode buffer into zeroes
    while (1) 
    {
      res = read(0, &buf[0], 1);
      if(res < 0) { uc[0] = 0; break; }
      //fprintf(stdout, "%c[%d]", buf[0], buf[0]);
      if ((buf[0] & mask[0]) == mask[0]) bytes++;
      if ((buf[0] & mask[1]) == mask[1]) bytes++;
      if ((buf[0] & mask[2]) == mask[2]) bytes++;
      
	switch (buf[0]) 
	{
	//scancodes for keypresses
	case 0x01: uc[0] = ESC_KEY; break;
	case 0x29 : uc[0] = SHIFT?'~':'`'; break;
	case 0x02 : uc[0] = SHIFT?'!':'1'; break;
	case 0x03 : uc[0] = SHIFT?'@':'2'; break;
	case 0x04 : uc[0] = SHIFT?'#':'3'; break;
	case 0x05 : uc[0] = SHIFT?'$':'4'; break;
	case 0x06 : uc[0] = SHIFT?'%':'5'; break;
	case 0x07 : uc[0] = SHIFT?'^':'6'; break;
	case 0x08 : uc[0] = SHIFT?'&':'7'; break;
	case 0x09 : uc[0] = SHIFT?'*':'8'; break;
	case 0x0a : uc[0] = SHIFT?'(':'9'; break;
	case 0x0b : uc[0] = SHIFT?')':'0'; break;
	case 0x0c : uc[0] = SHIFT?'_':'-'; break;
	case 0x0d : uc[0] = SHIFT?'+':'='; break;
	case 0x0e : uc[0] = BACKSPACE_KEY; break;
	case 0x0f : uc[0] = TAB_KEY; break;
	case 0x10 : uc[0] = SHIFT?'Q':'q'; break;
	case 0x11 : uc[0] = SHIFT?'W':'w'; break;
	case 0x12 : uc[0] = SHIFT?'E':'e'; break;
	case 0x13 : uc[0] = SHIFT?'R':'r'; break;
	case 0x14 : uc[0] = SHIFT?'T':'t'; break;
	case 0x15 : uc[0] = SHIFT?'Y':'y'; break;
	case 0x16 : uc[0] = SHIFT?'U':'u'; break;
	case 0x17 : uc[0] = SHIFT?'I':'i'; break;
	case 0x18 : uc[0] = SHIFT?'O':'o'; break;
	case 0x19 : uc[0] = SHIFT?'P':'p'; break;
	case 0x1a : uc[0] = SHIFT?'{':'['; break;
	case 0x1b : uc[0] = SHIFT?'}':']'; break;
	case 0x1c : uc[0] = ENTER_KEY; break;
	case 0x3a : uc[0] = CAPS_KEY; break;
	case 0x1e : uc[0] = SHIFT?'A':'a'; break;
	case 0x1f : uc[0] = SHIFT?'S':'s'; break;
	case 0x20 : uc[0] = SHIFT?'D':'d'; break;
	case 0x21 : uc[0] = SHIFT?'F':'f'; break;
	case 0x22 : uc[0] = SHIFT?'G':'g'; break;
	case 0x23 : uc[0] = SHIFT?'H':'h'; break;
	case 0x24 : uc[0] = SHIFT?'J':'j'; break;
	case 0x25 : uc[0] = SHIFT?'K':'k'; break;
	case 0x26 : uc[0] = SHIFT?'L':'l'; break;
	case 0x27 : uc[0] = SHIFT?':':';'; break;
	case 0x28 : uc[0] = SHIFT?'"':'\''; break;
	case 0x2b : uc[0] = SHIFT?'|':'\\'; break;
	case 0x2a : SHIFT = 1; uc[0] = SHIFT_DOWN; break;
	case 0x56 : uc[0] = SHIFT?'>':'<'; break;
	case 0x2c : uc[0] = SHIFT?'Z':'z'; break;
	case 0x2d : uc[0] = SHIFT?'X':'x'; break;
	case 0x2e : uc[0] = SHIFT?'C':'c'; break;
	case 0x2f : uc[0] = SHIFT?'V':'v'; break;
	case 0x30 : uc[0] = SHIFT?'B':'b'; break;
	case 0x31 : uc[0] = SHIFT?'N':'n'; break;
	case 0x32 : uc[0] = SHIFT?'M':'m'; break;
	case 0x33 : uc[0] = SHIFT?'<':','; break;
	case 0x34 : uc[0] = SHIFT?'>':'.'; break;
	case 0x35 : uc[0] = SHIFT?'?':'/'; break;
	case 0x36 : SHIFT = 1; uc[0] = SHIFT_DOWN; break;
	case 0x1d : CTRL = 1; uc[0] = 0; break;
	case 0x38 : ALT = 1; uc[0] = 0; break;
	case 0x39 : uc[0] = SPACE_KEY; break;
	//scancodes for keyreleases
	case -86: SHIFT = 0; uc[0] = SHIFT_UP; break;
	case -99: CTRL = 0; uc[0] = 0; break;
	case -72: ALT = 0; uc[0] = 0; break;
	case -74: SHIFT = 0; uc[0] = SHIFT_UP; break;
	case -32:
                res = read(0, &buf[0], 1);
		if(buf[0] == 73) { uc[0] = PGUP_KEY; break; }
		if(buf[0] == 81) { uc[0] = PGDOWN_KEY; break; }
		if(buf[0] == 72) { uc[0] = UP_KEY; break; }
		if(buf[0] == 71) { uc[0] = HOME_KEY; break; }
		if(buf[0] == 79) { uc[0] = END_KEY; break; }
		if(buf[0] == 82) { uc[0] = INS_KEY; break; }
		if(buf[0] == 83) { uc[0] = DEL_KEY; break; }
		if(buf[0] == 75) { uc[0] = LEFT_KEY; break; }
		if(buf[0] == 80) { uc[0] = DOWN_KEY; break; }
		if(buf[0] == 77) { uc[0] = RIGHT_KEY; break; }
		if(buf[0] == 29) { CTRL = 1; uc[0] = 0; break; }
		if(buf[0] == -99) { CTRL = 0; uc[0] = 0; break; }
		if(buf[0] == 56) { ALT = 1; uc[0] = 0; break; }
		if(buf[0] == -72) { ALT = 0; uc[0] = 0; break; }
		else uc[0] = 0; break;

        default:
	  uc[0] = buf[0];
	  int i = 0;
	  while(i < bytes) 
	  {
	    res = read(0, &buf[0], 1);
	    uc[++i] = buf[0];
	  }//end while
	  break;
	}//end switch
	//res = read(0, &buf[0], 1);
	break;
    }
    return uc;
}



char *getKeyUnderX()
{
  int c;
  //char uc[5];
  int bytes = 0;
  memset(uc, 0, 5);//set the unicode buffer into zeroes
  //uc[0] = '\0';
  
  ALT = 0; CTRL = 0; SHIFT = 0;
    while(1) 
    {
      c = getchar();
      if(c < 0) continue;
      //printf("%c[%d]",  c, c);
      //fflush(stdout);
      //bytes = 0;
      if ((c & mask[0]) == mask[0]) bytes++;
      if ((c & mask[1]) == mask[1]) bytes++;
      if ((c & mask[2]) == mask[2]) bytes++;
    
      //fprintf(stdout, "..%c, %d..", c, c);
      if(c == 127)
      { 
	uc[0] = BACKSPACE_KEY;
      }
      else if(c == 31)
      { 
	   c = getchar();
	   CTRL=1; uc[0] = '/';
      }
      else if(c == 26) { CTRL=1; uc[0] = 'z'; }
      else if(c == 25) { CTRL=1; uc[0] = 'y'; }
      else if(c == 03) { CTRL=1; uc[0] = 'c'; }
      else if(c == 24) { CTRL=1; uc[0] = 'x'; }
      else if(c == 22) { CTRL=1; uc[0] = 'v'; }
      else if(c == 15) { CTRL=1; uc[0] = 'o'; }
      else if(c == 11) { CTRL=1; uc[0] = 'k'; }
      else if(c == 16) { CTRL=1; uc[0] = 'p'; }
      else if(c == 07) { CTRL=1; uc[0] = 'g'; }
      else if(c == 06) { CTRL=1; uc[0] = 'f'; }
      else if(c == 05) { CTRL=1; uc[0] = 'e'; }
      else if(c == 04) { CTRL=1; uc[0] = 'd'; }
      else if(c == 02) { CTRL=1; uc[0] = 'b'; }
      else if(c == 01) { CTRL=1; uc[0] = 'a'; }
      else if(c == 19) { CTRL=1; uc[0] = 's'; }
      else if(c == 18) { CTRL=1; uc[0] = 'r'; }
      else if(c == 14) { CTRL=1; uc[0] = 'n'; }
      else if(c == 17) { CTRL=1; uc[0] = 'q'; }
      else if(c == 23) { CTRL=1; uc[0] = 'w'; }
      else if(c == 00) { CTRL=1; uc[0] = SPACE_KEY; }
      else if(c == 32) 
      {	//the SPACEBAR is pressed
	uc[0] = SPACE_KEY;
      }
      else if(c == 10 || c == 13) 
      {	//the ENTER is pressed
	uc[0] = ENTER_KEY;
      }
      else if(c == 9) 
      {	//the TAB is pressed
	uc[0] = TAB_KEY;
      }
      else if(c == 27) 
      {	//ESC key pressed -- maybe starting an escape sequence??
	c = getchar();
	//fprintf(stdout, "..%c, %d..", c, c);
	if((c == 'f') || (c == 'F')) { ALT=1; uc[0] = 'f'; }
	else if((c == 'e') || (c == 'E')) { ALT=1; uc[0] = 'e'; }
	else if((c == 'h') || (c == 'H')) { ALT=1; uc[0] = 'h'; }
	else if((c == 'o') || (c == 'O')) 
	{ 
	  c = getchar();
	  if(c == 72) { uc[0] = HOME_KEY; }
	  else if(c == 70) { uc[0] = END_KEY; }
	  else { ALT=1; uc[0] = 'o'; }	//if c == 111
	}
	else if((c == 'b') || (c == 'B')) { ALT=1; uc[0] = 'b'; }
	else if((c == 'v') || (c == 'V')) { ALT=1; uc[0] = 'v'; }
	else if((c == 'd') || (c == 'D')) { ALT=1; uc[0] = 'd'; }
	else if(c == 127) { ALT=1; uc[0] = BACKSPACE_KEY; }
	else if(c == 91) 
	{	//yep -- this is the left bracket '[' -- so there is something coming
	  c = getchar();
	  //fprintf(stdout, "..%c, %d..", c, c);
	  if(c == 65) { uc[0] = UP_KEY; }
	  else if(c == 66) { uc[0] = DOWN_KEY; }
	  else if(c == 67) { uc[0] = RIGHT_KEY; }
	  else if(c == 68) { uc[0] = LEFT_KEY; }
	  else if(c == 72) { uc[0] = HOME_KEY; }
	  else if(c == 70) { uc[0] = END_KEY; }
	  else if(c == 54) { c = getchar(); uc[0] = PGDOWN_KEY; }
	  else if(c == 53) { c = getchar(); uc[0] = PGUP_KEY; }
	  else if(c == 50) { c = getchar(); uc[0] = INS_KEY; }
	  else if(c == 51) 
	  { 
	    c = getchar(); 
	    if(c == 126) uc[0] = DEL_KEY;
	    else if(c == 59) 
	    { 
	      c = getchar();
	      c = getchar();
	      //if(c == 53) { c = getchar; 
	      CTRL = 1; uc[0] = DEL_KEY; //}
	    }
	  }
	  else if(c == 49)
	  { //this is CTRL-something
	    CTRL = 1; 
	    c = getchar();
	    //fprintf(stdout, "..%c, %d..", c, c);
	    if(c == 59) 
	    {
	      c = getchar();
	      if(c == 53 || c == 50)
	      {
		c = getchar();
		if(c == 67) uc[0] = RIGHT_KEY; 
		else if(c == 68) uc[0] = LEFT_KEY; 
		else if(c == 65) uc[0] = UP_KEY; 
		else if(c == 66) uc[0] = DOWN_KEY; 
		else if(c == 72) uc[0] = HOME_KEY; 
		else if(c == 70) uc[0] = END_KEY; 
	      }
	    }//end if
	  }//end if(c == 49)
	}//end if(c == 91)
	else uc[0] = ESC_KEY;
      }//end if(c == 27)
      ////////////////////////////////////////////
      ////////////////////////////////////////////
      //if(c >= 32 && c <= 126)
      else 
      {
	uc[0] = c;
	int i = 0;
	while(i < bytes)
	{
	  c = getchar();
	  uc[++i] = c;
	}//end while
      }//end else
      break;
    }
    return uc;
}



char *getKey()
{
	if(X_IS_RUNNING) return getKeyUnderX();
	else return getKeyUnderConsole();
}

