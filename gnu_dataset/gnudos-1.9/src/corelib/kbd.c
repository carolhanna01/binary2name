/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: kbd.c
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
#include "kbd.h"
#include <pthread.h>

unsigned short mask[] = {192, 224, 240};

/*int button;

typedef struct {
  int x;
  int y;
} mouse;
mouse pos;

pthread_t tid;
int kbd_buf[1024];
int MAX_KBD_BUF = 1024;
int kbd_buf_len = 0;
int end;
*/

//static unsigned short mask[] = {192, 224, 240};

static struct termios tty_attr_old;
static int old_keyboard_mode;

int getKeyUnderConsole();
int getKeyUnderX();
//void* _getKey(void *arg);
//void add_mouse_action();

/*void add_mouse_action() {
  if(X_IS_RUNNING) {
	  char b = getchar();
	  char b1 = (b & 3);	//mask the lower 2 bits ==> 00000011B
	  if(b1 == 0) button = 1;
	  if(b1 == 1) button = 2;
	  if(b1 == 2) button = 3;
	  if(b1 == 3) button = 0;
	  if(b & 4) SHIFT = 1;
	  if(b & 8) ALT = 1;
	  if(b & 16) CTRL = 1;
	  char c = getchar();
	  pos.x = c-32;
	  c = getchar();
	  pos.y = c-32;
	  //printf("%d, %d:%d\n", button, pos.x, pos.y);
	  mouse_handler(button, pos);
  }
}
*/

/******************************************
 * This function initiates a new thread
 * to watch for keyboard input and put it
 * into kbd_buf[] for the getKey() function
 * to retrieve it later.
 * ****************************************/
/*int init_kbd() {
  kbd_buf_len = 0;
  end = 0;
  if(tid) return 0;
  if(!initTerminal()) return 0;
  if(pthread_create(&tid, NULL, &_getKey, NULL) != 0)
    return 0;
  return 1;
}

void restore_kbd() {
  end = 1;
  pthread_join(tid, NULL);
  restore_mouse();
  restoreTerminal();
}
*/

/*******************************************
 * Shadow function that fills the keyboard
 * buffer with characters, to be retrieved
 * by proper function getKey().
 * *****************************************/
/*void* _getKey(void *arg) {
  while(!end) {
    if(kbd_buf_len < 0) kbd_buf_len = 0;
    if(X_IS_RUNNING) kbd_buf[kbd_buf_len] = getKeyUnderX();
    else { kbd_buf[kbd_buf_len] = getKeyUnderConsole();
      //int c = Gpm_Getc(stdin);
    }
    if(kbd_buf[kbd_buf_len] != 0) kbd_buf_len++;
    else continue;
    //check buffer length and shift if necessary
    if(kbd_buf_len >= MAX_KBD_BUF) {
      int i;
      for(i = 0; i < MAX_KBD_BUF-1; i++)
	kbd_buf[i] = kbd_buf[i+1];
      kbd_buf_len--;
    }
  }//end while
  int ret = 1;
  pthread_exit(&ret);
}
*/


/*
 * Initialize the terminal, setting the proper flags.
 */
int initTerminal()
{
    struct termios tty_attr;
    //int flags;

    /* make stdin non-blocking */
    /*
    flags = fcntl(0, F_GETFL);
    //flags |= O_NONBLOCK;
    fcntl(0, F_SETFL, flags);
    */

    /* save old keyboard mode */
    if (ioctl(0, KDGKBMODE, &old_keyboard_mode) < 0) 
    {
	X_IS_RUNNING = 1;
    } 
    else X_IS_RUNNING = 0;

    tcgetattr(0, &tty_attr_old);

    /* turn off buffering, echo and key processing */
    tty_attr = tty_attr_old;
    tty_attr.c_lflag &= ~(ICANON | ECHO | ISIG);
    tty_attr.c_iflag &= ~(ISTRIP | INLCR | ICRNL | IGNCR | IXON | IXOFF);
    tcsetattr(0, TCSANOW, &tty_attr);

    if(!X_IS_RUNNING) ioctl(0, KDSKBMODE, K_RAW);
    //ioctl(0, KDSKBMODE, K_UNICODE);
    ALT = 0; 
    CTRL = 0; 
    SHIFT = 0;
    return 1;
}


/*
 * Restore the terminal to it's previous state.
 * MUST be called before your program exits!.
 */
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


/*
 * Gets key presses under console (i.e. not under X).
 */
int getKeyUnderConsole()
{
    char buf[1];
    int res;//, i = 0;
    res = read(0, &buf[0], 1);
    if(res == -1) { return -1; }
    switch (buf[0])
    {
	//scancodes for keypresses
	case 0x01: return ESC_KEY;
	case 0x29 : return SHIFT?'~':'`';
	case 0x02 : return SHIFT?'!':'1';
	case 0x03 : return SHIFT?'@':'2';
	case 0x04 : return SHIFT?'#':'3';
	case 0x05 : return SHIFT?'$':'4';
	case 0x06 : return SHIFT?'%':'5';
	case 0x07 : return SHIFT?'^':'6';
	case 0x08 : return SHIFT?'&':'7';
	case 0x09 : return SHIFT?'*':'8';
	case 0x0a : return SHIFT?'(':'9';
	case 0x0b : return SHIFT?')':'0';
	case 0x0c : return SHIFT?'_':'-';
	case 0x0d : return SHIFT?'+':'=';
	case 0x0e : return BACKSPACE_KEY;
	case 0x0f : return TAB_KEY;
	case 0x10 : return SHIFT?'Q':'q';
	case 0x11 : return SHIFT?'W':'w';
	case 0x12 : return SHIFT?'E':'e';
	case 0x13 : return SHIFT?'R':'r';
	case 0x14 : return SHIFT?'T':'t';
	case 0x15 : return SHIFT?'Y':'y';
	case 0x16 : return SHIFT?'U':'u';
	case 0x17 : return SHIFT?'I':'i';
	case 0x18 : return SHIFT?'O':'o';
	case 0x19 : return SHIFT?'P':'p';
	case 0x1a : return SHIFT?'{':'[';
	case 0x1b : return SHIFT?'}':']';
	case 0x1c : return ENTER_KEY;
	case 0x3a : return CAPS_KEY;
	case 0x1e : return SHIFT?'A':'a';
	case 0x1f : return SHIFT?'S':'s';
	case 0x20 : return SHIFT?'D':'d';
	case 0x21 : return SHIFT?'F':'f';
	case 0x22 : return SHIFT?'G':'g';
	case 0x23 : return SHIFT?'H':'h';
	case 0x24 : return SHIFT?'J':'j';
	case 0x25 : return SHIFT?'K':'k';
	case 0x26 : return SHIFT?'L':'l';
	case 0x27 : return SHIFT?':':';';
	case 0x28 : return SHIFT?'"':'\'';
	case 0x2b : return SHIFT?'|':'\\';
	case 0x2a : SHIFT = 1; return SHIFT_DOWN;//LSH
	case 0x56 : return SHIFT?'>':'<';
	case 0x2c : return SHIFT?'Z':'z';
	case 0x2d : return SHIFT?'X':'x';
	case 0x2e : return SHIFT?'C':'c';
	case 0x2f : return SHIFT?'V':'v';
	case 0x30 : return SHIFT?'B':'b';
	case 0x31 : return SHIFT?'N':'n';
	case 0x32 : return SHIFT?'M':'m';
	case 0x33 : return SHIFT?'<':',';
	case 0x34 : return SHIFT?'>':'.';
	case 0x35 : return SHIFT?'?':'/';
	case 0x36 : SHIFT = 1; return SHIFT_DOWN;//RSH
	case 0x1d : CTRL = 1; return 0;
	case 0x38 : ALT = 1; return 0;
	case 0x39 : return SPACE_KEY;
	case 59:    return F1_KEY;
	case 60:    return F2_KEY;
	case 61:    return F3_KEY;
	case 62:    return F4_KEY;
	case 63:    return F5_KEY;
	case 64:    return F6_KEY;
	case 65:    return F7_KEY;
	case 66:    return F8_KEY;
	case 67:    return F9_KEY;
	case 68:    return F10_KEY;
	case 69:    return NUM_KEY;
	case 70:    return SCRL_KEY;
	case 87:    return F11_KEY;
	case 88:    return F12_KEY;
	//scancodes for keyreleases
	case -86: SHIFT = 0; return SHIFT_UP;//LSH
	case -99: CTRL = 0; return 0;//LCT
	case -72: ALT = 0; return 0;//LAL
	case -74: SHIFT = 0; return SHIFT_UP;//RSH
	case -32:
	  res = read(0, &buf[0], 1);
	  if(buf[0] == 73) return PGUP_KEY;
	  if(buf[0] == 81) return PGDOWN_KEY;
	  if(buf[0] == 72) return UP_KEY;
	  if(buf[0] == 71) return HOME_KEY;
	  if(buf[0] == 79) return END_KEY;
	  if(buf[0] == 82) return INS_KEY;
	  if(buf[0] == 83) return DEL_KEY;
	  if(buf[0] == 75) return LEFT_KEY;
	  if(buf[0] == 80) return DOWN_KEY;
	  if(buf[0] == 77) return RIGHT_KEY;
	  if(buf[0] == 29) { CTRL = 1; return 0;/* RCT */ }
	  if(buf[0] == -99) { CTRL = 0; return 0;/* RCT */ }
	  if(buf[0] == 56) { ALT = 1; return 0;/* RAL */ }
	  if(buf[0] == -72) { ALT = 0; return 0;/* RAL */ }
    }
    return 0;
}


/*
 * Gets key presses under X.
 */
int getKeyUnderX() 
{
  /*
   * Missing keys:
   * F1, F10, F11.
   * ^S (9), ^F (10), ^M (13).
   */
  int c;
  ALT = 0; CTRL = 0; SHIFT = 0;
  c = getchar();
  if(c < 0) return 0;
  if(c ==  1) { CTRL=1; return 'a'; }
  if(c ==  2) { CTRL=1; return 'b'; }
  if(c ==  3) { CTRL=1; return 'c'; }
  if(c ==  4) { CTRL=1; return 'd'; }
  if(c ==  5) { CTRL=1; return 'e'; }
  if(c ==  6) { CTRL=1; return 'f'; }
  if(c ==  7) { CTRL=1; return 'g'; }
  if(c ==  8) { CTRL=1; return 'h'; }
  //if(c ==  9) { CTRL=1; return 'i'; }//{ return CTRL_S_KEY; }
  if(c == 9)  { return TAB_KEY;     }
  //if(c == 10) { CTRL=1; return 'j'; }//{ return CTRL_F_KEY; }
  if(c == 10 || c == 13) { return ENTER_KEY; }
  if(c == 11) { CTRL=1; return 'k'; }
  if(c == 12) { CTRL=1; return 'l'; }
  //if(c == 13) { CTRL=1; return 'm'; }//{ return CTRL_F_KEY; }
  if(c == 14) { CTRL=1; return 'n'; }
  if(c == 15) { CTRL=1; return 'o'; }
  if(c == 16) { CTRL=1; return 'p'; }
  if(c == 17) { CTRL=1; return 'q'; }
  if(c == 18) { CTRL=1; return 'r'; }
  if(c == 19) { CTRL=1; return 's'; }
  if(c == 20) { CTRL=1; return 't'; }
  if(c == 21) { CTRL=1; return 'u'; }
  if(c == 22) { CTRL=1; return 'v'; }
  if(c == 23) { CTRL=1; return 'w'; }
  if(c == 24) { CTRL=1; return 'x'; }
  if(c == 25) { CTRL=1; return 'y'; }
  if(c == 26) { CTRL=1; return 'z'; }
  
  if(c == 28) { CTRL=1; return '\\'; }
  if(c == 29) { CTRL=1; return '5'; }
  if(c == 30) { CTRL=1; return '6'; }
  if(c == 31) { CTRL=1; return '/'; }
  if(c == 32) { return SPACE_KEY;   }

  if(c == 39) { CTRL=1; return '"'; }
  
  if(c == 44) { CTRL=1; return ','; }
  
  if(c == 46) { CTRL=1; return '.'; }
  if(c == 49) { CTRL=1; return '1'; }
  
  if(c == 27) 
  {	//ESC key pressed -- maybe starting an escape sequence??
	c = getchar();
	if((c == 'o') || (c == 'O'))
	{ 
	    c = getchar();
	    if(c == 72) { return HOME_KEY; }
	    else if(c == 70) { return END_KEY;}
	    else if(c == 81) { return F2_KEY; }
	    else if(c == 82) { return F3_KEY; }
	    else if(c == 83) { return F4_KEY; }
	    else { ALT=1; return 'o'; }	//if c == 111
	}
	if(c >= 'a' && c <= 'z')
	{
	  ALT = 1; return c;
	}
	if(c >= 'A' && c <= 'Z')
	{
	  ALT = 1; return c;
	}
	
	if(c == 10) { ALT = 1; return ENTER_KEY; }
	
	if(c == 91) 
	{	//yep -- this is the left bracket '[' -- so there is something coming
	      c = getchar();
//	      if(c == 'M') { add_mouse_action(); return 0; }
	      if(c == 65) { return UP_KEY; }
	      if(c == 66) { return DOWN_KEY; }
	      if(c == 67) { return RIGHT_KEY; }
	      if(c == 68) { return LEFT_KEY; }
	      if(c == 70) { return END_KEY; }
	      if(c == 72) { return HOME_KEY; }
	      if(c == 53)
	      {
		c = getchar();
		if(c == 126) return PGUP_KEY;
	      }
	      if(c == 54)
	      {
		c = getchar();
		if(c == 126) return PGDOWN_KEY;
	      }
	      if(c == 50)
	      {
		c = getchar();
		if(c == 48)
		{
		  c = getchar();
		  if(c == 126) return F9_KEY;
		}
		if(c == 52)
		{
		  c = getchar();
		  if(c == 126) return F12_KEY;
		}
		if(c == 126) return INS_KEY;
	      }
	      if(c == 51)
	      {
		c = getchar();
		if(c == 126) return DEL_KEY;
		if(c == 59)
		{
		  c = getchar();
		  if(c == 50)
		    if((c = getchar()) == 126) { SHIFT = 1; return DEL_KEY;}
		  if(c == 51)
		    if((c = getchar()) == 126) { ALT = 1; return DEL_KEY;  }
		  if(c == 53)
		    if((c = getchar()) == 126) { CTRL = 1; return DEL_KEY; }
		}
	      }
	      if(c == 49) 
	      { //this is CTRL-something
		c = getchar();
		if(c == 53)
		{
		  c = getchar();
		  if(c == 126) return F5_KEY;
		}
		if(c == 55)
		{
		  c = getchar();
		  if(c == 126) return F6_KEY;
		}
		if(c == 56)
		{
		  c = getchar();
		  if(c == 126) return F7_KEY;
		}
		if(c == 57)
		{
		  c = getchar();
		  if(c == 126) return F8_KEY;
		}
		
		//CTRL = 1; 
		if(c == 59) 
		{
		  c = getchar();
		  if(c == 54 || c == 53 || c == 51 || c == 50)
		  {
		    if(c == 50) SHIFT = 1;
		    if(c == 51) ALT = 1;
		    if(c == 54) { SHIFT = 1; CTRL = 1; }
		    else        CTRL  = 1;
		    c = getchar();
		    if(c == 67) return RIGHT_KEY; 
		    if(c == 68) return LEFT_KEY; 
		    if(c == 65) return UP_KEY; 
		    if(c == 66) return DOWN_KEY; 
		    if(c == 72) return HOME_KEY; 
		    if(c == 70) return END_KEY; 
		  }
		}//end if
	      }//end if(c == 49)
	}//end if(c == 91)
	if(c == 127) { ALT = 1; return BACKSPACE_KEY; }
	return ESC_KEY;
  }//end if(c == 27)
  
  if(c == 57) { CTRL=1; return '9'; }
  if(c == 94) { return '^'; }
  if(c == 95) { return '_'; }

  if(c == 127) { return BACKSPACE_KEY; }
  
  if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')  ||
     (c >= 32 && c<= 64) || (c >=123 && c <= 126))	//it is alphanumeric
	return c;

  return 0;
}


/*int getKey() {
  if(kbd_buf_len <= 0) return 0;
  int c, i;
  c = kbd_buf[0];
  for(i = 0; i < kbd_buf_len-2; i++)
    kbd_buf[i] = kbd_buf[i+1];
  kbd_buf_len--;
  return c;
}*/


/*
 * Gets the next key press from buffer.
 */
int getKey() 
{
  if(X_IS_RUNNING) return getKeyUnderX();
  else return getKeyUnderConsole();
}
