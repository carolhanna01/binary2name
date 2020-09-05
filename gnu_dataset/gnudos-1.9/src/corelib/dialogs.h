/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: dialogs.h
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

#include <signal.h>
#include <stdio.h>
#include "kbd.h"
#include "screen.h"

#ifndef __DIALOGS_H
#define __DIALOGS_H

/* Define boolean type */
#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif
/* Define a point */
typedef struct { int row; int col; } point;

/* buttons used in message boxes */
#define OK	1	//00000001
#define YES	2	//00000010
#define CANCEL	4	//00000100
#define NO	8	//00001000
#define ALL	16	//00010000
#define ABORT	32	//00100000
//button combinations-->  00000101	OK/CANCEL  = 5
			//00001010	YES/NO     = 10
			//00011010	YES/ALL/NO = 26

/* types of message displayed in message boxes */
typedef enum msgt { INFO, ERROR, CONFIRM } msgtype;

/* Message box width and height */
int MAX_MSG_BOX_W;
int MAX_MSG_BOX_H;
/* Maximum allowed length of a user input in an input box */
#define MAX_INPUT_MSG_LEN 100

//Function prototypes//

/* Draw an empty box, with an optional title
 * clearArea is a flag to indicate whether to clear the window with spaces
 */
void drawBox(int x1, int y1, int x2, int y2, char *title, int clearArea);
void drawBoxP(point p1, point p2, char *title, int clearArea);

/* Draw a message box with the given message and preset buttons */
int msgBox(char *msg, int buttons, msgtype tmsg);

/* Draw an input box with a message and a title */
char *inputBox(char *msg, char *title);

/* Draw an input box with a message, a title and an initial input value */
char *inputBoxI(char *msg, char *inputValue, char *title);

/* input string returned by inputBox() function */
char input[MAX_INPUT_MSG_LEN+1];

/* Print a unicode character to stdout */
void uputchar(char *ch);

int x, y, w, h;

/* Functions to catch system signals */
int catchSignals();
int catchAllSignals();
//static sig_atomic_t end = 0;
struct sigaction sa;

/* User defined signal handler - optional, can be an empty function */
void sighandler(int signo);

/* Show about dialog box */
int show_about(char *msg);

/* Show ReadMe window:
 * If you don't know what GNU_DOS_LEVEL is, or you don't want to
 * use it, pass 0. Also, if you pass NULL for title the window
 * title will be set to README by default.
 */
int show_readme(char* readme, char *title, int GNU_DOS_LEVEL);

#endif /* __DIALOGS_H */