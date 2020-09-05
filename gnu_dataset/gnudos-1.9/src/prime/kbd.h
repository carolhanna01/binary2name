/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: kbd.h
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
#ifndef __KBD_H
#define __KBD_H

#include "ukbd.h"
#include "unistd.h"
#include "linux/kd.h"
#include "termios.h"
#include "fcntl.h"
#include "string.h"
#include "sys/ioctl.h"
#include <stdlib.h>
#include <stdio.h>

extern unsigned short mask[];

/* Define the boolean type */
#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif

/* Control (Meta) Keys */
bool ALT;
bool CTRL;
bool SHIFT;
bool CAPS;
bool INSERT;

/* Initialize the terminal for program use */
int initTerminal();
/* Restore the terminal before program exits */
void restoreTerminal();
//int init_kbd();
//void restore_kbd();

/* A flag indicating if we are running under X */
bool X_IS_RUNNING;
/* Pilot function to get next key press */
int getKey();

/* Control and Function key definitions */
#define ESC_KEY		27
#define BACKSPACE_KEY	8
#define TAB_KEY		9
#define ENTER_KEY	13
#define CAPS_KEY	1
#define SHIFT_KEY	2
#define CTRL_KEY	3
#define ALT_KEY		4
#define SPACE_KEY	32
#define UP_KEY		5
#define DOWN_KEY	6
#define LEFT_KEY	7
#define RIGHT_KEY	10
#define DEL_KEY		11
#define	HOME_KEY	12
#define END_KEY		14
#define INS_KEY		15
#define SHIFT_DOWN	17
#define SHIFT_UP	18
#define PGUP_KEY	19
#define PGDOWN_KEY	20
#define F1_KEY		21
#define F2_KEY		22
#define F3_KEY		23
#define F4_KEY		24
#define F5_KEY		25
#define F6_KEY		26
#define F7_KEY		28
#define F8_KEY		29
#define F9_KEY		30
#define F10_KEY		31
#define F11_KEY		33
#define F12_KEY		34
#define NUM_KEY		35
#define SCRL_KEY	36

#endif /* __KBD_H */
