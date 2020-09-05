/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: kbd.h
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
#ifndef __KBD_H
#define __KBD_H

/* mask values for bit pattern of first byte in multi-byte
     UTF-8 sequences: 
       192 - 110xxxxx - for U+0080 to U+07FF 
       224 - 1110xxxx - for U+0800 to U+FFFF 
       240 - 11110xxx - for U+010000 to U+1FFFFF */
extern unsigned short mask[]; // = {192, 224, 240};

#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif

bool ALT;
bool CTRL;
bool SHIFT;
bool CAPS;
bool INSERT;

int initTerminal();
void restoreTerminal();

bool X_IS_RUNNING;
char *getKey();
char *getKeyUnderConsole();
char *getKeyUnderX();

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

//the Unicode character returned by getKey()
char uc[5];
#endif
