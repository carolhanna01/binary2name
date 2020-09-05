/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __MAIN_H

#define __MAIN_H



#include "Xh.h"

#include "std.h"

#include "const.h"

#if LIBMIKMOD

#include <mikmod.h>

#endif



Display *display;

char DisplayName[256] ;

int screen;

unsigned int depth;

unsigned int bpp;

Visual *visual;

unsigned int root_width;

unsigned int root_height;

Window root, win, win2;

unsigned int Height;

unsigned int Width;

Colormap cmap;

XGCValues gcVal;

#if MITSHM

//XShmSegmentInfo	shminfo;

XShmSegmentInfo	shminfo[2];

#endif

int CompletionType;

unsigned long gcMask;

GC gc;



extern char *screen_buffer, *screen_buffer2;

extern char *sky ;

int bool = -1;

XImage *xim;







int autorun = 0;

int dble = 0 ;

int multi = 0 ;

int mikmod = 0;

extern int player;

extern int OptionParsing (int argc , char *argv[]) ;

extern void init_keyboard(Display *);

extern void init_table_perspective(void);

extern int loop(Display *);

extern int init(char **sky_buffer,int numsky, int _depth);



extern int auto_run(void);

extern void fps(int sig);



extern void menu (char * buffer) ;

extern void liberation (void);

extern void Free_sprite (void);



#endif /* __MAIN_H */

