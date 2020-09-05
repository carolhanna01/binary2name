/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: screen.c
 *    This file is part of mino.
 *
 *    mino is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    mino is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with mino.  If not, see <http://www.gnu.org/licenses/>.
 */    

#include "defs.h"
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>	//included for terminal size query

void setScreenColors(int FG, int BG) 
{
  fprintf(stdout, "\x1b[%d;%dm", FG, BG);	//control sequence to set screen color
}

void getScreenSize() 
{
 struct winsize w;
 ioctl(0, TIOCGWINSZ, &w);	//find the size of the view
 SCREEN_H = w.ws_row;
 SCREEN_W = w.ws_col;
}

void clearScreen() 
{
 fprintf(stdout, "\x1b[2J");
 fprintf(stdout, "\x1b[37;4m");
 fprintf(stdout, "\e[3J\e[1;1H");
 fflush(stdout);
}
