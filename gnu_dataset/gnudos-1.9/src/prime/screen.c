/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014, 2015 (c)
 * 
 *    file: screen.c
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

#include "screen.h"
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>	//included for terminal size query

int FG_COLOR[color_components];
int BG_COLOR[color_components];

/* 
 * Set Screen Colors to ForeGround (FG) and BackGround (BG) colors 
 */
void setScreenColors(int FG, int BG) 
{
  //control sequence to set screen color
  fprintf(stdout, "\x1b[%d;%dm", FG, BG);
}

/*
 * Get Screen Size
 */
void getScreenSize() 
{
 struct winsize w;
 ioctl(0, TIOCGWINSZ, &w);	//find the size of the view
 //printf("Rows: %d\n", w.ws_row);
 //printf("Cols: %d\n", w.ws_col);
 SCREEN_H = w.ws_row;
 SCREEN_W = w.ws_col;
}

/* 
 * Clear the screen with the specified colors 
 */
void clearScreenC(int FG, int BG) 
{
  fprintf(stdout, "\e[2J");
  fprintf(stdout, "\e[%d;%dm", FG, BG);
  fprintf(stdout, "\e[3J\e[1;1H");
}

/* 
 * Clear the screen 
 */
void clearScreen() 
{
 fprintf(stdout, "\e[2J");
 fprintf(stdout, "\x1b[37;4m");
 fprintf(stdout, "\e[3J\e[1;1H");
}

/* 
 * Set the cursor at the given row and column 
 */
void locate(int row, int col) 
{
  fprintf(stdout, "\e[%d;%dH", row, col);
  fflush(stdout);
}

/* 
 * Get Screen Colors 
 */
void getScreenColors() 
{
  screen_colors[0] = "BLACK";
  screen_colors[1] = "RED";
  screen_colors[2] = "GREEN";
  screen_colors[3] = "BROWN";
  screen_colors[4] = "BLUE";
  screen_colors[5] = "MAGENTA";
  screen_colors[6] = "CYAN";
  screen_colors[7] = "WHITE";
  screen_colors[8] = "BGBLACK";
  screen_colors[9] = "BGRED";
  screen_colors[10] = "BGGREEN";
  screen_colors[11] = "BGBROWN";
  screen_colors[12] = "BGBLUE";
  screen_colors[13] = "BGMAGENTA";
  screen_colors[14] = "BGCYAN";
  screen_colors[15] = "BGWHITE";
}

/* 
 * Load Default Colors into the Color Array 
 */
void loadDefaultColors() 
{
  FG_COLOR[COLOR_WINDOW]         = 37;
  FG_COLOR[COLOR_HIGHLIGHT_TEXT] = 34;
  FG_COLOR[COLOR_MENU_BAR]       = 34;
  FG_COLOR[COLOR_STATUS_BAR]     = 34;
  FG_COLOR[COLOR_BUTTONS]        = 37;
  FG_COLOR[COLOR_HBUTTONS]       = 32;
  BG_COLOR[COLOR_WINDOW]         = 49;
  BG_COLOR[COLOR_HIGHLIGHT_TEXT] = 47;
  BG_COLOR[COLOR_MENU_BAR]       = 47;
  BG_COLOR[COLOR_STATUS_BAR]     = 47;
  BG_COLOR[COLOR_BUTTONS]        = 41;
  BG_COLOR[COLOR_HBUTTONS]       = 41;
}

void showCursor()
{
  //turn the cursor on
  printf("\e[?25h");
  fflush(stdout);
}

void hideCursor()
{
  //turn the cursor off
  printf("\e[?25l");
  fflush(stdout);
}

void reset_attribs()
{
  printf("\x1b[0m");
  fflush(stdout);
}
