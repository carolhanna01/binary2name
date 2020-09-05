/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2013, 2014, 2015 (c)
 * 
 *    file: properties.c
 *    This file is part of Prime.
 *
 *    Prime is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    Prime is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with Prime.  If not, see <http://www.gnu.org/licenses/>.
 */    

#include "defs.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

void refreshPropertiesDialog();
int UREAD, UWRITE, UEXECUTE;
int GREAD, GWRITE, GEXECUTE;
int OREAD, OWRITE, OEXECUTE;
int NAME_CHANGED = 0;
int x, y, w, h;
int sel;
struct stat statbuf;

/* defined in main.c */
extern char *input_str;
extern char *tmp, *tmp2;

void refreshPropertiesDialog() 
{
  //set the user interface
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  printf("\e[%d;%dH", x+1, y+2);
  if(activeWindow == DIR_WIN) printf("Dir name:  ");
  else printf("File name: ");
  if(strlen(input_str) > 26) 
  {
    int i; for(i=0; i<24; i++) putchar(input_str[i]);
    puts("..");
  } else printf("%s", input_str);
  
  printf("\e[%d;%dH", x+3, y+2);
  printf("Permissions:");
  printf("\e[%d;%dH", x+4, y+2);
  printf("Owner:      Group:      Others:    ");
  printf("\e[%d;%dH", x+5, y+2);
  printf("[ ] Read    [ ] Read    [ ] Read   ");
  printf("\e[%d;%dH", x+6, y+2);
  printf("[ ] Write   [ ] Write   [ ] Write  ");
  printf("\e[%d;%dH", x+7, y+2);
  printf("[ ] Execute [ ] Execute [ ] Execute");
  setScreenColors(FG_COLOR[COLOR_BUTTONS], BG_COLOR[COLOR_BUTTONS]);
  printf("\e[%d;%dH   OK   ", x+9, y+10);
  printf("\e[%d;%dH CANCEL ", x+9, y+22);

  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  if(UREAD) { printf("\e[%d;%dHX", x+5, y+3); }
  if(UWRITE) { printf("\e[%d;%dHX", x+6, y+3); }
  if(UEXECUTE) { printf("\e[%d;%dHX", x+7, y+3); }
  if(GREAD) { printf("\e[%d;%dHX", x+5, y+15); }
  if(GWRITE) { printf("\e[%d;%dHX", x+6, y+15); }
  if(GEXECUTE) { printf("\e[%d;%dHX", x+7, y+15); }
  if(OREAD) { printf("\e[%d;%dHX", x+5, y+27); }
  if(OWRITE) { printf("\e[%d;%dHX", x+6, y+27); }
  if(OEXECUTE) { printf("\e[%d;%dHX", x+7, y+27); }
  
  setScreenColors(FG_COLOR[COLOR_HIGHLIGHT_TEXT], BG_COLOR[COLOR_HIGHLIGHT_TEXT]);
  if(sel == 0) {
    printf("\e[%d;%dH", x+1, y+13);
    if(strlen(input_str) > 26) 
    {
      int i; for(i=0; i<24; i++) putchar(input_str[i]);
      puts("..");
    } else printf("%s", input_str);
  } else if(sel == 1) {
    printf("\e[%d;%dHRead", x+5, y+6);
  } else if(sel == 2) {
    printf("\e[%d;%dHRead", x+5, y+18);
  } else if(sel == 3) {
    printf("\e[%d;%dHRead", x+5, y+30);
  } else if(sel == 4) {
    printf("\e[%d;%dHWrite", x+6, y+6);
  } else if(sel == 5) {
    printf("\e[%d;%dHWrite", x+6, y+18);
  } else if(sel == 6) {
    printf("\e[%d;%dHWrite", x+6, y+30);
  } else if(sel == 7) {
    printf("\e[%d;%dHExecute", x+7, y+6);
  } else if(sel == 8) {
    printf("\e[%d;%dHExecute", x+7, y+18);
  } else if(sel == 9) {
    printf("\e[%d;%dHExecute", x+7, y+30);
  } else if(sel == 10) {
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    printf("\e[%d;%dH   OK   ", x+9, y+10);
  } else if(sel == 11) {
    setScreenColors(FG_COLOR[COLOR_HBUTTONS], BG_COLOR[COLOR_HBUTTONS]);
    printf("\e[%d;%dH CANCEL ", x+9, y+22);
  }//end if
  fflush(stdout);
}

void showPropertiesDialog() 
{
  //input_str = (char *) malloc(MAX_FILE_NAME_LEN);
  //if(!input_str) { msgBox("Insufficient memory", OK, ERROR); return; }
  if(activeWindow == DIR_WIN) strcpy(input_str, dirs[firstVisDir+selectedDir]);
  else strcpy(input_str, files[firstVisFile+selectedFile]);
  
  UREAD = UWRITE = UEXECUTE = 0;
  GREAD = GWRITE = GEXECUTE = 0;
  OREAD = OWRITE = OEXECUTE = 0;
  sel = 0;
  h = 10; w = 40;
  x = (SCREEN_H/2)-(h/2);
  y = (SCREEN_W/2)-(w/2);

  //******read dir/file permissions ******//
  //char *tmp = (char *) malloc(MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN+1);
  //if(!tmp) { msgBox("Insufficient memory", OK, ERROR); return; }
  memset(tmp, '\0', MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN);
  strcpy(tmp, cwd);
  strcat(tmp, "/");
  if(activeWindow == DIR_WIN) strcat(tmp, dirs[firstVisDir+selectedDir]);
  else strcat(tmp, files[firstVisFile+selectedFile]);
  lstat(tmp, &statbuf);
  if(statbuf.st_mode & S_IRUSR) { UREAD=1; }
  if(statbuf.st_mode & S_IWUSR) { UWRITE=1; }
  if(statbuf.st_mode & S_IXUSR) { UEXECUTE=1; }
  if(statbuf.st_mode & S_IRGRP) { GREAD=1; }
  if(statbuf.st_mode & S_IWGRP) { GWRITE=1; }
  if(statbuf.st_mode & S_IXGRP) { GEXECUTE=1; }
  if(statbuf.st_mode & S_IROTH) { OREAD=1; }
  if(statbuf.st_mode & S_IWOTH) { OWRITE=1; }
  if(statbuf.st_mode & S_IXOTH) { OEXECUTE=1; }

  //draw window
  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
  drawBox(x, y, x+h, w+y, " Properties ", YES);
  refreshPropertiesDialog();

  /////////////////////////////////////
  //Loop for user input  
  /////////////////////////////////////
  int c;
  int endme = 0;
  while(!endme) 
  {
    c = getKey();
    switch(c) 
    {
      case(TAB_KEY):
	if(sel >= 0) sel++;
	if(sel > 11) sel = 0;
	break;
      case('p'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_up;
	break;
      case(UP_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_up:
	if(sel == 0) sel = 10;
	else if(sel >= 10) sel = 7;
	else if(sel >= 1 && sel <= 3) sel = 0;
	else if(sel >= 4 && sel <= 9) sel -= 3;
	break;
      case('n'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_down;
	break;
      case(DOWN_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_down:
	if(sel == 0) sel = 1;
	else if(sel >= 10) sel = 0;
	else if(sel >= 7 && sel <= 9) sel = 10;
	else if(sel >= 1 && sel <= 6) sel += 3;
	break;
      case('f'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_right;
	break;
      case(RIGHT_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_right:
	if(sel > 0) sel++;
	if(sel > 11) sel = 0;
	break;
      case('b'):
	if(GNU_DOS_LEVEL > 1 && CTRL) goto do_left;
	break;
      case(LEFT_KEY):
	if(GNU_DOS_LEVEL > 1) break;
do_left:
	if(sel > 0) sel--;
	break;
      case('g'):
	if(GNU_DOS_LEVEL < 3) break;
	if(!CTRL) break;
	goto do_esc;
      case(ESC_KEY):
	if(GNU_DOS_LEVEL > 2) break;
do_esc:
	endme = 1;
	break;
      case(SPACE_KEY):
      case(ENTER_KEY):
	if(sel == 0)
	{
	  inputBoxI("Enter new name: ", input_str, " Rename... " );
	  if(strlen(input)) { strcpy(input_str, input); NAME_CHANGED = 1; }
	  setScreenColors(FG_COLOR[COLOR_WINDOW], BG_COLOR[COLOR_WINDOW]);
	  drawBox(x, y, x+h, w+y, " Properties ", YES);
	} else if(sel == 1) UREAD=!UREAD;
	else if(sel == 4) UWRITE=!UWRITE;
	else if(sel == 7) UEXECUTE=!UEXECUTE;
	else if(sel == 2) GREAD=!GREAD;
	else if(sel == 5) GWRITE=!GWRITE;
	else if(sel == 8) GEXECUTE=!GEXECUTE;
	else if(sel == 3) OREAD=!OREAD;
	else if(sel == 6) OWRITE=!OWRITE;
	else if(sel == 9) OEXECUTE=!OEXECUTE;
	else if(sel == 11) endme = 1;
	else if(sel == 10) 
	{
	  mode_t mode = 00;
	  if(UREAD) mode += 0400;
	  if(UWRITE) mode += 0200;
	  if(UEXECUTE) mode += 0100;
	  if(GREAD) mode += 040;
	  if(GWRITE) mode += 020;
	  if(GEXECUTE) mode += 010;
	  if(OREAD) mode += 04;
	  if(OWRITE) mode += 02;
	  if(OEXECUTE) mode += 01;
	  if(chmod(tmp, mode) != 0) 
	  {
	    msgBox(strerror(errno), OK, ERROR);
	  }
	  if(NAME_CHANGED) 
	  {
	    //char *tmp2 = (char *) malloc(MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN+1);
	    //if(!tmp2) { msgBox("Insufficient memory", OK, ERROR); return; }
	    memset(tmp2, '\0', MAX_DIR_NAME_LEN+MAX_FILE_NAME_LEN);
	    strcpy(tmp2, cwd);
	    strcat(tmp2, "/");
	    strcat(tmp2, input);
	    if(rename(tmp, tmp2) != 0)
	      msgBox(strerror(errno), OK, ERROR);
	    //free(tmp2);
	  }
	  endme = 1;
	}//end outer if
	break;
    }//end switch
    refreshPropertiesDialog();
  }//end while
  //free(input_str);
  //free(tmp);
}